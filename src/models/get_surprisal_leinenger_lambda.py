"""
Compute surprisal at the last occurrence of the target homophone
for the Leinenger & Rayner (2013) replication.

For each sentence, we:
1. Tokenize the full sentence
2. Find the last occurrence of the homophone token(s)
3. Compute surprisal at that position (and spillover)

Processes one item at a time (no batching) to avoid any artifacts
from left-padding with attention masks.

Usage:
    python get_surprisals_leinenger.py --model EleutherAI/pythia-14m
    python get_surprisals_leinenger.py --model EleutherAI/pythia-2.8b --checkpoints 0 1000 143000
    python get_surprisals_leinenger.py --run_all
"""

import argparse
import os
import time
import torch
import pandas as pd
import numpy as np
from transformers import AutoModelForCausalLM, AutoTokenizer
from tqdm import tqdm


ALL_CHECKPOINTS = [
    0, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512,
    1000, 2000, 4000, 8000, 16000, 32000, 64000, 128000, 143000
]



ALL_MODELS = [
    "EleutherAI/pythia-14m",
    "EleutherAI/pythia-70m",
    "EleutherAI/pythia-160m",
    "EleutherAI/pythia-410m",
    "EleutherAI/pythia-1b",
    "EleutherAI/pythia-1.4b",
    "EleutherAI/pythia-2.8b",
    "EleutherAI/pythia-6.9b",
    "EleutherAI/pythia-12b",
]


def find_last_occurrence(token_ids: list[int], target_ids: list[int]) -> int | None:
    """
    Find the starting index of the last occurrence of target_ids in token_ids.
    Returns the index of the first token of the last match, or None.
    """
    last_pos = None
    for i in range(len(token_ids) - len(target_ids) + 1):
        if token_ids[i:i + len(target_ids)] == target_ids:
            last_pos = i
    return last_pos


def count_parameters(model):
    total_params = 0
    for name, parameter in model.named_parameters():
        if not parameter.requires_grad:
            continue
        total_params += parameter.numel()
    return total_params


def compute_surprisals_single(
    model, tokenizer, sentence: str, homophone: str, device: str
) -> dict:
    """
    Compute surprisal at the last occurrence of the homophone in the sentence.
    No batching — avoids padding artifacts.
    """
    input_ids = tokenizer.encode(sentence, add_special_tokens=False)

    # Try space-prefixed version first (typical in-context form)
    target_ids = tokenizer.encode(" " + homophone, add_special_tokens=False)
    last_pos = find_last_occurrence(input_ids, target_ids)

    # Fall back to no-space version
    if last_pos is None:
        target_ids = tokenizer.encode(homophone, add_special_tokens=False)
        last_pos = find_last_occurrence(input_ids, target_ids)

    if last_pos is None:
        return {
            "target_token_surprisal": float("nan"),
            "target_span_surprisal_sum": float("nan"),
            "target_span_surprisal_mean": float("nan"),
            "spillover_1_surprisal": float("nan"),
            "spillover_2_surprisal": float("nan"),
            "n_target_tokens": 0,
            "target_token_str": None,
            "match_found": False,
        }

    target_len = len(target_ids)
    input_tensor = torch.tensor([input_ids], device=device)

    with torch.no_grad():
        outputs = model(input_ids=input_tensor)
        logits = outputs.logits[0]  # (seq_len, vocab)

    log_probs = torch.log_softmax(logits, dim=-1)

    # Surprisal for each token in the target span
    span_surprisals = []
    for j in range(target_len):
        pos = last_pos + j
        token_id = input_ids[pos]
        token_log_prob = log_probs[pos - 1, token_id].item()
        span_surprisals.append(-token_log_prob)

    # Spillover: 1 and 2 tokens after the target span
    spillover_1 = float("nan")
    spillover_2 = float("nan")
    spill_start = last_pos + target_len
    if spill_start < len(input_ids):
        token_id = input_ids[spill_start]
        spillover_1 = -log_probs[spill_start - 1, token_id].item()
    if spill_start + 1 < len(input_ids):
        token_id = input_ids[spill_start + 1]
        spillover_2 = -log_probs[spill_start, token_id].item()

    target_token_str = tokenizer.decode(input_ids[last_pos:last_pos + target_len])

    return {
        "target_token_surprisal": span_surprisals[-1],
        "target_span_surprisal_sum": sum(span_surprisals),
        "target_span_surprisal_mean": np.mean(span_surprisals),
        "spillover_1_surprisal": spillover_1,
        "spillover_2_surprisal": spillover_2,
        "n_target_tokens": target_len,
        "target_token_str": target_token_str,
        "match_found": True,
    }


def run_checkpoint(model_name: str, revision: str, tokenizer, df: pd.DataFrame,
                   device: str, output_dir: str, n_params: int = None):
    """Load a specific checkpoint and compute surprisals for all items."""

    checkpoint_str = f"step{revision}"
    output_path = os.path.join(
        output_dir, f"leinenger_{model_name.split('/')[-1]}_{checkpoint_str}_surprisals.csv"
    )

    if os.path.exists(output_path):
        print(f"  Skipping {checkpoint_str} — already exists at {output_path}")
        return

    print(f"  Loading checkpoint: {checkpoint_str}")
    model = AutoModelForCausalLM.from_pretrained(
        model_name, revision=checkpoint_str, use_safetensors=False
    ).to(device)
    model.eval()

    if n_params is None:
        n_params = count_parameters(model)

    all_results = []
    for _, row in tqdm(df.iterrows(), total=len(df),
                       desc=f"  {checkpoint_str}", leave=False):
        result = compute_surprisals_single(
            model, tokenizer, row["sentence"], row["homophone"], device
        )
        all_results.append(result)

    results_df = pd.DataFrame(all_results)
    out_df = pd.concat([df.reset_index(drop=True), results_df], axis=1)
    out_df["model"] = model_name
    out_df["checkpoint"] = int(revision)
    out_df["n_params"] = n_params

    # Report any failures
    n_missing = (~out_df["match_found"]).sum()
    if n_missing > 0:
        print(f"\n  WARNING: {n_missing} sentences had no match for the homophone!")
        print(out_df[~out_df["match_found"]][["homophone", "sentence"]])

    out_df.to_csv(output_path, index=False)
    print(f"  Saved to {output_path}")


    if device == "cuda":
        torch.cuda.empty_cache()
    elif device == "mps":
        torch.mps.empty_cache()


def run_single_model(model_name, checkpoints, input_path, output_dir, device):
    """Run all checkpoints for a single model."""
    print(f"\nLoading tokenizer for: {model_name}")
    tokenizer = AutoTokenizer.from_pretrained(model_name)
    if tokenizer.pad_token is None:
        tokenizer.pad_token = tokenizer.eos_token

    df = pd.read_csv(input_path)
    print(f"Loaded {len(df)} sentences")
    print(f"Running {len(checkpoints)} checkpoints for {model_name}")

    model_start = time.time()

    for ckpt in checkpoints:
        print(f"\n--- Checkpoint {ckpt} ---")
        run_checkpoint(
            model_name=model_name,
            revision=str(ckpt),
            tokenizer=tokenizer,
            df=df,
            device=device,
            output_dir=output_dir,
        )

    elapsed = time.time() - model_start
    print(f"\n{model_name} done in {elapsed/60:.1f} minutes")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--model", type=str, default="EleutherAI/pythia-1b")
    parser.add_argument("--input", type=str, default="data/raw/leinenger/leinenger.csv")
    parser.add_argument("--output_dir", type=str, default=None)
    parser.add_argument("--device", type=str, default=None)
    parser.add_argument("--checkpoints", type=int, nargs="*", default=None,
                        help="Specific checkpoints to run. Defaults to all.")
    parser.add_argument("--run_all", action="store_true",
                        help="Run all Pythia models (14m through 12b).")
    args = parser.parse_args()

    if args.device is None:
        if torch.cuda.is_available():
            device = "cuda"
        elif torch.backends.mps.is_available():
            device = "mps"
        else:
            device = "cpu"
    else:
        device = args.device
    print(f"Using device: {device}")

    if device == "cuda":
        print(f"GPU: {torch.cuda.get_device_name(0)}")
        vram = torch.cuda.get_device_properties(0).total_memory / 1e9
        print(f"VRAM: {vram:.1f} GB")
    elif device == "mps":
        print("Using Apple Silicon GPU (MPS)")

    if args.output_dir is None:
        args.output_dir = "data/processed/leinenger/"
    os.makedirs(args.output_dir, exist_ok=True)

    checkpoints = args.checkpoints if args.checkpoints else ALL_CHECKPOINTS

    if args.run_all:
        total_start = time.time()
        print(f"\n{'='*50}")
        print(f"Running all {len(ALL_MODELS)} Pythia models")
        print(f"Checkpoints per model: {len(checkpoints)}")
        print(f"{'='*50}")

        for i, model_name in enumerate(ALL_MODELS):
            print(f"\n{'='*50}")
            print(f"[{i+1}/{len(ALL_MODELS)}] {model_name}")
            print(f"{'='*50}")
            run_single_model(model_name, checkpoints, args.input, args.output_dir, device)

        total_elapsed = time.time() - total_start
        print(f"\n{'='*50}")
        print(f"ALL DONE in {total_elapsed/60:.1f} minutes ({total_elapsed/3600:.1f} hours)")
        print(f"Results in {args.output_dir}")
        print(f"{'='*50}")
        print(f"\nREMEMBER: Terminate your instance!")

    else:
        run_single_model(args.model, checkpoints, args.input, args.output_dir, device)

    print(f"\nDone! All results saved to {args.output_dir}")


if __name__ == "__main__":
    main()