"""
Compute surprisal at the last occurrence of the target homophone
for the Leinenger & Rayner (2013) replication.

For each sentence, we:
1. Tokenize the full sentence
2. Find the last occurrence of the homophone token(s)
3. Compute surprisal at that position (and spillover)

Usage:
    python get_surprisals_leinenger.py --model EleutherAI/pythia-14m
"""

import argparse
import torch
import pandas as pd
import numpy as np
from transformers import AutoModelForCausalLM, AutoTokenizer
from tqdm import tqdm


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


def compute_surprisals_batch(
    model, tokenizer, sentences: list[str], homophones: list[str], device: str
) -> list[dict]:
    """
    Compute surprisal at the last occurrence of each homophone in its sentence.
    """
    results = []

    # Pre-compute target token IDs and full sentence token IDs
    all_input_ids = []
    all_target_info = []  # (target_ids, last_pos, target_len)

    for sentence, homophone in zip(sentences, homophones):
        input_ids = tokenizer.encode(sentence, add_special_tokens=False)

        # Tokenize the homophone in isolation to get its token IDs
        # Add a space prefix since in-context it will typically follow a space
        target_ids = tokenizer.encode(" " + homophone, add_special_tokens=False)

        last_pos = find_last_occurrence(input_ids, target_ids)

        # If space-prefixed version doesn't match, try without space
        if last_pos is None:
            target_ids = tokenizer.encode(homophone, add_special_tokens=False)
            last_pos = find_last_occurrence(input_ids, target_ids)

        all_input_ids.append(input_ids)
        all_target_info.append((target_ids, last_pos, len(target_ids)))

    # Pad for batched forward pass
    max_len = max(len(ids) for ids in all_input_ids)
    padded_ids = []
    attention_masks = []
    for ids in all_input_ids:
        pad_len = max_len - len(ids)
        padded_ids.append([tokenizer.pad_token_id or 0] * pad_len + ids)
        attention_masks.append([0] * pad_len + [1] * len(ids))

    input_tensor = torch.tensor(padded_ids, device=device)
    attention_mask = torch.tensor(attention_masks, device=device)

    with torch.no_grad():
        outputs = model(input_ids=input_tensor, attention_mask=attention_mask)
        logits = outputs.logits

    log_probs = torch.log_softmax(logits, dim=-1)

    for i in range(len(sentences)):
        input_ids = all_input_ids[i]
        target_ids, last_pos, target_len = all_target_info[i]
        pad_offset = max_len - len(input_ids)

        if last_pos is None:
            results.append({
                "target_token_surprisal": float("nan"),
                "target_span_surprisal_sum": float("nan"),
                "target_span_surprisal_mean": float("nan"),
                "spillover_1_surprisal": float("nan"),
                "spillover_2_surprisal": float("nan"),
                "n_target_tokens": 0,
                "target_token_str": None,
                "match_found": False,
            })
            continue

        # Surprisal for each token in the target span
        span_surprisals = []
        for j in range(target_len):
            pos = last_pos + j
            logit_pos = pad_offset + pos - 1  # logits at t-1 predict token at t
            token_id = input_ids[pos]
            token_log_prob = log_probs[i, logit_pos, token_id].item()
            span_surprisals.append(-token_log_prob)

        # Spillover: 1 and 2 tokens after the target span
        spillover_1 = float("nan")
        spillover_2 = float("nan")
        spill_start = last_pos + target_len
        if spill_start < len(input_ids):
            logit_pos = pad_offset + spill_start - 1
            token_id = input_ids[spill_start]
            spillover_1 = -log_probs[i, logit_pos, token_id].item()
        if spill_start + 1 < len(input_ids):
            logit_pos = pad_offset + spill_start
            token_id = input_ids[spill_start + 1]
            spillover_2 = -log_probs[i, logit_pos, token_id].item()

        # Decode the matched tokens for verification
        target_token_str = tokenizer.decode(input_ids[last_pos:last_pos + target_len])

        results.append({
            "target_token_surprisal": span_surprisals[-1],  # last token of homophone
            "target_span_surprisal_sum": sum(span_surprisals),
            "target_span_surprisal_mean": np.mean(span_surprisals),
            "spillover_1_surprisal": spillover_1,
            "spillover_2_surprisal": spillover_2,
            "n_target_tokens": target_len,
            "target_token_str": target_token_str,
            "match_found": True,
        })

    return results


def count_parameters(model):
    """credit: https://stackoverflow.com/questions/49201236/check-the-total-number-of-parameters-in-a-pytorch-model"""
    
    total_params = 0
    for name, parameter in model.named_parameters():
        
        # if the param is not trainable, skip it
        if not parameter.requires_grad:
            continue
        
        # otherwise, count it towards your number of params
        params = parameter.numel()
        total_params += params
    print(f"Total Trainable Params: {total_params}")
    
    return total_params

    
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--model", type=str, default="EleutherAI/pythia-2.8b")
    parser.add_argument("--input", type=str, default="data/raw/leinenger/leinenger.csv")
    parser.add_argument("--output", type=str, default=None)
    parser.add_argument("--batch_size", type=int, default=16)
    parser.add_argument("--device", type=str, default=None)
    args = parser.parse_args()

    if args.device is None:
        device = "cuda" if torch.cuda.is_available() else "cpu"
    else:
        device = args.device
    print(f"Using device: {device}")

    if args.output is None:
        model_name = args.model.split("/")[-1]
        args.output = f"data/processed/leinenger/leinenger_{model_name}_surprisals.csv"

    # Load model
    print(f"Loading model: {args.model}")
    tokenizer = AutoTokenizer.from_pretrained(args.model)
    model = AutoModelForCausalLM.from_pretrained(args.model).to(device)
    model.eval()

    if tokenizer.pad_token is None:
        tokenizer.pad_token = tokenizer.eos_token

    # Load data
    df = pd.read_csv(args.input)
    print(f"Loaded {len(df)} sentences")

    # Process in batches
    all_results = []
    for start in tqdm(range(0, len(df), args.batch_size), desc="Computing surprisals"):
        end = min(start + args.batch_size, len(df))
        batch = df.iloc[start:end]
        batch_results = compute_surprisals_batch(
            model, tokenizer,
            batch["sentence"].tolist(),
            batch["homophone"].tolist(),
            device,
        )
        all_results.extend(batch_results)

    # Merge results
    results_df = pd.DataFrame(all_results)
    df = pd.concat([df.reset_index(drop=True), results_df], axis=1)
    df["model"] = args.model

    # Report any failures
    n_missing = (~df["match_found"]).sum()
    if n_missing > 0:
        print(f"\nWARNING: {n_missing} sentences had no match for the homophone!")
        print(df[~df["match_found"]][["homophone", "sentence"]])

    # Save
    import os
    os.makedirs(os.path.dirname(args.output), exist_ok=True)
    df.to_csv(args.output, index=False)
    print(f"\nSaved to {args.output}")

    # Summary
    print(f"\n=== Summary ===")
    print(f"Mean target surprisal: {df['target_token_surprisal'].mean():.3f}")
    print(f"\nBy condition × repeated:")
    print(df.groupby(["condition", "repeated"])["target_token_surprisal"].mean())

    # Show matched tokens for verification
    print(f"\n=== Token matches (first 10) ===")
    for _, row in df.head(10).iterrows():
        print(f"  {row['homophone']:10s} → '{row['target_token_str']}' "
              f"(n_tokens={row['n_target_tokens']}) "
              f"surprisal={row['target_token_surprisal']:.2f}")


if __name__ == "__main__":
    main()