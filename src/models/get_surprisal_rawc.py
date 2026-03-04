"""
Compute surprisal of target sentences in a simulated priming paradigm.

For each trial, we:
1. Tokenize the full context: "{prime} {target}"
2. Run a forward pass through the model
3. Identify which tokens belong to the target sentence
4. Compute surprisal (negative log probability) for:
   - The full target sentence (sum of token surprisals)
   - The mean token surprisal across target tokens
   - The final token of the target sentence only

NOTE: We process one item at a time (no batching) to avoid any
artifacts from left-padding with attention masks, which can subtly
alter surprisal values in models not trained with padding.

Usage:
    python get_surprisals.py --model EleutherAI/pythia-1.4b
    python get_surprisals.py --model EleutherAI/pythia-2.8b --checkpoints 0 1000 143000
"""

import argparse
import os
import torch
import pandas as pd
import numpy as np
from transformers import AutoModelForCausalLM, AutoTokenizer
from tqdm import tqdm


ALL_CHECKPOINTS = [
    0, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512,
    1000, 2000, 4000, 8000, 16000, 32000, 64000, 128000, 143000
]


def get_target_token_indices(tokenizer, prime: str, target: str) -> tuple[list[int], list[int]]:
    """
    Figure out which token positions in the full context correspond to the target.
    
    Returns:
        context_ids: full token ids for "{prime} {target}"
        target_positions: list of indices into context_ids that correspond to target tokens
    """
    context = prime + " " + target
    context_ids = tokenizer.encode(context, add_special_tokens=False)
    prime_ids = tokenizer.encode(prime + " ", add_special_tokens=False)
    
    prime_len = len(prime_ids)
    target_positions = list(range(prime_len, len(context_ids)))
    
    return context_ids, target_positions


def count_parameters(model):
    """Count trainable parameters."""
    total_params = 0
    for name, parameter in model.named_parameters():
        if not parameter.requires_grad:
            continue
        total_params += parameter.numel()
    return total_params


def compute_surprisals_single(
    model, tokenizer, prime: str, target: str, device: str
) -> dict:
    """
    Compute surprisals for a single prime-target pair.
    No batching — avoids padding artifacts.
    """
    context_ids, target_positions = get_target_token_indices(tokenizer, prime, target)
    
    input_ids = torch.tensor([context_ids], device=device)
    
    with torch.no_grad():
        outputs = model(input_ids=input_ids)
        logits = outputs.logits[0]  # (seq_len, vocab)
    
    log_probs = torch.log_softmax(logits, dim=-1)
    
    token_surprisals = []
    for pos in target_positions:
        token_id = context_ids[pos]
        token_log_prob = log_probs[pos - 1, token_id].item()
        token_surprisals.append(-token_log_prob)
    
    return {
        "target_surprisal_sum": sum(token_surprisals),
        "target_surprisal_mean": np.mean(token_surprisals),
        "final_token_surprisal": token_surprisals[-1] if token_surprisals else float("nan"),
        "n_target_tokens": len(token_surprisals),
    }


def run_checkpoint(model_name: str, revision: str, tokenizer, df: pd.DataFrame,
                   device: str, output_dir: str, n_params: int = None):
    """Load a specific checkpoint and compute surprisals for all items."""
    
    checkpoint_str = f"step{revision}"
    output_path = os.path.join(output_dir, f"{model_name.split('/')[-1]}_{checkpoint_str}_surprisals.csv")
    
    # Skip if already computed
    if os.path.exists(output_path):
        print(f"  Skipping {checkpoint_str} — already exists at {output_path}")
        return
    
    print(f"  Loading checkpoint: {checkpoint_str}")
    model = AutoModelForCausalLM.from_pretrained(
        model_name, revision=checkpoint_str
    ).to(device)
    model.eval()
    
    if n_params is None:
        n_params = count_parameters(model)
    
    # Process each item
    all_results = []
    for _, row in tqdm(df.iterrows(), total=len(df),
                       desc=f"  {checkpoint_str}", leave=False):
        result = compute_surprisals_single(
            model, tokenizer, row["prime"], row["target"], device
        )
        all_results.append(result)
    
    # Combine with original data
    results_df = pd.DataFrame(all_results)
    out_df = pd.concat([df.reset_index(drop=True), results_df], axis=1)
    out_df["model"] = model_name
    out_df["checkpoint"] = int(revision)
    out_df["n_params"] = n_params
    
    # Save
    out_df.to_csv(output_path, index=False)
    print(f"  Saved to {output_path}")
    
    # Free memory
    del model
    torch.cuda.empty_cache()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--model", type=str, default="EleutherAI/pythia-410m")
    parser.add_argument("--input", type=str, default="data/raw/rawc/rawc_stimuli_with_order.csv")
    parser.add_argument("--output_dir", type=str, default=None)
    parser.add_argument("--device", type=str, default=None)
    parser.add_argument("--checkpoints", type=int, nargs="*", default=None,
                        help="Specific checkpoints to run. Defaults to all.")
    args = parser.parse_args()
    
    # Set device
    if args.device is None:
        device = "cuda" if torch.cuda.is_available() else "cpu"
    else:
        device = args.device
    print(f"Using device: {device}")
    
    # Set output directory
    if args.output_dir is None:
        args.output_dir = "data/processed/rawc/"
    os.makedirs(args.output_dir, exist_ok=True)
    
    # Checkpoints to evaluate
    checkpoints = args.checkpoints if args.checkpoints else ALL_CHECKPOINTS
    
    # Load tokenizer (shared across checkpoints)
    print(f"Loading tokenizer for: {args.model}")
    tokenizer = AutoTokenizer.from_pretrained(args.model)
    if tokenizer.pad_token is None:
        tokenizer.pad_token = tokenizer.eos_token
    
    # Load data
    df = pd.read_csv(args.input)
    print(f"Loaded {len(df)} trials")
    
    print(f"\nRunning {len(checkpoints)} checkpoints for {args.model}")
    
    # Iterate over checkpoints
    for ckpt in checkpoints:
        print(f"\n--- Checkpoint {ckpt} ---")
        run_checkpoint(
            model_name=args.model,
            revision=str(ckpt),
            tokenizer=tokenizer,
            df=df,
            device=device,
            output_dir=args.output_dir,
        )
    
    print(f"\nDone! All results saved to {args.output_dir}")


if __name__ == "__main__":
    main()