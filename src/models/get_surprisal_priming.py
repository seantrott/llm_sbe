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

Usage:
    python get_surprisals.py --model EleutherAI/pythia-14m --batch_size 32
"""

import argparse
import torch
import pandas as pd
import numpy as np
from transformers import AutoModelForCausalLM, AutoTokenizer
from tqdm import tqdm


def get_target_token_indices(tokenizer, prime: str, target: str) -> tuple[list[int], list[int]]:
    """
    Figure out which token positions in the full context correspond to the target.
    
    Returns:
        context_ids: full token ids for "{prime} {target}"
        target_positions: list of indices into context_ids that correspond to target tokens
    """
    # Tokenize full context
    context = prime + " " + target
    context_ids = tokenizer.encode(context, add_special_tokens=False)
    
    # Tokenize prime alone (the target tokens start after this)
    prime_ids = tokenizer.encode(prime + " ", add_special_tokens=False)
    
    # Target positions are everything after the prime
    # But we need to be careful: tokenization isn't always cleanly separable.
    # So we verify by also tokenizing the target alone and checking lengths.
    target_ids = tokenizer.encode(target, add_special_tokens=False)
    
    # The safest approach: prime_len tokens are the prime, rest are target
    prime_len = len(prime_ids)
    target_positions = list(range(prime_len, len(context_ids)))
    
    return context_ids, target_positions

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



def compute_surprisals_batch(
    model, tokenizer, primes: list[str], targets: list[str], device: str
) -> list[dict]:
    """
    Compute surprisals for a batch of prime-target pairs.
    
    Returns list of dicts with:
        - target_surprisal_sum: sum of surprisals across target tokens
        - target_surprisal_mean: mean surprisal across target tokens
        - final_token_surprisal: surprisal of just the last token
        - n_target_tokens: number of tokens in the target
    """
    results = []
    
    # Get token info for each item
    all_context_ids = []
    all_target_positions = []
    for prime, target in zip(primes, targets):
        context_ids, target_positions = get_target_token_indices(tokenizer, prime, target)
        all_context_ids.append(context_ids)
        all_target_positions.append(target_positions)
    
    # Pad for batched forward pass
    max_len = max(len(ids) for ids in all_context_ids)
    padded_ids = []
    attention_masks = []
    for ids in all_context_ids:
        pad_len = max_len - len(ids)
        # Left-pad so the final tokens are aligned
        padded_ids.append([tokenizer.pad_token_id or 0] * pad_len + ids)
        attention_masks.append([0] * pad_len + [1] * len(ids))
    
    input_ids = torch.tensor(padded_ids, device=device)
    attention_mask = torch.tensor(attention_masks, device=device)
    
    with torch.no_grad():
        outputs = model(input_ids=input_ids, attention_mask=attention_mask)
        logits = outputs.logits  # (batch, seq_len, vocab)
    
    # Compute log probs: logits at position t predict token at position t+1
    log_probs = torch.log_softmax(logits, dim=-1)
    
    for i in range(len(primes)):
        context_ids = all_context_ids[i]
        target_positions = all_target_positions[i]
        pad_offset = max_len - len(context_ids)
        
        token_surprisals = []
        for pos in target_positions:
            # The logit at position (pos - 1) predicts the token at position pos
            logit_pos = pad_offset + pos - 1
            token_id = context_ids[pos]
            token_log_prob = log_probs[i, logit_pos, token_id].item()
            token_surprisals.append(-token_log_prob)
        
        results.append({
            "target_surprisal_sum": sum(token_surprisals),
            "target_surprisal_mean": np.mean(token_surprisals),
            "final_token_surprisal": token_surprisals[-1] if token_surprisals else float("nan"),
            "n_target_tokens": len(token_surprisals),
        })
    
    return results


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--model", type=str, default="EleutherAI/pythia-1.4b")
    parser.add_argument("--input", type=str, default="data/raw/rawc/rawc_stimuli_with_order.csv")
    parser.add_argument("--output", type=str, default=None,
                        help="Output CSV path. Defaults to data/processed/{model_name}_surprisals.csv")
    parser.add_argument("--batch_size", type=int, default=32)
    parser.add_argument("--device", type=str, default=None,
                        help="Device to use. Defaults to cuda if available.")
    args = parser.parse_args()
    
    # Set device
    if args.device is None:
        device = "cuda" if torch.cuda.is_available() else "cpu"
    else:
        device = args.device
    print(f"Using device: {device}")
    
    # Set output path
    if args.output is None:
        model_name = args.model.split("/")[-1]
        args.output = f"data/processed/{model_name}_surprisals.csv"
    
    # Load model and tokenizer
    print(f"Loading model: {args.model}")
    tokenizer = AutoTokenizer.from_pretrained(args.model)
    model = AutoModelForCausalLM.from_pretrained(args.model).to(device)
    model.eval()
    
    # Ensure pad token exists
    if tokenizer.pad_token is None:
        tokenizer.pad_token = tokenizer.eos_token
    
    # Load data
    df = pd.read_csv(args.input)
    print(f"Loaded {len(df)} trials")
    
    # Process in batches
    all_results = []
    print(df.shape)
    for start in tqdm(range(0, len(df), args.batch_size), desc="Computing surprisals"):
        end = min(start + args.batch_size, len(df))
        batch = df.iloc[start:end]
        
        batch_results = compute_surprisals_batch(
            model, tokenizer,
            batch["prime"].tolist(),
            batch["target"].tolist(),
            device,
        )
        all_results.extend(batch_results)
    
    # Add results to dataframe
    results_df = pd.DataFrame(all_results)
    df = pd.concat([df.reset_index(drop=True), results_df], axis=1)
    
    # Add model name column
    df["model"] = args.model
    df['n_params'] = count_parameters(model)
    
    # Save
    import os
    os.makedirs(os.path.dirname(args.output), exist_ok=True)
    df.to_csv(args.output, index=False)
    print(f"\nSaved results to {args.output}")
    

if __name__ == "__main__":
    main()