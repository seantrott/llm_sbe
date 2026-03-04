"""
Compute cosine similarity between each ambiguous word's static embedding (layer 0)
and the static embeddings of its disambiguating words.

For each word (e.g., "lamb"), we:
1. Get the token embedding for the ambiguous word
2. Get the token embeddings for each disambiguating word (M1_a, M1_b, M2_a, M2_b)
3. Compute cosine similarity between the ambiguous word and each disambiguator
4. Average within meaning (M1 vs M2) to get a dominance measure

If the static embedding encodes dominance, the ambiguous word should be closer
to the dominant sense's disambiguators.

Usage:
    python get_static_dominance.py --model EleutherAI/pythia-14m
"""

import argparse
import torch
import pandas as pd
import numpy as np
from transformers import AutoModel, AutoTokenizer
from torch.nn.functional import cosine_similarity


def extract_sense_disambiguators(df: pd.DataFrame) -> pd.DataFrame:
    """
    For each word, extract the disambiguating words for each sense (M1_a, M1_b, M2_a, M2_b).
    Uses the version column to map disambiguating_word1/2 to specific senses.
    """
    records = []

    for word in df["word"].unique():
        sub = df[(df["word"] == word) & (df["order"] == "s1_prime")]

        sense_map = {}

        for _, row in sub.iterrows():
            version = row["version"]
            dw1 = row["disambiguating_word1"]
            dw2 = row["disambiguating_word2"]

            # version like M1_a_M2_b: dw1 = M1_a's word, dw2 = M2_b's word
            parts = version.split("_")
            # Reconstruct: M1_a and M2_b
            v1 = parts[0] + "_" + parts[1]  # e.g., M1_a
            v2 = parts[2] + "_" + parts[3]  # e.g., M2_b

            sense_map[v1] = dw1
            sense_map[v2] = dw2

        # Now we should have M1_a, M1_b, M2_a, M2_b mapped
        ambiguity_type = sub["ambiguity_type"].iloc[0]
        word_class = sub["Class"].iloc[0]
        target_word = sub["string"].iloc[0] if "string" in sub.columns else word

        for sense_label, disambiguator in sense_map.items():
            meaning = sense_label.split("_")[0]  # M1 or M2
            records.append({
                "word": word,
                "target_word": target_word,
                "ambiguity_type": ambiguity_type,
                "Class": word_class,
                "sense_label": sense_label,
                "meaning": meaning,
                "disambiguator": disambiguator,
            })

    return pd.DataFrame(records)


def get_token_embedding(model, tokenizer, word: str) -> torch.Tensor:
    """
    Get the static (layer 0) token embedding for a word.
    Uses space-prefixed tokenization and averages if multiple subword tokens.
    """
    token_ids = tokenizer.encode(" " + word, add_special_tokens=False)
    embeddings = model.embed_in(torch.tensor([token_ids]))  # (1, n_tokens, hidden_dim)
    # Average across subword tokens
    return embeddings[0].mean(dim=0)  # (hidden_dim,)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--model", type=str, default="EleutherAI/pythia-2.8b")
    parser.add_argument("--input", type=str, default="data/raw/rawc/rawc_stimuli_with_order.csv")
    parser.add_argument("--output", type=str, default=None)
    args = parser.parse_args()

    if args.output is None:
        model_name = args.model.split("/")[-1]
        args.output = f"data/processed/static_dominance/static_dominance_{model_name}.csv"

    print(f"Loading model: {args.model}")
    tokenizer = AutoTokenizer.from_pretrained(args.model)
    model = AutoModel.from_pretrained(args.model)
    model.eval()

    # Get the embedding layer
    # For Pythia/GPT-NeoX models
    if hasattr(model, "embed_in"):
        embed_fn = model.embed_in
    elif hasattr(model, "gpt_neox"):
        embed_fn = model.gpt_neox.embed_in
    elif hasattr(model, "transformer"):
        embed_fn = model.transformer.wte
    else:
        raise ValueError("Cannot find embedding layer")

    def get_embedding(word: str) -> torch.Tensor:
        token_ids = tokenizer.encode(" " + word, add_special_tokens=False)
        with torch.no_grad():
            embeddings = embed_fn(torch.tensor(token_ids))  # (n_tokens, hidden_dim)
        return embeddings.mean(dim=0)  # (hidden_dim,)

    # Load data and extract sense-disambiguator mapping
    df = pd.read_csv(args.input)
    sense_df = extract_sense_disambiguators(df)
    print(f"Extracted {len(sense_df)} sense-disambiguator pairs "
          f"for {sense_df['word'].nunique()} words")

    # Compute cosine similarities
    results = []
    for _, row in sense_df.iterrows():
        target_emb = get_embedding(row["target_word"])
        disambig_emb = get_embedding(row["disambiguator"])

        cos_sim = cosine_similarity(
            target_emb.unsqueeze(0),
            disambig_emb.unsqueeze(0)
        ).item()

        results.append({
            **row.to_dict(),
            "cosine_sim": cos_sim,
        })

    results_df = pd.DataFrame(results)

    # Compute per-word, per-meaning averages
    meaning_avg = results_df.groupby(["word", "target_word", "ambiguity_type", "meaning"]).agg(
        mean_cosine=("cosine_sim", "mean"),
    ).reset_index()

    # Pivot to get M1 vs M2 side by side
    dominance_df = meaning_avg.pivot_table(
        index=["word", "target_word", "ambiguity_type"],
        columns="meaning",
        values="mean_cosine",
    ).reset_index()

    dominance_df.columns.name = None
    dominance_df = dominance_df.rename(columns={"M1": "cosine_M1", "M2": "cosine_M2"})
    dominance_df["cosine_diff_M1_M2"] = dominance_df["cosine_M1"] - dominance_df["cosine_M2"]

    # Save both detailed and summary
    results_df["model"] = args.model
    dominance_df["model"] = args.model

    import os
    os.makedirs(os.path.dirname(args.output), exist_ok=True)
    results_df.to_csv(args.output, index=False)

    summary_path = args.output.replace(".csv", "_summary.csv")
    dominance_df.to_csv(summary_path, index=False)

    print(f"\nSaved detailed results to {args.output}")
    print(f"Saved summary to {summary_path}")

    # Print summary
    print(f"\n=== Summary ===")
    print(f"Mean cosine to M1 disambiguators: {dominance_df['cosine_M1'].mean():.4f}")
    print(f"Mean cosine to M2 disambiguators: {dominance_df['cosine_M2'].mean():.4f}")
    print(f"Mean difference (M1 - M2): {dominance_df['cosine_diff_M1_M2'].mean():.4f}")
    print(f"\nBy ambiguity type:")
    print(dominance_df.groupby("ambiguity_type")[["cosine_M1", "cosine_M2", "cosine_diff_M1_M2"]].mean())

    # Show a few examples
    print(f"\n=== Examples ===")
    for _, row in dominance_df.head(10).iterrows():
        direction = "M1 dominant" if row["cosine_diff_M1_M2"] > 0 else "M2 dominant"
        print(f"  {row['word']:15s} M1={row['cosine_M1']:.4f}  M2={row['cosine_M2']:.4f}  "
              f"diff={row['cosine_diff_M1_M2']:+.4f}  ({direction})")


if __name__ == "__main__":
    main()