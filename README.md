## LMs and the subordinate-bias effect

This repository contains code to reproduce the analyses from the submitted article:

> Trott, S. (Under Review). Language models and the subordinate-bias effect: do distributional statistics capture meaning dominance?

## Data

All data required to reproduce the primary analyses is included in the `data` directory.

- `human`: Contains files with trial-level human data, as well as aggregated dominance judgments. 
- `raw`: Contains raw stimulus files for `rawc` and `leinenger` studies.
- `processed`: Contains the results of the LM modeling pipeline for both `rawc` and `leinenger` analyses.

## Modeling pipeline

The code to run LMs for either the Trott & Bergen (2023) stimuli or the Leinenger & Rayner (2013) stimuli is found in `src/models`.

For the Trott & Bergen (2023) stimuli:

```
python src/models/get_surprisal_rawc_lambda.py
``` 

For the Leinenger & Rayner (2013) stimuli:

```
python src/models/get_surprisal_leinenger_lambda.py
``` 

## Analysis

All analyses were conducted in R and are included in `.Rmd` files in `src/analysis`.

- Pre-registered analyses of LM behavior on the RAW-C data in `rawc_lm_analysis.Rmd`. 
- Pre-registered comparison of LM and human behavior on the RAW-C data in `rawc_lm_and_human_analysis.Rmd`.
- Analysis of LM behavior on the Leinenger & Rayner (2013) data in `leinenger_analysis.Rmd`.


## Contact

Please reach out to Sean Trott with any questions about the code or data in this repository. 