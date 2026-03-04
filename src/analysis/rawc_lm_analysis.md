---
title: "RAW-C Data: LM Analysis"
author: "Sean Trott"
date: "2-23-2026"
output:
  html_document:
    keep_md: yes
    toc: yes
    toc_float: yes
---




# Setup

Load libraries


``` r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.5.2     ✔ tibble    3.2.1
## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
## ✔ purrr     1.0.4     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(lme4)
```

```
## Loading required package: Matrix
## 
## Attaching package: 'Matrix'
## 
## The following objects are masked from 'package:tidyr':
## 
##     expand, pack, unpack
```

``` r
library(broom.mixed)
library(lmerTest)
```

```
## 
## Attaching package: 'lmerTest'
## 
## The following object is masked from 'package:lme4':
## 
##     lmer
## 
## The following object is masked from 'package:stats':
## 
##     step
```

``` r
library(kableExtra)
```

```
## 
## Attaching package: 'kableExtra'
## 
## The following object is masked from 'package:dplyr':
## 
##     group_rows
```

``` r
library(ggridges)
library(ggrepel)
```


# Load data


``` r
# setwd("/Users/seantrott/Dropbox/UCSD/Research/Ambiguity/SSD/llm_sbe/src/analysis")

# df_pythia = read_csv("../../data/processed/rawc/pythia-14m_surprisals.csv")
### First, load all pythia models
directory_path <- "../../data/processed/rawc/"
csv_files <- list.files(path = directory_path, pattern = "*.csv", full.names = TRUE)
csv_list <- csv_files %>%
  map(~ read_csv(.))
```

```
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Rows: 1344 Columns: 22
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (15): word, ambiguity_type, string, version, Class, order, version_with_...
## dbl  (6): target_surprisal_sum, target_surprisal_mean, final_token_surprisal...
## lgl  (1): same
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
df_pythia_models <- bind_rows(csv_list) %>%
  mutate(model_shorthand = sub("^EleutherAI/", "", model)) 
nrow(df_pythia_models)
```

```
## [1] 84672
```

``` r
table(df_pythia_models$model_shorthand)
```

```
## 
##  pythia-14m pythia-160m pythia-410m  pythia-70m 
##       26880       26880        4032       26880
```

``` r
### Load human dominance data
df_dominance = read_csv("../../data/human/dominance_norms_with_order.csv")
```

```
## New names:
## Rows: 896 Columns: 6
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (3): word, version_with_order, ambiguity_type dbl (3): ...1, dominance_right,
## sd_dominance
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## • `` -> `...1`
```

``` r
### Merge data
df_merged = df_pythia_models %>%
  inner_join(df_dominance)
```

```
## Joining with `by = join_by(word, ambiguity_type, version_with_order)`
```

``` r
##### CENTER PREDICTORS
df_merged <- df_merged %>%
  mutate(
    ambiguity_c = if_else(ambiguity_type == "Polysemy", 0.5, -0.5),
    dominance_z = scale(dominance_right)[,1]
  )

df_pythia_models <- df_pythia_models %>%
  mutate(
    ambiguity_c = if_else(ambiguity_type == "Polysemy", 0.5, -0.5),
    same_c = if_else(same == TRUE, 0.5, -0.5),
    same_label = if_else(same == TRUE, "Same Sense", "Different Sense")
  )
```


# Analyses

*Note that analyses of human data (RQ2, RQ4) are included in a separate file.

## RQ1: Does surprisal of the TARGET phrase track sense dominance?



``` r
developmental_results_rq1 <- df_merged %>%
  mutate(
    ambiguity_c = if_else(ambiguity_type == "Polysemy", 0.5, -0.5),
    dominance_z = scale(dominance_right)[,1]
  ) %>%
  group_by(model, model_shorthand, n_params, checkpoint) %>%
  group_map(~ {
    rq1_full    <- tryCatch(lmer(target_surprisal_mean ~ ambiguity_c + dominance_z + (1 | word), data = .x, REML = FALSE), error = function(e) NULL)
    rq1_reduced <- tryCatch(lmer(target_surprisal_mean ~ ambiguity_c +               (1 | word), data = .x, REML = FALSE), error = function(e) NULL)
    lrt <- if (!is.null(rq1_full) && !is.null(rq1_reduced)) anova(rq1_full, rq1_reduced) else NULL
    tibble(
      model            = .y$model,
      model_shorthand  = .y$model_shorthand,
      n_params         = .y$n_params,
      checkpoint       = .y$checkpoint,
      rq1_dom_beta     = if (!is.null(rq1_full)) fixef(rq1_full)["dominance_z"] else NA_real_,
      rq1_dom_se       = if (!is.null(rq1_full)) sqrt(diag(vcov(rq1_full)))["dominance_z"] else NA_real_,
      rq1_lrt_chi2     = if (!is.null(lrt)) lrt$Chisq[2] else NA_real_,
      rq1_lrt_p        = if (!is.null(lrt)) lrt$`Pr(>Chisq)`[2] else NA_real_,
      rq1_aic_full     = if (!is.null(rq1_full)) AIC(rq1_full) else NA_real_,
      rq1_aic_reduced  = if (!is.null(rq1_reduced)) AIC(rq1_reduced) else NA_real_,
      rq1_aic_delta    = rq1_aic_reduced - rq1_aic_full
    )
  }) %>%
  bind_rows() %>%
  arrange(n_params)
```

### Final step

First, we report results for the final step.


``` r
developmental_results_rq1 %>%
  filter(checkpoint == 143000) %>%
  mutate(
    rq1_lrt_p = format.pval(rq1_lrt_p, digits = 3),
    across(c(rq1_dom_beta, rq1_dom_se, rq1_lrt_chi2, rq1_aic_delta), ~ round(.x, 3))
  ) %>%
  select(model, n_params, rq1_dom_beta, rq1_dom_se, rq1_lrt_chi2, rq1_lrt_p, rq1_aic_delta) %>%
  kable(
    col.names = c("Model", "Params", "β (Dominance)", "SE", "χ²", "p", "ΔAIC"),
    caption = "RQ1: Dominance predicting target surprisal (final checkpoint)"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
```

<table class="table table-striped table-hover table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>RQ1: Dominance predicting target surprisal (final checkpoint)</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Model </th>
   <th style="text-align:right;"> Params </th>
   <th style="text-align:right;"> β (Dominance) </th>
   <th style="text-align:right;"> SE </th>
   <th style="text-align:right;"> χ² </th>
   <th style="text-align:left;"> p </th>
   <th style="text-align:right;"> ΔAIC </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> EleutherAI/pythia-14m </td>
   <td style="text-align:right;"> 14067712 </td>
   <td style="text-align:right;"> -0.101 </td>
   <td style="text-align:right;"> 0.017 </td>
   <td style="text-align:right;"> 33.199 </td>
   <td style="text-align:left;"> 8.32e-09 </td>
   <td style="text-align:right;"> 31.199 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EleutherAI/pythia-70m </td>
   <td style="text-align:right;"> 70426624 </td>
   <td style="text-align:right;"> -0.157 </td>
   <td style="text-align:right;"> 0.017 </td>
   <td style="text-align:right;"> 85.225 </td>
   <td style="text-align:left;"> &lt; 2e-16 </td>
   <td style="text-align:right;"> 83.225 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EleutherAI/pythia-160m </td>
   <td style="text-align:right;"> 162322944 </td>
   <td style="text-align:right;"> -0.147 </td>
   <td style="text-align:right;"> 0.017 </td>
   <td style="text-align:right;"> 75.231 </td>
   <td style="text-align:left;"> &lt; 2e-16 </td>
   <td style="text-align:right;"> 73.231 </td>
  </tr>
</tbody>
</table>

``` r
developmental_results_rq1 %>%
  filter(checkpoint == 143000) %>%
  mutate(
    rq1_lrt_p = format.pval(rq1_lrt_p, digits = 3),
    across(c(rq1_dom_beta, rq1_dom_se, rq1_lrt_chi2, rq1_aic_delta), ~ round(.x, 3))
  ) %>%
  select(model, n_params, rq1_dom_beta, rq1_dom_se, rq1_lrt_chi2, rq1_lrt_p, rq1_aic_delta) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c("Model", "Params", "$\\beta$", "SE", "$\\chi^2$", "$p$", "$\\Delta$AIC"),
    escape = FALSE,
    caption = "RQ1: Dominance predicting target surprisal (final checkpoint)"
  ) %>%
  kable_styling(latex_options = c("hold_position"))
```

\begin{table}[!h]
\centering
\caption{\label{tab:tables}RQ1: Dominance predicting target surprisal (final checkpoint)}
\centering
\begin{tabular}[t]{lrrrrlr}
\toprule
Model & Params & $\beta$ & SE & $\chi^2$ & $p$ & $\Delta$AIC\\
\midrule
EleutherAI/pythia-14m & 14067712 & -0.101 & 0.017 & 33.199 & 8.32e-09 & 31.199\\
EleutherAI/pythia-70m & 70426624 & -0.157 & 0.017 & 85.225 & < 2e-16 & 83.225\\
EleutherAI/pythia-160m & 162322944 & -0.147 & 0.017 & 75.231 & < 2e-16 & 73.231\\
\bottomrule
\end{tabular}
\end{table}

We also visualize the coefficients.


``` r
developmental_results_rq1 %>%
  filter(checkpoint == 143000) %>%
  mutate(
    model_shorthand = sub("^EleutherAI/", "", model),
    model_shorthand = fct_reorder(model_shorthand, n_params)
  ) %>%
  ggplot(aes(x = model_shorthand, y = rq1_dom_beta)) +
  geom_pointrange(aes(
    ymin = rq1_dom_beta - 1.96 * rq1_dom_se,
    ymax = rq1_dom_beta + 1.96 * rq1_dom_se
  )) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    x = "Model (ordered by parameter count)",
    y = "β (Dominance)",
    title = "RQ1: Effect of sense dominance on target surprisal",
    subtitle = "Point estimates ± 95% CI"
  ) +
  coord_flip() +
  theme_minimal(base_size = 13) 
```

![](rawc_lm_analysis_files/figure-html/coef_rq1_final-1.png)<!-- -->


And also the raw data.


``` r
df_merged %>%
  filter(checkpoint == 143000) %>%
  ggplot(aes(x = dominance_right,
             y = target_surprisal_mean)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm") +
  labs(x = "Human Dominance Rating",
       y = "Surprisal on Target") +
  theme_minimal(base_size = 13) +
  facet_wrap(~reorder(model_shorthand, n_params))
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](rawc_lm_analysis_files/figure-html/rq1_raw_data_final-1.png)<!-- -->


### RQ5: Model scaling


``` r
## scaling_results_rq1
developmental_results_rq1 %>%
  filter(checkpoint == 143000) %>%
  ggplot(aes(x = n_params,
             y = rq1_aic_delta)) +
  geom_point(size = 6,
             alpha = .5) +
  scale_x_log10() +
  geom_text_repel(aes(label=model_shorthand), size=3) +
  labs(x = "Parameters",
       y = "AIC Delta") +
  theme_minimal() +
  theme(text = element_text(size = 15),
        legend.position = "bottom") 
```

![](rawc_lm_analysis_files/figure-html/rq1_scaling-1.png)<!-- -->

``` r
developmental_results_rq1 %>%
  filter(checkpoint == 143000) %>%
  ggplot(aes(x = n_params,
             y = rq1_dom_beta)) +
  geom_point(size = 6,
             alpha = .5) +
  scale_x_log10() +
  geom_text_repel(aes(label=model_shorthand), size=3) +
  labs(x = "Parameters",
       y = "Dominance Coefficient") +
  theme_minimal() +
  theme(text = element_text(size = 15),
        legend.position = "bottom") 
```

![](rawc_lm_analysis_files/figure-html/rq1_scaling-2.png)<!-- -->

``` r
summary(lm(rq1_dom_beta ~ log10(n_params),
           data = developmental_results_rq1 %>%
  filter(checkpoint == 143000)))
```

```
## 
## Call:
## lm(formula = rq1_dom_beta ~ log10(n_params), data = developmental_results_rq1 %>% 
##     filter(checkpoint == 143000))
## 
## Residuals:
##         1         2         3 
##  0.005752 -0.016846  0.011094 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)
## (Intercept)      0.24346    0.21284   1.144    0.457
## log10(n_params) -0.04893    0.02747  -1.781    0.326
## 
## Residual standard error: 0.02097 on 1 degrees of freedom
## Multiple R-squared:  0.7604,	Adjusted R-squared:  0.5208 
## F-statistic: 3.173 on 1 and 1 DF,  p-value: 0.3256
```

``` r
summary(lm(rq1_aic_delta ~ log10(n_params),
           data = developmental_results_rq1 %>%
  filter(checkpoint == 143000)))
```

```
## 
## Call:
## lm(formula = rq1_aic_delta ~ log10(n_params), data = developmental_results_rq1 %>% 
##     filter(checkpoint == 143000))
## 
## Residuals:
##       1       2       3 
##  -5.362  15.704 -10.342 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)
## (Intercept)      -279.82     198.41  -1.410    0.393
## log10(n_params)    44.26      25.61   1.728    0.334
## 
## Residual standard error: 19.55 on 1 degrees of freedom
## Multiple R-squared:  0.7492,	Adjusted R-squared:  0.4984 
## F-statistic: 2.987 on 1 and 1 DF,  p-value: 0.3339
```


### RQ6: Developmental Trajectory


``` r
developmental_results_rq1 %>%
  mutate(significant = rq1_lrt_p < .05) %>%
  ggplot(aes(x = checkpoint,
             y = rq1_dom_beta,
             color = reorder(model_shorthand, n_params))) +
  geom_point(aes(shape = significant),
             size = 4, alpha = .7) +
  geom_line(size = 1.3) +
  geom_point(size = 4, alpha = .4) +
  theme_minimal() +
  labs(x = "Training Step (Log10)",
       y = "Coefficient (Dominance)",
       color = "") +
  scale_x_log10() +
  geom_vline(xintercept = 1000, 
             linetype = "dotted", 
             size = 1.2) +
  scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16),
                     labels = c("n.s.", "p < .05"),
                     name = NULL) +
  theme(text = element_text(size = 15),
        legend.position = "bottom") +
  scale_color_manual(values = viridisLite::viridis(4, option = "mako", 
                                                begin = 0.8, end = 0.15))
```

```
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

```
## Warning in scale_x_log10(): log-10 transformation introduced infinite values.
## log-10 transformation introduced infinite values.
## log-10 transformation introduced infinite values.
```

![](rawc_lm_analysis_files/figure-html/rq6_dev-1.png)<!-- -->

## RQ3: Does surprisal track sense boundaries?


``` r
developmental_results_rq3 <- df_pythia_models %>%
  mutate(
    ambiguity_c = if_else(ambiguity_type == "Polysemy", 0.5, -0.5),
    same_c      = if_else(same == TRUE, 0.5, -0.5)
  ) %>%
  group_by(model, model_shorthand, n_params, checkpoint) %>%
  group_map(~ {
    rq3_full    <- tryCatch(lmer(target_surprisal_mean ~ same_c + ambiguity_c + (1 | word), data = .x, REML = FALSE), error = function(e) NULL)
    rq3_reduced <- tryCatch(lmer(target_surprisal_mean ~           ambiguity_c + (1 | word), data = .x, REML = FALSE), error = function(e) NULL)
    lrt <- if (!is.null(rq3_full) && !is.null(rq3_reduced)) anova(rq3_full, rq3_reduced) else NULL
    tibble(
      model            = .y$model,
      model_shorthand  = .y$model_shorthand,
      n_params         = .y$n_params,
      checkpoint       = .y$checkpoint,
      rq3_same_beta    = if (!is.null(rq3_full)) fixef(rq3_full)["same_c"] else NA_real_,
      rq3_same_se      = if (!is.null(rq3_full)) sqrt(diag(vcov(rq3_full)))["same_c"] else NA_real_,
      rq3_lrt_chi2     = if (!is.null(lrt)) lrt$Chisq[2] else NA_real_,
      rq3_lrt_p        = if (!is.null(lrt)) lrt$`Pr(>Chisq)`[2] else NA_real_,
      rq3_aic_full     = if (!is.null(rq3_full)) AIC(rq3_full) else NA_real_,
      rq3_aic_reduced  = if (!is.null(rq3_reduced)) AIC(rq3_reduced) else NA_real_,
      rq3_aic_delta    = rq3_aic_reduced - rq3_aic_full
    )
  }) %>%
  bind_rows() %>%
  arrange(n_params)
```


### Final step

First, we report results for the final step.


``` r
developmental_results_rq3 %>%
  filter(checkpoint == 143000) %>%
  mutate(
    rq3_lrt_p = format.pval(rq3_lrt_p, digits = 3),
    across(c(rq3_same_beta, rq3_same_se, 
             rq3_lrt_chi2, rq3_aic_delta), ~ round(.x, 3))
  ) %>%
  select(model, n_params, rq3_same_beta, rq3_same_se, rq3_lrt_chi2, rq3_lrt_p, rq3_aic_delta) %>%
  kable(
    col.names = c("Model", "Params", "β (Same Sense)", "SE", "χ²", "p", "ΔAIC"),
    caption = "RQ3: Sense Boundary predicting target surprisal (final checkpoint)"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
```

<table class="table table-striped table-hover table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>RQ3: Sense Boundary predicting target surprisal (final checkpoint)</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Model </th>
   <th style="text-align:right;"> Params </th>
   <th style="text-align:right;"> β (Same Sense) </th>
   <th style="text-align:right;"> SE </th>
   <th style="text-align:right;"> χ² </th>
   <th style="text-align:left;"> p </th>
   <th style="text-align:right;"> ΔAIC </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> EleutherAI/pythia-14m </td>
   <td style="text-align:right;"> 14067712 </td>
   <td style="text-align:right;"> -0.359 </td>
   <td style="text-align:right;"> 0.033 </td>
   <td style="text-align:right;"> 115.073 </td>
   <td style="text-align:left;"> &lt;2e-16 </td>
   <td style="text-align:right;"> 113.073 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EleutherAI/pythia-70m </td>
   <td style="text-align:right;"> 70426624 </td>
   <td style="text-align:right;"> -0.426 </td>
   <td style="text-align:right;"> 0.030 </td>
   <td style="text-align:right;"> 190.472 </td>
   <td style="text-align:left;"> &lt;2e-16 </td>
   <td style="text-align:right;"> 188.472 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EleutherAI/pythia-160m </td>
   <td style="text-align:right;"> 162322944 </td>
   <td style="text-align:right;"> -0.594 </td>
   <td style="text-align:right;"> 0.029 </td>
   <td style="text-align:right;"> 355.356 </td>
   <td style="text-align:left;"> &lt;2e-16 </td>
   <td style="text-align:right;"> 353.356 </td>
  </tr>
</tbody>
</table>

``` r
developmental_results_rq3 %>%
  filter(checkpoint == 143000) %>%
  mutate(
    rq3_lrt_p = format.pval(rq3_lrt_p, digits = 3),
    across(c(rq3_same_beta, rq3_same_se, 
             rq3_lrt_chi2, rq3_aic_delta), ~ round(.x, 3))
  ) %>%
  select(model, n_params, rq3_same_beta, rq3_same_se, rq3_lrt_chi2, rq3_lrt_p, rq3_aic_delta) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c("Model", "Params", "$\\beta$", "SE", "$\\chi^2$", "$p$", "$\\Delta$AIC"),
    escape = FALSE,
    caption = "RQ3: Sense Boundary predicting target surprisal (final checkpoint)"
  ) %>%
  kable_styling(latex_options = c("hold_position"))
```

\begin{table}[!h]
\centering
\caption{\label{tab:tables_rq3}RQ3: Sense Boundary predicting target surprisal (final checkpoint)}
\centering
\begin{tabular}[t]{lrrrrlr}
\toprule
Model & Params & $\beta$ & SE & $\chi^2$ & $p$ & $\Delta$AIC\\
\midrule
EleutherAI/pythia-14m & 14067712 & -0.359 & 0.033 & 115.073 & <2e-16 & 113.073\\
EleutherAI/pythia-70m & 70426624 & -0.426 & 0.030 & 190.472 & <2e-16 & 188.472\\
EleutherAI/pythia-160m & 162322944 & -0.594 & 0.029 & 355.356 & <2e-16 & 353.356\\
\bottomrule
\end{tabular}
\end{table}

We also visualize the coefficients.


``` r
developmental_results_rq3 %>%
  filter(checkpoint == 143000) %>%
  mutate(
    model_shorthand = sub("^EleutherAI/", "", model),
    model_shorthand = fct_reorder(model_shorthand, n_params)
  ) %>%
  ggplot(aes(x = model_shorthand, y = rq3_same_beta)) +
  geom_pointrange(aes(
    ymin = rq3_same_beta - 1.96 * rq3_same_se,
    ymax = rq3_same_beta + 1.96 * rq3_same_se
  )) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    x = "Model (ordered by parameter count)",
    y = "β (Sense Boundary)",
    title = "RQ1: Effect of sense boundary on target surprisal",
    subtitle = "Point estimates ± 95% CI"
  ) +
  coord_flip() +
  theme_minimal(base_size = 13) 
```

![](rawc_lm_analysis_files/figure-html/coef_rq3_final-1.png)<!-- -->


And also the raw data.


``` r
df_pythia_models %>%
  filter(checkpoint == 143000) %>%
  ggplot(aes(x = target_surprisal_mean,
             fill = same_label,
             y = ambiguity_type)) +
  geom_density_ridges(color = NA, alpha = 0.7, scale = 0.9) +
  scale_fill_manual(values = c("Same Sense" = "#4E79A7", "Different Sense" = "#E15759")) +
  labs(x = "Mean target surprisal",
       y = NULL,
       fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  ) +
  facet_wrap(~reorder(model_shorthand, n_params))
```

```
## Picking joint bandwidth of 0.255
```

```
## Picking joint bandwidth of 0.206
```

```
## Picking joint bandwidth of 0.21
```

![](rawc_lm_analysis_files/figure-html/rq3_raw_data_final-1.png)<!-- -->

### RQ5: Model scaling


``` r
## scaling_results_rq1
developmental_results_rq3 %>%
  filter(checkpoint == 143000) %>%
  ggplot(aes(x = n_params,
             y = rq3_aic_delta)) +
  geom_point(size = 6,
             alpha = .5) +
  scale_x_log10() +
  geom_text_repel(aes(label=model), size=3) +
  labs(x = "Parameters",
       y = "AIC Delta") +
  theme_minimal() +
  theme(text = element_text(size = 15),
        legend.position = "bottom") 
```

![](rawc_lm_analysis_files/figure-html/rq3_scaling-1.png)<!-- -->

``` r
developmental_results_rq3 %>%
  filter(checkpoint == 143000) %>%
  ggplot(aes(x = n_params,
             y = rq3_same_beta)) +
  geom_point(size = 6,
             alpha = .5) +
  scale_x_log10() +
  geom_text_repel(aes(label=model), size=3) +
  labs(x = "Parameters",
       y = "Sense Boundary Coefficient") +
  theme_minimal() +
  theme(text = element_text(size = 15),
        legend.position = "bottom") 
```

![](rawc_lm_analysis_files/figure-html/rq3_scaling-2.png)<!-- -->

``` r
summary(lm(rq3_same_beta ~ log10(n_params),
           data = developmental_results_rq3 %>%
  filter(checkpoint == 143000)))
```

```
## 
## Call:
## lm(formula = rq3_same_beta ~ log10(n_params), data = developmental_results_rq3 %>% 
##     filter(checkpoint == 143000))
## 
## Residuals:
##        1        2        3 
## -0.01930  0.05653 -0.03723 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)
## (Intercept)      1.11938    0.71420   1.567    0.362
## log10(n_params) -0.20410    0.09218  -2.214    0.270
## 
## Residual standard error: 0.07038 on 1 degrees of freedom
## Multiple R-squared:  0.8306,	Adjusted R-squared:  0.6612 
## F-statistic: 4.903 on 1 and 1 DF,  p-value: 0.2701
```

``` r
summary(lm(rq3_aic_delta ~ log10(n_params),
           data = developmental_results_rq3 %>%
  filter(checkpoint == 143000)))
```

```
## 
## Call:
## lm(formula = rq3_aic_delta ~ log10(n_params), data = developmental_results_rq3 %>% 
##     filter(checkpoint == 143000))
## 
## Residuals:
##      1      2      3 
##  18.25 -53.44  35.19 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)
## (Intercept)     -1408.20     675.18  -2.086    0.285
## log10(n_params)   210.27      87.14   2.413    0.250
## 
## Residual standard error: 66.54 on 1 degrees of freedom
## Multiple R-squared:  0.8534,	Adjusted R-squared:  0.7068 
## F-statistic: 5.822 on 1 and 1 DF,  p-value: 0.2501
```


### RQ6: Developmental Trajectory


``` r
developmental_results_rq3 %>%
  mutate(significant = rq3_lrt_p < .05) %>%
  ggplot(aes(x = checkpoint,
             y = rq3_same_beta,
             color = reorder(model_shorthand, n_params))) +
  geom_point(aes(shape = significant),
             size = 4, alpha = .7) +
  geom_line(size = 1.3) +
  geom_point(size = 4, alpha = .4) +
  theme_minimal() +
  labs(x = "Training Step (Log10)",
       y = "Coefficient (Sense Boundary)",
       color = "") +
  scale_x_log10() +
  geom_vline(xintercept = 1000, 
             linetype = "dotted", 
             size = 1.2) +
  scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16),
                     labels = c("n.s.", "p < .05"),
                     name = NULL) +
  theme(text = element_text(size = 15),
        legend.position = "bottom") +
  scale_color_manual(values = viridisLite::viridis(4, option = "mako", 
                                                begin = 0.8, end = 0.15))
```

```
## Warning in scale_x_log10(): log-10 transformation introduced infinite values.
## log-10 transformation introduced infinite values.
## log-10 transformation introduced infinite values.
```

![](rawc_lm_analysis_files/figure-html/rq6_dev_senses-1.png)<!-- -->


