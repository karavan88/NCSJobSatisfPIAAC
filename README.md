# NCSJobSatisfPIAAC

Replication files for **"Are Non-Cognitive Skills Rewarded with Job Satisfaction Net of Pay? A Cross-Country Analysis of Well-Being at Work in High-Income Economies"** (Garen Avanesian).

The paper uses the OECD Survey of Adult Skills 2023 (PIAAC Cycle 2) to estimate the job-satisfaction premium associated with the Big Five personality traits at a given level of pay, in employees aged 25–65 in 23 countries. It decomposes the premium through pay and reported working conditions (causal mediation analysis), separates each reported condition into the occupation-by-industry group mean and the worker's deviation from it, tests trait-by-condition interactions, and estimates how much of the satisfaction gap between high and low occupational groups reflects personality composition.

## Structure

```
01_data/                         place the OECD public-use archives here (not included; see 01_data/README.md)
papers/job_satisfaction/
  R/                             analysis pipeline (00_unpack_data.R ... 11_pay_contrast.R, run_all.sh)
  manuscript/                    Quarto sources: main text, Online Resource 1, title page, bibliography
  results/                       cached estimates used by the manuscript (summary.rds and per-country files)
  PROTOCOL.md                    dated analysis plan and run record
papers/education_occupation_mismatch/R/
                                 shared helpers for replicate-weight inference and occupation codes
Makefile                         entry points (see below)
```

The layout mirrors the author's working repository, so every path in the code runs unchanged from the repository root.

## Reproducing the results

Requirements: R (4.4 or later), Quarto (1.5 or later), and the R packages `haven`, `survey`, `mediation`, `ggplot2`, `patchwork`, `knitr`, `kableExtra`, `flextable`, `officer`, `here`, `MASS` and `tidyselect`. Run `make check` to confirm them.

```sh
make data      # unpack the raw archives placed in 01_data/
make all       # full pipeline: import, estimation in every country, collection, rendering
make paper     # render the manuscript only, from the cached estimates in results/
```

`make all` estimates every model for every country with replicate weights and ten plausible values; set `CORES` to the number of parallel workers (default 6). `make paper` needs no raw data: it renders from `papers/job_satisfaction/results/`.

## Data

The raw data are the PIAAC Cycle 2 public-use files distributed by the OECD (<https://www.oecd.org/skills/piaac/data/>). They are not redistributed here; `01_data/README.md` lists the files to download, and `papers/job_satisfaction/results/inputs.csv` gives their MD5 checksums.
