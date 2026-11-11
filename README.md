# NCSJobSatisfPIAAC

Code that reproduces every number, table and figure of **"Are Non-Cognitive Skills Rewarded with Job Satisfaction Net of Pay? Evidence from High-Income Countries"** (Garen Avanesian) from the PIAAC Cycle 2 public-use files.

The study estimates a causal mediation model of the job-satisfaction return to each of the Big Five traits, with pay, job demands, job resources and skill underuse as mediator blocks, in employees aged 25–65 in 23 countries. It splits each reported condition into the occupation-by-industry group mean and the worker's deviation from it, tests trait-by-condition interactions, and estimates how much of the satisfaction gap between high and low occupational groups reflects personality composition. The mediation estimates are outputs of `mediation::mediate()` (Imai, Keele and Yamamoto 2010; Tingley et al. 2014) and their sensitivity analysis of `medsens()`; the other estimates are weighted least squares. Every model uses the final survey weight, and numeracy enters as the mean of its 10 plausible values.

## Structure

```
01_data/                     OECD public-use files (not tracked; see 01_data/README.md)
papers/job_satisfaction/
  R/                         analysis pipeline (see below)
  manuscript/                Quarto sources: main text, appendix, title page, bibliography
  results/                   estimates used by the manuscript (tracked, so the paper renders without re-estimating)
  PROTOCOL.md                dated analysis plan and run record
Makefile                     entry points
```

The `papers/` folder can hold further manuscripts on the same data, each with its own `R/`, `manuscript/` and `results/`; `01_data/` is shared.

## Requirements

- R 4.4 or later with `haven`, `tidyselect`, `mediation` (4.5.1), `ggplot2`, `patchwork`, `knitr`, `kableExtra`, `flextable`, `officer` and `here`; `make check` lists what is installed.
- Quarto 1.5 or later, Python 3 (post-processing of the Word files), and a TeX distribution with XeLaTeX for the PDF appendix.

## Data

Download the PIAAC Cycle 2 public-use files (SPSS format) from the OECD, <https://www.oecd.org/skills/piaac/data/>, into `01_data/` (`01_data/README.md` lists them; `results/inputs.csv` gives their MD5 checksums). Korea releases the Big Five in a separate file, `prg_2korms_big5.sav`, which `01_import.R` merges by respondent identifier. Nothing in `01_data/` is tracked by git.

## Run

```
make data         # unpack the OECD archives in 01_data/
make import       # read the 23 country files
make estimates    # weighted least squares estimates by country
make mediation    # mediate() and medsens() by country
make collect      # results/summary.rds, read by the manuscript
make render       # output/submission/: manuscript and appendix (Word), appendix (PDF), title page
```

`make all` runs `import` to `render` in order; `CORES` sets the number of countries estimated in parallel (default 6). The results of `make estimates` and `make mediation` are included, so `make render` works without the raw data.

Run time on a laptop with 8 cores: import 2 minutes, estimates 1 minute, mediation about 2 hours (1,000 quasi-Bayesian draws per estimate; `MED_SIMS` changes it), rendering 2 minutes.

## What produces what

| Step | Script | Output | Used for |
|---|---|---|---|
| Import | `R/01_import.R` | `results/raw_selected.rds` (not tracked), `coverage.csv`, `inputs.csv` | all estimates; Appendix Table A9 |
| Regressions | `R/02_estimate.R` | `results/estimates/<CC>.rds` | descriptives, group-mean and deviation coefficients, facets, interactions, occupational gap, robustness |
| Mediation | `R/03_mediation.R` | `results/mediation/<CC>.rds` | total, direct and indirect effects, group-mean and deviation split, sensitivity (ρ*) |
| Collection | `R/04_collect.R` | `results/summary.rds` | every table and figure |
| Rendering | `R/make_submission.sh` | `output/submission/` | manuscript, appendix, title page |

Sample rules and variable coding are in `R/functions.R`.
