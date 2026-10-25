# job_satisfaction

"Are Non-Cognitive Skills Rewarded with Job Satisfaction Net of Pay? A Cross-Country Analysis of Well-Being at Work in High-Income Economies" PIAAC Cycle 2, employees aged 25–65. Version 3 (15 October 2026),
prepared for Social Indicators Research.

H1: the subjective reward to personality (satisfaction net of pay deciles,
education, occupation and background). Then three routes: exposure (causal
mediation through pay and job conditions, joint ACME, sensitivity rho),
appraisal (job-cell actual vs perceived split, facets, life satisfaction) and
reactivity (pre-specified trait × condition products). Gelbach decomposition,
Oster bounds, a cognitive placebo and a tenure gradient are robustness checks.
Israel is excluded (author's decision). CHE, FIN and SVK have no pay deciles
and are flagged. Decisions and dated changes:
`PROTOCOL.md`. Version 1 (with the dropped reference-wage part) is in
`archive_v1/`.

```sh
make job-satisfaction CORES=6     # full pipeline
make job-satisfaction-render      # collect cached results and render
```

| File | Role |
|:-----|:-----|
| `R/01_import.R` | Raw import (employed), Korea personality link, coverage audit |
| `R/paper_functions.R` | Preparation, job conditions, replicate × PV engine, Gelbach |
| `R/channel_functions.R` | Exposure, reactivity, appraisal and verification estimators |
| `R/02_part1.R` | Baseline trait associations (P1) and version 1 sensitivities |
| `R/05_nonlinear.R` | Logit and ordered-logit sensitivities |
| `R/08_channels.R` | E1–E3, R, A1–A3, V1–V3 by country |
| `R/10_mediation.R` | Causal mediation by country: per-mediator and joint ACME, sensitivity, actual/perceived split |
| `R/11_pay_contrast.R` | Scale benchmark: top-vs-bottom earnings-decile satisfaction gap (point estimates) |
| `R/06_validate.R` | Checks against survey::svyglm and lm(), Gelbach adding-up |
| `R/07_collect.R` | Tidy tables with BH q-values → `results/summary.rds` (v5 adds `S$cell_diag`, models `A1_min20/A1_min30/A1_1digit/IND`, blocks `sens_occupation`, `sens_pay_complete`, `sens_nopay_complete`, `sens_logpay`, `sens_decile_wagesample`, `sens_decile_plus_logpay`, terms `acme_joint_core`, `ade_joint_core`, `facet_diff`) |
| `manuscript/job_satisfaction.qmd` | The paper (all numbers inline; APA style via `apa.csl`) |
| `manuscript/figure_helpers.R` | Figure conventions (v4): one ink colour, p<.05/p<.10/hollow, alphabetical country rows with mean/median + 95% CI at the bottom, direct/indirect bar figure, split figure, means figure |
| `output/job_satisfaction.html` | Rendered paper |

Every model is estimated separately by country with all ten plausible values
and all replicate weights (Fay or JK2), Rubin pooling; job-cell means are
rebuilt under every replicate weight. No pooled estimates; summaries count
countries.

## Version 5 (October 2026)

Response to the editorial assessment: `REVISION_PLAN.md` (plan and pushbacks), `RESPONSE_MATRIX.md` (concern → change → location), dated entry in `PROTOCOL.md`. New estimands (pay- and occupation-adjusted H1), core/extended joint decomposition, group-split diagnostics and sensitivity, equivalence tests, facet contrasts, the indicator test (ISCO 1–2 vs 8–9 gap, standardised to a common personality distribution) and the Oster table.

## Submission package (version 6)

`sh papers/job_satisfaction/R/make_submission.sh` (from the repository root) renders the title page, the anonymised manuscript (PDF and HTML) and the appendix into `output/submission/`. The manuscript sources are `manuscript/job_satisfaction.qmd`, `appendix.qmd`, `title_page.qmd`, with shared code in `_setup.R` and the abstract in `_abstract.qmd`.
