#!/bin/sh
# Full pipeline for the job satisfaction paper. Run from the repository root.
# CORES workers estimate countries in parallel (default 6).
set -e
CORES=${CORES:-6}
export PIAAC_CORES=$CORES OMP_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1 OPENBLAS_NUM_THREADS=1
Rscript papers/job_satisfaction/R/00_unpack_data.R
Rscript papers/job_satisfaction/R/01_import.R
Rscript papers/job_satisfaction/R/02_part1.R "$@"
Rscript papers/job_satisfaction/R/08_channels.R
Rscript papers/job_satisfaction/R/10_mediation.R
Rscript papers/job_satisfaction/R/11_pay_contrast.R
Rscript papers/job_satisfaction/R/05_nonlinear.R
Rscript papers/job_satisfaction/R/06_validate.R
Rscript papers/job_satisfaction/R/07_collect.R
sh papers/job_satisfaction/R/make_submission.sh
