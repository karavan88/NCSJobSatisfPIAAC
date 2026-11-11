# Reproduces "Are Non-Cognitive Skills Rewarded with Job Satisfaction Net of Pay?" from the
# PIAAC Cycle 2 public-use files (see README.md). Run from the repository root.
#
#   make data         unpack the OECD archives placed in 01_data/
#   make import       read the 23 country files (results/raw_selected.rds, coverage.csv)
#   make estimates    weighted least squares estimates by country (results/estimates/)
#   make mediation    mediation::mediate() estimates by country (results/mediation/)
#   make collect      one results file for the manuscript (results/summary.rds)
#   make render       manuscript (docx), appendix (docx and pdf) and title page
#   make all          everything above, in order
#   make check        confirm that R, Quarto and the R packages are available

CORES ?= 6
P := papers/job_satisfaction
RUN := PIAAC_CORES=$(CORES) OMP_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript

.PHONY: all data import estimates mediation collect render check
all: import estimates mediation collect render

data:
	Rscript $(P)/R/00_unpack_data.R

import:
	Rscript $(P)/R/01_import.R

estimates:
	$(RUN) $(P)/R/02_estimate.R

mediation:
	$(RUN) $(P)/R/03_mediation.R

collect:
	Rscript $(P)/R/04_collect.R

render:
	LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8 sh $(P)/R/make_submission.sh

check:
	R --version | head -1
	quarto --version
	Rscript -e 'p <- c("haven", "tidyselect", "mediation", "ggplot2", "patchwork", "knitr", "kableExtra", "flextable", "officer", "here"); for (x in p) cat(sprintf("%-11s %s\n", x, tryCatch(as.character(packageVersion(x)), error = function(e) "MISSING")))'
