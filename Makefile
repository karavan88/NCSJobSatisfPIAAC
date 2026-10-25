# Reproducible workflow for "Are Non-Cognitive Skills Rewarded with Job Satisfaction Net of Pay?"
# Run from the repository root.
#
#   make data      unpack the OECD PIAAC public-use archives in 01_data/
#   make all       full pipeline: import, estimation, collection, Word submission files
#   make paper     render the manuscript from the cached estimates in papers/job_satisfaction/results/
#   make check     confirm R, Quarto and the R packages are available

CORES ?= 6
MS := papers/job_satisfaction/manuscript

.PHONY: data all paper check

data:
	Rscript papers/job_satisfaction/R/00_unpack_data.R

all:
	CORES=$(CORES) sh papers/job_satisfaction/R/run_all.sh

paper:
	quarto render $(MS)/job_satisfaction.qmd
	quarto render $(MS)/appendix.qmd

check:
	R --version | head -1
	quarto --version
	Rscript -e 'p <- c("haven","survey","mediation","ggplot2","patchwork","knitr","kableExtra","flextable","officer","here","MASS","tidyselect"); m <- p[!vapply(p, requireNamespace, logical(1), quietly = TRUE)]; if (length(m)) stop("missing: ", paste(m, collapse = ", ")) else cat("all packages available\n")'
