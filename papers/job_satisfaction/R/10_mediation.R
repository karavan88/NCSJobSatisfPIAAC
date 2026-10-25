# Causal mediation (product of coefficients) by country: an alternative to the
# Gelbach decomposition, one mediator at a time, 1-5 satisfaction score.
# Run from the repository root: Rscript papers/job_satisfaction/R/10_mediation.R [countries]
options(warn = 1)
base <- "papers/job_satisfaction"
source(file.path(base, "R/paper_functions.R"))
source(file.path(base, "R/channel_functions.R"))
raw <- readRDS(file.path(base, "results/raw_selected.rds"))
countries <- setdiff(names(raw), c("DNK", "SGP", "JPN", "USA"))
no_job_cell <- c("AUT", "DEU")
dir.create(file.path(base, "results/mediation"), showWarnings = FALSE)
run_country <- function(cc) {
  d <- prepare(raw[[cc]], cc); s <- d[d$core_ok, ]
  p <- rep_pv(s, mediation_estimator(s, extended = has_extended(s), cells = !cc %in% no_job_cell))
  out <- tidy_pool(p, cc, "MED"); saveRDS(out, file.path(base, "results/mediation", paste0(cc, ".rds"))); cat(cc, "done\n"); out
}
todo <- if (length(commandArgs(TRUE))) commandArgs(TRUE) else countries
invisible(parallel::mclapply(todo, function(cc) tryCatch(run_country(cc), error = function(e) { cat(cc, "ERROR", conditionMessage(e), "\n"); NULL }),
  mc.cores = as.integer(Sys.getenv("PIAAC_CORES", "6")), mc.preschedule = FALSE))
