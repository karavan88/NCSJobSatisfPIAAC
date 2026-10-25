# Version 2: exposure, reactivity, appraisal and verification models, by country.
# Run from the repository root: Rscript papers/job_satisfaction/R/08_channels.R [countries]
options(warn = 1)
base <- "papers/job_satisfaction"
source(file.path(base, "R/paper_functions.R"))
source(file.path(base, "R/channel_functions.R"))
raw <- readRDS(file.path(base, "results/raw_selected.rds"))
countries <- setdiff(names(raw), c("DNK", "SGP", "JPN", "USA"))
no_job_cell <- c("AUT", "DEU")          # neither occupation nor industry released: A1 not estimable
dir.create(file.path(base, "results/channels"), showWarnings = FALSE)

run_country <- function(cc) {
  t0 <- Sys.time()
  d <- prepare(raw[[cc]], cc); s <- d[d$core_ok, , drop = FALSE]
  ext <- has_extended(s); fac <- has_facets(s)
  res <- list(); add <- function(p, model) res[[length(res) + 1]] <<- tidy_pool(p, cc, model)
  # E: exposure
  add(rep_pv(s, exposure_estimator(s)), "E1")
  if (ext) add(rep_pv(s, exposure_estimator(s, extended = TRUE)), "E1ext")
  add(rep_pv(s, selection_estimator(s, c(conditions_core, if (ext) conditions_ext))), "E2")
  if (cc %in% wage_countries) {
    sw <- s[is.finite(s$log_wage), ]
    add(rep_pv(sw, exposure_estimator(sw, extended = ext, pay = TRUE)), "E3")
  }
  # R: reactivity
  add(rep_pv(s, reactivity_estimator(s)), "R")
  if (ext) add(rep_pv(s, reactivity_estimator(s, extended = TRUE)), "Rext")
  # A: appraisal, with cell-definition sensitivities and diagnostics (version 5)
  diag <- NULL
  if (!cc %in% no_job_cell) {
    add(rep_pv(s, appraisal_estimator(s, extended = ext)), "A1")
    add(rep_pv(s, appraisal_estimator(s, extended = ext, min_others = 20, decomp = FALSE)), "A1_min20")
    add(rep_pv(s, appraisal_estimator(s, extended = ext, min_others = 30, decomp = FALSE)), "A1_min30")
    add(rep_pv(s, appraisal_estimator(s, extended = ext, resolution = "1digit", decomp = FALSE)), "A1_1digit")
    diag <- rbind(cell_diagnostics(s), cell_diagnostics(s, 20), cell_diagnostics(s, 30), cell_diagnostics(s, resolution = "1digit"))
    # Indicator test: managers and professionals (ISCO 1-2) against plant operators and
    # elementary occupations (ISCO 8-9); skilled (1) against elementary (4) where only the
    # occupational skill level is released.
    src <- attr(d, "occupation_source"); oc <- as.character(s$occupation)
    hi <- if (src == "ISCO 1-digit") oc %in% c("1", "2") else oc == "1"
    lo <- if (src == "ISCO 1-digit") oc %in% c("8", "9") else oc == "4"
    if (sum(hi) >= 100 && sum(lo) >= 100) add(rep_pv(s[hi | lo, ], indicator_gap_estimator(s, hi, lo)), "IND")
  }
  if (fac) { sf <- s[complete.cases(s[facets]), ]; add(rep_pv(sf, facet_estimator(sf)), "A2") }
  sl <- s[is.finite(s$life_sat), ]
  job_all <- cbind(mk(structural_f, sl), mk(cond_f(conditions_core), sl), nz(as.matrix(sl[c("underuse", "deficit", "match_unknown")])))
  add(rep_pv(sl, controls_estimator(sl, extra = job_all, label = "jobs")), "A3_jobs")
  add(rep_pv(sl, controls_estimator(sl, extra = cbind(job_all, life_sat = sl$life_sat), label = "jobs_lifesat")), "A3_lifesat")
  # V: verification
  ost <- oster(s)
  add(rep_pv(s, controls_estimator(s, skill = "PVAPS", label = "aps")), "V2_aps")
  add(rep_pv(s, controls_estimator(s, skill = "PVAPS", both = TRUE, label = "aps_num")), "V2_both")
  if (mean(s$tenure != "Unknown") > 0.9) {
    st <- s[s$tenure != "Unknown", ]; long <- as.numeric(st$tenure %in% c("5-9", "10-19", "20+"))
    tcen <- sapply(traits, function(v) st[[v]] - wm(st[[v]], st$SPFWT0))
    pr <- tcen * long; colnames(pr) <- paste0(traits, ":long_tenure")
    add(rep_pv(st, controls_estimator(st, extra = cbind(long_tenure = long), products = pr, label = "tenure")), "V3_tenure")
  }
  info <- data.frame(country = cc, n = nrow(s), extended = ext, facets = fac, a1 = !cc %in% no_job_cell,
    tenure = mean(s$tenure != "Unknown") > 0.9, n_wage = if (cc %in% wage_countries) sum(is.finite(s$log_wage)) else NA,
    n_facets = if (fac) sum(complete.cases(s[facets])) else NA, occupation_source = attr(d, "occupation_source"))
  out <- list(coef = do.call(rbind, res), oster = ost, info = info, diag = diag,
    seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
  saveRDS(out, file.path(base, "results/channels", paste0(cc, ".rds")))
  cat(cc, "done in", round(out$seconds), "s\n")
  out
}
todo <- if (length(commandArgs(TRUE))) commandArgs(TRUE) else countries
invisible(parallel::mclapply(todo, function(cc) tryCatch(run_country(cc),
  error = function(e) { cat(cc, "ERROR", conditionMessage(e), "\n"); NULL }),
  mc.cores = as.integer(Sys.getenv("PIAAC_CORES", "6")), mc.preschedule = FALSE))
