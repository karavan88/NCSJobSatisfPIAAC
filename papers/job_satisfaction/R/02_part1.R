# Part I: Big Five and job satisfaction in 24 countries (P1, P2, S1, S2, linear sensitivities).
# Run from the repository root: Rscript papers/job_satisfaction/R/02_part1.R
options(warn = 1)
base <- "papers/job_satisfaction"
source(file.path(base, "R/paper_functions.R"))
raw <- readRDS(file.path(base, "results/raw_selected.rds"))
countries <- setdiff(names(raw), c("DNK", "SGP", "JPN", "USA"))
dir.create(file.path(base, "results/part1"), showWarnings = FALSE)

run_country <- function(cc) {
  t0 <- Sys.time()
  d <- prepare(raw[[cc]], cc)
  s <- d[d$core_ok, , drop = FALSE]
  res <- list(); joint <- list(); flow <- data.frame(country = cc, employees = nrow(d), analysis = nrow(s))
  add <- function(p, model) res[[length(res) + 1]] <<- tidy_pool(p, cc, model)
  for (y in outcomes) {
    p <- rep_pv(s, part1_estimator(s, y)); add(p, paste0("main|", y))
    joint[[length(joint) + 1]] <- cbind(country = cc, model = paste0("main|", y), wald(p, paste0("base:", traits)))
    joint[[length(joint) + 1]] <- cbind(country = cc, model = paste0("full|", y), wald(p, paste0("full:", traits)))
    # S1: mismatch block after the job blocks
    p <- rep_pv(s, part1_estimator(s, y, extra_blocks = list(mismatch = ~ skills_match + educ_match)))
    add(p, paste0("s1|", y))
    add(rep_pv(s, part1_extra_estimator(s, y, list(mismatch = ~ skills_match + educ_match))), paste0("s1coef|", y))
  }
  # S2 (wage countries) is estimated in 02b_s2.R.
  # Linear sensitivities on the P1 (base) model
  add(rep_pv(s, coef_estimator(s, outcomes)), "sens_base")
  add(rep_pv(s, coef_estimator(s, outcomes, f = base_f_nopay)), "sens_nopay")
  add(rep_pv(s, coef_estimator(s, outcomes, skill = "PVLIT")), "sens_literacy")
  # Version 5 estimands: pay-and-occupation-adjusted premium; complete-case pay
  # comparison on one sample; continuous earnings where released.
  add(rep_pv(s, coef_estimator(s, outcomes, f = update(base_f, ~ . + occupation))), "sens_occupation")
  if (has_wage_deciles(s)) {
    sp <- s[s$wage_decile != "Unknown", ]
    add(rep_pv(sp, coef_estimator(sp, outcomes)), "sens_pay_complete")
    add(rep_pv(sp, coef_estimator(sp, outcomes, f = base_f_nopay)), "sens_nopay_complete")
  }
  if (sum(is.finite(s$log_wage)) > 500) {
    sw <- s[is.finite(s$log_wage), ]
    add(rep_pv(sw, coef_estimator(sw, outcomes)), "sens_decile_wagesample")
    add(rep_pv(sw, coef_estimator(sw, outcomes, f = update(base_f_nopay, ~ . + log_wage))), "sens_logpay")
    add(rep_pv(sw, coef_estimator(sw, outcomes, f = update(base_f, ~ . + log_wage))), "sens_decile_plus_logpay")
  }
  t6 <- paste0(traits, "_6")
  if (sum(complete.cases(s[t6])) > 0.9 * nrow(s)) {
    s6 <- s[complete.cases(s[t6]), ]; for (v in traits) s6[[v]] <- s6[[paste0(v, "_6")]]
    add(rep_pv(s6, coef_estimator(s6, outcomes)), "sens_six_item")
  }
  if (mean(s$tenure != "Unknown") > 0.9)
    add(rep_pv(s, coef_estimator(s, outcomes, f = update(base_f, ~ . + tenure))), "sens_tenure")
  sl <- s[is.finite(s$life_sat), ]
  add(rep_pv(sl, coef_estimator(sl, outcomes, f = update(base_f, ~ . + life_sat))), "sens_lifesat_control")
  sl$life_sat_y <- sl$life_sat
  add(rep_pv(sl, coef_estimator(sl, "life_sat_y")), "lifesat_outcome")
  add(rep_pv(sl, coef_estimator(sl, outcomes)), "sens_lifesat_sample")
  # Self-employed included: occupation and job content only
  a <- prepare(raw[[cc]], cc, employees_only = FALSE); a <- a[a$core_ok, ]
  add(rep_pv(a, coef_estimator(a, outcomes, f = update(base_f, ~ . + self_employed + occupation +
    taskdisc + taskdisc_miss + learning + learning_miss))), "sens_selfemployed")
  flow$with_self_employed <- nrow(a)
  out <- list(coef = do.call(rbind, res), joint = do.call(rbind, joint), flow = flow,
    seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
  saveRDS(out, file.path(base, "results/part1", paste0(cc, ".rds")))
  cat(cc, "done in", round(out$seconds), "s\n")
  out
}
todo <- if (length(commandArgs(TRUE))) commandArgs(TRUE) else countries
res <- parallel::mclapply(todo, function(cc) tryCatch(run_country(cc), error = function(e) { cat(cc, "ERROR", conditionMessage(e), "\n"); NULL }),
  mc.cores = as.integer(Sys.getenv("PIAAC_CORES", "6")), mc.preschedule = FALSE)
