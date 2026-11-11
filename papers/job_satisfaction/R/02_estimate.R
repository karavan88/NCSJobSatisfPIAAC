# papers/job_satisfaction/R/02_estimate.R
# Weighted least squares estimates by country: the premium under alternative
# specifications (H1), trait coefficients on the occupational-group mean and on the
# worker's deviation from it (H3a), facets (H3b), trait x condition products (H4),
# the occupational satisfaction gap (H5), and the robustness checks (Oster, life
# satisfaction, problem solving, tenure, logit models). Final survey weight, mean of
# the 10 plausible values, ordinary weighted-least-squares standard errors.
#
#   Rscript papers/job_satisfaction/R/02_estimate.R [countries]     (make estimates)
#
# Output: results/estimates/<CC>.rds, a list with
#   coef   data frame: country, model, term, estimate, se, p, lower, upper, n
#   oster, diag, info, flow, pay_contrast
options(warn = 1)
base <- "papers/job_satisfaction"
source(file.path(base, "R/functions.R"))
raw <- readRDS(file.path(base, "results/raw_selected.rds"))
countries <- study_countries(names(raw))
out_dir <- file.path(base, "results/estimates")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Trait coefficients for the 3 outcomes under formula f (terms "<outcome>|<trait>").
spec <- function(d, f = base_f, extra = NULL, focal = traits) {
  X <- cbind(X_base(d, f), extra)
  fit_rows(wlm(X, ymat(d), d$SPFWT0), focal, NULL, sep_outcome = FALSE)
}

# Logit models for the binary outcomes: weighted average marginal effects of the traits;
# the standard error carries the z statistic of the trait coefficient.
logit_rows <- function(d) {
  X <- X_base(d)
  do.call(rbind, lapply(c("very_satisfied", "not_satisfied"), function(y) {
    yv <- d[[y]]; wv <- d$SPFWT0 / mean(d$SPFWT0)
    m <- glm(yv ~ X - 1, family = quasibinomial(), weights = wv)
    b <- coef(m); names(b) <- sub("^X", "", names(b)); ok <- !is.na(b)
    se <- sqrt(diag(vcov(m))); names(se) <- sub("^X", "", names(se))
    pr <- plogis(drop(X[, ok] %*% b[ok]))
    ame <- sapply(traits, function(v) wm(pr * (1 - pr) * b[[v]], d$SPFWT0))
    z <- b[traits] / se[traits]
    est_rows(ame, ame / z, paste0("logit_", y, "|", traits))
  }))
}

# H5: satisfaction gap between a high and a low occupational group. The standardized gap
# is the group coefficient in a model with group-specific trait slopes and traits centred
# at the pooled (weighted) mean, which equals regression standardization of both groups to
# the pooled personality distribution.
gap_rows <- function(d, hi, lo) {
  g <- ifelse(hi, 1, ifelse(lo, 0, NA)); k <- !is.na(g); d <- d[k, ]; g <- g[k]; w <- d$SPFWT0
  Tc <- sapply(traits, function(v) d[[v]] - wm(d[[v]], w))
  y <- d$satisfaction_score; one <- rep(1, nrow(d))
  co <- function(X, term) { f <- wlm(X, y, w); est_rows(f$b[term, 1], f$se[term, 1], term) }
  r <- rbind(
    co(cbind(`(Intercept)` = one, raw = g), "raw"),
    co(cbind(`(Intercept)` = one, standardised = g, Tc, Tc * g), "standardised"),
    co(cbind(`(Intercept)` = one, traits = g, Tc), "traits"),
    co(cbind(`(Intercept)` = one, traits_pay = g, Tc, mk(~ wage_decile, d)), "traits_pay"),
    co(cbind(`(Intercept)` = one, traits_pay_conditions = g, Tc, mk(~ wage_decile, d), mk(cond_f(conditions_core), d), mismatch(d)),
       "traits_pay_conditions"))
  r$term <- paste0("gap|all|", r$term)
  rbind(r, data.frame(term = "gap|all|share_high", estimate = wm(g, w), se = NA, p = NA, lower = NA, upper = NA))
}

# Oster (2019): delta that sets each trait coefficient to zero and the bias-adjusted
# coefficient at delta = 1, Rmax = 1.3 x R2 of the full model (point estimates).
oster <- function(d) {
  w <- d$SPFWT0
  Xr <- cbind(1, dm(~ EXTR + CONS + OPEM + EMOS + AGRE + age + gender, d))
  Xf <- cbind(X_base(d), mk(structural_f, d), mk(cond_f(conditions_core), d), mismatch(d))
  r2 <- function(f, y) { e <- y - drop(f$X %*% f$b); 1 - wm(e^2, w) / wm((y - wm(y, w))^2, w) }
  do.call(rbind, lapply(outcomes, function(o) {
    y <- d[[o]]
    fr <- wlm(Xr, y, w); ff <- wlm(Xf, y, w)
    br <- fr$b[traits, 1]; bf <- ff$b[traits, 1]
    Rr <- r2(list(X = Xr, b = ifelse(is.na(fr$b[, 1]), 0, fr$b[, 1])), y)
    Rf <- r2(list(X = Xf, b = ifelse(is.na(ff$b[, 1]), 0, ff$b[, 1])), y); Rmax <- min(1.3 * Rf, 1)
    data.frame(country = d$country[1], outcome = o, trait = traits, b_restricted = br, b_full = bf,
      R2_restricted = Rr, R2_full = Rf, Rmax = Rmax, delta = bf * (Rf - Rr) / ((br - bf) * (Rmax - Rf)),
      beta_star = bf - (br - bf) * (Rmax - Rf) / (Rf - Rr))
  }))
}

run_country <- function(cc) {
  t0 <- Sys.time()
  d <- prepare(raw[[cc]], cc); s <- d[d$core_ok, , drop = FALSE]
  ext <- has_extended(s); fac <- has_facets(s); w <- s$SPFWT0
  res <- list()
  add <- function(r, model) { if (!is.null(r) && nrow(r)) { r$model <- model; r$n <- nrow(s); res[[length(res) + 1]] <<- r } }
  Y <- ymat(s)

  # ---- H1: the premium and its specifications ----
  b0 <- wlm(X_base(s), Y, w)
  for (y in outcomes) { r <- fit_rows(b0, c(traits, "numeracy"), NULL, outcomes = y, sep_outcome = FALSE)
    r$term <- paste0("base:", sub(".*\\|", "", r$term)); add(r, paste0("main|", y)) }
  add(spec(s), "sens_base")
  add(spec(s, base_f_nopay), "sens_nopay")
  sl <- s; sl$numeracy <- sl$literacy; add(spec(sl[is.finite(sl$numeracy), ]), "sens_literacy")
  add(spec(s, update(base_f, ~ . + occupation)), "sens_occupation")
  if (has_wage_deciles(s)) {
    sp <- s[s$wage_decile != "Unknown", ]
    add(spec(sp), "sens_pay_complete"); add(spec(sp, base_f_nopay), "sens_nopay_complete")
  }
  if (sum(is.finite(s$log_wage)) > 500) {
    sw <- s[is.finite(s$log_wage), ]
    add(spec(sw), "sens_decile_wagesample")
    add(spec(sw, update(base_f_nopay, ~ . + log_wage)), "sens_logpay")
    add(spec(sw, update(base_f, ~ . + log_wage)), "sens_decile_plus_logpay")
  }
  t6 <- paste0(traits, "_6")
  if (sum(complete.cases(s[t6])) > 0.9 * nrow(s)) {
    s6 <- s[complete.cases(s[t6]), ]; for (v in traits) s6[[v]] <- s6[[paste0(v, "_6")]]
    add(spec(s6), "sens_six_item")
  }
  if (has_tenure(s)) add(spec(s, update(base_f, ~ . + tenure)), "sens_tenure")
  a <- prepare(raw[[cc]], cc, employees_only = FALSE); a <- a[a$core_ok, ]
  add(spec(a, update(base_f, ~ . + self_employed + occupation + taskdisc + taskdisc_miss + learn_index + learn_index_miss)), "sens_selfemployed")
  add(logit_rows(s), "nonlinear")

  # ---- H4: trait x condition products (traits centred), entered together ----
  for (e in unique(c(FALSE, ext))) {
    conds <- c(conditions_core, if (e) conditions_ext)
    pairs <- c(pairs_tested[if (e) names(pairs_tested) else setdiff(names(pairs_tested), "R8")], pairs_placebo)
    tc <- sapply(traits, function(v) s[[v]] - wm(s[[v]], w))
    P <- sapply(pairs, function(p) tc[, p[1]] * s[[p[2]]])
    X <- cbind(X_base(s), mk(structural_f, s), mk(cond_f(conds), s), mismatch(s), P)
    f <- wlm(X, Y, w)
    add(rbind(fit_rows(f, names(pairs), "pair"), fit_rows(f, conds, "cond"), fit_rows(f, c(traits, "underuse"), "main")),
        if (e) "Rext" else "R")
  }

  # All trait x condition products, one at a time, each in the model with all conditions
  # (traits centred, conditions standardized): the heatmap of the main text.
  conds_all <- c(conditions_core, if (ext) conditions_ext, "underuse")
  tc <- sapply(traits, function(v) s[[v]] - wm(s[[v]], w))
  X <- cbind(X_base(s), mk(structural_f, s), mk(cond_f(setdiff(conds_all, "underuse")), s), mismatch(s))
  add(do.call(rbind, lapply(traits, function(v) do.call(rbind, lapply(conds_all, function(k) {
    nm <- paste0(v, ":", k); f <- wlm(cbind(X, setNames(data.frame(tc[, v] * s[[k]]), nm) |> as.matrix()), ymat(s, "satisfaction_score"), w)
    fit_rows(f, nm, "prod") })))), "PROD")

  # ---- H3a: group mean and deviation; group definitions ----
  diag <- NULL
  if (!cc %in% no_job_cell) {
    conds <- c(conditions_core, if (ext) conditions_ext)
    variants <- list(A1 = list(10, "finest"), A1_min20 = list(20, "finest"), A1_min30 = list(30, "finest"), A1_1digit = list(10, "1digit"))
    for (nm in names(variants)) {
      sc <- split_conditions(s, conds, variants[[nm]][[1]], variants[[nm]][[2]])
      r <- do.call(rbind, lapply(conds, function(k) {
        wk <- w * (s[[paste0(k, "_miss")]] == 0)
        rbind(fit_rows(wlm(X_base(s), sc$P[, k], wk), traits, "perceived", outcomes = "y"),
              fit_rows(wlm(X_base(s), sc$A[, k], wk), traits, "actualsel", outcomes = "y")) |>
          transform(term = sub("\\|y\\|", paste0("|", k, "|"), term))
      }))
      add(r, nm)
    }
    diag <- rbind(cell_diagnostics(s), cell_diagnostics(s, 20), cell_diagnostics(s, 30), cell_diagnostics(s, resolution = "1digit"))
    # H5: managers and professionals (ISCO 1-2) against plant operators and elementary
    # occupations (ISCO 8-9); skill level 1 against 4 where only the skill level is released.
    src <- attr(d, "occupation_source"); oc <- as.character(s$occupation)
    hi <- if (src == "ISCO 1-digit") oc %in% c("1", "2") else oc == "1"
    lo <- if (src == "ISCO 1-digit") oc %in% c("8", "9") else oc == "4"
    if (sum(hi) >= 100 && sum(lo) >= 100) add(gap_rows(s, hi, lo), "IND")
  }

  # ---- H3b: facets (entered together, without the domain scores) ----
  if (fac) {
    sf <- s[complete.cases(s[facets]), ]; wf <- sf$SPFWT0; Yf <- ymat(sf)
    ff <- reformulate(c(facets, "age", "gender", "education", "born", "mother", "father", "numeracy", "wage_decile"))
    X0 <- cbind(`(Intercept)` = 1, dm(ff, sf)); f0 <- wlm(X0, Yf, wf)
    em <- c("EMOS_ANXI", "EMOS_DEPR", "EMOS_VOLA")
    P <- sapply(em, function(v) (sf[[v]] - wm(sf[[v]], wf)) * sf$time_pressure); colnames(P) <- paste0(em, ":time_pressure")
    f1 <- wlm(cbind(X0, mk(structural_f, sf), mk(cond_f(conditions_core), sf), mismatch(sf), P), Yf, wf)
    difs <- list(c("EMOS_DEPR", "EMOS_ANXI"), c("EMOS_DEPR", "EMOS_VOLA"), c("AGRE_TRUS", "AGRE_COMP"), c("AGRE_TRUS", "AGRE_RESP"),
                 c("EXTR_ENER", "EXTR_SOCI"), c("EXTR_ENER", "EXTR_ASSE"))
    dr <- do.call(rbind, lapply(outcomes, function(o) do.call(rbind, lapply(difs, function(p) {
      L <- setNames(c(1, -1), p); lincomb_rows(f0, L, paste0("facet_diff|", o, "|", p[1], "-", p[2]), o) }))))
    r <- rbind(fit_rows(f0, facets, "facet"), fit_rows(f1, c(facets, colnames(P)), "facet_full"), dr)
    r$n <- nrow(sf); r$model <- "A2"; res[[length(res) + 1]] <- r
  }

  # ---- robustness: conditions with and without life satisfaction; problem solving; tenure ----
  sl <- s[is.finite(s$life_sat), ]
  job_all <- cbind(mk(structural_f, sl), mk(cond_f(conditions_core), sl), mismatch(sl))
  add(fit_rows(wlm(cbind(X_base(sl), job_all), ymat(sl), sl$SPFWT0), traits, "jobs"), "A3_jobs")
  add(fit_rows(wlm(cbind(X_base(sl), job_all, life_sat = sl$life_sat), ymat(sl), sl$SPFWT0), c(traits, "life_sat"), "jobs_lifesat"), "A3_lifesat")
  sa <- s[is.finite(s$aps), ]
  if (nrow(sa) > 0.5 * nrow(s)) {
    Xa <- X_base(sa); Xa <- cbind(Xa[, colnames(Xa) != "numeracy"], skill = sa$aps)
    add(fit_rows(wlm(Xa, ymat(sa), sa$SPFWT0), c(traits, "skill"), "aps"), "V2_aps")
  }
  if (has_tenure(s)) {
    st <- s[s$tenure != "Unknown", ]; long <- as.numeric(st$tenure %in% c("5-9", "10-19", "20+"))
    tcen <- sapply(traits, function(v) st[[v]] - wm(st[[v]], st$SPFWT0)); pr <- tcen * long
    colnames(pr) <- paste0(traits, ":long_tenure")
    add(fit_rows(wlm(cbind(X_base(st), long_tenure = long, pr), ymat(st), st$SPFWT0), c(traits, colnames(pr)), "tenure"), "V3_tenure")
  }

  # ---- scale benchmark: top vs bottom pay decile ----
  pay_contrast <- NULL
  if (has_wage_deciles(s)) {
    sp <- s; sp$wage_decile <- relevel(factor(sp$wage_decile), "1"); f <- wlm(X_base(sp), sp$satisfaction_score, sp$SPFWT0)
    pay_contrast <- data.frame(country = cc, top_vs_bottom = f$b["wage_decile10", 1], emos = f$b["EMOS", 1])
  }

  info <- data.frame(country = cc, n = nrow(s), extended = ext, facets = fac, a1 = !cc %in% no_job_cell,
    tenure = has_tenure(s), n_wage = if (cc %in% wage_countries) sum(is.finite(s$log_wage)) else NA,
    n_facets = if (fac) sum(complete.cases(s[facets])) else NA, occupation_source = attr(d, "occupation_source"),
    pay_share = mean(s$wage_decile != "Unknown"))
  coef <- do.call(rbind, res); coef$country <- cc
  out <- list(coef = coef, oster = oster(s), info = info, diag = diag, pay_contrast = pay_contrast,
    flow = data.frame(country = cc, employees = nrow(d), analysis = nrow(s), with_self_employed = nrow(a)),
    seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
  saveRDS(out, file.path(out_dir, paste0(cc, ".rds")))
  cat(cc, "done in", round(out$seconds), "s\n")
  invisible(out)
}

todo <- if (length(commandArgs(TRUE))) commandArgs(TRUE) else countries
invisible(parallel::mclapply(todo, function(cc) tryCatch(run_country(cc),
  error = function(e) { cat(cc, "ERROR", conditionMessage(e), "\n"); NULL }),
  mc.cores = as.integer(Sys.getenv("PIAAC_CORES", "6")), mc.preschedule = FALSE))
