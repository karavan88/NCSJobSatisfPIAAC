# Independent checks of the estimation engine.
# Run from the repository root: Rscript papers/job_satisfaction/R/06_validate.R
options(warn = 1)
suppressMessages(library(survey))
base <- "papers/job_satisfaction"
source(file.path(base, "R/paper_functions.R"))
source(file.path(base, "R/channel_functions.R"))
raw <- readRDS(file.path(base, "results/raw_selected.rds"))
checks <- list()
log_check <- function(name, ok, detail = "") {
  checks[[length(checks) + 1]] <<- data.frame(check = name, pass = isTRUE(ok), detail = detail)
  cat(if (isTRUE(ok)) "PASS" else "FAIL", name, detail, "\n")
}

# 1. Replicate variance for one plausible value against the survey package (Fay and JK2).
for (cc in c("ESP", "FRA", "CHL")) {
  d <- prepare(raw[[cc]], cc); s <- d[d$core_ok, ]
  s$numeracy <- (s$PVNUM1 - 250) / 50
  f <- satisfaction_score ~ EXTR + CONS + OPEM + EMOS + AGRE + numeracy + age + gender + education
  est <- function(w, j) { X <- cbind(1, dm(update(f, NULL ~ .), s)); b <- wls(X, s$satisfaction_score, w); b[traits] }
  ours <- rep_pv(s, est, pvs = 1)$table
  ri <- repinfo(s)
  des <- if (ri$method == "JK2") svrepdesign(data = s, weights = ~SPFWT0, repweights = s[paste0("SPFWT", 1:ri$r)],
      type = "JK2", combined.weights = TRUE, mse = TRUE)
    else svrepdesign(data = s, weights = ~SPFWT0, repweights = s[paste0("SPFWT", 1:ri$r)],
      type = "Fay", rho = unique(s$VEFAYFAC), combined.weights = TRUE, mse = TRUE)
  m <- svyglm(f, design = des)
  ref <- SE(m)[traits]
  diff <- max(abs(ours$se - ref) / ref)
  log_check(paste("replicate SE vs survey::svyglm,", cc, ri$method, ri$r, "replicates"), diff < 1e-6,
    sprintf("max relative SE difference %.2e", diff))
  log_check(paste("point estimate vs survey::svyglm,", cc), max(abs(ours$estimate - coef(m)[traits])) < 1e-8)
}

# 1b. Part I estimators reproduce lm() point estimates (intercept, factor coding).
d <- prepare(raw$AUT, "AUT"); s <- d[d$core_ok, ]; s$numeracy <- (s$PVNUM3 - 250) / 50
for (y in outcomes) {
  m <- lm(update(base_f, as.formula(paste(y, "~ . + numeracy"))), data = s, weights = SPFWT0)
  a <- coef_estimator(s, y)(s$SPFWT0, 3)
  b <- part1_estimator(s, y)(s$SPFWT0, 3)
  log_check(paste("coef_estimator and part1_estimator match lm(),", y),
    max(abs(a - coef(m)[traits])) < 1e-8 && max(abs(b[paste0("base:", traits)] - coef(m)[traits])) < 1e-8)
}

# 2. Leave-one-out cell mean against a brute-force loop.
d <- prepare(raw$EST, "EST"); s <- d[d$core_ok & is.finite(d$log_wage), ]; s <- s[cell_ok(s), ]
fast <- loo_mean(s$log_wage, s$cell, s$SPFWT0)
idx <- sample(nrow(s), 50)
slow <- sapply(idx, function(i) { o <- which(s$cell == s$cell[i] & seq_len(nrow(s)) != i); wm(s$log_wage[o], s$SPFWT0[o]) })
log_check("leave-one-out cell mean", max(abs(fast[idx] - slow)) < 1e-10)
log_check("own wage excluded from own reference", cor(fast - ave(s$log_wage, s$cell), s$log_wage) < 0)

# 3. Gelbach components add up to the change in each trait coefficient.
files <- list.files(file.path(base, "results/part1"), full.names = TRUE)
if (length(files)) {
  gap <- sapply(files, function(f) {
    t <- readRDS(f)$coef
    max(sapply(unique(t$model[grepl("^main|^s1\\||^s2", t$model)]), function(m) {
      u <- t[t$model == m, ]
      max(sapply(traits, function(v) abs(sum(u$estimate[grepl(paste0("^via_.*:", v, "$"), u$term)]) -
        u$estimate[u$term == paste0("change:", v)])))
    }))
  })
  log_check("Gelbach blocks sum to the coefficient change (all countries, models)", max(gap) < 1e-8,
    sprintf("max gap %.1e over %d countries", max(gap), length(files)))
}

# 6. Version 2: multi-outcome Gelbach adds up; reactivity products match lm().
d <- prepare(raw$CZE, "CZE"); s <- d[d$core_ok, ]
v <- exposure_estimator(s, extended = TRUE)(s$SPFWT0, 4)
gap <- max(sapply(outcomes, function(o) max(sapply(traits, function(t) abs(sum(v[grepl(paste0("^via_.*\\|", o, "\\|", t, "$"), names(v))]) -
  v[[paste0("change|", o, "|", t)]])))))
log_check("version 2 Gelbach blocks sum to the change (CZE, all outcomes)", gap < 1e-10, sprintf("max gap %.1e", gap))
r <- reactivity_estimator(s, extended = TRUE)(s$SPFWT0, 4)
s$numeracy <- (s$PVNUM4 - 250) / 50; tc <- function(v) s[[v]] - wm(s[[v]], s$SPFWT0)
s$R1 <- tc("EMOS") * s$time_pressure; s$R4 <- tc("EXTR") * s$social; s$R6 <- tc("OPEM") * s$underuse
s$R2 <- tc("EMOS") * s$repetitive; s$R3 <- tc("EMOS") * s$change; s$R5 <- tc("OPEM") * s$learning
s$R7 <- tc("CONS") * s$autonomy; s$R8 <- tc("AGRE") * s$support; s$PL1 <- tc("EXTR") * s$physical
s$PL2 <- tc("AGRE") * s$repetitive; s$PL3 <- tc("CONS") * s$social
f <- update(base_f, satisfaction_score ~ . + numeracy + occupation + contract + hours + firm_size + sector + manages +
  time_pressure + repetitive + physical + change + autonomy + learning + social + support + voice +
  time_pressure_miss + repetitive_miss + physical_miss + change_miss + autonomy_miss + learning_miss + social_miss + support_miss + voice_miss +
  underuse + deficit + match_unknown + R1 + R2 + R3 + R4 + R5 + R6 + R7 + R8 + PL1 + PL2 + PL3)
m <- lm(f, data = s, weights = SPFWT0)
log_check("reactivity products match lm() (CZE, score)",
  max(abs(r[paste0("pair|satisfaction_score|", c("R1", "R4", "R6", "R8", "PL1"))] - coef(m)[c("R1", "R4", "R6", "R8", "PL1")])) < 1e-8)

# 7. Causal mediation: product of coefficients equals mediation::mediate (CZE, PV1).
suppressMessages(library(mediation))
d <- prepare(raw$CZE, "CZE"); s <- d[d$core_ok, ]
v <- mediation_estimator(s, extended = TRUE)(s$SPFWT0, 1)
s <- s[s$time_pressure_miss == 0, ]   # the estimator drops rows without the mediator
s$numeracy <- (s$PVNUM1 - 250) / 50; s$w <- s$SPFWT0 / mean(s$SPFWT0)
fm <- lm(update(base_f, time_pressure ~ . + numeracy), data = s, weights = w)
fy <- lm(update(base_f, satisfaction_score ~ . + numeracy + time_pressure), data = s, weights = w)
set.seed(1); mm <- mediate(fm, fy, treat = "EMOS", mediator = "time_pressure", treat.value = 1, control.value = 0, sims = 2000)
ab <- coef(fm)[["EMOS"]] * coef(fy)[["time_pressure"]]
log_check("mediation ACME = a x b (exact) and matches mediation::mediate (simulation)",
  abs(v[["acme|time_pressure|EMOS"]] - ab) < 1e-10 && abs(mm$d0 - ab) < 0.1 * abs(ab),
  sprintf("ours %.5f, mediate %.5f", ab, mm$d0))

fr <- lm(update(base_f, satisfaction_score ~ . + numeracy), data = s, weights = w)
ms <- medsens(mediate(lm(update(base_f, time_pressure ~ . + numeracy), data = s), lm(update(base_f, satisfaction_score ~ . + numeracy + time_pressure), data = s),
  treat = "EMOS", mediator = "time_pressure", sims = 100), rho.by = 0.01)
rho_unw <- cor(resid(lm(update(base_f, time_pressure ~ . + numeracy), data = s)), resid(lm(update(base_f, satisfaction_score ~ . + numeracy), data = s)))
log_check("sensitivity: closed-form rho (ACME = 0) matches mediation::medsens grid (unweighted)", abs(rho_unw - ms$err.cr.d) <= 0.011,
  sprintf("closed form %.3f, medsens %.2f", rho_unw, ms$err.cr.d))

# 5. Version 5 additions: core joint ACME, facet contrasts, indicator gap, cell variants.
v5 <- v
log_check("joint ACME over the core set equals the full-set joint ACME where no extended module exists",
  { ok <- TRUE; for (f in list.files(file.path(base, "results/mediation"), full.names = TRUE)) { m <- readRDS(f)
      if (!any(grepl("^acme\\|support", m$term))) ok <- ok && max(abs(m$estimate[grepl("^acme_joint\\|", m$term)] - m$estimate[grepl("^acme_joint_core\\|", m$term)])) < 1e-12 }; ok })
log_check("joint ACME + joint direct effect reconcile with the total (CZE, core set; gap = missing-indicator and underuse auxiliaries)",
  abs(sum(v5[grepl("^acme_joint_core\\|all\\|EMOS", names(v5))]) + v5[["ade_joint_core|all|EMOS"]] - v5[["total|all|EMOS"]]) < 0.02,
  sprintf("joint %.4f + direct %.4f vs total %.4f", v5[["acme_joint_core|all|EMOS"]], v5[["ade_joint_core|all|EMOS"]], v5[["total|all|EMOS"]]))
sf <- d[d$core_ok & complete.cases(d[facets]), ]; fv <- facet_estimator(sf)(sf$SPFWT0, 1)
log_check("facet contrast equals the difference of facet coefficients (CZE)",
  abs(fv[["facet_diff|satisfaction_score|EMOS_DEPR-EMOS_ANXI"]] - (fv[["facet|satisfaction_score|EMOS_DEPR"]] - fv[["facet|satisfaction_score|EMOS_ANXI"]])) < 1e-12)
s0 <- d[d$core_ok, ]; oc <- as.character(s0$occupation); hi <- oc %in% c("1", "2"); lo <- oc %in% c("8", "9")
gv <- indicator_gap_estimator(s0, hi, lo)(s0$SPFWT0[hi | lo], 1)
raw <- with(s0[hi, ], wm(satisfaction_score, SPFWT0)) - with(s0[lo, ], wm(satisfaction_score, SPFWT0))
ss <- s0[hi | lo, ]; ss$hi <- as.numeric(hi[hi | lo]); lmg <- lm(satisfaction_score ~ hi + EXTR + CONS + OPEM + EMOS + AGRE, data = ss, weights = SPFWT0)
log_check("indicator gap: raw equals weighted mean difference and common-slope gap matches lm() (CZE)",
  abs(gv[["gap|all|raw"]] - raw) < 1e-10 && abs(gv[["gap|all|traits"]] - coef(lmg)[["hi"]]) < 1e-10,
  sprintf("raw %.4f, standardised %.4f, common slope %.4f", gv[["gap|all|raw"]], gv[["gap|all|standardised"]], gv[["gap|all|traits"]]))
dg <- cell_diagnostics(s0); dg20 <- cell_diagnostics(s0, 20)
log_check("cell diagnostics: a stricter minimum cell size uses the fallback more often (CZE)", dg20$share_fallback >= dg$share_fallback,
  sprintf("fallback share %.2f at 10, %.2f at 20; reliability of time pressure %.2f", dg$share_fallback, dg20$share_fallback, dg$reliability_time_pressure))

out <- do.call(rbind, checks)
write.csv(out, file.path(base, "results/validation.csv"), row.names = FALSE)
cat(sum(out$pass), "of", nrow(out), "checks passed\n")
