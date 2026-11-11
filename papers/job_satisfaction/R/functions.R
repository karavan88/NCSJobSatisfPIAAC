# papers/job_satisfaction/R/functions.R
# Sample rules, variable coding and estimators for the job satisfaction paper.
# Source from the repository root.
#
# Estimation basis, used by every script: one model per country, weighted least
# squares with the final survey weight (SPFWT0); numeracy, literacy and problem
# solving enter as the mean of their 10 plausible values (in 50-point units,
# centred at 250). Standard errors of the regression estimates are the ordinary
# weighted-least-squares standard errors; the mediation estimates carry the
# quasi-Bayesian intervals of mediation::mediate() (03_mediation.R).

traits <- c("EXTR", "CONS", "OPEM", "EMOS", "AGRE")
outcomes <- c("very_satisfied", "not_satisfied", "satisfaction_score")
wage_countries <- c("CHL", "CZE", "ESP", "EST", "FRA", "HRV", "IRL", "ITA", "LTU", "LVA", "NZL", "POL")
# Countries of the study: Cycle 2 countries that release the Big Five and the
# satisfaction item for employees. Denmark, Japan, Singapore and the United States
# release no Big Five scores or no satisfaction item; Israel is excluded (PROTOCOL.md).
study_countries <- function(available) setdiff(available, c("DNK", "SGP", "JPN", "USA", "ISR"))
no_job_cell <- c("AUT", "DEU")          # neither occupation nor industry released

wm <- function(x, w) sum(x * w) / sum(w)
bg <- function(x, levels) factor(ifelse(x %in% levels, as.character(x), "Unknown"))
fac <- function(x, levels) factor(ifelse(x %in% levels, x, NA))
relevel_first <- function(f, ref) { f <- factor(f); if (ref %in% levels(f)) relevel(f, ref) else f }
pv_mean <- function(d, skill) (rowMeans(as.matrix(d[paste0(skill, 1:10)])) - 250) / 50

# ---- wage rules (weighted 1/99 trim and [1/20, 20] x median) ---------------------
weighted_quantile <- function(x, w, p) {
  o <- order(x); x <- x[o]; cw <- cumsum(w[o]) / sum(w)
  vapply(p, function(pp) x[which(cw >= pp)[1]], numeric(1))
}
wage_keep <- function(wage, w, trim = c(0.01, 0.99), median_ratio = 20) {
  ok <- is.finite(wage) & wage > 0; keep <- ok
  q <- weighted_quantile(wage[ok], w[ok], c(trim, 0.5))
  keep[ok] <- wage[ok] >= q[1] & wage[ok] <= q[2] &
    wage[ok] >= q[3] / median_ratio & wage[ok] <= q[3] * median_ratio
  keep
}

# ---- reported job conditions -------------------------------------------------------
conditions_core <- c("time_pressure", "repetitive", "physical", "change", "autonomy", "learning", "social")
conditions_ext <- c("support", "voice")
facets <- c("EXTR_SOCI", "EXTR_ASSE", "EXTR_ENER", "AGRE_COMP", "AGRE_RESP", "AGRE_TRUS", "CONS_ORGA", "CONS_PROD",
  "CONS_RESP", "EMOS_ANXI", "EMOS_DEPR", "EMOS_VOLA", "OPEM_AEST", "OPEM_CURI", "OPEM_IMAG")

wstd <- function(x, w) { ok <- is.finite(x); m <- wm(x[ok], w[ok]); s <- sqrt(wm((x[ok] - m)^2, w[ok])); (x - m) / s }
row_mean <- function(M) { M <- as.matrix(M); r <- rowMeans(M, na.rm = TRUE); r[!is.finite(r)] <- NA; r }

add_job_conditions <- function(d) {
  v <- function(x, ok) ifelse(x %in% ok, x, NA_real_)
  d$time_pressure_raw <- v(d$H2_Q12, 1:5)
  d$repetitive_raw <- ifelse(d$H2_Q17 %in% 1:2, as.numeric(d$H2_Q17 == 1), NA)
  d$physical_raw <- v(d$H2_Q07a, 1:5)
  ch <- sapply(paste0("H2_Q18a0", 1:6), function(x) ifelse(d[[x]] %in% 1:2, as.numeric(d[[x]] == 1), NA))
  d$change_raw <- ifelse(rowSums(!is.na(ch)) > 0, rowSums(ch, na.rm = TRUE), NA)
  d$autonomy_raw <- row_mean(sapply(paste0("H2_Q08", letters[1:4]), function(x) v(d[[x]], 1:5)))
  d$learning_raw <- row_mean(sapply(paste0("H2_Q09", letters[1:3]), function(x) v(d[[x]], 1:5)))
  d$social_raw <- row_mean(sapply(c(paste0("H2_Q03", letters[1:4]), paste0("H2_Q05", letters[1:2])), function(x) v(d[[x]], 1:5)))
  sb <- ifelse(d$H2_Q13b %in% 1:3, (3 - d$H2_Q13b) / 2, NA); sc <- ifelse(d$H2_Q13c %in% 1:3, (3 - d$H2_Q13c) / 2, NA)
  d$support_raw <- ifelse(d$H2_Q13a %in% 2, 0, ifelse(d$H2_Q13a %in% 1, row_mean(cbind(sb, sc)), NA))
  d$voice_raw <- row_mean(sapply(paste0("H2_Q14", letters[1:2]), function(x) 6 - v(d[[x]], 1:5)))
  for (k in c(conditions_core, conditions_ext)) {
    x <- d[[paste0(k, "_raw")]]
    d[[paste0(k, "_miss")]] <- as.numeric(!is.finite(x))
    z <- if (any(is.finite(x))) wstd(x, d$SPFWT0) else rep(NA_real_, nrow(d))
    d[[k]] <- ifelse(is.finite(z), z, 0)       # missing: 0 with a missing-value indicator
  }
  d$underuse <- as.numeric(d$skills_match == "higher"); d$deficit <- as.numeric(d$skills_match == "lower")
  d$match_unknown <- as.numeric(d$skills_match == "Unknown")
  # Occupational group: finest released occupation x industry section.
  isco2 <- c(1:3, 11:14, 21:26, 31:35, 41:44, 51:54, 61:63, 71:75, 81:83, 91:96)
  occ2 <- ifelse(d$ISCO2C %in% sprintf("%02d", isco2) | suppressWarnings(as.numeric(d$ISCO2C)) %in% isco2, d$ISCO2C, NA)
  occ <- ifelse(!is.na(occ2), paste0("o2_", occ2), paste0("o1_", as.character(d$occupation)))
  ind <- ifelse(d$ISIC1C %in% LETTERS[1:21], d$ISIC1C, "unknown")
  d$occ_finest <- occ
  d$job_cell <- paste(occ, ind, sep = "|")
  d
}

# ---- analysis sample and coding --------------------------------------------------
prepare <- function(d, cc, employees_only = TRUE) {
  e <- d$AGEG10LFS %in% 2:5 & is.finite(d$SPFWT0) & d$SPFWT0 > 0 & d$D2_Q13 %in% 1:5
  e <- e & (if (employees_only) d$D2_Q04 %in% 1 else d$D2_Q04 %in% 1:2)
  d <- d[e, , drop = FALSE]
  d$country <- cc
  d$very_satisfied <- as.numeric(d$D2_Q13 == 1)
  d$not_satisfied <- as.numeric(d$D2_Q13 %in% 3:5)
  d$satisfaction_score <- 6 - d$D2_Q13
  d$self_employed <- as.numeric(d$D2_Q04 == 2)
  d$age <- fac(d$AGEG10LFS, 2:5); d$gender <- fac(d$GENDER_R, 1:2)
  d$education <- fac(d$EDCAT6_TC1, 1:6)
  d$born <- bg(d$A2_Q03a, 1:2); d$mother <- bg(d$J2_Q04c, 1:3); d$father <- bg(d$J2_Q05c, 1:3)
  d$occupation <- bg(d$ISCO1C, as.character(1:9))
  # Croatia, Ireland and New Zealand suppress ISCO in the public files; there the
  # occupation variable is the released 4-group occupational skill level.
  if (all(d$occupation == "Unknown")) d$occupation <- bg(d$ISCOSKIL4, 1:4)
  attr(d, "occupation_source") <- if (any(d$ISCO1C %in% as.character(1:9))) "ISCO 1-digit" else "ISCOSKIL4"
  d$contract <- bg(d$D2_Q10_C, 1:3)
  h <- d$D2_Q11_C
  d$hours <- factor(ifelse(is.finite(h) & h >= 1 & h <= 60,
    as.character(cut(h, c(0, 19, 34, 40, 48, 60), labels = c("<20", "20-34", "35-40", "41-48", ">48"))), "Unknown"))
  d$hours <- relevel_first(d$hours, "35-40")
  d$firm_size <- bg(d$D2_Q07a, 1:6); d$sector <- bg(d$D2_Q03, 1:3); d$manages <- bg(d$D2_Q09a, 1:2)
  for (v in c("TASKDISCC2_T1", "LEARNATWORKC2")) {
    nm <- if (v == "TASKDISCC2_T1") "taskdisc" else "learn_index"
    ok <- is.finite(d[[v]]); m <- if (any(ok)) wm(d[[v]][ok], d$SPFWT0[ok]) else 0
    d[[nm]] <- ifelse(ok, d[[v]] - m, 0); d[[paste0(nm, "_miss")]] <- as.numeric(!ok)
  }
  start <- d$D2_Q05a2
  ten <- ifelse(is.finite(start) & start >= 1950 & start <= 2023, 2023 - start, NA)
  d$tenure <- factor(ifelse(is.na(ten), "Unknown",
    as.character(cut(ten, c(-Inf, 1, 4, 9, 19, Inf), labels = c("0-1", "2-4", "5-9", "10-19", "20+")))))
  d$tenure <- relevel_first(d$tenure, "5-9")
  d$skills_match <- relevel_first(factor(ifelse(d$H2_Q19a %in% 1:3,
    c("higher", "matched", "lower")[pmax(1, pmin(3, d$H2_Q19a))], "Unknown")), "matched")
  d$life_sat <- ifelse(d$I2_Q05 %in% 0:10, d$I2_Q05, NA)
  d$log_wage <- NA_real_
  if (any(is.finite(d$EARNHRBONUSC2) & d$EARNHRBONUSC2 > 0 & d$D2_Q04 %in% 1)) {
    pos <- d$D2_Q04 %in% 1 & is.finite(d$EARNHRBONUSC2) & d$EARNHRBONUSC2 > 0
    keep <- pos; keep[pos] <- wage_keep(d$EARNHRBONUSC2[pos], d$SPFWT0[pos])
    d$log_wage[keep] <- log(d$EARNHRBONUSC2[keep])
  }
  # Within-country decile of hourly earnings incl. bonuses: released for employees in
  # 20 of the 23 countries (not CHE, FIN, SVK).
  d$wage_decile <- bg(d$EARNHRBONUSDCLC2, 1:10)
  d$pay <- suppressWarnings(as.numeric(as.character(d$wage_decile)))   # 1-10, NA where not released
  d <- add_job_conditions(d)
  core <- c("D2_Q13", traits, "age", "gender", "education", paste0("PVNUM", 1:10))
  d$core_ok <- complete.cases(d[core])
  d$numeracy <- ifelse(d$core_ok, pv_mean(d, "PVNUM"), NA)
  d$literacy <- if (all(paste0("PVLIT", 1:10) %in% names(d))) pv_mean(d, "PVLIT") else NA
  d$aps <- if (all(paste0("PVAPS", 1:10) %in% names(d))) pv_mean(d, "PVAPS") else NA
  d
}
has_extended <- function(d) mean(d$support_miss == 0) > 0.5 && mean(d$voice_miss == 0) > 0.5
has_facets <- function(d) mean(complete.cases(d[facets])) > 0.9
has_wage_deciles <- function(d) mean(d$wage_decile != "Unknown") > 0.5
has_tenure <- function(d) mean(d$tenure != "Unknown") > 0.9

# ---- model specification ---------------------------------------------------------
# Baseline: the five traits, age, sex, education, birthplace, parental education,
# numeracy and the pay decile. Where a country releases no deciles the factor has one
# level and dm() drops it, so the same formula runs everywhere.
base_f <- ~ EXTR + CONS + OPEM + EMOS + AGRE + age + gender + education + born + mother + father + numeracy + wage_decile
base_f_nopay <- ~ EXTR + CONS + OPEM + EMOS + AGRE + age + gender + education + born + mother + father + numeracy
structural_f <- ~ occupation + contract + hours + firm_size + sector + manages
cond_f <- function(set) reformulate(c(set, paste0(set, "_miss")))

# Design matrix without intercept; terms whose factors have fewer than two observed
# levels in this sample are dropped (e.g. hours are not released in Canada).
dm <- function(f, d) {
  tl <- attr(terms(f), "term.labels")
  single <- function(v) v %in% names(d) && is.factor(d[[v]]) && length(unique(d[[v]])) < 2
  tl <- tl[!vapply(tl, function(t) any(vapply(all.vars(as.formula(paste("~", t))), single, logical(1))), logical(1))]
  if (!length(tl)) return(matrix(numeric(0), nrow(d), 0))
  X <- model.matrix(reformulate(tl), d)
  X[, colnames(X) != "(Intercept)", drop = FALSE]
}
nz <- function(X) X[, colSums(X != 0) > 0, drop = FALSE]
mk <- function(f, d) nz(dm(f, d))
mismatch <- function(d) nz(as.matrix(d[c("underuse", "deficit", "match_unknown")]))

# ---- weighted least squares with model-based standard errors -----------------------
# X: design with intercept; Y: vector or matrix of outcomes; w: survey weight (rows with
# w = 0 are left out). Aliased columns are dropped and reported as NA.
wlm <- function(X, Y, w) {
  Y <- as.matrix(Y); if (is.null(colnames(Y))) colnames(Y) <- "y"
  k <- w > 0; X <- X[k, , drop = FALSE]; Y <- Y[k, , drop = FALSE]; w <- w[k]
  q <- qr(X * sqrt(w)); keep <- sort(q$pivot[seq_len(q$rank)])
  Xr <- X[, keep, drop = FALSE]
  f <- lm.wfit(Xr, Y, w); b <- as.matrix(f$coefficients)
  e <- Y - Xr %*% b; dfr <- nrow(Xr) - ncol(Xr)
  s2 <- colSums(w * e^2) / dfr
  XtWXi <- solve(crossprod(Xr * sqrt(w)))
  B <- SE <- matrix(NA_real_, ncol(X), ncol(Y), dimnames = list(colnames(X), colnames(Y)))
  B[keep, ] <- b; SE[keep, ] <- sqrt(outer(diag(XtWXi), s2))
  list(b = B, se = SE, n = nrow(Xr), df = dfr,
       vcov = function(o = 1) { V <- s2[o] * XtWXi; dimnames(V) <- list(colnames(Xr), colnames(Xr)); V })
}
# Tidy rows "kind|outcome|term" (or "prefix|term" when outcome is NULL).
est_rows <- function(estimate, se, term) {
  data.frame(term = term, estimate = unname(estimate), se = unname(se), p = 2 * pnorm(-abs(estimate / se)),
             lower = unname(estimate - 1.96 * se), upper = unname(estimate + 1.96 * se), stringsAsFactors = FALSE)
}
fit_rows <- function(fit, terms, kind, outcomes = colnames(fit$b), sep_outcome = TRUE) {
  terms <- intersect(terms, rownames(fit$b))
  do.call(rbind, lapply(outcomes, function(o) est_rows(fit$b[terms, o], fit$se[terms, o],
    if (sep_outcome) paste(kind, o, terms, sep = "|") else paste(o, terms, sep = "|"))))
}
# Linear combination L'b with its standard error.
lincomb_rows <- function(fit, L, term, o) {
  V <- fit$vcov(o); b <- fit$b[rownames(V), o]; Lv <- L[rownames(V)]; Lv[is.na(Lv)] <- 0
  est_rows(sum(Lv * b), sqrt(drop(t(Lv) %*% V %*% Lv)), term)
}
ymat <- function(d, ys = outcomes) { Y <- as.matrix(d[ys]); colnames(Y) <- ys; Y }
X_base <- function(d, f = base_f) cbind(`(Intercept)` = 1, dm(f, d))

# ---- occupational groups -----------------------------------------------------------
loo_mean <- function(y, cell, w) {
  sw <- ave(w, cell, FUN = sum); swy <- ave(w * y, cell, FUN = sum)
  (swy - w * y) / (sw - w)
}
# Leave-one-out group mean of x with fallback to the occupation alone when the group
# has fewer than min_others other members (unweighted count).
actual_mean <- function(x, d, w, min_others = 10, cell = d$job_cell, fallback = d$occ_finest) {
  n_job <- ave(rep(1, nrow(d)), cell, FUN = length) - 1
  a <- loo_mean(x, cell, w); b <- loo_mean(x, fallback, w)
  out <- ifelse(n_job >= min_others & is.finite(a), a, b)
  out[!is.finite(out)] <- wm(x, w)
  out
}
job_cell_def <- function(d, resolution = c("finest", "1digit")) {
  resolution <- match.arg(resolution)
  if (resolution == "finest") return(list(cell = d$job_cell, fallback = d$occ_finest))
  ind <- sub(".*\\|", "", d$job_cell); occ1 <- paste0("o1_", as.character(d$occupation))
  list(cell = paste(occ1, ind, sep = "|"), fallback = occ1)
}
# Group mean (A) and the worker's deviation from it (P) for each condition, observed rows.
split_conditions <- function(d, conds, min_others = 10, resolution = "finest") {
  cd <- job_cell_def(d, resolution)
  A <- sapply(conds, function(k) actual_mean(d[[k]], d, d$SPFWT0 * (d[[paste0(k, "_miss")]] == 0), min_others, cd$cell, cd$fallback))
  P <- sapply(conds, function(k) ifelse(d[[paste0(k, "_miss")]] == 0, d[[k]] - A[, k], 0))
  list(A = matrix(A, nrow(d), dimnames = list(NULL, conds)), P = matrix(P, nrow(d), dimnames = list(NULL, conds)))
}
# Group diagnostics: number and size of groups, use of the fallback, share of each
# condition's variance between groups, and reliability ICC(k) = (MSB - MSW) / MSB.
cell_diagnostics <- function(d, min_others = 10, resolution = "finest") {
  cd <- job_cell_def(d, resolution); w <- d$SPFWT0
  n_job <- ave(rep(1, nrow(d)), cd$cell, FUN = length) - 1
  used <- ifelse(n_job >= min_others, cd$cell, paste0("fb:", cd$fallback))
  k <- ave(rep(1, nrow(d)), used, FUN = length)
  out <- data.frame(country = d$country[1], resolution = resolution, min_others = min_others, n = nrow(d),
    cells_defined = length(unique(cd$cell)), cells_used = length(unique(used)), median_cell_size = median(k),
    share_fallback = wm(n_job < min_others, w), share_occ1_only = wm(grepl("^o1_", d$occ_finest), w),
    share_industry_unknown = wm(grepl("\\|unknown$", d$job_cell), w))
  for (v in conditions_core) {
    ok <- d[[paste0(v, "_miss")]] == 0; x <- d[[v]][ok]; g <- factor(used[ok]); ww <- w[ok]
    m <- ave(x * ww, g, FUN = sum) / ave(ww, g, FUN = sum)
    out[[paste0("between_", v)]] <- wm((m - wm(x, ww))^2, ww) / wm((x - wm(x, ww))^2, ww)
    a <- anova(lm(x ~ g)); msb <- a$`Mean Sq`[1]; msw <- a$`Mean Sq`[2]
    out[[paste0("reliability_", v)]] <- max(0, (msb - msw) / msb)
  }
  out
}

# ---- pre-specified trait x condition products (H4) and placebos --------------------
pairs_tested <- list(R1 = c("EMOS", "time_pressure"), R2 = c("EMOS", "repetitive"), R3 = c("EMOS", "change"),
  R4 = c("EXTR", "social"), R5 = c("OPEM", "learning"), R6 = c("OPEM", "underuse"), R7 = c("CONS", "autonomy"),
  R8 = c("AGRE", "support"))
pairs_placebo <- list(PL1 = c("EXTR", "physical"), PL2 = c("AGRE", "repetitive"), PL3 = c("CONS", "social"))
