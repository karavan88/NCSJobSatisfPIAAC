# papers/job_satisfaction/R/paper_functions.R
# Preparation and estimators for the job satisfaction paper. Inference reuses
# repinfo() and pool() from the education-occupation mismatch paper, and the
# wage rules from R/piaac_prepare.R. Source from the repository root.

source("papers/education_occupation_mismatch/R/inference.R")   # repinfo(), pool()
source("papers/education_occupation_mismatch/R/mappings.R")    # parse_occ(), ilo_vertical()

traits <- c("EXTR", "CONS", "OPEM", "EMOS", "AGRE")
trait_label <- c(EXTR = "Extraversion", CONS = "Conscientiousness",
                 OPEM = "Open-mindedness", EMOS = "Emotional stability",
                 AGRE = "Agreeableness")
transition <- c("CZE", "EST", "HRV", "LTU", "LVA", "POL")
wage_countries <- c("CHL", "CZE", "ESP", "EST", "FRA", "HRV", "IRL", "ISR", "ITA", "LTU", "LVA", "NZL", "POL")
outcomes <- c("very_satisfied", "not_satisfied", "satisfaction_score")
outcome_label <- c(very_satisfied = "Extremely satisfied (0/1)", not_satisfied = "Not satisfied (0/1)",
                   satisfaction_score = "Satisfaction score (1-5)")

wm <- function(x, w) sum(x * w) / sum(w)
bg <- function(x, levels) factor(ifelse(x %in% levels, as.character(x), "Unknown"))
fac <- function(x, levels) factor(ifelse(x %in% levels, x, NA))
# Reference level first, Unknown last.
relevel_first <- function(f, ref) { f <- factor(f); if (ref %in% levels(f)) relevel(f, ref) else f }

# The shared wage rules (weighted 1/99 trim and [1/20, 20] x median), copied
# from R/piaac_prepare.R so that this file does not load the wage-paper setup.
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

# ---- preparation --------------------------------------------------------------
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
    nm <- if (v == "TASKDISCC2_T1") "taskdisc" else "learning"
    ok <- is.finite(d[[v]])
    m <- if (any(ok)) wm(d[[v]][ok], d$SPFWT0[ok]) else 0
    d[[nm]] <- ifelse(ok, d[[v]] - m, 0); d[[paste0(nm, "_miss")]] <- as.numeric(!ok)
  }
  start <- d$D2_Q05a2
  ten <- ifelse(is.finite(start) & start >= 1950 & start <= 2023, 2023 - start, NA)
  d$tenure <- factor(ifelse(is.na(ten), "Unknown",
    as.character(cut(ten, c(-Inf, 1, 4, 9, 19, Inf), labels = c("0-1", "2-4", "5-9", "10-19", "20+")))))
  d$tenure <- relevel_first(d$tenure, "5-9")
  d$skills_match <- relevel_first(factor(ifelse(d$H2_Q19a %in% 1:3,
    c("higher", "matched", "lower")[pmax(1, pmin(3, d$H2_Q19a))], "Unknown")), "matched")
  o <- parse_occ(d$ISCO08_C, d$ISCO2C, d$ISCO1C)
  v <- ilo_vertical(o$major, d$EDA_ISCED1, d$EDA_ISCED1_C)
  d$educ_match <- relevel_first(factor(ifelse(!is.na(v$vertical) & v$vertical == 1, "over",
    ifelse(!is.na(v$undereducated) & v$undereducated == 1, "under",
      ifelse(!is.na(v$vertical_match) & v$vertical_match == 1, "matched", "Unknown")))), "matched")
  d$life_sat <- ifelse(d$I2_Q05 %in% 0:10, d$I2_Q05, NA)
  # Earnings: shared rules applied among employees with positive earnings.
  d$log_wage <- NA_real_
  if (any(is.finite(d$EARNHRBONUSC2) & d$EARNHRBONUSC2 > 0 & d$D2_Q04 %in% 1)) {
    pos <- d$D2_Q04 %in% 1 & is.finite(d$EARNHRBONUSC2) & d$EARNHRBONUSC2 > 0
    keep <- pos; keep[pos] <- wage_keep(d$EARNHRBONUSC2[pos], d$SPFWT0[pos])
    d$log_wage[keep] <- log(d$EARNHRBONUSC2[keep])
  }
  # Within-country decile of hourly earnings incl. bonuses: released for employees in
  # 20 of the 23 countries (not CHE, FIN, SVK). The objective-reward control for H1.
  d$wage_decile <- bg(d$EARNHRBONUSDCLC2, 1:10)
  d$cell <- interaction(d$occupation, d$gender, d$age, drop = TRUE)
  d <- add_job_conditions(d)
  core <- c("D2_Q13", traits, "age", "gender", "education", paste0("PVNUM", 1:10))
  d$core_ok <- complete.cases(d[core])
  d
}

# ---- linear algebra -------------------------------------------------------------
# Weighted least squares; aliased columns get coefficient 0 (their contribution
# to fitted values is nil), so focal coefficients stay defined.
wls <- function(X, Y, w) {
  f <- lm.wfit(X, Y, w)
  b <- as.matrix(f$coefficients); b[is.na(b)] <- 0
  if (ncol(b) == 1) drop(b) else b
}
# Design matrix without intercept. Terms whose factors have fewer than two
# observed levels in this sample are dropped (e.g. hours are not released in Canada).
dm <- function(f, d) {
  tl <- attr(terms(f), "term.labels")
  single <- function(v) v %in% names(d) && is.factor(d[[v]]) && length(unique(d[[v]])) < 2
  tl <- tl[!vapply(tl, function(t) any(vapply(all.vars(as.formula(paste("~", t))), single, logical(1))), logical(1))]
  if (!length(tl)) return(matrix(numeric(0), nrow(d), 0))
  X <- model.matrix(reformulate(tl), d)
  X[, colnames(X) != "(Intercept)", drop = FALSE]
}

# ---- replicate x plausible-value engine ---------------------------------------
# est(w, j) returns a named numeric vector for weight vector w and PV j.
rep_pv <- function(d, est, pvs = 1:10) {
  ri <- repinfo(d); W <- ri$w
  bs <- us <- list()
  for (j in seq_along(pvs)) {
    th <- do.call(cbind, lapply(seq_len(ncol(W)), function(k) est(W[, k], pvs[j])))
    stopifnot(!anyNA(th[, 1]))
    b <- th[, 1]; delta <- sweep(th[, -1, drop = FALSE], 1, b)
    bs[[j]] <- b; us[[j]] <- ri$factor * tcrossprod(delta)
  }
  B <- do.call(cbind, bs); rownames(B) <- rownames(th)
  p <- pool(B, us, ri$r)
  p$table$n <- nrow(d); p$r <- ri$r; p$method <- ri$method
  p
}
wald <- function(p, terms) {
  i <- match(terms, p$table$term); b <- p$table$estimate[i]
  stat <- drop(t(b) %*% solve(p$cov[i, i, drop = FALSE], b))
  data.frame(statistic = stat, df = length(i), p = pchisq(stat, length(i), lower.tail = FALSE))
}

# ---- Part I --------------------------------------------------------------------
# Version 3: pay (within-country hourly-earnings decile) is part of the baseline
# control set. Where a country releases no deciles the factor has one level and
# dm() drops it, so the same formula runs everywhere.
base_f <- ~ EXTR + CONS + OPEM + EMOS + AGRE + age + gender + education + born + mother + father + wage_decile
base_f_nopay <- ~ EXTR + CONS + OPEM + EMOS + AGRE + age + gender + education + born + mother + father
job_blocks <- list(
  occupation = ~ occupation,
  contract_hours = ~ contract + hours,
  employer = ~ firm_size + sector,
  job_content = ~ manages + taskdisc + taskdisc_miss + learning + learning_miss)

# Gelbach (2016) decomposition with the base model X0 and added blocks.
gelbach <- function(X0, blocks, Y, w, focal) {
  b0 <- wls(X0, Y, w)
  X1 <- cbind(X0, do.call(cbind, blocks))
  b1 <- wls(X1, Y, w)
  out <- list(base = b0[focal], full = b1[focal])
  for (k in names(blocks)) {
    G <- as.matrix(wls(X0, blocks[[k]], w)); if (ncol(blocks[[k]]) == 1) G <- matrix(G, ncol = 1)
    rownames(G) <- colnames(X0)
    out[[k]] <- drop(G[focal, , drop = FALSE] %*% b1[colnames(blocks[[k]])])
  }
  out
}

# Builds the Part I estimator for one country and one outcome.
# extra_blocks: further named blocks (formulas) added after the job blocks.
part1_estimator <- function(d, y, skill = "PVNUM", extra_blocks = list(), base = base_f,
                            focal = traits, blocks = job_blocks) {
  X0 <- cbind(`(Intercept)` = 1, dm(base, d)); Y <- d[[y]]
  B <- lapply(c(blocks, extra_blocks), function(f) {
    if (is.matrix(f)) return(f)
    X <- dm(f, d); X[, colSums(X != 0) > 0, drop = FALSE]
  })
  B <- B[vapply(B, ncol, 1L) > 0]
  function(w, j) {
    num <- if (is.null(skill)) NULL else (d[[paste0(skill, j)]] - 250) / 50
    X <- if (is.null(num)) X0 else cbind(X0, numeracy = num)
    g <- gelbach(X, B, Y, w, focal)
    fn <- c(focal, if (!is.null(num)) "numeracy")
    b0 <- wls(X, Y, w)[fn]
    out <- c(setNames(b0, paste0("base:", fn)), setNames(g$full, paste0("full:", focal)),
      setNames(g$base - g$full, paste0("change:", focal)))
    for (k in names(B)) out <- c(out, setNames(g[[k]], paste0("via_", k, ":", focal)))
    out
  }
}

# Coefficients of named extra regressors in the full model (S1: mismatch; S2: pay).
part1_extra_estimator <- function(d, y, extra, skill = "PVNUM", base = base_f) {
  X0 <- cbind(`(Intercept)` = 1, dm(base, d)); Y <- d[[y]]
  B <- do.call(cbind, lapply(c(job_blocks, extra), function(f) dm(f, d)))
  B <- B[, colSums(B != 0) > 0, drop = FALSE]
  E <- do.call(cbind, lapply(extra, function(f) dm(f, d))); keep <- colnames(E)[colSums(E != 0) > 0]
  function(w, j) {
    X <- cbind(X0, numeracy = (d[[paste0(skill, j)]] - 250) / 50, B)
    b <- wls(X, Y, w)
    setNames(b[keep], paste0("coef:", keep))
  }
}

# ---- Part II: reference wages ---------------------------------------------------
loo_mean <- function(y, cell, w) {
  sw <- ave(w, cell, FUN = sum); swy <- ave(w * y, cell, FUN = sum)
  (swy - w * y) / (sw - w)
}
cell_rank <- function(y, cell, w) {
  r <- numeric(length(y))
  for (g in split(seq_along(y), cell)) {
    o <- g[order(y[g])]; cw <- cumsum(w[o]) - w[o] / 2
    r[o] <- if (sum(w[o]) > 0) cw / sum(w[o]) else 0.5
  }
  r
}
gap_breaks <- c(-Inf, -0.5, -0.3, -0.1, 0.1, 0.3, 0.5, Inf)
gap_labels <- c("< -0.5", "-0.5 to -0.3", "-0.3 to -0.1", "-0.1 to 0.1", "0.1 to 0.3", "0.3 to 0.5", "> 0.5")
part2_controls <- ~ age + gender + education + born + mother + father + hours + contract + firm_size + sector

part2_estimator <- function(d) {
  Z0 <- dm(part2_controls, d)
  CF <- dm(~ cell, d)                          # cell fixed effects (age, sex, occupation absorbed)
  OCC <- dm(~ occupation, d)
  MZ <- dm(~ age * gender + education + occupation, d)   # Clark-Oswald wage equation
  Y <- as.matrix(d[outcomes]); y <- d$log_wage
  w0 <- d$SPFWT0
  ref0 <- loo_mean(y, d$cell, w0)
  function(w, j) {
    num <- (d[[paste0("PVNUM", j)]] - 250) / 50; lit <- (d[[paste0("PVLIT", j)]] - 250) / 50
    Z <- cbind(Z0, numeracy = num)
    # (a) cell reference, rebuilt under this replicate weight
    ref <- loo_mean(y, d$cell, w); bad <- !is.finite(ref); ref[bad] <- ref0[bad]
    below <- pmax(ref - y, 0); rank <- cell_rank(y, d$cell, w)
    # (b) Mincer reference (c) skill reference
    refm <- drop(cbind(1, MZ) %*% wls(cbind(1, MZ), y, w))
    SZ <- cbind(1, num, num^2, lit, lit^2, dm(~ age * gender, d))
    refs <- drop(SZ %*% wls(SZ, y, w))
    gapbin <- cut(ref - y, gap_breaks, labels = gap_labels)
    GB <- model.matrix(~ relevel(gapbin, "-0.1 to 0.1"))[, -1, drop = FALSE]
    colnames(GB) <- paste0("bin ", setdiff(gap_labels, "-0.1 to 0.1"))
    pick <- function(b, terms, model) {
      b <- as.matrix(b); out <- c()
      for (o in colnames(Y)) out <- c(out, setNames(b[terms, o], paste0(model, "|", o, "|", terms)))
      out
    }
    one <- rep(1, length(y))
    c(
      pick(wls(cbind(1, Z, lnw = y), Y, w), "lnw", "own_only"),
      pick(wls(cbind(1, Z, lnw = y, ref = ref), Y, w), c("lnw", "ref"), "level_cell"),
      pick(wls(cbind(1, Z, OCC, lnw = y, ref = ref), Y, w), c("lnw", "ref"), "level_cell_occ"),
      pick(wls(cbind(1, Z, lnw = y, ref = refm), Y, w), c("lnw", "ref"), "level_mincer"),
      pick(wls(cbind(1, Z, lnw = y, ref = refs), Y, w), c("lnw", "ref"), "level_skill"),
      pick(wls(cbind(1, Z, CF, lnw = y, below = below), Y, w), c("lnw", "below"), "envy_cell"),
      pick(wls(cbind(1, Z, lnw = y, ref = refm, below = pmax(refm - y, 0)), Y, w), c("lnw", "ref", "below"), "envy_mincer"),
      pick(wls(cbind(1, Z, lnw = y, ref = refs, below = pmax(refs - y, 0)), Y, w), c("lnw", "ref", "below"), "envy_skill"),
      pick(wls(cbind(1, Z, CF, lnw = y, rank = rank), Y, w), c("lnw", "rank"), "rank_cell"),
      pick(wls(cbind(1, Z, CF, lnw = y, GB), Y, w), c("lnw", colnames(GB)), "bins_cell"),
      # descriptive: share below and gap SD
      c(`desc|all|share_below_cell` = wm(below > 0, w), `desc|all|sd_gap_cell` = sqrt(wm((ref - y)^2, w) - wm(ref - y, w)^2),
        `desc|all|r2_mincer` = 1 - wm((y - refm)^2, w) / wm((y - wm(y, w))^2, w),
        `desc|all|r2_skill` = 1 - wm((y - refs)^2, w) / wm((y - wm(y, w))^2, w))
    )
  }
}

# S2 with one pay block (own log wage and position relative to the cell
# reference). The two are not separately identified once occupation, sex and age
# are controlled (their difference is the cell reference), so they form one
# block. The own-wage coefficient is reported from the model without the
# relative position. The cell reference is rebuilt per replicate.
part1_wage_estimator <- function(d, y) {
  X0 <- cbind(`(Intercept)` = 1, dm(base_f, d)); Y <- d[[y]]
  B <- lapply(job_blocks, function(f) { X <- dm(f, d); X[, colSums(X != 0) > 0, drop = FALSE] })
  B <- B[vapply(B, ncol, 1L) > 0]
  ref0 <- loo_mean(d$log_wage, d$cell, d$SPFWT0)
  function(w, j) {
    X <- cbind(X0, numeracy = (d[[paste0("PVNUM", j)]] - 250) / 50)
    ref <- loo_mean(d$log_wage, d$cell, w); bad <- !is.finite(ref); ref[bad] <- ref0[bad]
    BB <- c(B, list(pay = cbind(lnw = d$log_wage, relpos = d$log_wage - ref)))
    g <- gelbach(X, BB, Y, w, traits)
    b1 <- wls(cbind(X, do.call(cbind, B), lnw = d$log_wage), Y, w)
    out <- c(setNames(g$base, paste0("base:", traits)), setNames(g$full, paste0("full:", traits)),
      setNames(g$base - g$full, paste0("change:", traits)), `coef:lnw` = b1[["lnw"]])
    for (k in names(BB)) out <- c(out, setNames(g[[k]], paste0("via_", k, ":", traits)))
    out
  }
}

tidy_pool <- function(p, cc, model) {
  t <- p$table; t$country <- cc; t$model <- model; t$reps <- p$r; t$method <- p$method; t
}

# Trait (and other focal) coefficients for several outcomes in one fit.
coef_estimator <- function(d, ys, f = base_f, skill = "PVNUM", focal = traits) {
  X0 <- cbind(`(Intercept)` = 1, dm(f, d)); Y <- as.matrix(d[ys]); colnames(Y) <- ys
  function(w, j) {
    X <- if (is.null(skill)) X0 else cbind(X0, numeracy = (d[[paste0(skill, j)]] - 250) / 50)
    b <- as.matrix(wls(X, Y, w)); colnames(b) <- ys
    out <- c()
    for (o in ys) out <- c(out, setNames(b[focal, o], paste0(o, "|", focal)))
    out
  }
}
# Cells with at least 10 other members (unweighted) define the Part II sample.
cell_ok <- function(d, min_others = 10) ave(rep(1, nrow(d)), d$cell, FUN = length) - 1 >= min_others


# ---- Version 2: job conditions, job cells, facets --------------------------------
conditions_core <- c("time_pressure", "repetitive", "physical", "change", "autonomy", "learning", "social")
conditions_ext <- c("support", "voice")
condition_label <- c(time_pressure = "Time pressure", repetitive = "Repetitive tasks", physical = "Physical work",
  change = "Change in work environment", autonomy = "Autonomy", learning = "Learning at work",
  social = "Social tasks", support = "Support", voice = "Voice")
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
    d[[k]] <- ifelse(is.finite(z), z, 0)
  }
  d$underuse <- as.numeric(d$skills_match == "higher"); d$deficit <- as.numeric(d$skills_match == "lower")
  d$match_unknown <- as.numeric(d$skills_match == "Unknown")
  # Job cell for "actual" conditions: finest occupation x industry section.
  occ2 <- ifelse(d$ISCO2C %in% sprintf("%02d", c(1:3, 11:14, 21:26, 31:35, 41:44, 51:54, 61:63, 71:75, 81:83, 91:96)) |
    suppressWarnings(as.numeric(d$ISCO2C)) %in% c(11:14, 21:26, 31:35, 41:44, 51:54, 61:63, 71:75, 81:83, 91:96), d$ISCO2C, NA)
  occ <- ifelse(!is.na(occ2), paste0("o2_", occ2), paste0("o1_", as.character(d$occupation)))
  ind <- ifelse(d$ISIC1C %in% LETTERS[1:21], d$ISIC1C, "unknown")
  d$occ_finest <- occ
  d$job_cell <- paste(occ, ind, sep = "|")
  d$aps_ok <- complete.cases(d[paste0("PVAPS", 1:10)])
  d
}
# A country has the extended module if support and voice are observed for most employees.
has_extended <- function(d) mean(d$support_miss == 0) > 0.5 && mean(d$voice_miss == 0) > 0.5
has_facets <- function(d) mean(complete.cases(d[facets])) > 0.9

# Leave-one-out job-cell mean of x with fallback to the occupation cell when the
# job cell has fewer than min_others other members (unweighted count).
actual_mean <- function(x, d, w, min_others = 10, cell = d$job_cell, fallback = d$occ_finest) {
  n_job <- ave(rep(1, nrow(d)), cell, FUN = length) - 1
  a <- loo_mean(x, cell, w); b <- loo_mean(x, fallback, w)
  out <- ifelse(n_job >= min_others & is.finite(a), a, b)
  out[!is.finite(out)] <- wm(x, w)
  out
}

structural_f <- ~ occupation + contract + hours + firm_size + sector + manages
cond_f <- function(set) reformulate(c(set, paste0(set, "_miss")))

has_wage_deciles <- function(d) mean(d$wage_decile != "Unknown") > 0.5
