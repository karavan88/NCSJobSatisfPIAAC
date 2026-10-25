# Version 2 estimators: exposure (E), reactivity (R), appraisal (A), verification (V).
# Each *_estimator(d) returns est(w, j) for rep_pv(): a named vector for weight
# vector w and plausible value j. Source after paper_functions.R.

nz <- function(X) X[, colSums(X != 0) > 0, drop = FALSE]
mk <- function(f, d) nz(dm(f, d))
num_pv <- function(d, j, skill = "PVNUM") (d[[paste0(skill, j)]] - 250) / 50
flat <- function(b, rows, prefix) {           # b: p x outcomes matrix -> "prefix|outcome|row"
  b <- as.matrix(b); out <- c()
  for (o in colnames(b)) out <- c(out, setNames(b[rows, o], paste0(prefix, "|", o, "|", rows)))
  out
}
ymat <- function(d, ys = outcomes) { Y <- as.matrix(d[ys]); colnames(Y) <- ys; Y }

# Gelbach (2016) for several outcomes at once. The auxiliary regressions of the
# blocks on the base design do not depend on the outcome.
gelbach_m <- function(X0, blocks, Y, w, focal) {
  b0 <- as.matrix(wls(X0, Y, w)); colnames(b0) <- colnames(Y)
  X1 <- cbind(X0, do.call(cbind, blocks)); b1 <- as.matrix(wls(X1, Y, w)); colnames(b1) <- colnames(Y)
  out <- list(base = b0[focal, , drop = FALSE], full = b1[focal, , drop = FALSE])
  for (k in names(blocks)) {
    G <- matrix(wls(X0, blocks[[k]], w), nrow = ncol(X0), dimnames = list(colnames(X0), colnames(blocks[[k]])))
    out[[k]] <- G[focal, , drop = FALSE] %*% b1[colnames(blocks[[k]]), , drop = FALSE]
  }
  out
}
gelbach_out <- function(g, focal) {
  r <- c(flat(g$base, focal, "base"), flat(g$full, focal, "full"), flat(g$base - g$full, focal, "change"))
  for (k in setdiff(names(g), c("base", "full"))) r <- c(r, flat(g[[k]], focal, paste0("via_", k)))
  r
}

exposure_blocks <- function(d, extended = FALSE) {
  B <- list(structural = mk(structural_f, d),
    demands = mk(cond_f(c("time_pressure", "repetitive", "physical", "change")), d),
    autonomy = mk(cond_f("autonomy"), d), learning = mk(cond_f("learning"), d), social = mk(cond_f("social"), d),
    underuse = nz(as.matrix(d[c("underuse", "deficit", "match_unknown")])))
  if (extended) B$support_voice <- mk(cond_f(conditions_ext), d)
  B[vapply(B, ncol, 1L) > 0]
}

# E1 / E3: decomposition of the trait coefficients; pay = TRUE adds log wage last.
exposure_estimator <- function(d, extended = FALSE, pay = FALSE) {
  X0 <- cbind(`(Intercept)` = 1, dm(base_f, d)); Y <- ymat(d)
  B <- exposure_blocks(d, extended)
  if (pay) B$pay <- cbind(lnw = d$log_wage)
  function(w, j) gelbach_out(gelbach_m(cbind(X0, numeracy = num_pv(d, j)), B, Y, w, traits), traits)
}

# E2: which traits select into which conditions (observed rows only).
selection_estimator <- function(d, conds) {
  X0 <- cbind(`(Intercept)` = 1, dm(base_f, d))
  function(w, j) {
    X <- cbind(X0, numeracy = num_pv(d, j)); out <- c()
    for (k in conds) {
      wk <- w * (d[[paste0(k, "_miss")]] == 0)
      out <- c(out, setNames(wls(X, d[[k]], wk)[traits], paste0("select|", k, "|", traits)))
    }
    out
  }
}

pairs_tested <- list(R1 = c("EMOS", "time_pressure"), R2 = c("EMOS", "repetitive"), R3 = c("EMOS", "change"),
  R4 = c("EXTR", "social"), R5 = c("OPEM", "learning"), R6 = c("OPEM", "underuse"), R7 = c("CONS", "autonomy"),
  R8 = c("AGRE", "support"))
pairs_placebo <- list(PL1 = c("EXTR", "physical"), PL2 = c("AGRE", "repetitive"), PL3 = c("CONS", "social"))
pair_sign <- c(R1 = 1, R2 = 1, R3 = 1, R4 = 1, R5 = 1, R6 = -1, R7 = 1, R8 = 1, PL1 = 0, PL2 = 0, PL3 = 0)

# R: all pre-specified products entered together; traits centred at the sample mean.
reactivity_estimator <- function(d, extended = FALSE) {
  w0 <- d$SPFWT0
  tc <- sapply(traits, function(v) d[[v]] - wm(d[[v]], w0))
  conds <- c(conditions_core, if (extended) conditions_ext)
  pairs <- c(pairs_tested[if (extended) names(pairs_tested) else setdiff(names(pairs_tested), "R8")], pairs_placebo)
  P <- sapply(pairs, function(p) tc[, p[1]] * d[[p[2]]])
  X0 <- cbind(`(Intercept)` = 1, dm(base_f, d), mk(structural_f, d), mk(cond_f(conds), d),
    nz(as.matrix(d[c("underuse", "deficit", "match_unknown")])), P)
  Y <- ymat(d)
  function(w, j) {
    b <- as.matrix(wls(cbind(X0, numeracy = num_pv(d, j)), Y, w)); colnames(b) <- colnames(Y)
    c(flat(b, names(pairs), "pair"), flat(b, conds, "cond"), flat(b, c(traits, "underuse"), "main"))
  }
}

# A1: occupational-group mean vs individual deviation in reported conditions; the
# group mean is rebuilt under every replicate weight. Variants (version 5): the
# minimum number of other members a cell needs before falling back to the
# occupation alone, and the cell resolution (finest released occupation x industry,
# or 1-digit occupation x industry in every country). decomp = FALSE skips the
# Gelbach part for the sensitivity variants.
job_cell_def <- function(d, resolution = c("finest", "1digit")) {
  resolution <- match.arg(resolution)
  if (resolution == "finest") return(list(cell = d$job_cell, fallback = d$occ_finest))
  ind <- sub(".*\\|", "", d$job_cell); occ1 <- paste0("o1_", as.character(d$occupation))
  list(cell = paste(occ1, ind, sep = "|"), fallback = occ1)
}
appraisal_estimator <- function(d, extended = FALSE, min_others = 10, resolution = "finest", decomp = TRUE) {
  conds <- c(conditions_core, if (extended) conditions_ext)
  cd <- job_cell_def(d, resolution)
  X0 <- cbind(`(Intercept)` = 1, dm(base_f, d)); Y <- ymat(d)
  S0 <- mk(~ contract + hours + firm_size + sector + manages, d)
  U <- nz(as.matrix(d[c("underuse", "deficit", "match_unknown")]))
  M <- mk(cond_f(conds), d); M <- M[, grepl("_miss$", colnames(M)), drop = FALSE]
  function(w, j) {
    X <- cbind(X0, numeracy = num_pv(d, j))
    A <- sapply(conds, function(k) actual_mean(d[[k]], d, w * (d[[paste0(k, "_miss")]] == 0), min_others, cd$cell, cd$fallback))
    Pp <- sapply(conds, function(k) ifelse(d[[paste0(k, "_miss")]] == 0, d[[k]] - A[, k], 0))
    colnames(A) <- paste0("actual_", conds); colnames(Pp) <- paste0("perceived_", conds)
    out <- c()
    for (k in conds) {
      wk <- w * (d[[paste0(k, "_miss")]] == 0)
      out <- c(out, setNames(wls(X, Pp[, paste0("perceived_", k)], wk)[traits], paste0("perceived|", k, "|", traits)),
                    setNames(wls(X, A[, paste0("actual_", k)], wk)[traits], paste0("actualsel|", k, "|", traits)))
    }
    if (!decomp) return(out)
    B <- list(structural = S0, actual = A, perceived = cbind(Pp, M), underuse = U)
    B <- B[vapply(B, ncol, 1L) > 0]
    c(out, gelbach_out(gelbach_m(X, B, Y, w, traits), traits),
      `desc|all|share_jobcell` = wm(ave(rep(1, nrow(d)), cd$cell, FUN = length) - 1 >= min_others, w))
  }
}

# Job-cell diagnostics (point estimates, final weight): how many cells, how large,
# how often the fallback is used, and how much of each condition varies between
# cells. Reliability of the cell mean is ICC(k) = (MSB - MSW) / MSB from a one-way
# unweighted ANOVA on the cells actually used.
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

# Indicator test (version 5): the satisfaction gap between a high and a low
# occupational group, raw and after standardising both groups to the pooled
# personality distribution (regression standardisation with group-specific
# slopes), and the common-slope gap after adding pay deciles and reported
# conditions. hi and lo are logical vectors on d.
indicator_gap_estimator <- function(d, hi, lo) {
  g <- ifelse(hi, 1, ifelse(lo, 0, NA)); keep <- !is.na(g); d <- d[keep, , drop = FALSE]; g <- g[keep]
  Tm <- as.matrix(d[traits]); y <- d$satisfaction_score
  Xp <- nz(dm(~ wage_decile, d))
  Xc <- cbind(mk(cond_f(conditions_core), d), nz(as.matrix(d[c("underuse", "deficit", "match_unknown")])))
  G <- cbind(`(Intercept)` = 1, hi = g)
  function(w, j) {
    xbar <- apply(Tm, 2, wm, w = w)
    std <- function(gg) { i <- g == gg; b <- wls(cbind(1, Tm[i, , drop = FALSE]), y[i], w[i])
      wm(y[i], w[i]) + sum(b[traits] * (xbar - apply(Tm[i, , drop = FALSE], 2, wm, w = w[i]))) }
    c(`gap|all|raw` = wm(y[g == 1], w[g == 1]) - wm(y[g == 0], w[g == 0]),
      `gap|all|standardised` = std(1) - std(0),
      `gap|all|traits` = wls(cbind(G, Tm), y, w)[["hi"]],
      `gap|all|traits_pay` = wls(cbind(G, Tm, Xp), y, w)[["hi"]],
      `gap|all|traits_pay_conditions` = wls(cbind(G, Tm, Xp, Xc), y, w)[["hi"]],
      `gap|all|share_high` = wm(g, w))
  }
}

# A2: facets, and facet x time pressure products for emotional stability.
facet_estimator <- function(d) {
  w0 <- d$SPFWT0
  ff <- reformulate(c(facets, "age", "gender", "education", "born", "mother", "father"))
  X0 <- cbind(`(Intercept)` = 1, dm(ff, d))
  em <- c("EMOS_ANXI", "EMOS_DEPR", "EMOS_VOLA")
  P <- sapply(em, function(v) (d[[v]] - wm(d[[v]], w0)) * d$time_pressure); colnames(P) <- paste0(em, ":time_pressure")
  X1 <- cbind(X0, mk(structural_f, d), mk(cond_f(conditions_core), d), nz(as.matrix(d[c("underuse", "deficit", "match_unknown")])), P)
  Y <- ymat(d)
  function(w, j) {
    n <- num_pv(d, j)
    b0 <- as.matrix(wls(cbind(X0, numeracy = n), Y, w)); colnames(b0) <- colnames(Y)
    b1 <- as.matrix(wls(cbind(X1, numeracy = n), Y, w)); colnames(b1) <- colnames(Y)
    # Direct facet contrasts (version 5): differences of coefficients within a trait,
    # with their own replicate-weight standard errors.
    dif <- rbind(`EMOS_DEPR-EMOS_ANXI` = b0["EMOS_DEPR", ] - b0["EMOS_ANXI", ], `EMOS_DEPR-EMOS_VOLA` = b0["EMOS_DEPR", ] - b0["EMOS_VOLA", ],
      `AGRE_TRUS-AGRE_COMP` = b0["AGRE_TRUS", ] - b0["AGRE_COMP", ], `AGRE_TRUS-AGRE_RESP` = b0["AGRE_TRUS", ] - b0["AGRE_RESP", ],
      `EXTR_ENER-EXTR_SOCI` = b0["EXTR_ENER", ] - b0["EXTR_SOCI", ], `EXTR_ENER-EXTR_ASSE` = b0["EXTR_ENER", ] - b0["EXTR_ASSE", ])
    c(flat(b0, facets, "facet"), flat(b1, c(facets, colnames(P)), "facet_full"), flat(dif, rownames(dif), "facet_diff"))
  }
}

# A3 and V2/V3: trait coefficients under alternative control sets.
controls_estimator <- function(d, extra = NULL, skill = "PVNUM", both = FALSE, products = NULL, label = "coef") {
  X0 <- cbind(`(Intercept)` = 1, dm(base_f, d)); if (!is.null(extra)) X0 <- cbind(X0, extra)
  if (!is.null(products)) X0 <- cbind(X0, products)
  focal <- c(traits, colnames(products)); Y <- ymat(d)
  function(w, j) {
    X <- cbind(X0, skill = num_pv(d, j, skill)); if (both) X <- cbind(X, numeracy_also = num_pv(d, j))
    b <- as.matrix(wls(X, Y, w)); colnames(b) <- colnames(Y)
    flat(b, c(focal, "skill", if (both) "numeracy_also"), label)
  }
}

# V1: Oster (2019) delta that sets each trait coefficient to zero, point estimates.
oster <- function(d) {
  w <- d$SPFWT0; n <- rowMeans(sapply(1:10, function(j) num_pv(d, j)))
  Xr <- cbind(1, dm(~ EXTR + CONS + OPEM + EMOS + AGRE + age + gender, d))
  Xf <- cbind(1, dm(base_f, d), numeracy = n, mk(structural_f, d), mk(cond_f(conditions_core), d),
    nz(as.matrix(d[c("underuse", "deficit", "match_unknown")])))
  r2 <- function(X, y) { b <- wls(X, y, w); e <- y - drop(X %*% b); 1 - wm(e^2, w) / wm((y - wm(y, w))^2, w) }
  do.call(rbind, lapply(outcomes, function(o) {
    y <- d[[o]]; br <- wls(Xr, y, w)[traits]; bf <- wls(Xf, y, w)[traits]
    Rr <- r2(Xr, y); Rf <- r2(Xf, y); Rmax <- min(1.3 * Rf, 1)
    delta <- bf * (Rf - Rr) / ((br - bf) * (Rmax - Rf))
    bstar <- bf - (br - bf) * (Rmax - Rf) / (Rf - Rr)
    data.frame(country = d$country[1], outcome = o, trait = traits, b_restricted = br, b_full = bf,
      R2_restricted = Rr, R2_full = Rf, Rmax = Rmax, delta = delta, beta_star = bstar)
  }))
}

# ---- Causal mediation (Imai, Keele and Tingley 2010), one mediator at a time ----
# With linear mediator and outcome models and no treatment-mediator interaction,
# the ACME equals a * b: a = trait coefficient in the mediator model, b = mediator
# coefficient in the outcome model (both with the other traits and P1 controls).
# Inference by replicate weights x plausible values, like every other estimate.
# With job cells, time pressure and autonomy are also split into the job-cell
# mean (actual) and the deviation from it (perceived).
mediation_estimator <- function(d, extended = FALSE, cells = TRUE,
                                split = c("time_pressure", "autonomy", "learning", "social", "voice")) {
  meds <- c(conditions_core, if (extended) conditions_ext, "underuse")
  X0 <- cbind(`(Intercept)` = 1, dm(base_f, d)); y <- d$satisfaction_score
  U <- cbind(deficit = d$deficit, match_unknown = d$match_unknown)
  wcor <- function(a, b, w) { a <- a - wm(a, w); b <- b - wm(b, w); wm(a * b, w) / sqrt(wm(a^2, w) * wm(b^2, w)) }
  one <- function(X, mv, w, label) {
    a <- wls(X, mv, w); f <- wls(cbind(X, M = mv), y, w); r <- wls(X, y, w)
    # Imai, Keele and Yamamoto (2010): with linear models the ACME is zero when the
    # correlation between the mediator- and outcome-model errors equals the
    # correlation between the mediator residual and the reduced-form outcome residual.
    rho <- wcor(mv - drop(X %*% a), y - drop(X %*% r), w)
    c(setNames(a[traits] * f[["M"]], paste0("acme|", label, "|", traits)),
      setNames(f[traits], paste0("ade|", label, "|", traits)),
      setNames(a[traits], paste0("a|", label, "|", traits)), setNames(f[["M"]], paste0("b|", label, "|M")),
      setNames(rho, paste0("rho|", label, "|zero")))
  }
  # Pay as a mediator (exposure through objective rewards): decile 1-10 as a
  # numeric mediator, with the pay dummies removed from the control set.
  Xnp <- cbind(`(Intercept)` = 1, dm(base_f_nopay, d))
  pay <- suppressWarnings(as.numeric(as.character(d$wage_decile))); pay_ok <- is.finite(pay)
  function(w, j) {
    X <- cbind(X0, numeracy = num_pv(d, j))
    out <- setNames(wls(X, y, w)[traits], paste0("total|all|", traits))
    if (mean(pay_ok) > 0.5) out <- c(out, one(cbind(Xnp, numeracy = num_pv(d, j)), ifelse(pay_ok, pay, 0), w * pay_ok, "pay"),
      setNames(wls(cbind(Xnp, numeracy = num_pv(d, j)), y, w)[traits], paste0("total_nopay|all|", traits)))
    for (m in meds) {
      if (m == "underuse") out <- c(out, one(cbind(X, U), d$underuse, w, m))
      else out <- c(out, one(X, d[[m]], w * (d[[paste0(m, "_miss")]] == 0), m))
    }
    # Joint mediation through all conditions together (VanderWeele and Vansteelandt 2014):
    # sum over mediators of a_k * b_k, with b_k from the outcome model containing all of them.
    # Estimated for the full set available in the country ("all": extended where released)
    # and for the common core set (seven conditions plus skill underuse) in every country,
    # so that joint shares are comparable across countries (version 5).
    joint_fit <- function(set) {
      Mx <- cbind(sapply(setdiff(set, "underuse"), function(m) d[[m]]), underuse = d$underuse,
        sapply(setdiff(set, "underuse"), function(m) d[[paste0(m, "_miss")]]), U)
      colnames(Mx)[(length(set) + 1):(2 * length(set) - 1)] <- paste0(setdiff(set, "underuse"), "_miss")
      Mx <- nz(Mx); fj <- wls(cbind(X, Mx), y, w)
      joint <- rep(0, length(traits)); names(joint) <- traits
      for (m in set) { a <- wls(X, d[[m]], w)[traits]; joint <- joint + a * fj[[m]] }
      list(joint = joint, direct = fj[traits])
    }
    ja <- joint_fit(meds); core <- c(conditions_core, "underuse")
    jc <- if (setequal(core, meds)) ja else joint_fit(core)
    out <- c(out, setNames(ja$joint, paste0("acme_joint|all|", traits)), setNames(ja$direct, paste0("ade_joint|all|", traits)),
      setNames(jc$joint, paste0("acme_joint_core|all|", traits)), setNames(jc$direct, paste0("ade_joint_core|all|", traits)))
    if (cells) for (m in intersect(split, meds)) {
      wk <- w * (d[[paste0(m, "_miss")]] == 0)
      act <- actual_mean(d[[m]], d, wk); per <- d[[m]] - act
      out <- c(out, one(X, act, wk, paste0(m, "_actual")), one(X, per, wk, paste0(m, "_perceived")))
    }
    out
  }
}
