# papers/job_satisfaction/R/03_mediation.R
# Causal mediation estimates with mediation::mediate() (Imai, Keele and Yamamoto
# 2010; Tingley et al. 2014), country by country: the total effect of each Big Five
# trait on job satisfaction, its indirect effects through 5 mediator blocks (pay, job
# characteristics, job demands, job resources, skill underuse) and its direct effect.
#
#   Rscript papers/job_satisfaction/R/03_mediation.R [countries]      (make mediation)
#
# Environment: MED_SIMS (quasi-Bayesian draws, default 1000), MED_RHO_SIMS (medsens
# draws, default 100), PIAAC_CORES (parallel countries, default 6).
#
# Outcomes (linear models, so the effects add up): the main outcome is being extremely
# satisfied (linear probability model) and gets every model below. Being not satisfied
# (neutral or dissatisfied) and the 1-5 score get the block models, total and direct effects.
#
# Blocks: pay = earnings decile (1-10; missing set to the mean with an indicator; absent
# in the 3 countries that release no deciles); job = occupation, contract type, weekly
# hours, firm size, sector, supervisory role and tenure (indicator sets with an "Unknown"
# level; a term not released in a country is dropped); demands = time pressure, repetitive
# tasks, physical work, change; resources = autonomy, learning, social tasks, and support and
# voice where the extended module is fielded; skill underuse = underuse, with the deficit
# and unknown categories. Missing condition values are 0 with an indicator in the block.
#
# For each country and outcome, the outcome equation with the traits, the baseline
# covariates (age, sex, education, birthplace, parental education, numeracy) and every block
# column gives, for each block k, the index H_k = X_k b_k (its fitted contribution). Mediator
# equations: H_k ~ traits + covariates. Outcome equation: y ~ traits + covariates + H_1 + ...
# + H_K. mediate(treat = trait, mediator = H_k) returns the average causal mediation effect
# (ACME) through block k; a second model with the single index H_all = sum of the H_k returns
# the total effect and the average direct effect (ADE). The block ACMEs add up to the ACME
# through H_all. Main outcome only: the same construction with each reported condition split
# into its occupational-group mean and the worker's deviation from it ("split", "cond"), and
# medsens() on the cumulative indices H_1, H_1 + H_2, ... (emotional stability), which gives
# rho*, the error correlation at which the indirect effect is zero.
# Ordinal outcome: mediate() admits an ordered outcome only with the nonparametric bootstrap,
# so the ordinal check is an ordered probit (MASS::polr) of the 1-5 score on the traits and
# covariates, without and with every mediator variable: latent coefficients ("ord_coef") and
# the effect of 1 SD of the trait on the probability of being extremely satisfied
# ("ord_top") and of being not satisfied ("ord_bottom"), with intervals from 500 draws of the
# coefficients.
# Models are weighted with the final survey weight; numeracy is the mean of the 10 plausible
# values; the treatment contrast is 1 SD of the trait (-0.5 to 0.5).
# Output: results/mediation/<CC>.rds, a list with
#   est    data frame: term "kind|mediator|trait", estimate, se, p, lower, upper, outcome
#   coefs  coefficients of the total-effect and outcome equations, by outcome
#   rho    rho* for the cumulative blocks; grid  ACME under each rho; additivity check
options(warn = 1)
base <- "papers/job_satisfaction"
source(file.path(base, "R/functions.R"))
stopifnot(requireNamespace("mediation", quietly = TRUE), requireNamespace("MASS", quietly = TRUE))
raw <- readRDS(file.path(base, "results/raw_selected.rds"))
countries <- study_countries(names(raw))
sims <- as.integer(Sys.getenv("MED_SIMS", "1000"))
rho_sims <- as.integer(Sys.getenv("MED_RHO_SIMS", "100"))
out_dir <- file.path(base, "results/mediation")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
seed <- 20261110
demands <- c("time_pressure", "repetitive", "physical", "change")
resources_core <- c("autonomy", "learning", "social")
med_outcomes <- c("very_satisfied", "not_satisfied", "satisfaction_score")
main_outcome <- med_outcomes[1]

med <- function(mm, my, tr, m, n = sims) {
  set.seed(seed)
  mediation::mediate(mm, my, treat = tr, mediator = m, control.value = -0.5, treat.value = 0.5, sims = n)
}
med_rows <- function(x, kinds, label, tr) {
  map <- list(acme = c("d0", "d0.sims", "d0.ci", "d0.p"), ade = c("z0", "z0.sims", "z0.ci", "z0.p"),
              total = c("tau.coef", "tau.sims", "tau.ci", "tau.p"))
  do.call(rbind, lapply(kinds, function(k) { f <- map[[k]]
    data.frame(term = paste(k, label, tr, sep = "|"), estimate = x[[f[1]]], se = sd(x[[f[2]]]),
               p = x[[f[4]]], lower = x[[f[3]]][1], upper = x[[f[3]]][2], stringsAsFactors = FALSE) }))
}

run_country <- function(cc) {
  t0 <- Sys.time()
  d <- prepare(raw[[cc]], cc); s <- d[d$core_ok, , drop = FALSE]
  ext <- has_extended(s); cells <- !cc %in% no_job_cell; w <- s$SPFWT0
  Xn <- mk(base_f_nopay, s); X <- Xn[, !colnames(Xn) %in% traits, drop = FALSE]; colnames(X) <- paste0("c", seq_len(ncol(X)))
  # mediate() needs a full-rank design: covariates that are constant or aliased in this
  # sample (e.g. a missing-value indicator that is always 0) are dropped before fitting.
  wfit <- function(lhs, cols, data) {
    f <- function(cl) lm(as.formula(paste(lhs, "~", paste(c(traits, cl), collapse = " + "))), data = data, weights = w)
    m <- f(cols); na <- names(coef(m))[is.na(coef(m))]
    if (length(na)) m <- f(setdiff(cols, na))
    m
  }
  resources <- c(resources_core, if (ext) conditions_ext)
  B <- list()
  if (has_wage_deciles(s)) {
    pm <- is.finite(s$pay); pay <- ifelse(pm, s$pay, wm(s$pay[pm], w[pm]))
    B$pay <- cbind(pay = pay, pay_miss = as.numeric(!pm))
  }
  cols_of <- function(v) as.matrix(s[c(v, paste0(v, "_miss"))])
  B$job <- mk(job_f, s)
  B$demands <- cols_of(demands); B$resources <- cols_of(resources)
  B$underuse <- as.matrix(s[c("underuse", "deficit", "match_unknown")])
  B <- lapply(B, nz)
  Zn <- do.call(cbind, unname(B)); blk_of <- setNames(rep(names(B), vapply(B, ncol, 1L)), colnames(Zn))

  # Indices H_k from one outcome equation with every column of every group.
  indices <- function(G, Dx) {
    Z <- do.call(cbind, unname(G)); colnames(Z) <- paste0("z", seq_len(ncol(Z)))
    grp <- rep(names(G), vapply(G, ncol, 1L))
    full <- wfit("y", c(setdiff(names(Dx), c("y", traits, "w")), colnames(Z)), cbind(Dx, Z))
    b <- coef(full)[colnames(Z)]; b[is.na(b)] <- 0
    sapply(names(G), function(k) drop(Z[, grp == k, drop = FALSE] %*% b[grp == k]))
  }
  rows <- list(); cur <- NULL
  add <- function(r) { r$outcome <- cur; rows[[length(rows) + 1]] <<- r }
  # mediate() through each index, in one outcome equation with all indices; optionally the
  # total and direct effects through their sum.
  mediate_set <- function(H, Dx, label_of = identity, with_total = FALSE) {
    E <- cbind(Dx, H); hn <- colnames(H); cv <- setdiff(names(Dx), c("y", traits, "w"))
    my <- wfit("y", c(cv, hn), E); mm <- lapply(setNames(hn, hn), function(h) wfit(h, cv, E))
    E$H_all <- rowSums(H); my_all <- wfit("y", c(cv, "H_all"), E); mm_all <- wfit("H_all", cv, E)
    chk <- c()
    for (tr in traits) {
      acme <- 0
      for (h in hn) { x <- med(mm[[h]], my, tr, h); add(med_rows(x, "acme", label_of(h), tr)); acme <- acme + x$d0 }
      if (with_total) {
        x <- med(mm_all, my_all, tr, "H_all"); add(med_rows(x, c("acme", "ade", "total"), "all", tr))
        chk[tr] <- acme - x$d0
      }
    }
    list(E = E, cv = cv, check = chk)
  }
  # Full specification: coefficients of the total-effect equation and of the outcome
  # equation with every mediator variable, with their real names.
  spec_rows <- function(y, Xm, eq) {
    f <- wlm(cbind(`(Intercept)` = 1, Xm), y, w); b <- f$b[, 1]; ok <- !is.na(b)
    e <- y - drop(cbind(`(Intercept)` = 1, Xm)[, ok, drop = FALSE] %*% b[ok])
    r <- est_rows(b, f$se[, 1], names(b)); r$eq <- eq; r$n <- f$n
    r$r2 <- 1 - wm(e^2, w) / wm((y - wm(y, w))^2, w); r
  }

  additivity <- list(); coefs <- list(); sens <- NULL
  for (o in med_outcomes) {
    cur <- o
    D <- data.frame(y = s[[o]], s[traits], X, w = w, check.names = FALSE)
    Hb <- indices(B, D)
    mb <- mediate_set(Hb, D, with_total = TRUE); additivity[[o]] <- mb$check
    cf <- rbind(spec_rows(s[[o]], Xn, "total"), spec_rows(s[[o]], cbind(Xn, Zn), "outcome"))
    cf$block <- ifelse(cf$term %in% names(blk_of), blk_of[cf$term], ""); cf$outcome <- o; cf$country <- cc
    coefs[[o]] <- cf
    if (o != main_outcome) next

    # ---- sensitivity: cumulative blocks (main outcome) ----
    sens <- lapply(seq_len(ncol(Hb)), function(k) {
      E <- mb$E; E$Hc <- rowSums(Hb[, seq_len(k), drop = FALSE])
      x <- med(wfit("Hc", mb$cv, E), wfit("y", c(mb$cv, "Hc"), E), "EMOS", "Hc", rho_sims)
      ss <- mediation::medsens(x, rho.by = 0.01, effect.type = "indirect", sims = rho_sims)
      list(rho = data.frame(blocks = paste(colnames(Hb)[seq_len(k)], collapse = "+"), k = k, rho_star = ss$err.cr.d[1]),
           grid = data.frame(k = k, rho = ss$rho, acme = ss$d0, lo = ss$lower.d0, hi = ss$upper.d0))
    })

    # ---- group mean and deviation, by block and by condition (main outcome) ----
    if (cells) {
      conds <- c(demands, resources)
      sc <- split_conditions(s, conds)
      obs <- sapply(conds, function(k) s[[paste0(k, "_miss")]] == 0)
      A <- sc$A * obs; P <- sc$P * obs
      Ds <- cbind(D, nz(as.matrix(s[paste0(conds, "_miss")])))       # indicators become covariates here
      keep <- function(G) G[!vapply(G, is.null, logical(1))]
      Gs <- keep(c(B["pay"], B["job"], list(demands_mean = A[, demands, drop = FALSE], demands_dev = P[, demands, drop = FALSE],
                   resources_mean = A[, resources, drop = FALSE], resources_dev = P[, resources, drop = FALSE]), B["underuse"]))
      mediate_set(indices(Gs, Ds), Ds, label_of = function(h) if (grepl("_(mean|dev)$", h)) h else paste0("split_", h))
      Gc <- keep(c(B["pay"], B["job"], setNames(lapply(conds, function(k) A[, k, drop = FALSE]), paste0(conds, "_mean")),
                   setNames(lapply(conds, function(k) P[, k, drop = FALSE]), paste0(conds, "_dev")), B["underuse"]))
      mediate_set(indices(Gc, Ds), Ds, label_of = function(h) if (grepl("_(mean|dev)$", h)) h else paste0("cond_", h))
    }
  }

  # ---- ordinal outcome: ordered probit of the 1-5 score ----
  cur <- "satisfaction_score"
  ord <- tryCatch({
    Zo <- Zn; colnames(Zo) <- paste0("z", seq_len(ncol(Zo)))
    Eo <- data.frame(yo = factor(s$satisfaction_score, levels = 1:5, ordered = TRUE), s[traits], X, Zo, w = w / mean(w), check.names = FALSE)
    cv <- colnames(X)
    one <- function(cols, label) {
      f <- function(cl) suppressWarnings(MASS::polr(as.formula(paste("yo ~", paste(c(traits, cl), collapse = " + "))), data = Eo,
        weights = w, method = "probit", Hess = TRUE))
      # polr cannot invert the Hessian with aliased columns: drop them first, as in wfit().
      al <- coef(lm(as.formula(paste("as.numeric(yo) ~", paste(c(traits, cols), collapse = " + "))), data = Eo, weights = w))
      cols <- setdiff(cols, names(al)[is.na(al)])
      m <- f(cols)
      # Near-duplicate dummies (France, Poland) push coefficients to +-15 and leave a
      # non-finite Hessian: drop those columns and refit.
      for (i in 1:3) {
        big <- intersect(names(coef(m))[abs(coef(m)) > 8], cols)
        if (all(is.finite(m$Hessian)) && !length(big)) break
        cols <- setdiff(cols, big); m <- f(cols)
      }
      b <- coef(m)
      M <- model.matrix(delete.response(terms(m)), m$model)[, names(b), drop = FALSE]; ww <- m$model[["(weights)"]]
      V <- vcov(m); th <- c(b, m$zeta)
      set.seed(seed); dr <- rbind(th, MASS::mvrnorm(500, th, V[names(th), names(th)]))
      kb <- length(b)
      do.call(rbind, lapply(traits, function(tr) {
        eff <- t(apply(dr, 1, function(t) { bb <- t[1:kb]; z <- t[-(1:kb)]; eta <- drop(M %*% bb)
          e1 <- eta + bb[[tr]] * (0.5 - M[, tr]); e0 <- eta + bb[[tr]] * (-0.5 - M[, tr])
          c(top = wm(pnorm(z[4] - e0) - pnorm(z[4] - e1), ww),
            bottom = wm(pnorm(z[3] - e1) - pnorm(z[3] - e0), ww), coef = bb[[tr]]) }))
        do.call(rbind, lapply(c("top", "bottom", "coef"), function(k) { x <- eff[-1, k]; se <- sd(x)
          data.frame(term = paste(paste0("ord_", k), label, tr, sep = "|"), estimate = eff[1, k], se = se,
            p = 2 * pnorm(-abs(eff[1, k] / se)), lower = unname(quantile(x, .025)), upper = unname(quantile(x, .975))) }))
      }))
    }
    rbind(one(cv, "none"), one(c(cv, colnames(Zo)), "all"))
  }, error = function(e) { cat(cc, "ordinal check failed:", conditionMessage(e), "\n"); NULL })
  if (!is.null(ord)) add(ord)

  est <- do.call(rbind, rows); est$country <- cc; est$n <- nrow(s); rownames(est) <- NULL
  out <- list(est = est, additivity = additivity, coefs = do.call(rbind, coefs), main_outcome = main_outcome,
              rho = cbind(country = cc, do.call(rbind, lapply(sens, `[[`, "rho"))),
              grid = cbind(country = cc, do.call(rbind, lapply(sens, `[[`, "grid"))),
              blocks = names(B), sims = sims, rho_sims = rho_sims,
              mediation_version = as.character(utils::packageVersion("mediation")))
  saveRDS(out, file.path(out_dir, paste0(cc, ".rds")))
  cat(sprintf("%s done in %.1f min; max |sum ACME_k - ACME_all| = %.1e; rho* = %s\n", cc,
              as.numeric(difftime(Sys.time(), t0, units = "mins")), max(abs(unlist(additivity))),
              paste(sprintf("%.2f", out$rho$rho_star), collapse = " ")))
  invisible(out)
}

todo <- if (length(commandArgs(TRUE))) commandArgs(TRUE) else countries
invisible(parallel::mclapply(todo, function(cc) tryCatch(run_country(cc),
  error = function(e) { cat(cc, "ERROR", conditionMessage(e), "\n"); NULL }),
  mc.cores = as.integer(Sys.getenv("PIAAC_CORES", "6")), mc.preschedule = FALSE))
