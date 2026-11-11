# papers/job_satisfaction/R/03_mediation.R
# Causal mediation estimates with mediation::mediate() (Imai, Keele and Yamamoto
# 2010; Tingley et al. 2014), country by country: the total effect of each Big Five
# trait on the 1-5 satisfaction score, its indirect effects through 4 mediator blocks
# (pay, job demands, job resources, skill underuse) and its direct effect.
#
#   Rscript papers/job_satisfaction/R/03_mediation.R [countries]      (make mediation)
#
# Environment: MED_SIMS (quasi-Bayesian draws, default 1000), MED_RHO_SIMS (medsens
# draws, default 100), PIAAC_CORES (parallel countries, default 6).
#
# Blocks: pay = earnings decile (1-10; missing set to the mean with an indicator; absent
# in the 3 countries that release no deciles); demands = time pressure, repetitive tasks,
# physical work, change; resources = autonomy, learning, social tasks, and support and
# voice where the extended module is fielded; skill underuse = underuse, with the deficit
# and unknown categories. Missing condition values are 0 with an indicator in the block.
#
# For each country, the outcome equation with the traits, the baseline covariates (age,
# sex, education, birthplace, parental education, numeracy) and every block column gives,
# for each block k, the index H_k = X_k b_k (its fitted contribution). Mediator equations:
# H_k ~ traits + covariates. Outcome equation: satisfaction ~ traits + covariates + H_1 +
# ... + H_K. mediate(treat = trait, mediator = H_k) returns the average causal mediation
# effect (ACME) through block k; a second model with the single index H_all = sum of the
# H_k returns the total effect and the average direct effect (ADE). The block ACMEs add up
# to the ACME through H_all. The same construction with each reported condition split into
# its occupational-group mean and the worker's deviation from it gives the ACME through the
# group means and through the deviations, by block ("split") and by condition ("cond").
# Models are weighted least squares with the final survey weight; numeracy is the mean of
# the 10 plausible values; the treatment contrast is 1 SD of the trait.
# Sensitivity: medsens() on the cumulative indices H_1, H_1 + H_2, ... (emotional stability;
# rho* does not depend on the trait with linear models) gives rho*, the error correlation at
# which the indirect effect is zero, and the ACME under each assumed rho.
# Output: results/mediation/<CC>.rds, a list with
#   est    data frame: term "kind|mediator|trait" (kind acme, ade, total; mediator a block,
#          "all", or a split index), estimate, se (SD of the draws), p, lower, upper
#   rho    rho* for the cumulative blocks; grid  ACME under each rho; additivity check
options(warn = 1)
base <- "papers/job_satisfaction"
source(file.path(base, "R/functions.R"))
stopifnot(requireNamespace("mediation", quietly = TRUE))
raw <- readRDS(file.path(base, "results/raw_selected.rds"))
countries <- study_countries(names(raw))
sims <- as.integer(Sys.getenv("MED_SIMS", "1000"))
rho_sims <- as.integer(Sys.getenv("MED_RHO_SIMS", "100"))
out_dir <- file.path(base, "results/mediation")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
seed <- 20261110
demands <- c("time_pressure", "repetitive", "physical", "change")
resources_core <- c("autonomy", "learning", "social")

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
  X <- mk(base_f_nopay, s); X <- X[, !colnames(X) %in% traits, drop = FALSE]; colnames(X) <- paste0("c", seq_len(ncol(X)))
  D <- data.frame(y = s$satisfaction_score, s[traits], X, w = w, check.names = FALSE)
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
  B$demands <- cols_of(demands); B$resources <- cols_of(resources)
  B$underuse <- as.matrix(s[c("underuse", "deficit", "match_unknown")])
  B <- lapply(B, nz)

  # Indices H_k from one outcome equation with every column of every group.
  indices <- function(G, Dx) {
    Z <- do.call(cbind, unname(G)); colnames(Z) <- paste0("z", seq_len(ncol(Z)))
    grp <- rep(names(G), vapply(G, ncol, 1L))
    full <- wfit("y", c(setdiff(names(Dx), c("y", traits, "w")), colnames(Z)), cbind(Dx, Z))
    b <- coef(full)[colnames(Z)]; b[is.na(b)] <- 0
    sapply(names(G), function(k) drop(Z[, grp == k, drop = FALSE] %*% b[grp == k]))
  }
  rows <- list(); add <- function(r) rows[[length(rows) + 1]] <<- r
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

  # ---- blocks: total, direct and the block ACMEs ----
  Hb <- indices(B, D)
  mb <- mediate_set(Hb, D, with_total = TRUE)

  # ---- sensitivity: cumulative blocks ----
  sens <- lapply(seq_len(ncol(Hb)), function(k) {
    E <- mb$E; E$Hc <- rowSums(Hb[, seq_len(k), drop = FALSE])
    x <- med(wfit("Hc", mb$cv, E), wfit("y", c(mb$cv, "Hc"), E), "EMOS", "Hc", rho_sims)
    ss <- mediation::medsens(x, rho.by = 0.01, effect.type = "indirect", sims = rho_sims)
    list(rho = data.frame(blocks = paste(colnames(Hb)[seq_len(k)], collapse = "+"), k = k, rho_star = ss$err.cr.d[1]),
         grid = data.frame(k = k, rho = ss$rho, acme = ss$d0, lo = ss$lower.d0, hi = ss$upper.d0))
  })

  # ---- group mean and deviation, by block and by condition ----
  if (cells) {
    conds <- c(demands, resources)
    sc <- split_conditions(s, conds)
    obs <- sapply(conds, function(k) s[[paste0(k, "_miss")]] == 0)
    A <- sc$A * obs; P <- sc$P * obs
    Ds <- cbind(D, nz(as.matrix(s[paste0(conds, "_miss")])))       # indicators become covariates here
    keep <- function(G) G[!vapply(G, is.null, logical(1))]
    Gs <- keep(c(B["pay"], list(demands_mean = A[, demands, drop = FALSE], demands_dev = P[, demands, drop = FALSE],
                 resources_mean = A[, resources, drop = FALSE], resources_dev = P[, resources, drop = FALSE]), B["underuse"]))
    mediate_set(indices(Gs, Ds), Ds, label_of = function(h) if (grepl("_(mean|dev)$", h)) h else paste0("split_", h))
    Gc <- keep(c(B["pay"], setNames(lapply(conds, function(k) A[, k, drop = FALSE]), paste0(conds, "_mean")),
                 setNames(lapply(conds, function(k) P[, k, drop = FALSE]), paste0(conds, "_dev")), B["underuse"]))
    mediate_set(indices(Gc, Ds), Ds, label_of = function(h) if (grepl("_(mean|dev)$", h)) h else paste0("cond_", h))
  }

  est <- do.call(rbind, rows); est$country <- cc; est$n <- nrow(s); rownames(est) <- NULL
  out <- list(est = est, additivity = mb$check,
              rho = cbind(country = cc, do.call(rbind, lapply(sens, `[[`, "rho"))),
              grid = cbind(country = cc, do.call(rbind, lapply(sens, `[[`, "grid"))),
              blocks = colnames(Hb), sims = sims, rho_sims = rho_sims,
              mediation_version = as.character(utils::packageVersion("mediation")))
  saveRDS(out, file.path(out_dir, paste0(cc, ".rds")))
  cat(sprintf("%s done in %.1f min; max |sum ACME_k - ACME_all| = %.1e; rho* = %s\n", cc,
              as.numeric(difftime(Sys.time(), t0, units = "mins")), max(abs(mb$check)),
              paste(sprintf("%.2f", out$rho$rho_star), collapse = " ")))
  invisible(out)
}

todo <- if (length(commandArgs(TRUE))) commandArgs(TRUE) else countries
invisible(parallel::mclapply(todo, function(cc) tryCatch(run_country(cc),
  error = function(e) { cat(cc, "ERROR", conditionMessage(e), "\n"); NULL }),
  mc.cores = as.integer(Sys.getenv("PIAAC_CORES", "6")), mc.preschedule = FALSE))
