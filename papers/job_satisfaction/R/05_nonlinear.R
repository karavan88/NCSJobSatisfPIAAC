# Part I sensitivities 1-2: logistic models for the two binary outcomes and an
# ordered logit for the five categories. Each reports weighted average marginal
# effects of the traits (on the probability of the outcome; for the ordered
# logit, on the probability of "extremely satisfied"). All plausible values and
# replicate weights; P1 (base) specification.
# Run from the repository root: Rscript papers/job_satisfaction/R/05_nonlinear.R
options(warn = 1)
base <- "papers/job_satisfaction"
source(file.path(base, "R/paper_functions.R"))
raw <- readRDS(file.path(base, "results/raw_selected.rds"))
countries <- setdiff(names(raw), c("DNK", "SGP", "JPN", "USA"))
dir.create(file.path(base, "results/nonlinear"), showWarnings = FALSE)

nonlinear_estimator <- function(d) {
  X0 <- dm(base_f, d); X0 <- X0[, colSums(X0 != 0) > 0, drop = FALSE]
  y5 <- d$satisfaction_score                     # 5 = extremely satisfied
  start_ol <- NULL; start_lg <- list()
  function(w, j) {
    X <- cbind(X0, numeracy = (d[[paste0("PVNUM", j)]] - 250) / 50)
    keep <- w > 0; Xk <- X[keep, , drop = FALSE]; wk <- w[keep]
    out <- c()
    for (y in c("very_satisfied", "not_satisfied")) {
      Z <- cbind(1, Xk)
      f <- suppressWarnings(glm.fit(Z, d[[y]][keep], weights = wk / mean(wk), family = quasibinomial(),
        start = start_lg[[y]]))
      b <- f$coefficients; b[is.na(b)] <- 0
      if (is.null(start_lg[[y]])) start_lg[[y]] <<- b
      pr <- plogis(drop(Z %*% b))
      out <- c(out, setNames(sapply(traits, function(v) wm(pr * (1 - pr) * b[[v]], wk)), paste0("logit_", y, "|", traits)))
    }
    # Ordered logit; drop empty categories within this replicate.
    yf <- factor(y5[keep])
    args <- list(yf ~ Xk, weights = wk / mean(wk), method = "logistic", Hess = FALSE, control = list(maxit = 200))
    if (!is.null(start_ol) && nlevels(yf) == 5) args$start <- start_ol
    ol <- do.call(MASS::polr, args)
    if (is.null(start_ol) && nlevels(yf) == 5) start_ol <<- c(coef(ol), ol$zeta)
    beta <- coef(ol); names(beta) <- sub("^Xk", "", names(beta)); beta[is.na(beta)] <- 0
    eta <- drop(Xk %*% beta); z <- ol$zeta[length(ol$zeta)]
    dens <- dlogis(z - eta)
    c(out, setNames(sapply(traits, function(v) wm(dens * beta[[v]], wk)), paste0("ologit_top|", traits)))
  }
}

run_country <- function(cc) {
  t0 <- Sys.time()
  d <- prepare(raw[[cc]], cc); s <- d[d$core_ok, , drop = FALSE]
  p <- rep_pv(s, nonlinear_estimator(s))
  out <- list(coef = tidy_pool(p, cc, "nonlinear"), seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
  saveRDS(out, file.path(base, "results/nonlinear", paste0(cc, ".rds")))
  cat(cc, "done in", round(out$seconds), "s\n")
  out
}
todo <- if (length(commandArgs(TRUE))) commandArgs(TRUE) else countries
invisible(parallel::mclapply(todo, function(cc) tryCatch(run_country(cc),
  error = function(e) { cat(cc, "ERROR", conditionMessage(e), "\n"); NULL }),
  mc.cores = as.integer(Sys.getenv("PIAAC_CORES", "4")), mc.preschedule = FALSE))
