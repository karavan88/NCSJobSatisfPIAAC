# papers/job_satisfaction/R/04_collect.R
# Collects the country results of 02_estimate.R and 03_mediation.R into one file,
# results/summary.rds, read by the manuscript. Adds Benjamini-Hochberg q-values within
# each family of tests (model x kind x outcome across countries and terms).
#
#   Rscript papers/job_satisfaction/R/04_collect.R      (make collect)
options(warn = 1)
base <- "papers/job_satisfaction"
source(file.path(base, "R/functions.R"))
rd <- function(dir) lapply(list.files(file.path(base, "results", dir), "\\.rds$", full.names = TRUE), readRDS)
bh <- function(p) p.adjust(p, "BH")
S <- list()

est <- rd("estimates"); stopifnot(length(est) > 0)
co <- do.call(rbind, lapply(est, `[[`, "coef"))

# ---- H1: the premium and its specifications ----
p1 <- co[grepl("^main\\||^sens_", co$model), ]
p1$block <- sub("\\|.*", "", p1$model)
main <- p1$block == "main"
p1$outcome <- ifelse(main, sub(".*\\|", "", p1$model), sub("\\|.*", "", p1$term))
p1$kind <- ifelse(main, sub(":.*", "", p1$term), "coef")
p1$var <- ifelse(main, sub(".*:", "", p1$term), sub(".*\\|", "", p1$term))
p1$family <- paste(p1$block, p1$outcome, p1$kind, ifelse(p1$var %in% traits, "trait", p1$var))
p1$q <- ave(p1$p, p1$family, FUN = bh)
S$part1 <- p1

n <- co[co$model == "nonlinear", ]
n$spec <- sub("\\|.*", "", n$term); n$var <- sub(".*\\|", "", n$term); n$q <- ave(n$p, n$spec, FUN = bh)
S$nonlinear <- n

# ---- conditions, groups, facets, products, gap, robustness ----
cc <- co[!grepl("^main\\||^sens_|^nonlinear$", co$model), ]
parts <- strsplit(cc$term, "|", fixed = TRUE)
cc$kind <- sapply(parts, `[`, 1); cc$outcome <- sapply(parts, `[`, 2); cc$var <- sapply(parts, `[`, 3)
cc$pair_type <- ifelse(cc$kind == "pair", ifelse(grepl("^PL", cc$var), "placebo", "tested"), "")
cc$family <- paste(cc$model, cc$kind, cc$outcome, cc$pair_type)
cc$q <- ave(cc$p, cc$family, FUN = function(p) if (all(is.na(p))) p else bh(p))
S$channels <- cc

S$info <- do.call(rbind, lapply(est, `[[`, "info"))
S$pay_share <- S$info[c("country", "pay_share")]
S$oster <- do.call(rbind, lapply(est, `[[`, "oster"))
dg <- lapply(est, `[[`, "diag"); dg <- dg[!vapply(dg, is.null, logical(1))]
S$cell_diag <- do.call(rbind, dg)
S$flow1 <- do.call(rbind, lapply(est, `[[`, "flow"))
S$pay_contrast <- do.call(rbind, lapply(est, `[[`, "pay_contrast"))

# ---- mediation (mediate) ----
md <- rd("mediation")
if (length(md)) {
  m <- do.call(rbind, lapply(md, `[[`, "est")); pr <- strsplit(m$term, "|", fixed = TRUE)
  m$kind <- sapply(pr, `[`, 1); m$mediator <- sapply(pr, `[`, 2); m$var <- sapply(pr, `[`, 3)
  m$q <- ave(m$p, paste(m$kind, m$mediator), FUN = function(p) if (all(is.na(p))) p else bh(p))
  S$mediation <- m
  S$med_rho <- do.call(rbind, lapply(md, `[[`, "rho"))
  S$med_grid <- do.call(rbind, lapply(md, `[[`, "grid"))
  S$med_info <- list(sims = md[[1]]$sims, rho_sims = md[[1]]$rho_sims, version = md[[1]]$mediation_version,
                     additivity = max(abs(unlist(lapply(md, `[[`, "additivity")))))
}

# ---- descriptives ----
raw <- readRDS(file.path(base, "results/raw_selected.rds"))
S$distribution <- do.call(rbind, lapply(S$info$country, function(k) {
  d <- raw[[k]]; e <- d$D2_Q04 %in% 1 & d$AGEG10LFS %in% 2:5 & d$D2_Q13 %in% 1:5 & d$SPFWT0 > 0
  data.frame(country = k, category = 1:5, share = sapply(1:5, function(c) wm(d$D2_Q13[e] == c, d$SPFWT0[e])), n = sum(e))
}))
S$condition_means <- do.call(rbind, lapply(S$info$country, function(k) {
  d <- prepare(raw[[k]], k); s <- d[d$core_ok, ]
  do.call(rbind, lapply(c(conditions_core, conditions_ext), function(v) {
    ok <- s[[paste0(v, "_miss")]] == 0
    if (mean(ok) < 0.5) return(NULL)
    data.frame(country = k, condition = v, mean_raw = wm(s[[paste0(v, "_raw")]][ok], s$SPFWT0[ok]), share_observed = mean(ok))
  }))
}))
# Table 1: weighted descriptives of the analysis sample by country.
S$desc <- do.call(rbind, lapply(S$info$country, function(k) {
  d <- prepare(raw[[k]], k); s <- d[d$core_ok, ]; w <- s$SPFWT0
  data.frame(country = k, n = nrow(s), women = wm(s$gender == 2, w), tertiary = wm(s$EDCAT6_TC1 %in% 4:6, w),
    numeracy = wm(rowMeans(as.matrix(s[paste0("PVNUM", 1:10)])), w), satisfaction = wm(s$satisfaction_score, w),
    not_satisfied = wm(s$not_satisfied, w), pay_released = has_wage_deciles(s), extended = has_extended(s),
    time_pressure = wm(s$time_pressure_raw[s$time_pressure_miss == 0], w[s$time_pressure_miss == 0]),
    autonomy = wm(s$autonomy_raw[s$autonomy_miss == 0], w[s$autonomy_miss == 0]),
    underuse = wm(s$underuse[s$match_unknown == 0], w[s$match_unknown == 0]))
}))
S$coverage <- read.csv(file.path(base, "results/coverage.csv"))
S$created <- Sys.time()
saveRDS(S, file.path(base, "results/summary.rds"))
cat("estimates:", length(est), " mediation:", length(md), "\n")
