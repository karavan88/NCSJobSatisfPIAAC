# Version 2: collect country results into tidy tables with BH q-values for the manuscript.
# Run from the repository root: Rscript papers/job_satisfaction/R/07_collect.R
options(warn = 1)
base <- "papers/job_satisfaction"
source(file.path(base, "R/paper_functions.R"))
source(file.path(base, "R/channel_functions.R"))
rd <- function(dir) lapply(list.files(file.path(base, "results", dir), "\\.rds$", full.names = TRUE), readRDS)
bh <- function(p) p.adjust(p, "BH")
# Israel is excluded from the paper (author's decision, 15 October 2026; PROTOCOL.md). Filter before BH.
exclude <- c("ISR")
keep <- function(x) x[!x$country %in% exclude, , drop = FALSE]
S <- list()

# ---- P1 and version 1 sensitivities (02_part1.R) ----
p1 <- rd("part1"); stopifnot(length(p1) > 0)
co <- do.call(rbind, lapply(p1, `[[`, "coef"))
co <- keep(co[!grepl("^s2\\|", co$model), ])
co$block <- sub("\\|.*", "", co$model); co$outcome <- ifelse(grepl("\\|", co$model), sub(".*\\|", "", co$model), NA)
co$kind <- sub(":.*", "", co$term); co$var <- sub(".*:", "", co$term)
sens <- grepl("^sens_|^lifesat", co$model)
co$outcome[sens] <- sub("\\|.*", "", co$term[sens]); co$var[sens] <- sub(".*\\|", "", co$term[sens]); co$kind[sens] <- "coef"
co$family <- paste(co$block, co$outcome, co$kind, ifelse(co$var %in% traits, "trait", co$var))
co$q <- ave(co$p, co$family, FUN = bh)
S$part1 <- co
jo <- keep(do.call(rbind, lapply(p1, `[[`, "joint"))); jo$q <- ave(jo$p, jo$model, FUN = bh); S$joint <- jo
S$flow1 <- keep(do.call(rbind, lapply(p1, function(x) x$flow[c("country", "employees", "analysis", "with_self_employed")])))

nl <- rd("nonlinear")
if (length(nl)) {
  n <- keep(do.call(rbind, lapply(nl, `[[`, "coef")))
  n$spec <- sub("\\|.*", "", n$term); n$var <- sub(".*\\|", "", n$term)
  n$q <- ave(n$p, n$spec, FUN = bh); S$nonlinear <- n
}

# ---- Channels (08_channels.R) ----
ch <- rd("channels")
cc <- keep(do.call(rbind, lapply(ch, `[[`, "coef")))
parts <- strsplit(cc$term, "|", fixed = TRUE)
cc$kind <- sapply(parts, `[`, 1); cc$outcome <- sapply(parts, `[`, 2); cc$var <- sapply(parts, `[`, 3)
# Families: model x kind x outcome across countries and terms; tested and placebo pairs separately.
cc$pair_type <- ifelse(cc$kind == "pair", ifelse(grepl("^PL", cc$var), "placebo", "tested"), "")
cc$family <- paste(cc$model, cc$kind, cc$outcome, cc$pair_type)
cc$q <- ave(cc$p, cc$family, FUN = bh)
S$channels <- cc
S$info <- keep(do.call(rbind, lapply(ch, `[[`, "info")))
S$oster <- keep(do.call(rbind, lapply(ch, `[[`, "oster")))
dg <- lapply(ch, `[[`, "diag"); dg <- dg[!vapply(dg, is.null, logical(1))]
if (length(dg)) S$cell_diag <- keep(do.call(rbind, dg))

md <- rd("mediation")
if (length(md)) {
  m <- keep(do.call(rbind, md)); pr <- strsplit(m$term, "|", fixed = TRUE)
  m$kind <- sapply(pr, `[`, 1); m$mediator <- sapply(pr, `[`, 2); m$var <- sapply(pr, `[`, 3)
  m$q <- ave(m$p, paste(m$kind, m$mediator), FUN = bh)
  S$mediation <- m
}
fo <- list()  # causal forests dropped on 15 October 2026 (see PROTOCOL.md); code in archive_v1/
if (length(fo)) {
  S$forest_summary <- do.call(rbind, lapply(fo, function(x) do.call(rbind, lapply(x, `[[`, "summary"))))
  S$forest_summary$q_calibration <- ave(S$forest_summary$differential_p, S$forest_summary$trait, FUN = bh)
  b <- do.call(rbind, lapply(fo, function(x) do.call(rbind, lapply(x, `[[`, "blp"))))
  b$q <- NA; i <- b$term != "at_mean"; b$q[i] <- ave(b$p[i], b$trait[i], FUN = bh)
  S$forest_blp <- b
  S$forest_importance <- do.call(rbind, lapply(fo, function(x) do.call(rbind, lapply(x, `[[`, "importance"))))
}

# ---- Descriptives ----
raw <- readRDS(file.path(base, "results/raw_selected.rds"))
S$distribution <- do.call(rbind, lapply(setdiff(names(raw), exclude), function(k) {
  d <- raw[[k]]; e <- d$D2_Q04 %in% 1 & d$AGEG10LFS %in% 2:5 & d$D2_Q13 %in% 1:5 & d$SPFWT0 > 0
  if (!any(e)) return(NULL)
  data.frame(country = k, category = 1:5, share = sapply(1:5, function(c) wm(d$D2_Q13[e] == c, d$SPFWT0[e])), n = sum(e))
}))
pay_share <- list()
S$condition_means <- do.call(rbind, lapply(unique(S$info$country), function(k) {
  d <- prepare(raw[[k]], k); s <- d[d$core_ok, ]
  pay_share[[k]] <<- data.frame(country = k, pay_share = mean(s$wage_decile != "Unknown"))
  do.call(rbind, lapply(c(conditions_core, conditions_ext), function(v) {
    ok <- s[[paste0(v, "_miss")]] == 0
    if (mean(ok) < 0.5) return(NULL)
    data.frame(country = k, condition = v, mean_raw = wm(s[[paste0(v, "_raw")]][ok], s$SPFWT0[ok]), share_observed = mean(ok))
  }))
}))
S$pay_share <- do.call(rbind, pay_share)
S$coverage <- read.csv(file.path(base, "results/coverage.csv"))
if (file.exists(file.path(base, "results/validation.csv"))) S$validation <- read.csv(file.path(base, "results/validation.csv"))
S$created <- Sys.time()
saveRDS(S, file.path(base, "results/summary.rds"))
cat("P1:", length(p1), " channels:", length(ch), " mediation:", length(md), " nonlinear:", length(nl), "\n")
