# Scale benchmark for H1: satisfaction difference between the top and bottom
# within-country earnings deciles, from the baseline model (point estimates, PV1,
# final weight), by country. Run from the repository root.
base <- "papers/job_satisfaction"; source(file.path(base, "R/paper_functions.R"))
raw <- readRDS(file.path(base, "results/raw_selected.rds"))
out <- do.call(rbind, lapply(setdiff(names(raw), c("DNK", "SGP", "JPN", "USA", "ISR")), function(cc) {
  d <- prepare(raw[[cc]], cc); s <- d[d$core_ok, ]; if (!has_wage_deciles(s)) return(NULL)
  s$numeracy <- (s$PVNUM1 - 250) / 50; s$wage_decile <- relevel(factor(s$wage_decile), "1")
  m <- lm(update(base_f, satisfaction_score ~ . + numeracy), data = s, weights = SPFWT0)
  data.frame(country = cc, top_vs_bottom = coef(m)[["wage_decile10"]], emos = coef(m)[["EMOS"]])
}))
saveRDS(out, file.path(base, "results/pay_contrast.rds")); print(out)
