# Shared setup for the job satisfaction manuscript, appendix and title page.
# Sourced from the setup chunk of each Quarto file; defines data objects, helpers and labels.
invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
is_pdf <- knitr::is_latex_output()
is_docx <- isTRUE(knitr::pandoc_to() == "docx")
if (is_docx) knitr::opts_chunk$set(dpi = 300)
if (is_pdf) { knitr::opts_chunk$set(dev = "cairo_pdf"); options(kableExtra.latex.load_packages = FALSE) }
suppressPackageStartupMessages({ library(ggplot2); library(knitr); library(kableExtra); library(patchwork) })
here::i_am("papers/job_satisfaction/manuscript/_setup.R")
root <- here::here()
source(file.path(root, "papers/job_satisfaction/manuscript/figure_helpers.R"))
S <- readRDS(file.path(root, "papers/job_satisfaction/results/summary.rds"))
traits <- c("EMOS", "EXTR", "AGRE", "CONS", "OPEM")
tl <- c(EMOS = "Emotional stability", EXTR = "Extraversion", AGRE = "Agreeableness", CONS = "Conscientiousness", OPEM = "Open-mindedness")
cname <- c(AUT = "Austria", BEL = "Belgium", CAN = "Canada", CHE = "Switzerland", CHL = "Chile", CZE = "Czechia", DEU = "Germany",
  ESP = "Spain", EST = "Estonia", FIN = "Finland", FRA = "France", GBR = "England (UK)", HRV = "Croatia", HUN = "Hungary", IRL = "Ireland",
  ITA = "Italy", KOR = "Korea", LTU = "Lithuania", LVA = "Latvia", NZL = "New Zealand", POL = "Poland", PRT = "Portugal", SVK = "Slovakia")
cl <- c(pay = "Pay (earnings decile)", time_pressure = "Time pressure", repetitive = "Repetitive tasks", physical = "Physical work",
  change = "Change in work environment", autonomy = "Autonomy", learning = "Learning at work", social = "Social tasks",
  support = "Support", voice = "Voice", underuse = "Skill underuse")
fl <- c(EXTR_SOCI = "E: sociability", EXTR_ASSE = "E: assertiveness", EXTR_ENER = "E: energy",
  AGRE_COMP = "A: compassion", AGRE_RESP = "A: respectfulness", AGRE_TRUS = "A: trust",
  CONS_ORGA = "C: organization", CONS_PROD = "C: productiveness", CONS_RESP = "C: responsibility",
  EMOS_ANXI = "ES: (low) anxiety", EMOS_DEPR = "ES: (low) depression", EMOS_VOLA = "ES: (low) volatility",
  OPEM_AEST = "O: aesthetic sensitivity", OPEM_CURI = "O: intellectual curiosity", OPEM_IMAG = "O: creative imagination")
f3 <- function(x) sprintf("%.3f", x); f2 <- function(x) sprintf("%.2f", x); f1 <- function(x) sprintf("%.1f", x)
pc <- function(x, d = 0) sprintf(paste0("%.", d, "f"), 100 * x)
big <- function(x) format(x, big.mark = ",")
mm <- function(x, d = 3) sprintf(paste0("%.", d, "f / %.", d, "f"), mean(x), median(x))
rng <- function(x, d = 3) paste0(sprintf(paste0("%.", d, "f"), min(x)), " to ", sprintf(paste0("%.", d, "f"), max(x)))
# Word output: flextable tables (native, editable Word tables) in Times New Roman with
# journal-style rules (top and bottom of the header, bottom of the body) and the note
# as a footer row. PDF/HTML output: kableExtra.
md_esc <- function(x) gsub("\\*", "\\\\*", x)
ft_build <- function(s, note = NULL, size = 8, land = FALSE, wrap = NULL) {
  d <- s$df; fs <- min(size, 9)
  ft <- flextable::flextable(d)
  ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
  ft <- flextable::fontsize(ft, size = fs, part = "all")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::border_remove(ft)
  thick <- officer::fp_border(width = 1); thin <- officer::fp_border(width = 0.5)
  ft <- flextable::hline_top(ft, border = thick, part = "header")
  ft <- flextable::hline_bottom(ft, border = thin, part = "header")
  ft <- flextable::hline_bottom(ft, border = thick, part = "body")
  a <- if (is.null(s$align)) rep("l", ncol(d)) else strsplit(s$align, "")[[1]]
  am <- c(l = "left", c = "center", r = "right")
  for (j in seq_len(min(length(a), ncol(d)))) ft <- flextable::align(ft, j = j, align = am[[a[j]]], part = "all")
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  ft <- flextable::valign(ft, valign = "bottom", part = "header")
  ft <- flextable::valign(ft, valign = "top", part = "body")
  ft <- flextable::padding(ft, padding.top = 1.5, padding.bottom = 1.5, padding.left = 3, padding.right = 3, part = "all")
  if (!is.null(note)) {
    ft <- flextable::add_footer_lines(ft, "")
    ft <- flextable::compose(ft, part = "footer", i = 1, j = 1, value = flextable::as_paragraph(flextable::as_i("Note. "), note))
    ft <- flextable::font(ft, fontname = "Times New Roman", part = "footer")
    ft <- flextable::fontsize(ft, size = fs - 1, part = "footer")
    ft <- flextable::align(ft, align = "left", part = "footer")
    ft <- flextable::padding(ft, padding.top = 3, part = "footer")
  }
  flextable::set_table_properties(ft, layout = "autofit", width = 1)
}
fin <- function(k, note = NULL, size = 8, land = FALSE, wrap = NULL, long = FALSE) {
  if (is_docx) return(ft_build(k, note, size, land, wrap))
  if (is_pdf) {
    k <- kable_styling(k, latex_options = if (long) "repeat_header" else "HOLD_position", font_size = size)
    if (!is.null(wrap)) for (i in seq_along(wrap)) k <- column_spec(k, as.integer(names(wrap)[i]), width = wrap[[i]])
  } else k <- kable_styling(k, full_width = FALSE, font_size = 12)
  if (!is.null(note)) k <- footnote(k, general = note, general_title = "Note.", footnote_as_chunk = TRUE, threeparttable = TRUE)
  if (is_pdf && land) k <- landscape(k)
  if (is_pdf) { a <- attributes(k); k <- gsub("\\\\addlinespace\n?", "", k); attributes(k) <- a }  # knitr inserts a gap every five rows in Quarto; remove it
  k
}
kb <- function(x, ..., align = NULL, caption = NULL) {
  if (is_docx) return(list(df = as.data.frame(x, check.names = FALSE), align = align, caption = caption))
  kable(x, ..., align = align, caption = caption, booktabs = TRUE, linesep = "")
}
# Word only: a landscape page for wide tables (raw section breaks; ignored in other formats)
sect_portrait <- '<w:p><w:pPr><w:sectPr><w:pgSz w:w="11906" w:h="16838"/><w:pgMar w:top="1417" w:right="1247" w:bottom="1417" w:left="1247" w:header="708" w:footer="708" w:gutter="0"/></w:sectPr></w:pPr></w:p>'
sect_landscape <- '<w:p><w:pPr><w:sectPr><w:pgSz w:w="16838" w:h="11906" w:orient="landscape"/><w:pgMar w:top="1247" w:right="1417" w:bottom="1247" w:left="1417" w:header="708" w:footer="708" w:gutter="0"/></w:sectPr></w:pPr></w:p>'
theme_set(paper_theme())
col5 <- c("Emotional stability" = "#c44536", "Extraversion" = "#2a6f97", "Agreeableness" = "#7b5ea7", "Conscientiousness" = "#8ab17d", "Open-mindedness" = "#e9c46a")
p1 <- S$part1; CH <- S$channels; info <- S$info; MED_all <- S$mediation
MED <- MED_all[MED_all$outcome == "very_satisfied", ]
nC <- nrow(info); n_ext <- sum(info$extended); n_fac <- sum(info$facets); n_a1 <- sum(info$a1); n_ten <- sum(info$tenure)
flag <- S$pay_share$country[S$pay_share$pay_share < 0.5]; n_pay <- nC - length(flag)
flagname <- local({ x <- cname[flag]; if (length(x) < 2) x else paste0(paste(head(x, -1), collapse = ", "), if (length(x) > 2) ", and " else " and ", tail(x, 1)) })
signp <- function(k, n) binom.test(k, n)$p.value
fmtp <- function(p) ifelse(p < 0.001, "< 0.001", paste0("= ", sprintf("%.3f", p)))
# Main outcome: extremely satisfied (1 = category 5). Effects of binary outcomes are
# probabilities; pp() prints them in percentage points.
o <- "very_satisfied"
pp <- function(x, d = 1) sprintf(paste0("%.", d, "f"), 100 * x)

# ---- H1: trait coefficients under the three estimands ----
K <- p1[p1$block == "main" & p1$kind == "base" & p1$var %in% traits, ]
Ks <- K[K$outcome == o, ]
blk <- function(b, out = o) p1[p1$block == b & p1$var %in% traits & p1$outcome == out, ]
KNs <- blk("sens_nopay"); KO <- blk("sens_occupation"); KPC <- blk("sens_pay_complete"); KNC <- blk("sens_nopay_complete")
KLW <- blk("sens_logpay"); KDW <- blk("sens_decile_wagesample"); KDL <- blk("sens_decile_plus_logpay")
KJ <- blk("sens_job"); KCL <- blk("sens_clark")
n_lw <- length(unique(KLW$country))
kq <- function(v, s = 1, u = Ks) sum(u$var == v & u$q < .05 & sign(u$estimate) == s)   # after BH adjustment
kp5 <- function(v, s = 1, u = Ks) sum(u$var == v & u$p < .05 & sign(u$estimate) == s)  # unadjusted 5%
kp <- function(v, s = 1, u = Ks) sum(u$var == v & u$p < .10 & sign(u$estimate) == s)   # unadjusted 10%
adj <- function(n, m, after = "", paren = TRUE) if (n == m) paste0(n, after) else if (paren) sprintf("%d%s (%d after adjustment)", n, after, m) else sprintf("%d%s, %d after adjustment", n, after, m)
sm_ci <- function(x, d = 3) { r <- summ(x); c(mean = ci_txt(r$estimate[1], r$lower[1], r$upper[1], d), median = ci_txt(r$estimate[2], r$lower[2], r$upper[2], d)) }
kmean <- function(v, u = Ks) mean(u$estimate[u$var == v]); kmed <- function(v, u = Ks) median(u$estimate[u$var == v])
kc <- function(out, v, s) { u <- K[K$outcome == out & K$var == v, ]; sum(u$p < .05 & sign(u$estimate) == s) }
rgK <- function(out, v) { u <- K[K$outcome == out & K$var == v, ]; e <- 100 * abs(u$estimate); paste0(f1(min(e)), "–", f1(max(e))) }
ratio_means <- function(v, a, b) { m <- merge(a[a$var == v, ], b[b$var == v, ], by = "country"); 1 - mean(m$estimate.x) / mean(m$estimate.y) }
pay_absorb <- function(v) ratio_means(v, Ks, KNs)            # pay-adjusted vs unadjusted, all respondents
pay_absorb_cc <- function(v) ratio_means(v, KPC, KNC)        # the same, complete pay cases only
pct1 <- function(x) if (abs(x) < 0.005) "less than 1" else pc(x, 1)
occ_absorb <- function(v) ratio_means(v, KO, Ks)             # occupation added to the pay-adjusted model
# ---- Mediation (mediate): total, direct and indirect effects through the 5 blocks ----
blk <- c(pay = "Pay", job = "Job characteristics", demands = "Job demands", resources = "Job resources", underuse = "Skill underuse")
med_info <- S$med_info
# set_med(out): point the mediation helpers below at one outcome (default: the main one).
set_med <- function(out = o) {
  M <- MED_all[MED_all$outcome == out & !grepl("^ord_", MED_all$kind), ]
  tt <- M[M$kind == "total", c("country", "var", "estimate", "q", "p")]; names(tt)[3:5] <- c("total", "total_q", "total_p")
  A <- merge(M[M$kind == "acme", ], tt, by = c("country", "var")); A$share <- A$estimate / A$total
  assign("MED", M, envir = globalenv()); assign("AC", A, envir = globalenv())
  assign("TOT", M[M$kind == "total", ], envir = globalenv()); assign("ADE", M[M$kind == "ade", ], envir = globalenv())
  invisible(NULL)
}
set_med()
# with_med(out, expr): evaluate expr with the helpers pointed at another outcome, then restore.
with_med <- function(out, expr) { set_med(out); on.exit(set_med()); expr }
# acm(m, v): ACME through mediator m ("pay", "demands", "resources", "underuse", "all", a split index
# such as "demands_dev", or a condition index such as "time_pressure_mean"), one row per country.
acm <- function(m, v) { u <- AC[AC$mediator == m & AC$var == v, ]; u[order(u$country), ] }
mrow <- function(kind, m, v) { u <- MED[MED$kind == kind & MED$mediator == m & MED$var == v, ]; u[order(u$country), ] }
npos <- function(m, v) { u <- acm(m, v); sum(u$p < .05 & u$estimate > 0) }; nposq <- function(m, v) { u <- acm(m, v); sum(u$q < .05 & u$estimate > 0) }
nneg <- function(m, v) { u <- acm(m, v); sum(u$p < .05 & u$estimate < 0) }; nnegq <- function(m, v) { u <- acm(m, v); sum(u$q < .05 & u$estimate < 0) }
nmed <- function(m, v) nrow(acm(m, v))
amean <- function(m, v) mean(acm(m, v)$estimate)
tmean <- function(v) mean(TOT$estimate[TOT$var == v]); dmean <- function(v) mean(ADE$estimate[ADE$var == v])
tsig <- function(v) sum(TOT$var == v & TOT$p < .05 & TOT$estimate > 0)
tsig10 <- function(v) sum(TOT$var == v & TOT$p < .10 & TOT$estimate > 0)
tposn <- function(v) sum(TOT$var == v & TOT$estimate > 0)
listify <- function(x) { x <- unname(x); if (length(x) < 2) x else paste0(paste(head(x, -1), collapse = ", "), if (length(x) > 2) ", and " else " and ", tail(x, 1)) }
# Countries with a total effect significant at level a with sign s, as an English list.
tnames <- function(v, s = 1, a = .05) { u <- TOT[TOT$var == v & TOT$p < a & sign(TOT$estimate) == s, ]; listify(sort(cname[u$country])) }
tneg <- function(v) sum(TOT$var == v & TOT$p < .05 & TOT$estimate < 0)
# share of the mean total effect: mean ACME (or ADE) over mean total
mshare <- function(m, v) if (m == "direct") dmean(v) / tmean(v) else amean(m, v) / tmean(v)
# Net of pay: total effect minus the ACME through pay (no pay block where no decile is released).
netpay_cc <- function(v) { t <- TOT[TOT$var == v, c("country", "estimate")]; a <- acm("pay", v)
  t$pay <- a$estimate[match(t$country, a$country)]; t$pay[is.na(t$pay)] <- 0; t$net <- t$estimate - t$pay; t }
netpay <- function(v) mean(netpay_cc(v)$net)
payshare <- function(v) amean("pay", v) / mean(TOT$estimate[TOT$var == v & TOT$country %in% acm("pay", v)$country])
# Sum of the 5 block ACMEs (= ACME through all blocks) as a share of the total, mean of countries.
medshare <- function(v) amean("all", v) / tmean(v)
# Countries with a significant positive (s = 1) or negative (s = -1) ACME through m.
nsig <- function(m, v, s = 1) if (s > 0) npos(m, v) else nneg(m, v)
RHO <- S$med_rho; GRID <- S$med_grid
RHO$last <- sub(".*\\+", "", RHO$blocks)
GRID <- merge(GRID, RHO[c("country", "k", "last")], by = c("country", "k"))
# rho* of the cumulative blocks ending with block b (pay; ...; underuse = all blocks), mean of countries.
rho_last <- function(b) mean(RHO$rho_star[RHO$last == b])
# ---- Ordinal outcome: ordered probit of the 1-5 score, without ("none") and with all mediator
# variables ("all"): effect of 1 SD on P(extremely satisfied) ("top"), on P(not satisfied)
# ("bottom") and the latent coefficient ("coef") ----
ordc <- function(kind, v, m = "none") { u <- MED_all[MED_all$kind == paste0("ord_", kind) & MED_all$mediator == m & MED_all$var == v, ]; u[order(u$country), ] }
ord_sig <- function(kind, v, s = 1, m = "none") { u <- ordc(kind, v, m); sum(u$p < .05 & sign(u$estimate) == s) }
# ---- Asymmetry: effect on P(extremely satisfied) + effect on P(not satisfied), WLS with pay ----
TB <- p1[p1$block == "main" & p1$kind == "base" & p1$outcome == "top_plus_bottom" & p1$var %in% traits, ]
tb <- function(v, s) sum(TB$var == v & TB$p < .05 & sign(TB$estimate) == s); tbmean <- function(v) mean(TB$estimate[TB$var == v])
# Benchmark from the ordered probit (single index): implied effect on P(top) + P(bottom), and the
# ratio |effect on P(not satisfied)| / effect on P(extremely satisfied), observed vs implied.
ord_implied <- function(v) { a <- ordc("top", v); b <- ordc("bottom", v); data.frame(country = a$country, implied = a$estimate + b$estimate) }
tb_below <- function(v) { i <- ord_implied(v); t <- TB[TB$var == v, ]; sum(t$estimate[match(i$country, t$country)] < i$implied) }
tb_above <- function(v) nrow(ord_implied(v)) - tb_below(v)
bt_ratio <- function(v) -with_med("not_satisfied", tmean(v)) / tmean(v)
ord_ratio <- function(v) -mean(ordc("bottom", v)$estimate) / mean(ordc("top", v)$estimate)
# Logit average marginal effects (robustness).
NLv <- S$nonlinear
lg <- function(out, v, s = 1) { u <- NLv[NLv$spec == paste0("logit_", out) & NLv$var == v, ]; sum(u$p < .05 & sign(u$estimate) == s) }
lgmean <- function(out, v) mean(NLv$estimate[NLv$spec == paste0("logit_", out) & NLv$var == v])
# ---- Full specifications (appendix): coefficients of the total-effect and outcome equations ----
MC <- S$med_coefs
# Mediation table in mediate's layout: rows (ACME by mediator, ADE, total effect, proportion
# mediated), traits in columns; cells: mean of the country estimates with stars from its
# interval (sqrt(sum se^2) / K), and the 95% interval below.
# Binary outcomes: percentage points with 2 decimals (sc = 100); the 1-5 score: points (sc = 1, d = 3).
med_cell <- function(u, sc = 100, d = 2) { r <- summ(u)[1, ]; z <- r$estimate / r$se
  st <- ifelse(abs(z) > qnorm(.995), "***", ifelse(abs(z) > qnorm(.975), "**", ifelse(abs(z) > qnorm(.95), "*", "")))
  g <- function(x) sprintf(paste0("%.", d, "f"), sc * x)
  sprintf("%s%s\n[%s, %s]", g(r$estimate), st, g(r$lower), g(r$upper)) }
sm_pp <- function(x) { r <- summ(x); ci_txt(100 * r$estimate[1], 100 * r$lower[1], 100 * r$upper[1], 1) }
med_table <- function(meds, labels, ...) {
  tv <- c("EMOS", "EXTR", "AGRE", "CONS", "OPEM")
  rows <- lapply(seq_along(meds), function(i) c(labels[i], sapply(tv, function(v) med_cell(acm(meds[i], v), ...))))
  rows <- c(rows, list(c("ADE: direct effect", sapply(tv, function(v) med_cell(mrow("ade", "all", v), ...))),
    c("Total effect", sapply(tv, function(v) med_cell(mrow("total", "all", v), ...))),
    c("Proportion mediated (%)", sapply(tv, function(v) pc(sum(sapply(meds, amean, v = v)) / tmean(v))))))
  tab <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE); names(tab) <- c("Effect", tl[tv]); tab
}
# ---- Appraisal: occupational-group means and individual deviations ----
perc <- function(k, v = "EMOS", model = "A1") CH[CH$model == model & CH$kind == "perceived" & CH$outcome == k & CH$var == v, ]
actsel <- function(k, v = "EMOS", model = "A1") CH[CH$model == model & CH$kind == "actualsel" & CH$outcome == k & CH$var == v, ]
pneg <- function(k, v = "EMOS", model = "A1") sum(perc(k, v, model)$estimate < 0)
# Equivalence: 90% interval within +/- margin (two one-sided tests at 5%).
equiv <- function(u, margin) sum(abs(u$estimate) + qnorm(.95) * u$se < margin)
eq_margin_cell <- 0.03   # SD of the condition per SD of the trait: less than half the typical deviation coefficient
FA <- CH[CH$model == "A2" & CH$kind == "facet", ]
fsig <- function(v) { u <- FA[FA$var == v & FA$outcome == o, ]; sum(u$p < .05 & u$estimate > 0) }; fsigq <- function(v) { u <- FA[FA$var == v & FA$outcome == o, ]; sum(u$q < .05 & u$estimate > 0) }
fpos <- function(v) { u <- FA[FA$var == v & FA$outcome == o, ]; sum(u$estimate > 0) }
fmean <- function(v) mean(FA$estimate[FA$var == v & FA$outcome == o])
FAP <- CH[CH$model == "A2" & CH$kind == "facet_full" & grepl(":", CH$var) & CH$outcome == o, ]
FD <- CH[CH$model == "A2" & CH$kind == "facet_diff" & CH$outcome == o, ]
fdiff <- function(k) { u <- FD[FD$var == k, ]; c(n = nrow(u), pos = sum(u$p < .05 & u$estimate > 0), mean = mean(u$estimate), lo = summ(u)$lower[1], hi = summ(u)$upper[1]) }
# ---- Indicator test ----
IND <- CH[CH$model == "IND" & CH$kind == "gap" & CH$outcome %in% c(o, "all"), ]
gap <- function(k) { u <- IND[IND$var == k, ]; u[order(u$country), ] }
n_ind <- length(unique(IND$country)); ind_skil <- intersect(unique(IND$country), info$country[info$occupation_source == "ISCOSKIL4"])
gap_change <- function(from, to) { a <- gap(from); b <- gap(to); m <- merge(a, b, by = "country"); m$estimate.y - m$estimate.x }
# ---- Reactivity ----
ext_c <- info$country[info$extended]
RX <- CH[CH$kind == "pair" & ((CH$model == "Rext") | (CH$model == "R" & !CH$country %in% ext_c)), ]
RX$q2 <- ave(RX$p, paste(RX$outcome, RX$pair_type), FUN = function(p) p.adjust(p, "BH"))
pair_def <- data.frame(pair = c("R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "PL1", "PL2", "PL3"),
  hyp = c("H4a", "H4a", "H4a", "H4b", "H4c", "H4c", "H4d", "H4e", "Placebo", "Placebo", "Placebo"),
  trait = c("EMOS", "EMOS", "EMOS", "EXTR", "OPEM", "OPEM", "CONS", "AGRE", "EXTR", "AGRE", "CONS"),
  condition = c("time_pressure", "repetitive", "change", "social", "learning", "underuse", "autonomy", "support", "physical", "repetitive", "social"),
  sign = c(1, 1, 1, 1, 1, -1, 1, 1, 0, 0, 0))
eq_margin_int <- 0.015   # probability per SD x SD (1.5 pp): about half the slope of autonomy, learning or voice
rsum <- function(pr) { u <- RX[RX$var == pr & RX$outcome == o, ]; s <- pair_def$sign[pair_def$pair == pr]; sm <- summ(u)
  c(n = nrow(u), pred = if (s == 0) NA else sum(u$q2 < .05 & sign(u$estimate) == s), nominal = sum(u$p < .05), mean = mean(u$estimate), lo = sm$lower[1], hi = sm$upper[1],
    median = median(u$estimate), equiv = equiv(u, eq_margin_int), sameside = if (s == 0) sum(u$estimate > 0) else sum(sign(u$estimate) == s)) }
tested <- pair_def$pair[pair_def$sign != 0]; placebo <- pair_def$pair[pair_def$sign == 0]
r_tested_sig <- sum(sapply(tested, function(p) rsum(p)["pred"])); r_tested_n <- sum(sapply(tested, function(p) rsum(p)["n"]))
r_nom_tested <- sum(sapply(tested, function(p) rsum(p)["nominal"])); r_nom_placebo <- sum(sapply(placebo, function(p) rsum(p)["nominal"]))
r_placebo_n <- sum(sapply(placebo, function(p) rsum(p)["n"]))
mde_med <- median(2.8 * RX$se[RX$outcome == o & RX$var != "R6"])
condslope <- CH[CH$kind %in% c("cond", "main") & CH$outcome == o & ((CH$model == "Rext") | (CH$model == "R" & !CH$country %in% ext_c)), ]
cs <- function(v, s) sum(condslope$var == v & condslope$p < .05 & sign(condslope$estimate) == s)
# ---- Robustness ----
gch <- function(model, kind, var) { u <- CH[CH$model == model & CH$kind == kind & CH$var == var & CH$outcome == o, ]; u[order(u$country), ] }
OS <- S$oster[S$oster$outcome == o, ]
APS <- CH[CH$model == "V2_aps" & CH$var == "skill" & CH$outcome == o, ]
TEN <- CH[CH$model == "V3_tenure" & grepl(":long_tenure", CH$var) & CH$outcome == o, ]
LSJ <- CH[CH$model == "A3_jobs" & CH$var %in% traits & CH$outcome == o, ]; LSL <- CH[CH$model == "A3_lifesat" & CH$var %in% traits & CH$outcome == o, ]
PC <- S$pay_contrast[S$pay_contrast$outcome == o, ]
Dn <- S$distribution[S$distribution$category >= 3 & S$distribution$country %in% info$country, ]; ns_share <- tapply(Dn$share, Dn$country, sum)
CD <- S$cell_diag

# ---- objects shared by the main text and the appendix ----
ml <- c(all = "All blocks jointly", pay = "Pay", job = "Job characteristics", demands = "Job demands", resources = "Job resources", underuse = "Skill underuse")
clm <- c(cl, all = "All core conditions jointly")
conds <- c("time_pressure", "repetitive", "physical", "change", "autonomy", "learning", "social", "support", "voice")
fls <- sub("^[A-Z]+: ", "", fl); fls <- sub("^\\(low\\)", "(Low)", fls); substr(fls, 1, 1) <- toupper(substr(fls, 1, 1))
FAo <- FA[FA$outcome == o & FA$var %in% names(fl), ]
fl2 <- setNames(paste0(tl[sub("_.*", "", names(fl))], "\n", fls), names(fl))
