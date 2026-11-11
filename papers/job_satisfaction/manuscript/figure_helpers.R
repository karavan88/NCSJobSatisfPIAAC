# Figure helpers for the job satisfaction manuscript (version 4).
# Conventions shared by every figure:
#   * one ink colour for estimates; colour is used only to distinguish parts of an effect;
#   * significance marked three ways from the unadjusted p-value: filled (p < 0.05),
#     grey (p < 0.10), hollow (neither). Adjusted counts are reported in the text and tables;
#   * countries alphabetical, top to bottom, then a gap, then a Mean row with its
#     95% interval (sqrt(sum se^2)/K); the median is computed by summ() but not drawn;
#   * traits as panels, separated by a strip and a border.
# ASCII only inside figure text: the png device does not render every Unicode glyph.

ink <- "grey15"; c_blue <- "#4a9fe0"; c_pink <- "#e8859a"; c_orange <- "#e9a23b"; c_grey <- "grey72"

paper_theme <- function(base_size = 11) theme_minimal(base_size = base_size, base_family = "Helvetica") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), legend.position = "bottom",
    strip.text = element_text(face = "bold", size = base_size), strip.background = element_rect(fill = "grey92", colour = NA),
    panel.border = element_rect(colour = "grey78", fill = NA, linewidth = 0.5), panel.spacing.x = unit(14, "pt"),
    plot.title.position = "plot", axis.title.x = element_text(size = base_size - 2), plot.caption = element_text(hjust = 0, size = base_size - 2.5))

# ---- significance from the unadjusted p-value ----
sig3 <- function(p) factor(ifelse(!is.na(p) & p < .05, "p < 0.05", ifelse(!is.na(p) & p < .10, "p < 0.10", "not significant")),
  levels = c("p < 0.05", "p < 0.10", "not significant"))
scale_sig3 <- function(name = NULL) scale_fill_manual(values = c(`p < 0.05` = ink, `p < 0.10` = "grey68", `not significant` = "white"), name = name, drop = FALSE)
sig_point <- function(data, mapping, size = 1.9) geom_point(data = data, mapping, shape = 21, colour = ink, size = size, stroke = 0.6)

# ---- summary rows (wages-paper convention) ----
z975 <- qnorm(.975)
median_ci <- function(est, se, n_sim = 20000, seed = 20261016) {
  set.seed(seed); k <- length(est)
  draws <- matrix(rnorm(n_sim * k, rep(est, each = n_sim), rep(se, each = n_sim)), nrow = n_sim)
  med <- apply(draws, 1, median)
  c(estimate = median(est), lower = unname(quantile(med, .025)), upper = unname(quantile(med, .975)))
}
summ <- function(x) {  # x: data frame with estimate and se; one row per country
  k <- nrow(x); m <- mean(x$estimate); s <- sqrt(sum(x$se^2)) / k; md <- median_ci(x$estimate, x$se)
  data.frame(row = c("Mean", "Median"), estimate = c(m, md[["estimate"]]), lower = c(m - z975 * s, md[["lower"]]), upper = c(m + z975 * s, md[["upper"]]), se = c(s, NA))
}
ci_txt <- function(e, lo, hi, d = 3) sprintf(paste0("%.", d, "f [%.", d, "f, %.", d, "f]"), e, lo, hi)

# ---- country axis with Mean and Median at the bottom ----
# Rows are keyed "panel country" so that each panel keeps the full set of rows; the scale
# is trained on a blank layer holding every key in level order.
country_levels <- function(panel, countries, cname) {
  ord <- countries[order(cname[countries], decreasing = TRUE)]
  c(paste(panel, "Mean"), paste(panel, "gap"), paste(panel, ord))
}
# When every panel has the same countries (the usual case) the axis is shared: one set
# of row labels at the left, keyed by country alone, and the panels use a fixed y scale.
# Otherwise rows are keyed "panel country" and each panel gets its own y scale.
axis_frame <- function(panels, countries_by_panel, cname, flag = character(), panel_col = "panel", union = FALSE) {
  sets <- lapply(countries_by_panel[panels], function(x) sort(unique(x)))
  shared <- union || all(vapply(sets, function(x) identical(x, sets[[1]]), logical(1)))
  lab_of <- function(k) if (k %in% flag) paste0(cname[k], " †") else cname[k]
  if (shared) {
    all_c <- sort(unique(unlist(sets))); ord <- all_c[order(cname[all_c], decreasing = TRUE)]
    lev <- c("Mean", "gap", ord)
    lab <- setNames(c("Mean", "", vapply(ord, lab_of, character(1))), lev)
    blank <- expand.grid(key = factor(lev, levels = lev), panel = factor(panels, levels = panels)); names(blank)[2] <- panel_col; blank$x <- 0
  } else {
    lev <- unlist(lapply(panels, function(p) country_levels(p, countries_by_panel[[p]], cname)))
    lab <- setNames(sub("^.* ", "", lev), lev); lab[grepl(" gap$", lev)] <- ""
    for (k in unique(unlist(countries_by_panel))) lab[grepl(paste0(" ", k, "$"), lev)] <- lab_of(k)
    blank <- data.frame(key = factor(lev, levels = lev)); blank[[panel_col]] <- factor(sub(" [^ ]+$", "", lev), levels = panels); blank$x <- 0
  }
  list(levels = lev, labels = lab, blank = blank, shared = shared, scales = if (shared) "fixed" else "free_y")
}
key_of <- function(af, panel, country) if (af$shared) factor(country, levels = af$levels) else factor(paste(panel, country), levels = af$levels)
summary_band <- function() annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, fill = "grey94")

# ---- Figure: coefficients by country, traits as panels (H1 and similar) ----
fig_country_dots <- function(u, panel_levels, cname, flag = character(), xlab) {
  # u: country, panel, estimate, se, lower, upper, p
  cb <- split(u$country, u$panel); af <- axis_frame(panel_levels, cb, cname, flag)
  u$key <- key_of(af, u$panel, u$country); u$sig <- sig3(u$p); u$panel <- factor(u$panel, levels = panel_levels)
  rows <- do.call(rbind, lapply(panel_levels, function(p) { s <- summ(u[u$panel == p, ]); s$panel <- p; s }))
  rows <- rows[rows$row == "Mean", ]; rows$key <- key_of(af, rows$panel, rows$row); rows$panel <- factor(rows$panel, levels = panel_levels)
  ggplot() + geom_blank(data = af$blank, aes(x, key)) + summary_band() + geom_vline(xintercept = 0, colour = "grey55") +
    geom_errorbar(data = u, aes(xmin = lower, xmax = upper, y = key), width = 0, colour = "grey65", linewidth = 0.4, orientation = "y") +
    sig_point(u, aes(estimate, key, fill = sig)) +
    geom_errorbar(data = rows, aes(xmin = lower, xmax = upper, y = key), width = 0, colour = ink, linewidth = 0.6, orientation = "y") +
    geom_point(data = rows, aes(estimate, key), shape = 18, size = 3.4, colour = ink) +
    scale_sig3() + scale_y_discrete(labels = af$labels) + facet_wrap(~ panel, nrow = 1, scales = af$scales) +
    labs(x = xlab, y = NULL) + paper_theme() + theme(axis.text.y = element_text(size = 8.5))
}

# ---- Figure: direct and indirect effects by country (total = indirect + direct) ----
fig_direct_indirect <- function(D, panel_levels, cname, flag = character(), xlab, indirect_label = "Indirect effect (through pay and job conditions)") {
  # D: country, panel, total (estimate, se, lower, upper, p), indirect (I, p_ind), direct (Dr)
  cb <- split(D$country, D$panel); af <- axis_frame(panel_levels, cb, cname, flag)
  D$key <- key_of(af, D$panel, D$country); D$sig <- sig3(D$p); D$panel <- factor(D$panel, levels = panel_levels)
  opp_lab <- "Indirect effect, opposite in sign to the total"
  D$ind_col <- ifelse(D$I == 0 | sign(D$I) == sign(D$estimate), indirect_label, opp_lab)
  D$dir_col <- "Direct effect"; D$ind_sig <- D$p_ind < .05
  rows <- do.call(rbind, lapply(panel_levels, function(p) { x <- D[D$panel == p, ]; s <- summ(x); s$panel <- p; s$I <- c(mean(x$I), NA); s }))
  rows <- rows[rows$row == "Mean", ]; rows$key <- key_of(af, rows$panel, rows$row); rows$panel <- factor(rows$panel, levels = panel_levels)
  mrow <- rows[rows$row == "Mean", ]
  cols <- setNames(c(c_blue, c_grey, c_pink), c(indirect_label, "Direct effect", opp_lab))
  ggplot() + geom_blank(data = af$blank, aes(x, key)) + summary_band() + geom_vline(xintercept = 0, colour = "grey45") +
    geom_segment(data = D, aes(x = I, xend = estimate, y = key, yend = key, colour = dir_col), linewidth = 2.6) +
    geom_segment(data = D, aes(x = 0, xend = I, y = key, yend = key, colour = ind_col, alpha = ind_sig), linewidth = 2.6) +
    geom_segment(data = mrow, aes(x = I, xend = estimate, y = key, yend = key), colour = c_grey, linewidth = 2.6) +
    geom_segment(data = mrow, aes(x = 0, xend = I, y = key, yend = key), colour = c_blue, linewidth = 2.6) +
    geom_errorbar(data = D, aes(xmin = lower, xmax = upper, y = key), width = 0, colour = "grey40", linewidth = 0.35, orientation = "y") +
    sig_point(D, aes(estimate, key, fill = sig)) +
    geom_errorbar(data = rows, aes(xmin = lower, xmax = upper, y = key), width = 0, colour = ink, linewidth = 0.6, orientation = "y") +
    geom_point(data = rows, aes(estimate, key), shape = 18, size = 3.4, colour = ink) +
    scale_colour_manual(values = cols, name = NULL, breaks = names(cols)) +
    scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.35), guide = "none") +
    scale_sig3(name = "Total effect:") + scale_y_discrete(labels = af$labels) + facet_wrap(~ panel, nrow = 1, scales = af$scales) +
    labs(x = xlab, y = NULL) +
    guides(colour = guide_legend(order = 1, override.aes = list(linewidth = 4)), fill = guide_legend(order = 2)) +
    paper_theme() + theme(axis.text.y = element_text(size = 8.5), legend.box = "vertical", legend.spacing.y = unit(0, "pt"))
}

# ---- Figure: indirect effect split into two parts (job cell mean and deviation) ----
fig_split <- function(H, panel_levels, cname, xlab, flag = character(), part_labels = c(dev = "Through the worker's deviation from the occupational-group mean", act = "Through the occupational-group mean")) {
  # H: country, panel, estimate/se/lower/upper/p of the full indirect effect, dev, p_dev, act, p_act
  cb <- split(H$country, H$panel); af <- axis_frame(panel_levels, cb, cname, flag, union = TRUE)  # rows for every country; panels with fewer countries leave blanks
  H$key <- key_of(af, H$panel, H$country); H$sig <- sig3(H$p); H$panel <- factor(H$panel, levels = panel_levels)
  H$dev_sig <- H$p_dev < .05; H$act_sig <- H$p_act < .05; H$dev_lab <- part_labels[["dev"]]; H$act_lab <- part_labels[["act"]]
  rows <- do.call(rbind, lapply(panel_levels, function(p) { x <- H[H$panel == p, ]; s <- summ(x); s$panel <- p; s$dev <- c(mean(x$dev), NA); s$act <- c(mean(x$act), NA); s }))
  rows <- rows[rows$row == "Mean", ]; rows$key <- key_of(af, rows$panel, rows$row); rows$panel <- factor(rows$panel, levels = panel_levels)
  mrow <- rows[rows$row == "Mean", ]
  cols <- setNames(c(c_blue, c_orange), part_labels[c("dev", "act")])
  ggplot() + geom_blank(data = af$blank, aes(x, key)) + summary_band() + geom_vline(xintercept = 0, colour = "grey45") +
    geom_segment(data = H, aes(x = dev, xend = dev + act, y = key, yend = key, colour = act_lab, alpha = act_sig), linewidth = 2.6) +
    geom_segment(data = H, aes(x = 0, xend = dev, y = key, yend = key, colour = dev_lab, alpha = dev_sig), linewidth = 2.6) +
    geom_segment(data = mrow, aes(x = dev, xend = dev + act, y = key, yend = key), colour = c_orange, linewidth = 2.6) +
    geom_segment(data = mrow, aes(x = 0, xend = dev, y = key, yend = key), colour = c_blue, linewidth = 2.6) +
    geom_errorbar(data = H, aes(xmin = lower, xmax = upper, y = key), width = 0, colour = "grey40", linewidth = 0.35, orientation = "y") +
    sig_point(H, aes(estimate, key, fill = sig)) +
    geom_errorbar(data = rows, aes(xmin = lower, xmax = upper, y = key), width = 0, colour = ink, linewidth = 0.6, orientation = "y") +
    geom_point(data = rows, aes(estimate, key), shape = 18, size = 3.4, colour = ink) +
    scale_colour_manual(values = cols, name = NULL, breaks = names(cols)) +
    scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.35), guide = "none") +
    scale_sig3(name = "Indirect effect through the condition:") + scale_y_discrete(labels = af$labels) + facet_wrap(~ panel, nrow = 1, scales = af$scales) +
    labs(x = xlab, y = NULL) +
    guides(colour = guide_legend(order = 1, override.aes = list(linewidth = 4)), fill = guide_legend(order = 2)) +
    paper_theme() + theme(axis.text.y = element_text(size = 8.5), legend.box = "vertical", legend.spacing.y = unit(0, "pt"))
}

# ---- Figure: mean effect across countries with interval, items on the y axis, panels ----
fig_means <- function(W, xlab, caption = NULL, band_last = TRUE, bg = NULL, x_pad = 0.45, label_at = NULL) {
  # W: panel (factor), item (factor, bottom to top), estimate, lower, upper, lab (text at right), highlight (logical)
  # bg: optional data frame of country points: panel, item, estimate
  if (is.null(label_at)) label_at <- max(c(W$upper, if (!is.null(bg)) bg$estimate)) + 0.08 * diff(range(c(W$lower, W$upper)))
  p <- ggplot(W, aes(estimate, item)) + geom_vline(xintercept = 0, colour = "grey45")
  if (band_last) p <- p + annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, fill = "grey94")
  if (!is.null(bg)) p <- p + geom_point(data = bg, aes(estimate, item), colour = "grey75", size = 1.1, position = position_jitter(height = 0.15, seed = 1))
  p + geom_errorbar(aes(xmin = lower, xmax = upper), width = 0, colour = ink, linewidth = 0.5, orientation = "y") +
    geom_point(aes(shape = highlight), size = 2.7, colour = ink) +
    geom_text(aes(x = label_at, label = lab), hjust = 0, size = 2.7, colour = "grey30") +
    scale_shape_manual(values = c(`TRUE` = 18, `FALSE` = 16), guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.05, x_pad))) +
    labs(x = xlab, y = NULL, caption = caption) + paper_theme() + theme(axis.text.y = element_text(size = 9))
}

# Count label "n+ / N", "n- / N", "a+ b- / N" or "0 / N"
count_label <- function(npos, nneg, n) {
  core <- ifelse(npos + nneg == 0, "0", ifelse(npos > 0 & nneg > 0, paste0(npos, "+ ", nneg, "-"), ifelse(nneg > 0, paste0(nneg, "-"), paste0(npos, "+"))))
  paste0(core, " / ", n)
}

# ---- Schematic path diagram for the methods section ----
draw_schematic <- function() {
  # Trait -> 4 mediator blocks -> satisfaction, plus the direct path; colours as in Fig. 3.
  bl <- c("Pay", "Job demands", "Job resources", "Skill underuse"); by <- c(3.3, 2.5, 1.7, 0.9)
  bc <- c("#e9a23b", "#c44536", "#4a9fe0", "#7b5ea7")
  ar <- arrow(length = unit(5, "pt"), type = "closed")
  g <- ggplot()
  for (k in 1:4) g <- g +
    annotate("segment", x = 0.55, y = 2.1, xend = 1.45, yend = by[k], arrow = ar, colour = "grey45", linewidth = 0.4) +
    annotate("segment", x = 2.55, y = by[k], xend = 3.4, yend = 2.1, arrow = ar, colour = "grey45", linewidth = 0.4)
  g + annotate("segment", x = 0, y = 1.75, xend = 0, yend = 0.25, colour = "grey35") +
    annotate("segment", x = 0, y = 0.25, xend = 4, yend = 0.25, colour = "grey35") +
    annotate("segment", x = 4, y = 0.25, xend = 4, yend = 1.72, arrow = ar, colour = "grey35") +
    geom_label(aes(x = 2, y = by, label = bl), size = 3.4, fontface = "bold", fill = bc, colour = "white",
      label.size = 0, label.padding = unit(0.45, "lines")) +
    geom_label(aes(x = c(0, 4), y = 2.1, label = c("Trait", "Job\nsatisfaction")), size = 4, fontface = "bold",
      fill = c("#3d3d3d", "#264653"), colour = "white", label.size = 0, label.padding = unit(0.6, "lines")) +
    annotate("text", x = 2, y = 4, label = "Indirect effects (ACME): trait \u2192 block \u2192 satisfaction", size = 3.2) +
    annotate("text", x = 2, y = 0.05, label = "Direct effect (ADE): difference at the same values of all 4 blocks", size = 3.2) +
    annotate("text", x = 2, y = -0.45, label = "Total effect = 4 ACMEs + ADE.  Net of pay = total effect \u2212 ACME through pay.", size = 3, colour = "grey25") +
    scale_x_continuous(limits = c(-0.8, 4.8)) + scale_y_continuous(limits = c(-0.6, 4.2)) + theme_void()
}

# ---- Figure: indirect effects through each mediator block and the direct effect, by country ----
# Parts stack from zero: positive parts to the right, negative parts to the left; the circle is
# the total effect with its interval. Bottom row: means of the country estimates.
block_cols <- c(Pay = "#e9a23b", `Job demands` = "#c44536", `Job resources` = "#4a9fe0", `Skill underuse` = "#7b5ea7", `Direct effect` = "grey72")
fig_blocks_by_country <- function(parts, tot, panel_levels, cname, flag = character(), xlab) {
  # parts: country, panel, part (names of block_cols), value; tot: country, panel, estimate, se, lower, upper, p
  cb <- split(tot$country, tot$panel); af <- axis_frame(panel_levels, cb, cname, flag)
  mrow <- aggregate(value ~ panel + part, parts, mean); mrow$country <- "Mean"
  rows <- do.call(rbind, lapply(panel_levels, function(p) { s <- summ(tot[tot$panel == p, ]); s$panel <- p; s[s$row == "Mean", ] }))
  P <- rbind(parts[c("country", "panel", "part", "value")], mrow[c("country", "panel", "part", "value")])
  P$part <- factor(P$part, levels = names(block_cols))
  P <- P[order(P$country, P$panel, P$part), ]
  P$end <- ave(ifelse(P$value > 0, P$value, 0), P$country, P$panel, FUN = cumsum) * (P$value > 0) +
           ave(ifelse(P$value < 0, P$value, 0), P$country, P$panel, FUN = cumsum) * (P$value <= 0)
  P$start <- P$end - P$value
  P$key <- key_of(af, P$panel, P$country); P$panel <- factor(P$panel, levels = panel_levels)
  tot$key <- key_of(af, tot$panel, tot$country); tot$sig <- sig3(tot$p); tot$panel <- factor(tot$panel, levels = panel_levels)
  rows$key <- key_of(af, rows$panel, "Mean"); rows$panel <- factor(rows$panel, levels = panel_levels)
  ggplot() + geom_blank(data = af$blank, aes(x, key)) + summary_band() + geom_vline(xintercept = 0, colour = "grey45") +
    geom_segment(data = P, aes(x = start, xend = end, y = key, yend = key, colour = part), linewidth = 2.6) +
    geom_errorbar(data = tot, aes(xmin = lower, xmax = upper, y = key), width = 0, colour = "grey30", linewidth = 0.35, orientation = "y") +
    sig_point(tot, aes(estimate, key, fill = sig)) +
    geom_errorbar(data = rows, aes(xmin = lower, xmax = upper, y = key), width = 0, colour = ink, linewidth = 0.6, orientation = "y") +
    geom_point(data = rows, aes(estimate, key), shape = 18, size = 3.4, colour = ink) +
    scale_colour_manual(values = block_cols, name = NULL, drop = FALSE) +
    scale_sig3(name = "Total effect:") + scale_y_discrete(labels = af$labels) + facet_wrap(~ panel, nrow = 1, scales = af$scales) +
    labs(x = xlab, y = NULL) +
    guides(colour = guide_legend(order = 1, override.aes = list(linewidth = 4)), fill = guide_legend(order = 2)) +
    paper_theme() + theme(axis.text.y = element_text(size = 8.5), legend.box = "vertical", legend.spacing.y = unit(0, "pt"))
}

# ---- Figure: sensitivity of the indirect effects to unobserved confounding (medsens) ----
# a: rho*, the error correlation at which the cumulative indirect effect is zero, by country;
# b: share of the indirect effect that remains at each assumed error correlation (mean of countries).
fig_sensitivity <- function(rho, grid, cname, block_labels) {
  rho$cn <- factor(cname[rho$country], levels = rev(sort(unique(cname[rho$country]))))
  rho$lab <- factor(block_labels[rho$k], levels = block_labels)
  m <- aggregate(rho_star ~ lab, rho, mean)
  a <- ggplot(rho, aes(rho_star, cn)) + geom_vline(xintercept = 0, colour = "grey55") +
    geom_point(shape = 21, fill = "white", colour = ink, size = 1.8) +
    geom_vline(data = m, aes(xintercept = rho_star), linetype = 2, colour = ink) +
    facet_wrap(~ lab, nrow = 1) + labs(x = "ρ*: error correlation at which the ACME is zero", y = NULL, title = "a") +
    paper_theme() + theme(axis.text.y = element_text(size = 8))
  # Panel b: mean ACME across countries at each assumed rho over the mean ACME at rho = 0
  # (a ratio of means, so countries with an ACME near zero do not dominate).
  g <- grid[grid$rho > -1e-9 & grid$rho <= 0.4, ]
  g$rho <- round(g$rho, 2)
  s <- aggregate(acme ~ k + rho, g, mean); s0 <- s[s$rho == 0, c("k", "acme")]; names(s0)[2] <- "acme0"
  s <- merge(s, s0, by = "k"); s$share <- s$acme / s$acme0
  s$lab <- factor(block_labels[s$k], levels = block_labels)
  b <- ggplot(s, aes(rho, 100 * share, colour = lab)) + geom_hline(yintercept = 0, colour = "grey55") + geom_line(linewidth = 0.8) +
    scale_colour_manual(values = c("#e9a23b", "#c44536", "#4a9fe0", "#7b5ea7")[seq_along(block_labels)], name = NULL) +
    coord_cartesian(ylim = c(-100, 100)) +
    labs(x = "Assumed error correlation ρ", y = "ACME remaining (%)", title = "b") + paper_theme()
  a / b + plot_layout(heights = c(1.4, 1))
}

# ---- Figure: heatmap of all trait x condition products (mean of countries) ----
fig_heatmap <- function(H, outline) {
  # H: trait (label), condition (label), estimate (mean), n_sig, n; outline: data frame trait, condition
  lim <- max(abs(H$estimate))
  ggplot(H, aes(condition, trait, fill = estimate)) + geom_tile(colour = "white", linewidth = 0.6) +
    geom_tile(data = outline, aes(condition, trait), inherit.aes = FALSE, fill = NA, colour = ink, linewidth = 0.9) +
    geom_text(aes(label = sprintf("%.3f\n%d / %d", estimate, n_sig, n)), size = 2.6, lineheight = 0.9) +
    scale_fill_gradient2(low = "#4a9fe0", mid = "white", high = "#c44536", limits = c(-lim, lim), name = "Mean product\n(points per SD x SD)") +
    labs(x = NULL, y = NULL) + paper_theme() +
    theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 35, hjust = 1), panel.border = element_blank(), legend.position = "right")
}
