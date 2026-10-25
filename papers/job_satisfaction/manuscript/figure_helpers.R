# Figure helpers for the job satisfaction manuscript (version 4).
# Conventions shared by every figure:
#   * one ink colour for estimates; colour is used only to distinguish parts of an effect;
#   * significance marked three ways from the unadjusted p-value: filled (p < 0.05),
#     grey (p < 0.10), hollow (neither). Adjusted counts are reported in the text and tables;
#   * countries alphabetical, top to bottom, then a gap, then Mean and Median rows with
#     95% intervals (mean: sqrt(sum se^2)/K; median: simulated), as in the wages paper;
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
  c(paste(panel, "Median"), paste(panel, "Mean"), paste(panel, "gap"), paste(panel, ord))
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
    lev <- c("Median", "Mean", "gap", ord)
    lab <- setNames(c("Median", "Mean", "", vapply(ord, lab_of, character(1))), lev)
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
summary_band <- function() annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 2.5, fill = "grey94")

# ---- Figure: coefficients by country, traits as panels (H1 and similar) ----
fig_country_dots <- function(u, panel_levels, cname, flag = character(), xlab) {
  # u: country, panel, estimate, se, lower, upper, p
  cb <- split(u$country, u$panel); af <- axis_frame(panel_levels, cb, cname, flag)
  u$key <- key_of(af, u$panel, u$country); u$sig <- sig3(u$p); u$panel <- factor(u$panel, levels = panel_levels)
  rows <- do.call(rbind, lapply(panel_levels, function(p) { s <- summ(u[u$panel == p, ]); s$panel <- p; s }))
  rows$key <- key_of(af, rows$panel, rows$row); rows$panel <- factor(rows$panel, levels = panel_levels)
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
  rows$key <- key_of(af, rows$panel, rows$row); rows$panel <- factor(rows$panel, levels = panel_levels)
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
  rows$key <- key_of(af, rows$panel, rows$row); rows$panel <- factor(rows$panel, levels = panel_levels)
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
  nodes <- data.frame(x = c(0, 2, 4), y = c(1, 2, 1), lab = c("Trait\nT", "Job condition\nM", "Job satisfaction\nS"))
  ggplot() +
    annotate("segment", x = 0.5, y = 1.25, xend = 1.4, yend = 1.85, arrow = arrow(length = unit(6, "pt"), type = "closed"), colour = "grey35") +
    annotate("segment", x = 2.6, y = 1.85, xend = 3.4, yend = 1.25, arrow = arrow(length = unit(6, "pt"), type = "closed"), colour = "grey35") +
    annotate("segment", x = 0, y = 0.72, xend = 0, yend = 0.58, colour = "grey35") +
    annotate("segment", x = 0, y = 0.58, xend = 4, yend = 0.58, colour = "grey35") +
    annotate("segment", x = 4, y = 0.58, xend = 4, yend = 0.7, arrow = arrow(length = unit(6, "pt"), type = "closed"), colour = "grey35") +
    geom_label(data = nodes, aes(x, y, label = lab), size = 4, fontface = "bold", fill = c("#c44536", "#e9c46a", "#264653"),
      colour = c("white", "black", "white"), label.size = 0, label.padding = unit(0.6, "lines")) +
    annotate("text", x = 0.75, y = 1.75, label = "a: does the trait go with\nmore of the reported condition?", size = 3, hjust = 1, lineheight = 0.9) +
    annotate("text", x = 3.25, y = 1.75, label = "b: does the reported condition\ngo with more satisfaction?", size = 3, hjust = 0, lineheight = 0.9) +
    annotate("text", x = 2, y = 0.42, label = "c': direct part, the association that remains at the same reported condition", size = 3) +
    annotate("text", x = 2, y = 0.1, label = "indirect part = a x b (an effect only under the assumptions in the text)\nM is split into the occupational-group mean and the worker's deviation from it", size = 3, lineheight = 0.95) +
    scale_x_continuous(limits = c(-1.2, 5.2)) + scale_y_continuous(limits = c(-0.1, 2.4)) + theme_void()
}
