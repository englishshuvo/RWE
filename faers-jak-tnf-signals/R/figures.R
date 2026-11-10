# Four figures, one theme. PNG at 300 dpi.

theme_study <- function(legend = "bottom") {
  ggplot2::theme_minimal(base_size = 13, base_family = "Inter") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", size = 15, color = COL$ink,
        margin = ggplot2::margin(b = 3)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 10.5, color = COL$muted, margin = ggplot2::margin(b = 12)
      ),
      plot.caption = ggplot2::element_text(
        size = 8.2, color = COL$muted, hjust = 0, lineheight = 1.15,
        margin = ggplot2::margin(t = 10)
      ),
      axis.title = ggplot2::element_text(color = COL$ink, size = 11),
      axis.text = ggplot2::element_text(color = COL$ink, size = 10.5),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = COL$faint, linewidth = 0.3),
      strip.background = ggplot2::element_rect(fill = COL$tile, color = NA),
      strip.text = ggplot2::element_text(
        face = "bold", color = COL$ink, size = 11,
        margin = ggplot2::margin(6, 8, 6, 8)
      ),
      legend.position = legend,
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(color = COL$ink, size = 10),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(14, 16, 10, 12)
    )
}

save_fig <- function(path, plot, width, height) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename = path,
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white",
    device = grDevices::png,
    type = "cairo",
    create.dir = TRUE
  )
  invisible(path)
}

forest_frame <- function(contrasts) {
  dat <- contrasts[contrasts$period == "full" & contrasts$estimable, , drop = FALSE]
  dat$drug <- factor(dat$comparison_label, levels = DRUG_ORDER)
  dat$outcome_label <- factor(dat$outcome_label, levels = c("Venous thromboembolism", "Serious infection"))
  dat$rule <- ifelse(dat$signal, "Meets the signal rule", "Does not meet the signal rule")
  dat$row_label <- sprintf(
    "%s (%s to %s)   n = %s",
    dat$ror_fmt, dat$ror_low_fmt, dat$ror_high_fmt, fmt_int(dat$a)
  )
  dat
}

zoom_limits <- function(lo, hi) {
  lo <- lo[is.finite(lo)]
  hi <- hi[is.finite(hi)]
  zoom_lo <- min(0.5, min(lo) * 0.9)
  zoom_lo <- max(0.2, zoom_lo)
  zoom_hi <- max(2, max(hi) * 1.08)
  clipped <- FALSE
  if (min(lo) < 0.2 || max(hi) > 8) {
    zoom_lo <- 0.25
    zoom_hi <- 8
    clipped <- TRUE
  }
  list(lo = zoom_lo, hi = zoom_hi, clipped = clipped)
}

write_forest <- function(contrasts, path) {
  dat <- forest_frame(contrasts)
  zoom <- zoom_limits(dat$ror_low, dat$ror_high)
  dat$plot_lo <- pmax(dat$ror_low, zoom$lo)
  dat$plot_hi <- pmin(dat$ror_high, zoom$hi)
  breaks <- c(0.25, 0.5, 1, 2, 4, 8)
  breaks <- breaks[breaks >= zoom$lo * 0.99 & breaks <= zoom$hi * 1.01]
  clip_note <- if (zoom$clipped) {
    "\nBars that meet the edge of the axis continue outside the plotted range. The number beside each row is the full interval."
  } else {
    ""
  }
  p <- ggplot2::ggplot(dat, ggplot2::aes(y = drug)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.4, color = "#a8a29e") +
    ggplot2::geom_segment(
      ggplot2::aes(x = plot_lo, xend = plot_hi, yend = drug),
      linewidth = 0.75, color = COL$navy, lineend = "round"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = pmin(pmax(ror, zoom$lo), zoom$hi), shape = rule),
      size = 2.8, color = COL$navy, fill = "white", stroke = 0.8
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = zoom$hi, label = row_label),
      hjust = -0.06, size = 3.15, color = COL$ink, family = "Inter"
    ) +
    ggplot2::scale_x_log10(breaks = breaks, minor_breaks = NULL) +
    ggplot2::coord_cartesian(xlim = c(zoom$lo, zoom$hi), clip = "off") +
    ggplot2::scale_shape_manual(
      values = c("Meets the signal rule" = 16, "Does not meet the signal rule" = 21),
      drop = FALSE
    ) +
    ggplot2::facet_wrap(~outcome_label, ncol = 1) +
    ggplot2::labs(
      title = "Reporting odds ratios, JAK inhibitors versus TNF inhibitors",
      subtitle = "FAERS reports received 1 January 2019 to 30 September 2026, immunology indications",
      x = "Reporting odds ratio (log scale)",
      y = NULL,
      caption = paste0(
        "n is the number of JAK reports with the outcome, after reports that also name a TNF inhibitor are removed.\n",
        "A filled point means the lower 95% confidence limit is above 1 and n is at least 3. The dashed line is 1.",
        clip_note
      )
    ) +
    theme_study() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(14, 196, 10, 12),
      panel.spacing.y = ggplot2::unit(14, "pt")
    )
  save_fig(path, p, width = 11.2, height = 7.4)
}

write_quarterly <- function(quarterly, path) {
  long <- rbind(
    data.frame(
      mid = quarterly$mid,
      reports = quarterly$jak_only_vte,
      group = "JAK inhibitors",
      stringsAsFactors = FALSE
    ),
    data.frame(
      mid = quarterly$mid,
      reports = quarterly$tnf_only_vte,
      group = "TNF inhibitors",
      stringsAsFactors = FALSE
    )
  )
  long$group <- factor(long$group, levels = c("JAK inhibitors", "TNF inhibitors"))
  y_top <- max(long$reports)
  p <- ggplot2::ggplot(long, ggplot2::aes(x = mid, y = reports, color = group, linetype = group)) +
    ggplot2::geom_vline(xintercept = as.Date("2021-09-01"), linewidth = 0.45, color = COL$crimson, linetype = "solid") +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.7) +
    ggplot2::annotate(
      "text",
      x = as.Date("2021-09-01"),
      y = y_top,
      label = "Boxed warning\n1 September 2021",
      hjust = -0.06,
      vjust = 1,
      size = 3.15,
      color = COL$crimson,
      family = "Inter",
      lineheight = 0.95
    ) +
    ggplot2::scale_color_manual(values = c("JAK inhibitors" = COL$navy, "TNF inhibitors" = COL$copper)) +
    ggplot2::scale_linetype_manual(values = c("JAK inhibitors" = "solid", "TNF inhibitors" = "dashed")) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
    ggplot2::scale_y_continuous(labels = function(x) fmt_int(x), expand = ggplot2::expansion(mult = c(0.02, 0.12))) +
    ggplot2::labs(
      title = "VTE reports each quarter",
      subtitle = "JAK and TNF reports with an immunology indication, 2019 Q1 through 2026 Q3",
      x = "Quarter (plotted at the middle month)",
      y = "Reports with a VTE term",
      caption = paste(
        "A report that names both a JAK inhibitor and a TNF inhibitor is left out of both lines.",
        "The VTE terms are deep vein thrombosis, pulmonary embolism, venous thrombosis, thrombosis, and embolism venous.",
        "The term thrombosis is not specific to veins.",
        sep = "\n"
      )
    ) +
    theme_study() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = COL$faint, linewidth = 0.3))
  save_fig(path, p, width = 10.6, height = 6.1)
}

write_prepost <- function(contrasts, path) {
  sub <- contrasts[contrasts$period %in% c("pre", "post") & contrasts$estimable, , drop = FALSE]
  wide_rows <- lapply(split(sub, interaction(sub$comparison, sub$outcome, drop = TRUE)), function(chunk) {
    pre <- chunk[chunk$period == "pre", ]
    post <- chunk[chunk$period == "post", ]
    if (!nrow(pre) || !nrow(post)) return(NULL)
    data.frame(
      drug = pre$comparison_label,
      outcome_label = pre$outcome_label,
      ror_pre = pre$ror,
      ror_post = post$ror,
      stringsAsFactors = FALSE
    )
  })
  wide_rows <- wide_rows[!vapply(wide_rows, is.null, logical(1))]
  wide <- do.call(rbind, wide_rows)
  wide$drug <- factor(wide$drug, levels = DRUG_ORDER)
  wide$outcome_label <- factor(wide$outcome_label, levels = c("Venous thromboembolism", "Serious infection"))
  zoom <- zoom_limits(c(wide$ror_pre, wide$ror_post), c(wide$ror_pre, wide$ror_post))
  long <- rbind(
    data.frame(drug = wide$drug, outcome_label = wide$outcome_label, ror = wide$ror_pre, when = "Before 1 September 2021", stringsAsFactors = FALSE),
    data.frame(drug = wide$drug, outcome_label = wide$outcome_label, ror = wide$ror_post, when = "On or after 1 September 2021", stringsAsFactors = FALSE)
  )
  long$when <- factor(long$when, levels = c("Before 1 September 2021", "On or after 1 September 2021"))
  long$plot_ror <- pmin(pmax(long$ror, zoom$lo), zoom$hi)
  wide$x_pre <- pmin(pmax(wide$ror_pre, zoom$lo), zoom$hi)
  wide$x_post <- pmin(pmax(wide$ror_post, zoom$lo), zoom$hi)
  breaks <- c(0.25, 0.5, 1, 2, 4, 8)
  breaks <- breaks[breaks >= zoom$lo * 0.99 & breaks <= zoom$hi * 1.01]
  clip_note <- if (zoom$clipped) " Points at the edge of the axis sit outside the plotted range." else ""
  p <- ggplot2::ggplot(wide, ggplot2::aes(y = drug)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.4, color = "#a8a29e") +
    ggplot2::geom_segment(
      ggplot2::aes(x = x_pre, xend = x_post, yend = drug),
      linewidth = 1.15, color = "#d6d3d1", lineend = "round"
    ) +
    ggplot2::geom_point(
      data = long,
      ggplot2::aes(x = plot_ror, fill = when, color = when),
      size = 3.1, shape = 21, stroke = 0.7
    ) +
    ggplot2::scale_x_log10(breaks = breaks, minor_breaks = NULL) +
    ggplot2::coord_cartesian(xlim = c(zoom$lo, zoom$hi), clip = "on") +
    ggplot2::scale_fill_manual(values = c("Before 1 September 2021" = "white", "On or after 1 September 2021" = COL$crimson)) +
    ggplot2::scale_color_manual(values = c("Before 1 September 2021" = COL$navy, "On or after 1 September 2021" = COL$crimson)) +
    ggplot2::facet_wrap(~outcome_label, ncol = 1) +
    ggplot2::labs(
      title = "Reporting odds ratios before and after the boxed warning",
      subtitle = "Same JAK versus TNF contrasts, split on reports received 1 September 2021",
      x = "Reporting odds ratio (log scale)",
      y = NULL,
      caption = paste0(
        "Open circles are reports received before the warning. Filled circles are reports received on or after 1 September 2021.",
        "\nThe line joins the two point estimates. It is not a confidence interval.",
        clip_note
      )
    ) +
    theme_study()
  save_fig(path, p, width = 10.4, height = 7.2)
}

write_heatmap <- function(contrasts, path) {
  dat <- contrasts[contrasts$period == "full" & contrasts$estimable, , drop = FALSE]
  dat$drug <- factor(dat$comparison_label, levels = DRUG_ORDER)
  dat$outcome_label <- factor(dat$outcome_label, levels = c("Venous thromboembolism", "Serious infection"))
  dat$log2_ror <- log2(dat$ror)
  dat$star <- ifelse(dat$signal, "*", "")
  dat$corr <- ifelse(dat$continuity_correction, " corr.", "")
  dat$label <- sprintf("%s%s%s\n%s to %s", dat$ror_fmt, dat$star, dat$corr, dat$ror_low_fmt, dat$ror_high_fmt)
  dat$text_col <- ifelse(abs(dat$log2_ror) >= 0.85, "white", COL$ink)
  cap <- paste(
    "Each tile is the reporting odds ratio against all TNF inhibitors.",
    "* lower 95% confidence limit above 1, with at least 3 JAK reports.",
    sep = "\n"
  )
  if (any(dat$continuity_correction)) {
    cap <- paste(cap, "A tile marked corr. added 0.5 to every cell because one observed cell was zero.")
  }
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = outcome_label, y = drug, fill = log2_ror)) +
    ggplot2::geom_tile(color = "white", linewidth = 1.6, width = 0.92, height = 0.86) +
    ggplot2::geom_text(
      ggplot2::aes(label = label, color = text_col),
      size = 3.8, lineheight = 0.95, family = "Inter"
    ) +
    ggplot2::scale_x_discrete(labels = c("VTE terms", "Serious infection")) +
    ggplot2::scale_fill_gradient2(
      low = COL$navy, mid = "#F7F4EF", high = COL$copper,
      midpoint = 0, name = "log2 ROR",
      breaks = c(-1, 0, 1),
      labels = c("0.5", "1", "2")
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::labs(
      title = "Reporting odds ratio by drug and outcome",
      subtitle = "JAK inhibitor versus all TNF inhibitors, 2019 Q1 through 2026 Q3",
      x = NULL,
      y = NULL,
      caption = cap
    ) +
    theme_study(legend = "right") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(face = "bold", size = 12, margin = ggplot2::margin(t = 8)),
      legend.key.height = ggplot2::unit(22, "pt"),
      plot.margin = ggplot2::margin(14, 18, 16, 12)
    )
  save_fig(path, p, width = 9.4, height = 6.2)
}

write_figures <- function(contrasts, quarterly, out_dir) {
  write_forest(contrasts, file.path(out_dir, "figure-forest-ror.png"))
  write_quarterly(quarterly, file.path(out_dir, "figure-vte-quarterly.png"))
  write_prepost(contrasts, file.path(out_dir, "figure-pre-post-ror.png"))
  write_heatmap(contrasts, file.path(out_dir, "figure-heatmap-ror.png"))
}
