# Three figures. PNG at 300 dpi.

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
      panel.grid.major.y = ggplot2::element_line(color = COL$faint, linewidth = 0.3),
      panel.grid.major.x = ggplot2::element_blank(),
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
      plot.margin = ggplot2::margin(14, 22, 12, 12)
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

cancer_labels <- function(ids) {
  factor(
    vapply(ids, function(id) SITES[[id]]$label, character(1)),
    levels = vapply(SITE_ORDER, function(id) SITES[[id]]$label, character(1))
  )
}

write_survival_figure <- function(survival, path) {
  dat <- subset_dims(survival, sex = 1L, race = 1L, age = 1L)
  dat$panel <- cancer_labels(dat$cancer)
  marks <- APPROVALS[APPROVALS$on_figure, , drop = FALSE]
  marks$panel <- cancer_labels(marks$cancer)
  marks$year <- as.integer(format(marks$date, "%Y")) +
    (as.integer(format(marks$date, "%j")) - 1) / 365
  marks$figure_label <- ifelse(marks$drug == "Polatuzumab vedotin", "Polatuzumab", marks$drug)
  cap <- paste(
    "Observed 5-year relative survival. SEER 21 registries excluding Illinois, November 2025 submission.",
    "Vertical lines are FDA approval dates. They are context, not effects of those drugs.",
    "The lymphoma panel is non-Hodgkin lymphoma. Polatuzumab is a DLBCL approval on that broader series.",
    "Rituximab (1997) is before this axis and is not drawn. Source: SEER*Explorer.",
    sep = "\n"
  )
  plot <- ggplot2::ggplot(dat, ggplot2::aes(year, estimate)) +
    ggplot2::geom_vline(
      data = marks,
      ggplot2::aes(xintercept = year),
      color = COL$copper,
      linewidth = 0.4
    ) +
    ggplot2::geom_line(color = COL$navy, linewidth = 0.8) +
    ggplot2::geom_point(color = COL$navy, size = 1.6) +
    ggplot2::geom_text(
      data = marks,
      ggplot2::aes(x = year, y = Inf, label = figure_label),
      inherit.aes = FALSE,
      angle = 90,
      hjust = 1.08,
      vjust = 1.15,
      size = 2.7,
      color = COL$ink,
      family = "Inter"
    ) +
    ggplot2::facet_wrap(~panel, ncol = 1, scales = "free_y") +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2020, by = 4), limits = c(1999.4, 2021.6)) +
    ggplot2::labs(
      title = "5-year relative survival",
      subtitle = "Diagnosis year. Each panel has its own vertical scale. Lines mark FDA approvals.",
      x = "Year of diagnosis",
      y = "5-year relative survival, percent",
      caption = cap
    ) +
    theme_study("none")
  save_fig(path, plot, width = 9.4, height = 11.6)
}

write_incidence_figure <- function(incidence, path) {
  obs <- subset_dims(incidence, slice = "sex", sex = 1L, race = 1L, age = 1L, rate_type = 1L)
  delay <- subset_dims(incidence, slice = "delay", sex = 1L, race = 1L, age = 1L, rate_type = 2L)
  obs$panel <- cancer_labels(obs$cancer)
  delay$panel <- cancer_labels(delay$cancer)
  obs$point <- ifelse(obs$year == COVID_YEAR, "2020, left out of the fit", "Observed")
  cap <- paste(
    "Rates per 100,000, age-adjusted to the 2000 US standard population.",
    "SEER 21 registries, November 2025 submission. Points are observed. The line is delay-adjusted.",
    "The open point is 2020. SEER left that rate out of the joinpoint fit. Source: SEER*Explorer.",
    sep = "\n"
  )
  plot <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = delay,
      ggplot2::aes(year, estimate, color = "Delay-adjusted"),
      linewidth = 0.7
    ) +
    ggplot2::geom_point(
      data = obs,
      ggplot2::aes(year, estimate, shape = point),
      color = COL$navy,
      size = 1.8
    ) +
    ggplot2::facet_wrap(~panel, ncol = 1, scales = "free_y") +
    ggplot2::scale_color_manual(values = c("Delay-adjusted" = COL$copper)) +
    ggplot2::scale_shape_manual(values = c(
      "Observed" = 16,
      "2020, left out of the fit" = 1
    )) +
    ggplot2::guides(
      shape = ggplot2::guide_legend(order = 1, override.aes = list(color = COL$navy)),
      color = ggplot2::guide_legend(order = 2)
    ) +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2023, by = 4)) +
    ggplot2::labs(
      title = "Age-adjusted incidence",
      subtitle = "Observed points and delay-adjusted lines. The open point is 2020.",
      x = "Year of diagnosis",
      y = "Rate per 100,000",
      caption = cap,
      color = NULL,
      shape = NULL
    ) +
    theme_study("bottom")
  save_fig(path, plot, width = 9.4, height = 11.4)
}

write_gap_figure <- function(gap, path) {
  dat <- gap
  dat$panel <- cancer_labels(dat$cancer)
  cap <- paste(
    "White (includes Hispanic) minus Black (includes Hispanic), in percentage points.",
    "These are the only two race groups on the SEER*Explorer survival trend.",
    "Below zero, the Black group had the higher 5-year relative survival.",
    "SEER 21 excluding Illinois, November 2025 submission. Source: SEER*Explorer.",
    sep = "\n"
  )
  plot <- ggplot2::ggplot(dat, ggplot2::aes(year, gap_pp, color = panel)) +
    ggplot2::geom_hline(yintercept = 0, color = COL$muted, linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::scale_color_manual(values = stats::setNames(
      unname(CANCER_COLORS),
      vapply(SITE_ORDER, function(id) SITES[[id]]$label, character(1))
    )) +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2018, by = 3)) +
    ggplot2::labs(
      title = "Change in the 5-year survival gap",
      subtitle = "White minus Black. Both groups include Hispanic ethnicity.",
      x = "Year of diagnosis",
      y = "Gap, percentage points",
      caption = cap,
      color = NULL
    ) +
    theme_study("bottom")
  save_fig(path, plot, width = 9.6, height = 6.8)
}

write_figures <- function(incidence, survival, gap, out_dir) {
  write_survival_figure(survival, file.path(out_dir, "figure-survival-trends.png"))
  write_incidence_figure(incidence, file.path(out_dir, "figure-incidence-trends.png"))
  write_gap_figure(gap, file.path(out_dir, "figure-survival-gap.png"))
}
