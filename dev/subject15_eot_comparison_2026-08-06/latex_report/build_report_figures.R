#!/usr/bin/env Rscript

args <- commandArgs(FALSE)
file.arg <- args[grepl("^--file=", args)]
script.path <- normalizePath(
  sub("^--file=", "", file.arg[[1L]]),
  mustWork = TRUE
)
report.dir <- dirname(script.path)
analysis.dir <- normalizePath(file.path(report.dir, ".."), mustWork = TRUE)
figure.dir <- file.path(report.dir, "figures")
generated.dir <- file.path(report.dir, "generated")
dir.create(figure.dir, recursive = TRUE, showWarnings = FALSE)
dir.create(generated.dir, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(ggplot2)
  library(gridExtra)
  library(scales)
})

comparison <- read.csv(
  file.path(analysis.dir, "subject15_maximum_basin_eot_comparison.csv"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
visits <- read.csv(
  file.path(analysis.dir, "subject15_visit_basin_assignments.csv"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
sensitivity <- read.csv(
  file.path(analysis.dir, "subject15_eot_visit_threshold_sensitivity.csv"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
m17.flow <- read.csv(
  file.path(analysis.dir, "subject15_m17_local_flow.csv"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
summary <- readRDS(
  file.path(analysis.dir, "subject15_eot_comparison_summary.rds")
)

observed <- comparison[comparison$has_direct_subject_visit, , drop = FALSE]
observed$basin_label <- factor(
  observed$basin_label,
  levels = paste0("M", seq_len(17L))
)
visits$basin_label <- factor(
  visits$basin_label,
  levels = rev(paste0("M", seq_len(17L)))
)

base.theme <- theme_bw(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(7, 10, 7, 7)
  )

save_grid <- function(filename, grob, width = 7.25, height = 4.7) {
  cairo_pdf_14 <- function(filename, width, height, ...) {
    grDevices::cairo_pdf(
      filename = filename,
      width = width,
      height = height,
      onefile = TRUE
    )
  }
  ggsave(
    file.path(figure.dir, filename),
    plot = grob,
    width = width,
    height = height,
    units = "in",
    device = cairo_pdf_14
  )
}

# Figure 1: the occupation-density mass and direct visit fractions agree closely.
observed$label_x <- observed$subject_visit_share
observed$label_y <- observed$density_mass
mass.label.positions <- data.frame(
  basin_label = paste0("M", seq_len(17L)),
  label_x = c(
    0.270, 0.164, 0.112,
    0.062, 0.071, 0.082,
    0.043,
    0.022, 0.024, 0.031, 0.036, 0.041,
    0.0115, 0.0125, 0.0140, 0.0160, 0.0190
  ),
  label_y = c(
    0.284, 0.145, 0.125,
    0.076, 0.083, 0.064,
    0.047,
    0.0350, 0.0315, 0.0275, 0.0230, 0.0190,
    0.0208, 0.0165, 0.0140, 0.0115, 0.0096
  ),
  stringsAsFactors = FALSE
)
observed$label_x <- mass.label.positions$label_x[
  match(as.character(observed$basin_label), mass.label.positions$basin_label)
]
observed$label_y <- mass.label.positions$label_y[
  match(as.character(observed$basin_label), mass.label.positions$basin_label)
]

p.mass.scatter <- ggplot(
  observed,
  aes(x = subject_visit_share, y = density_mass)
) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linewidth = 0.45,
    linetype = "dashed",
    color = "grey45"
  ) +
  geom_point(
    aes(size = graph_support_vertices, fill = basin_label == "M17"),
    shape = 21,
    color = "black",
    stroke = 0.35,
    alpha = 0.9
  ) +
  geom_point(
    data = observed[observed$basin_label == "M17", , drop = FALSE],
    shape = 21,
    size = 2.8,
    fill = "#E45756",
    color = "black",
    stroke = 0.4,
    inherit.aes = TRUE,
    show.legend = FALSE
  ) +
  geom_segment(
    aes(
      xend = label_x,
      yend = label_y
    ),
    linewidth = 0.25,
    color = "grey50",
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = label_x, y = label_y, label = basin_label),
    size = 2.7,
    check_overlap = FALSE
  ) +
  scale_x_log10(labels = label_percent(accuracy = 0.1)) +
  scale_y_log10(labels = label_percent(accuracy = 0.1)) +
  scale_fill_manual(
    values = c(`FALSE` = "#4C78A8", `TRUE` = "#E45756"),
    guide = "none"
  ) +
  scale_size_area(
    max_size = 6,
    breaks = c(1, 100, 300, 700, 1000),
    name = "Graph support"
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "A. Density mass tracks direct visit frequency",
    subtitle = "Dashed line is exact agreement; both axes are logarithmic.",
    x = "Direct visit share",
    y = "Occupation-density mass"
  ) +
  base.theme

observed$mass_visit_difference_pp <- 100 * (
  observed$density_mass - observed$subject_visit_share
)
p.mass.diff <- ggplot(
  observed,
  aes(
    x = factor(basin_label, levels = rev(levels(basin_label))),
    y = mass_visit_difference_pp,
    fill = mass_visit_difference_pp >= 0
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.45, color = "grey35") +
  geom_col(width = 0.68) +
  coord_flip() +
  scale_fill_manual(
    values = c(`FALSE` = "#72B7B2", `TRUE` = "#F58518"),
    guide = "none"
  ) +
  labs(
    title = "B. Basin-level discrepancies are small",
    subtitle = "Positive values mean density mass exceeds direct visit share.",
    x = NULL,
    y = "Density mass minus visit share (percentage points)"
  ) +
  base.theme

save_grid(
  "density_mass_vs_direct_visits.pdf",
  arrangeGrob(p.mass.scatter, p.mass.diff, ncol = 1),
  height = 7.8
)

# Figure 2: graph support describes the reconstructed ambient field, not the
# number of observed subject trajectory vertices.
observed$support_label_x <- observed$graph_support_vertices
observed$support_label_y <- observed$subject_visit_count
support.label.positions <- data.frame(
  basin_label = paste0("M", seq_len(17L)),
  x.factor = c(
    1.03, 1.02, 1.00, 1.03, 1.00, 1.00, 1.00, 1.00, 1.02,
    1.00, 1.00, 1.00, 0.88, 1.00, 0.84, 1.14, 1.00
  ),
  y = c(
    20.0, 9.5, 8.7, 4.7, 5.6, 4.6, 3.35, 1.72, 2.20,
    1.72, 2.20, 1.72, 1.22, 0.84, 0.82, 0.72, 1.28
  ),
  stringsAsFactors = FALSE
)
position.index <- match(
  as.character(observed$basin_label),
  support.label.positions$basin_label
)
observed$support_label_x <- observed$graph_support_vertices *
  support.label.positions$x.factor[position.index]
observed$support_label_y <- support.label.positions$y[position.index]
p.support <- ggplot(
  observed,
  aes(x = graph_support_vertices, y = subject_visit_count)
) +
  geom_point(
    aes(
      size = density_mass,
      fill = basin_label == "M17"
    ),
    shape = 21,
    color = "black",
    stroke = 0.35,
    alpha = 0.9
  ) +
  geom_segment(
    aes(
      xend = support_label_x,
      yend = support_label_y
    ),
    linewidth = 0.25,
    color = "grey50",
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      x = support_label_x,
      y = support_label_y,
      label = basin_label
    ),
    size = 3,
    check_overlap = FALSE
  ) +
  scale_x_log10(
    breaks = c(1, 3, 10, 30, 100, 300, 1000),
    labels = label_number()
  ) +
  scale_y_log10(
    breaks = c(1, 2, 3, 5, 10, 20),
    labels = label_number()
  ) +
  scale_fill_manual(
    values = c(`FALSE` = "#59A14F", `TRUE` = "#E45756"),
    guide = "none"
  ) +
  scale_size_area(
    max_size = 8,
    labels = label_percent(accuracy = 0.1),
    name = "Density mass"
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Graph support and direct trajectory support are different quantities",
    subtitle = paste0(
      "M17 is a one-vertex graph basin and one observed visit; Spearman ",
      "\u03c1 = ",
      sprintf("%.3f", summary$spearman_graph_support_vs_visit_count),
      " among M1\u2013M17."
    ),
    x = "Graph support: vertices assigned by CLOSEST ascent",
    y = "Direct Subject 15 visits"
  ) +
  base.theme
save_grid("graph_support_vs_direct_visits.pdf", p.support, height = 4.8)

# Figure 3: visit-count thresholds show the cost of an empirical support rule.
p.threshold.count <- ggplot(
  sensitivity,
  aes(x = minimum_direct_visits, y = eligible_basin_count)
) +
  geom_line(linewidth = 0.75, color = "#4C78A8") +
  geom_point(size = 2.2, color = "#4C78A8") +
  geom_text(
    aes(label = eligible_basin_count),
    nudge_y = 0.7,
    size = 3
  ) +
  scale_x_continuous(breaks = sensitivity$minimum_direct_visits) +
  scale_y_continuous(
    breaks = seq(0, 18, by = 3),
    limits = c(0, 18.5)
  ) +
  labs(
    title = "A. Number of retained candidate basins",
    x = "Minimum direct visits required",
    y = "Eligible basins"
  ) +
  base.theme

coverage <- rbind(
  data.frame(
    minimum_direct_visits = sensitivity$minimum_direct_visits,
    measure = "Direct visits",
    share = sensitivity$visit_share_covered
  ),
  data.frame(
    minimum_direct_visits = sensitivity$minimum_direct_visits,
    measure = "Midpoint exposure",
    share = sensitivity$midpoint_time_share_covered
  ),
  data.frame(
    minimum_direct_visits = sensitivity$minimum_direct_visits,
    measure = "Density mass",
    share = sensitivity$density_mass_covered
  )
)
p.threshold.coverage <- ggplot(
  coverage,
  aes(
    x = minimum_direct_visits,
    y = share,
    color = measure,
    shape = measure
  )
) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = sensitivity$minimum_direct_visits) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0.35, 1.02),
    breaks = seq(0.4, 1, by = 0.1)
  ) +
  scale_color_manual(
    values = c(
      "Direct visits" = "#4C78A8",
      "Midpoint exposure" = "#F58518",
      "Density mass" = "#54A24B"
    )
  ) +
  labs(
    title = "B. Coverage retained under each threshold",
    x = "Minimum direct visits required",
    y = "Share retained",
    color = NULL,
    shape = NULL
  ) +
  base.theme
save_grid(
  "visit_threshold_sensitivity.pdf",
  arrangeGrob(p.threshold.count, p.threshold.coverage, ncol = 2),
  height = 4.55
)

# Figure 4: temporal order distinguishes a repeatedly occupied basin from a
# singleton or isolated visit.
p.sequence <- ggplot(
  visits,
  aes(x = time_idx, y = basin_label, group = 1)
) +
  geom_line(
    color = "grey70",
    linewidth = 0.35
  ) +
  geom_point(
    aes(fill = state, shape = cst_norm),
    size = 2.6,
    color = "black",
    stroke = 0.25
  ) +
  scale_fill_manual(
    values = c("S1" = "#4C78A8", "S3" = "#E45756")
  ) +
  scale_shape_manual(
    values = c("III" = 21, "IV-B" = 24)
  ) +
  guides(
    fill = guide_legend(
      order = 1,
      override.aes = list(shape = 21, size = 3)
    ),
    shape = guide_legend(
      order = 2,
      override.aes = list(fill = "white", size = 3)
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, 72, by = 6),
    minor_breaks = seq(0, 72, by = 1),
    limits = c(0.5, 72.5)
  ) +
  labs(
    title = "Observed Subject 15 basin sequence",
    subtitle = paste0(
      "Seventy observed visits over time indices 1\u201372; missing indices 43 ",
      "and 65 create visible gaps."
    ),
    x = "Study time index (days)",
    y = "Maximum basin (density-mass label)",
    fill = "State",
    shape = "CST"
  ) +
  base.theme +
  theme(
    panel.grid.minor.x = element_line(
      linewidth = 0.2,
      color = "grey92"
    ),
    panel.grid.major.y = element_line(
      linewidth = 0.3,
      color = "grey85"
    )
  )
save_grid("observed_basin_sequence.pdf", p.sequence, height = 5.2)

tex_escape <- function(value) {
  value <- gsub("\\\\", "\\\\textbackslash{}", as.character(value))
  value <- gsub("([%&#_$])", "\\\\\\1", value, perl = TRUE)
  value
}

format_num <- function(x, digits = 3) {
  formatC(x, digits = digits, format = "f")
}

top17.rows <- vapply(
  seq_len(nrow(observed)),
  function(i) {
    row <- observed[i, , drop = FALSE]
    paste0(
      as.character(row$basin_label),
      " & ", row$extremum_vertex,
      " & ", row$graph_support_vertices,
      " & ", row$subject_visit_count,
      " & ", row$observed_run_count,
      " & ", row$max_observed_run_length,
      " & ", format_num(100 * row$density_mass, 2),
      " & ", format_num(row$midpoint_time_weight_days, 1),
      " \\\\"
    )
  },
  character(1)
)
writeLines(top17.rows, file.path(generated.dir, "top17_rows.tex"))

threshold.rows <- vapply(
  seq_len(nrow(sensitivity)),
  function(i) {
    row <- sensitivity[i, , drop = FALSE]
    paste0(
      row$minimum_direct_visits,
      " & ", row$eligible_basin_count,
      " & ", row$visits_covered,
      " & ", format_num(100 * row$visit_share_covered, 1),
      " & ", format_num(row$midpoint_days_covered, 1),
      " & ", format_num(100 * row$midpoint_time_share_covered, 1),
      " & ", format_num(100 * row$density_mass_covered, 1),
      " \\\\"
    )
  },
  character(1)
)
writeLines(
  c(
    "\\begin{tabular}{",
    "  @{}S[table-format=2.0]",
    "  S[table-format=2.0]",
    "  S[table-format=2.0]",
    "  S[table-format=3.1]",
    "  S[table-format=2.1]",
    "  S[table-format=3.1]",
    "  S[table-format=3.1]@{}",
    "}",
    "\\toprule",
    "{Min. visits} & {Basins} & {Visits} & {Visit coverage} &",
    "{Days} & {Time coverage} & {Mass coverage} \\\\",
    "\\midrule",
    threshold.rows,
    "\\bottomrule",
    "\\end{tabular}"
  ),
  file.path(generated.dir, "threshold_table.tex")
)

m17.rows <- vapply(
  seq_len(nrow(m17.flow)),
  function(i) {
    row <- m17.flow[i, , drop = FALSE]
    paste0(
      row$source_vertex,
      " & ", format_num(row$source_field, 6),
      " & ", row$selected_next_vertex,
      " & ", format_num(row$selected_edge_length, 5),
      " & ", format_num(row$edge_length_to_m17, 5),
      " & ", tex_escape(row$selected_root_basin),
      " \\\\"
    )
  },
  character(1)
)
writeLines(
  c(
    "\\begin{tabular}{",
    "  @{}S[table-format=4.0]",
    "  S[table-format=1.6]",
    "  S[table-format=4.0]",
    "  S[table-format=1.5]",
    "  S[table-format=1.5]",
    "  l@{}",
    "}",
    "\\toprule",
    "{Source} & {Field} & {Chosen successor} & {Chosen length} &",
    "{M17 length} & {Final root} \\\\",
    "\\midrule",
    m17.rows,
    "\\bottomrule",
    "\\end{tabular}"
  ),
  file.path(generated.dir, "m17_neighbor_table.tex")
)

commands <- c(
  sprintf(
    "\\newcommand{\\MassVisitSpearman}{%.3f}",
    summary$spearman_mass_vs_visit_count
  ),
  sprintf(
    "\\newcommand{\\SupportVisitSpearman}{%.3f}",
    summary$spearman_graph_support_vs_visit_count
  ),
  sprintf(
    "\\newcommand{\\MassVisitTV}{%.2f\\%%}",
    100 * summary$density_mass_visit_share_total_variation
  ),
  "\\newcommand{\\TopSeventeenMass}{more than 99.9999999999\\%}",
  sprintf(
    "\\newcommand{\\DirectlyVisitedBasinCount}{%d}",
    summary$directly_visited_basin_count
  ),
  sprintf(
    "\\newcommand{\\MaximumBasinCount}{%d}",
    summary$maximum_basin_count
  ),
  sprintf(
    "\\newcommand{\\SubjectVisitCount}{%d}",
    summary$subject_visit_count
  ),
  sprintf(
    "\\newcommand{\\ObservedSpanDays}{%.0f}",
    summary$observed_time_span_days
  )
)
writeLines(commands, file.path(generated.dir, "summary_commands.tex"))

cat(
  sprintf(
    "Wrote 4 vector figures and 4 generated TeX fragments under %s\n",
    report.dir
  )
)
