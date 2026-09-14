# Publication figures ---------------------------------------------------------

trajectory_theme <- function() {
  ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      legend.position = "top",
      strip.text = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
}

plot_crude_trajectories <- function(source) {
  ggplot2::ggplot(
    source,
    ggplot2::aes(x = period_index, y = mean, color = SurgeryTypeName,
                 fill = SurgeryTypeName, group = SurgeryTypeName)
  ) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                         alpha = 0.14, color = NA) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::facet_wrap(~OutcomeLabel, scales = "free_y", ncol = 2) +
    ggplot2::scale_x_continuous(breaks = seq_along(period_levels), labels = period_levels) +
    ggplot2::scale_color_manual(values = procedure_colors) +
    ggplot2::scale_fill_manual(values = procedure_colors) +
    ggplot2::labs(x = "Follow-up period", y = NULL,
                  color = "Procedure", fill = "Procedure") +
    trajectory_theme()
}

plot_adjusted_trajectories <- function(source) {
  ggplot2::ggplot(
    source,
    ggplot2::aes(x = period_index, y = emmean, color = SurgeryTypeName,
                 fill = SurgeryTypeName, group = SurgeryTypeName)
  ) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                         alpha = 0.14, color = NA) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::facet_wrap(~OutcomeLabel, scales = "free_y", ncol = 2) +
    ggplot2::scale_x_continuous(breaks = seq_along(period_levels), labels = period_levels) +
    ggplot2::scale_color_manual(values = procedure_colors) +
    ggplot2::scale_fill_manual(values = procedure_colors) +
    ggplot2::labs(x = "Follow-up period", y = "Adjusted mean",
                  color = "Procedure", fill = "Procedure") +
    trajectory_theme()
}

clean_term_label <- function(term) {
  term |>
    stringr::str_replace_all("followup_period", "") |>
    stringr::str_replace_all("SurgeryTypeNameOAGB", "OAGB") |>
    stringr::str_replace_all("SurgeryTypeNameRYGB", "RYGB") |>
    stringr::str_replace_all("SexMale", "Male sex") |>
    stringr::str_replace_all("T2DM_FBSYes", "Diabetes: yes") |>
    stringr::str_replace_all("HTN_FBSYes", "Hypertension: yes") |>
    stringr::str_replace_all("Hypothyroidism_FBSYes", "Hypothyroidism: yes") |>
    stringr::str_replace_all(":", " × ")
}

plot_one_coefficient_panel <- function(fixed_effects, outcome) {
  plot_data <- fixed_effects |>
    dplyr::filter(Outcome == outcome, term != "(Intercept)") |>
    dplyr::mutate(
      term_clean = clean_term_label(term),
      term_clean = factor(term_clean, levels = rev(term_clean))
    )
  ggplot2::ggplot(plot_data, ggplot2::aes(x = estimate, y = term_clean)) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = conf.low, xmax = conf.high),
                            height = 0.15) +
    ggplot2::geom_point(size = 1.4) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#B2182B") +
    ggplot2::labs(title = outcome_labels[[outcome]], x = "Coefficient (95% CI)", y = NULL) +
    ggplot2::theme_bw(base_size = 8) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   panel.grid.minor = ggplot2::element_blank())
}

plot_coefficient_panel <- function(fixed_effects) {
  plots <- stats::setNames(
    lapply(analysis_outcomes, function(x) plot_one_coefficient_panel(fixed_effects, x)),
    analysis_outcomes
  )
  (plots$Weight | plots$BMI) /
    (plots$TWL | plots$FATP) /
    (plots$PMM | plots$FFM) /
    (plots$SMMI | plots$ASM)
}

