# Export publication and machine-readable outputs ----------------------------

save_figure_pair <- function(plot, stem, paths, width, height) {
  ggplot2::ggsave(file.path(paths$figures, paste0(stem, ".png")),
                  plot, width = width, height = height, dpi = 300)
  ggplot2::ggsave(file.path(paths$figures, paste0(stem, ".pdf")),
                  plot, width = width, height = height)
}

export_all_results <- function(
    paths, table1, table2, table2_long, figure1, figure2, figure_s1,
    crude_source, adjusted_source, fixed_effects, followup_counts,
    model_diagnostics) {

  flextable::save_as_docx(
    "Table 1" = gtsummary::as_flex_table(table1),
    path = file.path(paths$tables, "Table_1_Baseline_Characteristics.docx")
  )
  flextable::save_as_docx(
    "Table 2" = table2,
    path = file.path(paths$tables, "Table_2_Change_Contrasts.docx")
  )

  readr::write_csv(gtsummary::as_tibble(table1, col_labels = FALSE),
                   file.path(paths$source_data, "Table_1_Source_Data.csv"))
  readr::write_csv(table2_long,
                   file.path(paths$source_data, "Table_2_Source_Data.csv"))
  readr::write_csv(crude_source,
                   file.path(paths$source_data, "Figure_1_Source_Data.csv"))
  readr::write_csv(adjusted_source,
                   file.path(paths$source_data, "Figure_2_Source_Data.csv"))
  readr::write_csv(followup_counts,
                   file.path(paths$source_data, "Followup_Counts.csv"))
  readr::write_csv(fixed_effects,
                   file.path(paths$model_results, "Mixed_Model_Fixed_Effects.csv"))
  readr::write_csv(model_diagnostics,
                   file.path(paths$model_results, "Model_Diagnostics.csv"))

  openxlsx::write.xlsx(
    list(
      Table_2_numeric = table2_long,
      Followup_counts = followup_counts,
      Model_diagnostics = model_diagnostics
    ),
    file.path(paths$tables, "Analysis_Details.xlsx"),
    overwrite = TRUE
  )

  save_figure_pair(figure1, "Figure_1_Crude_Trajectories", paths, 8.5, 10)
  save_figure_pair(figure2, "Figure_2_Adjusted_Trajectories", paths, 8.5, 10)
  save_figure_pair(figure_s1, "Supplementary_Figure_S1_Coefficients", paths, 14, 18)

  capture.output(utils::sessionInfo(),
                 file = file.path(paths$output_root, "sessionInfo.txt"))
  invisible(TRUE)
}

