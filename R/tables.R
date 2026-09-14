# Publication tables ----------------------------------------------------------

create_table1 <- function(df_baseline) {
  df_baseline |>
    dplyr::select(
      SurgeryTypeName, Sex, Age, BMI0, Weight0, FATP0, PMM0, FFM0,
      SMMI0, ASM0, T2DM_FBS, Hypothyroidism_FBS, HTN_FBS
    ) |>
    gtsummary::tbl_summary(
      by = SurgeryTypeName,
      type = list(
        Sex ~ "categorical", T2DM_FBS ~ "categorical",
        Hypothyroidism_FBS ~ "categorical", HTN_FBS ~ "categorical"
      ),
      statistic = list(
        gtsummary::all_continuous() ~ "{mean} ({sd})",
        gtsummary::all_categorical() ~ "{n} ({p}%)"
      ),
      digits = gtsummary::all_continuous() ~ 1,
      missing = "no",
      label = list(
        Sex ~ "Sex", Age ~ "Age (years)", BMI0 ~ "Baseline BMI (kg/m²)",
        Weight0 ~ "Baseline weight (kg)",
        FATP0 ~ "Baseline body fat percentage (%)",
        PMM0 ~ "Baseline predicted muscle mass (kg)",
        FFM0 ~ "Baseline fat-free mass (kg)",
        SMMI0 ~ "Baseline skeletal muscle mass index (kg/m²)",
        ASM0 ~ "Baseline appendicular skeletal muscle mass (kg)",
        T2DM_FBS ~ "Diabetes", Hypothyroidism_FBS ~ "Hypothyroidism",
        HTN_FBS ~ "Hypertension"
      )
    ) |>
    gtsummary::add_p(
      test = list(
        gtsummary::all_continuous() ~ "aov",
        gtsummary::all_categorical() ~ "chisq.test"
      )
    ) |>
    gtsummary::modify_header(label ~ "**Characteristic**") |>
    gtsummary::modify_caption("**Table 1. Baseline characteristics by procedure**") |>
    gtsummary::bold_labels()
}

create_table2 <- function(table2_long) {
  display <- table2_long |>
    dplyr::mutate(
      Outcome = factor(Outcome, levels = muscle_outcomes,
                       labels = unname(outcome_labels[muscle_outcomes])),
      Display = sprintf("%.2f (%.2f to %.2f)", Estimate, Lower, Upper)
    ) |>
    dplyr::select(Followup, Outcome, Contrast, Display) |>
    tidyr::pivot_wider(names_from = Contrast, values_from = Display) |>
    dplyr::rename(`Follow-up period` = Followup)

  table <- flextable::flextable(display) |>
    flextable::set_caption(
      "Table 2. Adjusted between-procedure differences in changes from preoperative values for muscle-related outcomes"
    ) |>
    flextable::theme_booktabs() |>
    flextable::bold(part = "header") |>
    flextable::autofit()

  for (contrast_name in names(procedure_pairs)) {
    significant <- table2_long |>
      dplyr::filter(Contrast == contrast_name) |>
      dplyr::arrange(Followup, Outcome) |>
      dplyr::pull(Significant)
    table <- flextable::bold(table, i = which(significant), j = contrast_name,
                             bold = TRUE, part = "body")
  }

  note <- paste(
    "Values are adjusted differences in change from the preoperative period",
    "(change in the first procedure minus change in the second procedure), with",
    "Tukey-adjusted 95% confidence intervals. Negative estimates indicate a greater",
    "decline in the first procedure; positive estimates indicate a greater decline",
    "in the second procedure. Bold values have confidence intervals excluding zero."
  )
  flextable::add_footer_lines(table, values = note) |>
    flextable::fontsize(part = "footer", size = 8)
}

