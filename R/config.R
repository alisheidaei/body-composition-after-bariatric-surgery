# Project-wide configuration --------------------------------------------------

required_packages <- c(
  "tidyverse", "haven", "lme4", "emmeans", "broom.mixed",
  "gtsummary", "flextable", "officer", "openxlsx", "patchwork"
)

period_levels <- c("Pre", "0–1m", "1–6m", "6–12m", "12–24m", ">24m")
procedure_levels <- c("SG", "OAGB", "RYGB")
analysis_outcomes <- c("Weight", "BMI", "TWL", "FATP", "PMM", "FFM", "SMMI", "ASM")
muscle_outcomes <- c("PMM", "FFM", "SMMI", "ASM")

outcome_labels <- c(
  Weight = "Weight (kg)",
  BMI = "BMI (kg/m²)",
  TWL = "Total weight loss (%)",
  FATP = "Body fat percentage (%)",
  PMM = "Predicted muscle mass (kg)",
  FFM = "Fat-free mass (kg)",
  SMMI = "Skeletal muscle mass index (kg/m²)",
  ASM = "Appendicular skeletal muscle mass (kg)"
)

procedure_colors <- c(SG = "#D55E00", OAGB = "#009E73", RYGB = "#0072B2")

check_required_packages <- function() {
  missing <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) > 0L) {
    stop("Install these packages before running the analysis: ",
         paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

initialize_paths <- function(project_root) {
  data_setting <- Sys.getenv("BIA_DATA_PATH", unset = "")
  output_setting <- Sys.getenv("BIA_OUTPUT_DIR", unset = "")

  data_file <- if (nzchar(data_setting)) {
    data_setting
  } else {
    file.path(project_root, "data", "Final Clean Data.dta")
  }

  output_root <- if (nzchar(output_setting)) {
    output_setting
  } else {
    file.path(project_root, "outputs")
  }

  paths <- list(
    data_file = data_file,
    output_root = output_root,
    tables = file.path(output_root, "tables"),
    figures = file.path(output_root, "figures"),
    source_data = file.path(output_root, "source_data"),
    model_results = file.path(output_root, "model_results")
  )
  invisible(lapply(paths[names(paths) != "data_file"], dir.create,
                   recursive = TRUE, showWarnings = FALSE))
  paths
}
