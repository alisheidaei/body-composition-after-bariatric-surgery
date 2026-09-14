# Main entry point ------------------------------------------------------------
# This file may be sourced from any working directory.

rm(list = ls())
options(stringsAsFactors = FALSE, contrasts = c("contr.treatment", "contr.poly"))

detect_project_root <- function() {
  source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(source_file) && nzchar(source_file)) {
    return(dirname(normalizePath(source_file, winslash = "/", mustWork = TRUE)))
  }

  file_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_argument) == 1L) {
    script_file <- sub("^--file=", "", file_argument)
    return(dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE)))
  }

  if (file.exists("run_analysis.R")) {
    return(normalizePath(".", winslash = "/", mustWork = TRUE))
  }

  stop("Could not determine the project directory. Open BIA-analysis.Rproj and try again.")
}

project_root <- detect_project_root()

module_relative_paths <- c(
  "R/config.R",
  "R/data_preparation.R",
  "R/modeling.R",
  "R/contrasts.R",
  "R/tables.R",
  "R/figures.R",
  "R/exports.R"
)

module_files <- file.path(project_root, module_relative_paths)

missing_modules <- module_files[!file.exists(module_files)]
if (length(missing_modules) > 0L) {
  stop("The project package is incomplete. Missing: ",
       paste(missing_modules, collapse = ", "))
}

invisible(lapply(module_files, source))
check_required_packages()
emmeans::emm_options(lmer.df = "asymptotic")

paths <- initialize_paths(project_root)
df <- read_and_prepare_data(paths$data_file)
followup_counts <- summarize_followup(df)
df_baseline <- select_patient_baseline(df)

table1 <- create_table1(df_baseline)
crude_source <- summarize_crude_trajectories(df)

models <- fit_all_models(df)
model_diagnostics <- diagnose_models(models)
adjusted_source <- obtain_adjusted_trajectories(models)
table2_long <- obtain_all_change_contrasts(models)
fixed_effects <- obtain_fixed_effects(models)

figure1 <- plot_crude_trajectories(crude_source)
figure2 <- plot_adjusted_trajectories(adjusted_source)
figure_s1 <- plot_coefficient_panel(fixed_effects)
table2 <- create_table2(table2_long)

export_all_results(
  paths = paths,
  table1 = table1,
  table2 = table2,
  table2_long = table2_long,
  figure1 = figure1,
  figure2 = figure2,
  figure_s1 = figure_s1,
  crude_source = crude_source,
  adjusted_source = adjusted_source,
  fixed_effects = fixed_effects,
  followup_counts = followup_counts,
  model_diagnostics = model_diagnostics
)

message("Analysis completed. Results are in: ", normalizePath(paths$output_root))
