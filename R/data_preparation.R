# Data import, validation, and harmonization ---------------------------------

standardize_followup <- function(x) {
  if (is.numeric(x) || inherits(x, "haven_labelled")) {
    return(factor(as.numeric(x), levels = -1:4, labels = period_levels))
  }
  value <- stringr::str_squish(as.character(x))
  value <- dplyr::case_when(
    value %in% c("-1", "Pre", "Pre-op", "Preoperative") ~ "Pre",
    value %in% c("0", "0-1m", "0–1m", "0-1 months", "0–1 months") ~ "0–1m",
    value %in% c("1", "1-6m", "1–6m", "1-6 months", "1–6 months") ~ "1–6m",
    value %in% c("2", "6-12m", "6–12m", "6-12 months", "6–12 months") ~ "6–12m",
    value %in% c("3", "12-24m", "12–24m", "12-24 months", "12–24 months") ~ "12–24m",
    value %in% c("4", ">24m", ">24 months") ~ ">24m",
    TRUE ~ NA_character_
  )
  factor(value, levels = period_levels)
}

standardize_procedure <- function(x) {
  value <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  value <- dplyr::case_when(
    stringr::str_detect(value, "sleeve|^sg$") ~ "SG",
    stringr::str_detect(value, "mini|one[- ]?anastomosis|^oagb$") ~ "OAGB",
    stringr::str_detect(value, "roux|ryg|^rygb$") ~ "RYGB",
    TRUE ~ NA_character_
  )
  factor(value, levels = procedure_levels)
}

binary_factor <- function(x) {
  if (is.factor(x) || is.character(x)) {
    value <- stringr::str_to_lower(as.character(x))
    value <- dplyr::case_when(
      value %in% c("0", "no", "female") ~ 0,
      value %in% c("1", "yes", "male") ~ 1,
      TRUE ~ NA_real_
    )
  } else {
    value <- as.numeric(x)
  }
  factor(value, levels = c(0, 1), labels = c("No", "Yes"))
}

read_and_prepare_data <- function(data_file) {
  if (!file.exists(data_file)) {
    stop("Data file not found at '", data_file,
         "'. Place it there or define BIA_DATA_PATH.")
  }
  df <- haven::read_dta(data_file)

  required <- c(
    "ID", "followup_period", "SurgeryTypeName", "Sex", "Age",
    "T2DM_FBS", "Hypothyroidism_FBS", "HTN_FBS",
    analysis_outcomes,
    "BMI0", "Weight0", "FATP0", "PMM0", "FFM0", "SMMI0", "ASM0"
  )
  missing <- setdiff(required, names(df))
  if (length(missing) > 0L) {
    stop("Required variables are missing: ", paste(missing, collapse = ", "))
  }

  original_procedures <- unique(as.character(df$SurgeryTypeName))
  df <- df |>
    dplyr::mutate(
      followup_period = standardize_followup(followup_period),
      SurgeryTypeName = standardize_procedure(SurgeryTypeName),
      Sex = factor(binary_factor(Sex), levels = c("No", "Yes"),
                   labels = c("Female", "Male")),
      T2DM_FBS = binary_factor(T2DM_FBS),
      Hypothyroidism_FBS = binary_factor(Hypothyroidism_FBS),
      HTN_FBS = binary_factor(HTN_FBS),
      ID = factor(ID)
    )

  if (anyNA(df$SurgeryTypeName)) {
    stop("Unrecognized procedure name. Original values: ",
         paste(original_procedures, collapse = ", "))
  }
  if (anyNA(df$followup_period)) {
    warning("Rows with missing or unrecognized follow-up periods will be excluded.")
  }
  df
}

summarize_followup <- function(df) {
  assessments <- df |>
    dplyr::count(followup_period, name = "n_assessments", .drop = FALSE)
  patients <- df |>
    dplyr::filter(!is.na(followup_period)) |>
    dplyr::distinct(ID, followup_period) |>
    dplyr::count(followup_period, name = "n_unique_patients", .drop = FALSE)
  dplyr::full_join(assessments, patients, by = "followup_period")
}

select_patient_baseline <- function(df) {
  candidates <- df |> dplyr::filter(followup_period == "Pre")
  if ("followup_date" %in% names(candidates)) {
    baseline <- candidates |>
      dplyr::arrange(ID, dplyr::desc(followup_date)) |>
      dplyr::group_by(ID) |>
      dplyr::slice(1L) |>
      dplyr::ungroup()
  } else {
    warning("followup_date is unavailable; using the first preoperative record per patient.")
    baseline <- candidates |>
      dplyr::group_by(ID) |>
      dplyr::slice(1L) |>
      dplyr::ungroup()
  }
  if (nrow(baseline) != dplyr::n_distinct(baseline$ID)) {
    stop("Baseline selection did not produce one record per patient.")
  }
  baseline
}

summarize_crude_trajectories <- function(df) {
  df |>
    dplyr::filter(!is.na(followup_period)) |>
    dplyr::select(ID, SurgeryTypeName, followup_period,
                  dplyr::all_of(analysis_outcomes)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(analysis_outcomes),
      names_to = "Outcome",
      values_to = "Value"
    ) |>
    dplyr::group_by(Outcome, SurgeryTypeName, followup_period) |>
    dplyr::summarise(
      n = sum(!is.na(Value)),
      mean = mean(Value, na.rm = TRUE),
      sd = stats::sd(Value, na.rm = TRUE),
      se = sd / sqrt(n),
      lower = mean - 1.96 * se,
      upper = mean + 1.96 * se,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      OutcomeLabel = factor(outcome_labels[Outcome],
                            levels = unname(outcome_labels[analysis_outcomes])),
      period_index = as.integer(followup_period)
    )
}

