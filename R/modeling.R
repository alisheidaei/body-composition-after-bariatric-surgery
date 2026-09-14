# Mixed-effects models, marginal means, and diagnostics -----------------------

fit_mixed_model <- function(outcome, df) {
  formula <- stats::reformulate(
    c(
      "followup_period * SurgeryTypeName", "Age", "Sex", "T2DM_FBS",
      "HTN_FBS", "Hypothyroidism_FBS", "(1 | ID)"
    ),
    response = outcome
  )
  lme4::lmer(
    formula,
    data = df,
    REML = TRUE,
    na.action = stats::na.exclude,
    control = lme4::lmerControl(optimizer = "bobyqa")
  )
}

fit_all_models <- function(df) {
  models <- lapply(analysis_outcomes, fit_mixed_model, df = df)
  stats::setNames(models, analysis_outcomes)
}

standardize_ci_columns <- function(x) {
  lower <- names(x)[grepl("^(lower.CL|asymp.LCL)$", names(x))]
  upper <- names(x)[grepl("^(upper.CL|asymp.UCL)$", names(x))]
  if (length(lower) != 1L || length(upper) != 1L) {
    stop("Could not identify confidence-interval columns returned by emmeans.")
  }
  names(x)[names(x) == lower] <- "lower"
  names(x)[names(x) == upper] <- "upper"
  x
}

obtain_adjusted_trajectories <- function(models) {
  purrr::imap_dfr(models, function(model, outcome) {
    emm <- emmeans::emmeans(model, ~followup_period * SurgeryTypeName)
    result <- standardize_ci_columns(as.data.frame(stats::confint(emm)))
    dplyr::mutate(result, Outcome = outcome, .before = 1)
  }) |>
    dplyr::mutate(
      OutcomeLabel = factor(outcome_labels[Outcome],
                            levels = unname(outcome_labels[analysis_outcomes])),
      period_index = as.integer(followup_period)
    )
}

diagnose_models <- function(models) {
  purrr::imap_dfr(models, function(model, outcome) {
    messages <- model@optinfo$conv$lme4$messages
    tibble::tibble(
      Outcome = outcome,
      n_observations = stats::nobs(model),
      n_patients = dplyr::n_distinct(stats::model.frame(model)$ID),
      singular_fit = lme4::isSingular(model, tol = 1e-4),
      convergence_messages = if (is.null(messages)) "" else paste(messages, collapse = "; ")
    )
  })
}

obtain_fixed_effects <- function(models) {
  purrr::imap_dfr(models, function(model, outcome) {
    broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE,
                      conf.method = "Wald") |>
      dplyr::mutate(Outcome = outcome, .before = 1)
  })
}

