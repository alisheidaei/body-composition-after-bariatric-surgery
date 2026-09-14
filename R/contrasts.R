# Adjusted contrasts in change from the preoperative period -------------------

procedure_pairs <- list(
  "SG - OAGB" = c("SG", "OAGB"),
  "SG - RYGB" = c("SG", "RYGB"),
  "OAGB - RYGB" = c("OAGB", "RYGB")
)

obtain_change_contrasts <- function(model, outcome) {
  emm <- emmeans::emmeans(model, ~followup_period * SurgeryTypeName)
  grid <- as.data.frame(emm)

  purrr::map_dfr(period_levels[-1], function(period) {
    vectors <- purrr::imap(procedure_pairs, function(pair, contrast_name) {
      coefficient <- numeric(nrow(grid))
      locate <- function(target_period, target_procedure) {
        which(as.character(grid$followup_period) == target_period &
                as.character(grid$SurgeryTypeName) == target_procedure)
      }
      coefficient[locate(period, pair[1])] <- 1
      coefficient[locate("Pre", pair[1])] <- -1
      coefficient[locate(period, pair[2])] <- -1
      coefficient[locate("Pre", pair[2])] <- 1
      coefficient
    })

    result <- emmeans::contrast(emm, method = vectors, adjust = "tukey")
    result <- as.data.frame(
      summary(result, infer = c(TRUE, TRUE), adjust = "tukey")
    )
    result <- standardize_ci_columns(result)
    result |>
      dplyr::transmute(
        Followup = period,
        Outcome = outcome,
        Contrast = contrast,
        Estimate = estimate,
        SE = SE,
        df = df,
        Lower = lower,
        Upper = upper,
        P_adjusted = p.value
      )
  })
}

obtain_all_change_contrasts <- function(models) {
  purrr::imap_dfr(models[muscle_outcomes], obtain_change_contrasts) |>
    dplyr::mutate(
      Followup = factor(Followup, levels = period_levels[-1]),
      Outcome = factor(Outcome, levels = muscle_outcomes),
      Contrast = factor(Contrast, levels = names(procedure_pairs)),
      Significant = Lower > 0 | Upper < 0
    ) |>
    dplyr::arrange(Followup, Outcome, Contrast)
}

