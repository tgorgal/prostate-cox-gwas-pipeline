# Selección de covariables para modelos multivariantes
# 1. Filtro variables (<10%)
# 2. Cox univariante  (p < 0.05)
# 3. StepAIC  (bidireccional)
# 4. LASSO  (penalización)
# 5. Unión + variables forzadas

library(readxl)
library(dplyr)
library(survival)
library(MASS)
library(glmnet)
library(broom)
library(writexl)

input_file <- "results/06_survival_dataset.xlsx"
output_file <- "results/08_variable_selection.xlsx"

Pr <- read_excel(input_file, sheet = "survival_dataset")

outcomes <- list(
  OS = c("OS_time_months", "OS_event"),
  BCR = c("BCR_time_months", "BCR_event"),
  Local = c("Local_time_months", "Local_event"),
  Pelvic = c("Pelvic_time_months", "Pelvic_event"),
  Distant = c("Distant_time_months", "Distant_event")
)

candidate_covariates <- c(
  "Edad_r", "PSA_r", "T_r_label", "Gl_Score_Diag",
  "ISUP_Grade", "EAU_Risk_Score", "Smoker_r_label",
  "DM_r_label", "RA_r_label", "HTA_r_label", "HC_r_label",
  "CardDis_r_label", "TUR_r_label", "HRR_r_label",
  "PTV1_r", "dose_fx_r", "fx_r", "PTV3_r", "HT_Conc_label"
)

forced_covariates <- c(
  "Edad_r", "PSA_r", "ISUP_Grade", "EAU_Risk_Score"  # Podemos quitar ISUP
)

categorical_covariates <- c(
  "T_r_label", "ISUP_Grade", "EAU_Risk_Score", "Smoker_r_label",
  "DM_r_label", "RA_r_label", "HTA_r_label", "HC_r_label",
  "CardDis_r_label", "TUR_r_label", "HRR_r_label", "HT_Conc_label"
)

Pr <- Pr %>%
  mutate(across(any_of(categorical_covariates), as.factor))

binary_filter_all <- list()
univariate_all <- list()
stepaic_all <- list()
lasso_all <- list()
final_all <- list()

for (outcome_name in names(outcomes)) {
  time_col <- outcomes[[outcome_name]][1]
  event_col <- outcomes[[outcome_name]][2]

  data_outcome <- Pr %>%
    filter(!is.na(.data[[time_col]]), !is.na(.data[[event_col]]))

  vars_available <- candidate_covariates[
    candidate_covariates %in% colnames(data_outcome)
  ]

  # ==========================
  # 1. Filtro de variables binarias poco frecuentes
  # ==========================

  binary_filter <- list()

  for (var in vars_available) {
    x <- data_outcome[[var]]
    x <- x[!is.na(x)]

    tab <- table(x)
    if (length(tab) == 2) {
      minor_category <- names(tab)[which.min(tab)]
      minor_n <- min(tab)
      total_n <- sum(tab)
      minor_percent <- as.numeric(minor_n / total_n * 100)

      if (
        minor_percent < 10 &&
          !minor_category %in% c("", "empty", "NA", "<NA>")
      ) {
        binary_filter[[var]] <- data.frame(
          outcome = outcome_name,
          variable = var,
          minor_category = minor_category,
          minor_n = as.integer(minor_n),
          total_n = as.integer(total_n)      │,
          minor_percent = round(minor_percent, 2),
          reason = "Binary variable with minor category <10%"
        )
      }
    }
  }

  binary_filter <- bind_rows(binary_filter)
  excluded_vars <- binary_filter$variable

  vars_eligible <- setdiff(vars_available, excluded_vars)

  vars_eligible <- vars_eligible[
    sapply(vars_eligible, function(v) {
      length(unique(na.omit(data_outcome[[v]]))) > 1
    })
  ]

  # ==========================
  # 2. Cox univariante
  # ==========================

  univariate_results <- list()

  for (var in vars_eligible) {
    formula_uni <- as.formula(
      paste0("Surv(", time_col, ", ", event_col, ") ~ ", var)
    )

    model_uni <- tryCatch(
      coxph(formula_uni, data = data_outcome),
      error = function(e) NULL
    )

    if (!is.null(model_uni)) {
      univariate_results[[var]] <- tidy(
        model_uni,
        exponentiate = TRUE,
        conf.int = TRUE
      ) %>%
        mutate(
          outcome = outcome_name,
          variable = var,
          HR = estimate,
          CI95 = paste0(round(conf.low, 3), " - ", round(conf.high, 3)),
          p_value = p.value,
          significant = if_else(p.value < 0.05, "Yes", "No"),
          .before = 1
        )
    }
  }

  univariate_results <- bind_rows(univariate_results)

  univariate_selected <- univariate_results %>%
    filter(p_value < 0.05) %>%  # Cambiar P adjusted
    pull(variable) %>%
    unique()

  # ==========================
  # 3. StepAIC
  # ==========================

  full_formula <- as.formula(
    paste0(
      "Surv(", time_col, ", ", event_col, ") ~ ",
      paste(vars_eligible, collapse = " + ")
    )
  )

  full_model <- tryCatch(
    coxph(full_formula, data = data_outcome),
    error = function(e) NULL
  )

  if (!is.null(full_model)) {
    step_model <- tryCatch(
      stepAIC(full_model, direction = "both", trace = FALSE),
      error = function(e) NULL
    )
  } else {
    step_model <- NULL
  }

  if (!is.null(step_model)) {
    stepaic_selected <- attr(terms(step_model), "term.labels")

    stepaic_results <- data.frame(
      outcome = outcome_name,
      selected_variable = stepaic_selected,
      method = "stepAIC",
      note = NA_character_
    )
  } else {
    stepaic_selected <- character(0)

    stepaic_results <- data.frame(
      outcome = outcome_name,
      selected_variable = NA_character_,
      method = "stepAIC",
      note = "stepAIC failed"
    )
  }

  # ==========================
  # 4. LASSO
  # ==========================

  lasso_data <- data_outcome %>%
    dplyr::select(all_of(c(time_col, event_col, vars_eligible))) %>%
    na.omit()

  if (nrow(lasso_data) >= 20 && sum(lasso_data[[event_col]] == 1) >= 5) {
    x_formula <- as.formula(
      paste0("~ ", paste(vars_eligible, collapse = " + "))
    )

    x <- model.matrix(x_formula, data = lasso_data)[, -1, drop = FALSE]
    y <- Surv(lasso_data[[time_col]], lasso_data[[event_col]])

    cv_fit <- tryCatch(
      cv.glmnet(x, y, family = "cox", alpha = 1),
      error = function(e) NULL
    )

    if (!is.null(cv_fit)) {
      coefs <- as.matrix(coef(cv_fit, s = "lambda.1se"))
      selected <- coefs[coefs[, 1] != 0, , drop = FALSE]

      if (nrow(selected) > 0) {
        lasso_results <- data.frame(
          outcome = outcome_name,
          selected_term = rownames(selected),
          coefficient = selected[, 1],
          method = "LASSO",
          note = NA_character_
        )

        lasso_selected <- rownames(selected)
      } else {
        lasso_results <- data.frame(
          outcome = outcome_name,
          selected_term = NA_character_,
          coefficient = NA_real_,
          method = "LASSO",
          note = "No variables selected"
        )

        lasso_selected <- character(0)
      }
    } else {
      lasso_results <- data.frame(
        outcome = outcome_name,
        selected_term = NA_character_,
        coefficient = NA_real_,
        method = "LASSO",
        note = "LASSO failed"
      )

      lasso_selected <- character(0)
    }
  } else {
    lasso_results <- data.frame(
      outcome = outcome_name,
      selected_term = NA_character_,
      coefficient = NA_real_,
      method = "LASSO",
      note = "Not enough complete cases or events"
    )

    lasso_selected <- character(0)
  }

  # ==========================
  # 5. Tabla final de candidatas
  # ==========================

  forced_available <- forced_covariates[
    forced_covariates %in% vars_eligible
  ]

  final_candidates <- unique(c(
    forced_available,
    univariate_selected,
    stepaic_selected,
    lasso_selected
  ))

  final_table <- data.frame(
    outcome = outcome_name,
    variable = final_candidates,
    forced = final_candidates %in% forced_available,
    selected_univariate = final_candidates %in% univariate_selected,
    selected_stepAIC = final_candidates %in% stepaic_selected,
    selected_LASSO = sapply(
      final_candidates,
      function(v) any(grepl(paste0("^", v), lasso_selected))
    )
  )

  binary_filter_all[[outcome_name]] <- binary_filter
  univariate_all[[outcome_name]] <- univariate_results
  stepaic_all[[outcome_name]] <- stepaic_results
  lasso_all[[outcome_name]] <- lasso_results
  final_all[[outcome_name]] <- final_table
}

write_xlsx(
  list(
    binary_filter = bind_rows(binary_filter_all),
    univariate_selection = bind_rows(univariate_all),
    stepAIC_selection = bind_rows(stepaic_all),
    lasso_selection = bind_rows(lasso_all),
    final_covariate_candidates = bind_rows(final_all)
  ),
  output_file
)

cat("Archivo creado:\n")
cat(output_file, "\n")
