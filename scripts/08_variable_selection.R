# Selección de covariables para modelos multivariantes
# 1. Filtro de categorías poco frecuentes (<10%, binarias y multinivel)
# 2. Cox univariante (LR test, FDR por variable)
# 3. StepAIC (bidireccional)
# 4. LASSO (penalización, lambda.1se oficial + lambda.min diagnóstico)
# 5. Unión + variables forzadas
# 6. Trazabilidad completa de todas las candidatas por outcome

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

# Reimponer el orden de niveles de TStage_imputed
# (se pierde al pasar por Excel)
T_levels_full <- c("Tx","T1","T1b","T1c","T2","T2a","T2b","T2c",
                    "T3","T3a","T3b","T3c","T4")

Pr <- Pr %>%
  mutate(TStage_imputed = factor(TStage_imputed, levels = T_levels_full))

outcomes <- list(
  OS = c("OS_time_months", "OS_event"),
  BCR = c("BCR_time_months", "BCR_event"),
  Local = c("Local_time_months", "Local_event"),
  Pelvic = c("Pelvic_time_months", "Pelvic_event"),
  Distant = c("Distant_time_months", "Distant_event")
)

# ==========================
# Conjunto de covariables candidatas
# (PSA_r, ISUP_Grade y TStage_imputed compiten libremente frente a
# EAU_Risk_Score, que ya los combina; no se fuerzan ambos a la vez)
# ==========================

candidate_covariates <- c(
  "Edad_r", "PSA_r", "TStage_imputed", "ISUP_Grade", "EAU_Risk_Score",
  "Smoker_r_label", "DM_r_label", "RA_r_label", "HTA_r_label",
  "HC_r_label", "CardDis_r_label", "TUR_r_label", "HRR_r_label",
  "PTV1_r", "dose_fx_r", "fx_r", "PTV3_r", "HT_Conc_label"
)

# Opción B: se fuerza el score compuesto (EAU_Risk_Score), que ya
# incorpora el efecto de PSA/ISUP/T agrupados por relevancia clínica;
# sus componentes individuales quedan libres para competir en la
# selección (univariante/stepAIC/LASSO) por si aportan señal adicional.
forced_covariates <- c("Edad_r", "EAU_Risk_Score")

categorical_covariates <- c(
  "TStage_imputed", "ISUP_Grade", "EAU_Risk_Score", "Smoker_r_label",
  "DM_r_label", "RA_r_label", "HTA_r_label", "HC_r_label",
  "CardDis_r_label", "TUR_r_label", "HRR_r_label", "HT_Conc_label"
)

Pr <- Pr %>%
  mutate(across(
    any_of(setdiff(categorical_covariates, "TStage_imputed")),
    as.factor
  ))

binary_filter_all <- list()
univariate_all <- list()
stepaic_all <- list()
lasso_all <- list()
lasso_min_all <- list()
all_candidates_status_all <- list()
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
  # 1. Filtro de categorías poco frecuentes
  # (cubre binarias y multinivel: cualquier categoría <10%)
  # ==========================

  binary_filter <- list()

  for (var in vars_available) {
    x <- data_outcome[[var]]
    x <- x[!is.na(x)]

    if (!(is.factor(x) || is.character(x))) next  # solo aplica a categóricas

    tab <- table(x)

    if (length(tab) < 2) next  # sin variabilidad, se filtra más abajo

    total_n <- sum(tab)
    percents <- as.numeric(tab / total_n * 100)
    rare_levels <- names(tab)[percents < 10]

    if (length(rare_levels) > 0) {
      binary_filter[[var]] <- data.frame(
        outcome = outcome_name,
        variable = var,
        rare_category = rare_levels,
        rare_n = as.integer(tab[rare_levels]),
        total_n = as.integer(total_n),
        rare_percent = round(percents[percents < 10], 2),
        reason = "Category with frequency <10%"
      )
    }
  }

  binary_filter <- bind_rows(binary_filter)
  excluded_vars <- unique(binary_filter$variable)

  # --- Aviso en consola de categorías raras detectadas ---
  if (nrow(binary_filter) > 0) {
    cat("\n⚠️  [", outcome_name, "] Categorías con frecuencia <10% (variable excluida):\n", sep = "")
    for (i in seq_len(nrow(binary_filter))) {
      cat(
        "   -", binary_filter$variable[i],
        "| categoría:", binary_filter$rare_category[i],
        "| n =", binary_filter$rare_n[i],
        "/", binary_filter$total_n[i],
        paste0("(", binary_filter$rare_percent[i], "%)"),
        "\n"
      )
    }
  } else {
    cat("\n✅ [", outcome_name, "] Sin categorías por debajo del 10% en las candidatas.\n", sep = "")
  }

  vars_eligible <- setdiff(vars_available, excluded_vars)

  vars_eligible <- vars_eligible[
    sapply(vars_eligible, function(v) {
      length(unique(na.omit(data_outcome[[v]]))) > 1
    })
  ]

  # ==========================
  # 2. Cox univariante (LR test, p-valor por variable + FDR)
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
      lr_table <- anova(model_uni)
      p_value_variable <- lr_table[["Pr(>|Chi|)"]][nrow(lr_table)]

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
          p_value_level = p.value,        # Wald, por nivel (referencia)
          p_value = p_value_variable,     # LR, por variable
          .before = 1
        )
    }
  }

  univariate_results <- bind_rows(univariate_results)

  # FDR calculado una vez por variable (no una vez por nivel)
  if (nrow(univariate_results) > 0) {
    variable_pvals <- univariate_results %>%
      distinct(variable, p_value) %>%
      mutate(p_value_FDR = p.adjust(p_value, method = "BH"))

    univariate_results <- univariate_results %>%
      left_join(variable_pvals, by = c("variable", "p_value")) %>%
      mutate(significant_FDR = if_else(p_value_FDR < 0.05, "Yes", "No"))
  } else {
    univariate_results <- univariate_results %>%
      mutate(p_value_FDR = numeric(0), significant_FDR = character(0))
  }

  univariate_selected <- univariate_results %>%
    filter(p_value_FDR < 0.05) %>%
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
  # 4. LASSO (lambda.1se oficial + lambda.min diagnóstico)
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
      # --- Selección oficial: lambda.1se (criterio ya acordado) ---
      coefs_1se <- as.matrix(coef(cv_fit, s = "lambda.1se"))
      selected_1se <- coefs_1se[coefs_1se[, 1] != 0, , drop = FALSE]

      # --- Comprobación diagnóstica: lambda.min (más permisivo) ---
      coefs_min <- as.matrix(coef(cv_fit, s = "lambda.min"))
      selected_min <- coefs_min[coefs_min[, 1] != 0, , drop = FALSE]
      lasso_selected_min <- rownames(selected_min)

      cat(
        "\n[", outcome_name, "] LASSO — lambda.1se: ", nrow(selected_1se),
        " variable(s) seleccionada(s) | lambda.min: ", nrow(selected_min),
        " variable(s) seleccionada(s)\n",
        sep = ""
      )

      if (nrow(selected_1se) > 0) {
        lasso_results <- data.frame(
          outcome = outcome_name,
          selected_term = rownames(selected_1se),
          coefficient = selected_1se[, 1],
          method = "LASSO (lambda.1se)",
          note = NA_character_
        )

        lasso_selected <- rownames(selected_1se)
      } else {
        lasso_results <- data.frame(
          outcome = outcome_name,
          selected_term = NA_character_,
          coefficient = NA_real_,
          method = "LASSO (lambda.1se)",
          note = "No variables selected"
        )

        lasso_selected <- character(0)
      }

      # Tabla diagnóstica con lambda.min, guardada aparte (no afecta a la selección final)
      if (nrow(selected_min) > 0) {
        lasso_results_min <- data.frame(
          outcome = outcome_name,
          selected_term = rownames(selected_min),
          coefficient = selected_min[, 1],
          method = "LASSO (lambda.min, diagnóstico)",
          note = NA_character_
        )
      } else {
        lasso_results_min <- data.frame(
          outcome = outcome_name,
          selected_term = NA_character_,
          coefficient = NA_real_,
          method = "LASSO (lambda.min, diagnóstico)",
          note = "No variables selected"
        )
      }
    } else {
      lasso_results <- data.frame(
        outcome = outcome_name,
        selected_term = NA_character_,
        coefficient = NA_real_,
        method = "LASSO (lambda.1se)",
        note = "LASSO failed"
      )

      lasso_results_min <- data.frame(
        outcome = outcome_name,
        selected_term = NA_character_,
        coefficient = NA_real_,
        method = "LASSO (lambda.min, diagnóstico)",
        note = "LASSO failed"
      )

      lasso_selected <- character(0)
      lasso_selected_min <- character(0)
    }
  } else {
    lasso_results <- data.frame(
      outcome = outcome_name,
      selected_term = NA_character_,
      coefficient = NA_real_,
      method = "LASSO (lambda.1se)",
      note = "Not enough complete cases or events"
    )

    lasso_results_min <- data.frame(
      outcome = outcome_name,
      selected_term = NA_character_,
      coefficient = NA_real_,
      method = "LASSO (lambda.min, diagnóstico)",
      note = "Not enough complete cases or events"
    )

    lasso_selected <- character(0)
    lasso_selected_min <- character(0)
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
    ),
    selected_LASSO_min = sapply(
      final_candidates,
      function(v) any(grepl(paste0("^", v), lasso_selected_min))
    )
  )

  # ==========================
  # 6. Trazabilidad completa: estado de cada candidata para este outcome
  # ==========================

  # Variables que no llegaron ni a estar disponibles en el dataset
  vars_not_available <- setdiff(candidate_covariates, vars_available)

  # Variables disponibles pero excluidas por categoría rara (<10%)
  vars_excluded_rare <- intersect(vars_available, excluded_vars)

  # Variables que pasaron el filtro de categoría rara pero no tenían variabilidad
  vars_after_rare_filter <- setdiff(vars_available, excluded_vars)
  vars_no_variability <- setdiff(vars_after_rare_filter, vars_eligible)

  status_rows <- lapply(candidate_covariates, function(var) {
    if (var %in% vars_not_available) {
      status <- "No disponible en el dataset"
    } else if (var %in% vars_excluded_rare) {
      status <- "Excluida por categoría rara (<10%)"
    } else if (var %in% vars_no_variability) {
      status <- "Excluida por falta de variabilidad"
    } else {
      is_forced <- var %in% forced_available
      is_uni <- var %in% univariate_selected
      is_step <- var %in% stepaic_selected
      is_lasso <- any(grepl(paste0("^", var), lasso_selected))
      is_lasso_min <- any(grepl(paste0("^", var), lasso_selected_min))

      if (is_forced || is_uni || is_step || is_lasso) {
        status <- "Incluida"
      } else {
        status <- "No seleccionada por ningún método"
      }
    }

    data.frame(
      outcome = outcome_name,
      variable = var,
      status = status,
      forced = var %in% forced_available,
      selected_univariate = var %in% univariate_selected,
      selected_stepAIC = var %in% stepaic_selected,
      selected_LASSO = any(grepl(paste0("^", var), lasso_selected)),
      selected_LASSO_min = any(grepl(paste0("^", var), lasso_selected_min))
    )
  })

  all_candidates_status_all[[outcome_name]] <- bind_rows(status_rows)

  binary_filter_all[[outcome_name]] <- binary_filter
  univariate_all[[outcome_name]] <- univariate_results
  stepaic_all[[outcome_name]] <- stepaic_results
  lasso_all[[outcome_name]] <- lasso_results
  lasso_min_all[[outcome_name]] <- lasso_results_min
  final_all[[outcome_name]] <- final_table
}

write_xlsx(
  list(
    binary_filter = bind_rows(binary_filter_all),
    univariate_selection = bind_rows(univariate_all),
    stepAIC_selection = bind_rows(stepaic_all),
    lasso_selection = bind_rows(lasso_all),
    lasso_selection_lambda_min = bind_rows(lasso_min_all),
    all_candidates_status = bind_rows(all_candidates_status_all),
    final_covariate_candidates = bind_rows(final_all)
  ),
  output_file
)

cat("Archivo creado:\n")
cat(output_file, "\n")
