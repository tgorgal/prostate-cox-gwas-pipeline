# Modelos multivariantes finales de supervivencia
# Usa el conjunto de covariables seleccionado en el script 08
# (final_covariate_candidates) para ajustar un único modelo Cox
# por outcome, con todas las variables juntas y ajustadas entre sí.
#
# Diagnósticos incluidos:
#   - Riesgos proporcionales (cox.zph)
#   - Colinealidad residual (VIF, en escala GVIF^(1/(2*Df)))
#   - N / eventos por modelo
#   - N por nivel de cada covariable, sobre el subconjunto de casos
#     completos realmente usado por el modelo multivariante
#   - Ratio eventos por parámetro (regla general: >= 10 EPV)

library(readxl)
library(dplyr)
library(tidyr)
library(survival)
library(broom)
library(car)
library(writexl)

input_survival <- "results/06_survival_dataset.xlsx"
input_selection <- "results/08_variable_selection.xlsx"
output_file <- "results/09_multivariate_models.xlsx"

Pr <- read_excel(input_survival, sheet = "survival_dataset")

# ==========================
# Reimponer niveles explícitos (mismo bloque que en el script 08)
# ==========================

T_levels_full <- c("Tx","T1","T1b","T1c","T2","T2a","T2b","T2c",
                    "T3","T3a","T3b","T3c","T4")

eau_risk_levels <- c("Low-risk", "Intermediate_Favourable",
                      "Intermediate_Unfavourable", "High_risk")

eau_extent_levels <- c("Localised", "Locally_advanced")

eau_risk_group_levels <- c("Low-risk", "Intermediate-risk", "High-risk")

binary_no_yes_vars <- c(
  "DM_r_label", "RA_r_label", "HTA_r_label", "HC_r_label",
  "CardDis_r_label", "TUR_r_label", "HRR_r_label", "HT_Conc_label"
)

Pr <- Pr %>%
  mutate(
    TStage_imputed = factor(TStage_imputed, levels = T_levels_full),
    Smoker_r_label = factor(Smoker_r_label, levels = c("no", "ex-smoker", "yes")),
    EAU_Risk_Score = factor(EAU_Risk_Score, levels = eau_risk_levels),
    EAU_Extent = factor(EAU_Extent, levels = eau_extent_levels),
    EAU_Risk_Group = factor(EAU_Risk_Group, levels = eau_risk_group_levels),
    ISUP_Grade = factor(ISUP_Grade, levels = 1:5),
    across(all_of(binary_no_yes_vars), ~ factor(.x, levels = c("no", "yes")))
  )

outcomes <- list(
  OS = c("OS_time_months", "OS_event"),
  BCR = c("BCR_time_months", "BCR_event"),
  Local = c("Local_time_months", "Local_event"),
  Pelvic = c("Pelvic_time_months", "Pelvic_event"),
  Distant = c("Distant_time_months", "Distant_event")
)

# ==========================
# Cargar covariables finales seleccionadas en el script 08
# ==========================

final_candidates <- read_excel(input_selection, sheet = "final_covariate_candidates")

covariates_by_outcome <- final_candidates %>%
  group_by(outcome) %>%
  summarise(variables = list(unique(variable)), .groups = "drop")

get_covariates <- function(outcome_name) {
  row <- covariates_by_outcome %>% filter(outcome == outcome_name)
  if (nrow(row) == 0) return(character(0))
  row$variables[[1]]
}

multivariate_results_all <- list()
zph_results_all <- list()
vif_results_all <- list()
model_summary_all <- list()
model_warnings_all <- list()
level_counts_all <- list()

# Nota sobre la interpretación de VIF_adjusted (se exporta también al Excel):
# car::vif() devuelve, para variables categóricas con >1 grado de libertad,
# GVIF, Df y GVIF^(1/(2*Df)). Esta última columna es la que se reporta aquí
# como "VIF_adjusted" porque es comparable directamente entre variables con
# distinto número de niveles. IMPORTANTE: está en la escala de la raíz
# cuadrada del VIF clásico, no del VIF en sí. Los umbrales habituales
# (VIF > 5 / VIF > 10) equivalen aquí aproximadamente a:
#   sqrt(5)  ~= 2.236
#   sqrt(10) ~= 3.162
# No aplicar los umbrales clásicos (5 / 10) directamente sobre esta columna.
vif_note <- data.frame(
  nota = paste(
    "VIF_adjusted = GVIF^(1/(2*Df)), calculado con car::vif().",
    "Esta cantidad esta en la escala de la RAIZ CUADRADA del VIF clasico.",
    "Umbrales equivalentes aproximados: VIF>5 -> VIF_adjusted>2.236;",
    "VIF>10 -> VIF_adjusted>3.162.",
    "No interpretar VIF_adjusted con los umbrales clasicos (5 / 10) sin convertir."
  )
)

for (outcome_name in names(outcomes)) {
  time_col <- outcomes[[outcome_name]][1]
  event_col <- outcomes[[outcome_name]][2]

  covs <- get_covariates(outcome_name)
  covs <- covs[covs %in% colnames(Pr)]

  if (length(covs) == 0) {
    cat("\n⚠️  [", outcome_name, "] Sin covariables seleccionadas, se omite el modelo.\n", sep = "")
    next
  }

  data_outcome <- Pr %>%
    filter(!is.na(.data[[time_col]]), !is.na(.data[[event_col]]))

  formula_multi <- as.formula(
    paste0("Surv(", time_col, ", ", event_col, ") ~ ", paste(covs, collapse = " + "))
  )

  warning_message <- NA_character_

  model <- tryCatch(
    withCallingHandlers(
      coxph(formula_multi, data = data_outcome),
      warning = function(w) {
        warning_message <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      warning_message <<- conditionMessage(e)
      NULL
    }
  )

  if (!is.na(warning_message)) {
    model_warnings_all[[outcome_name]] <- data.frame(
      outcome = outcome_name,
      warning = warning_message
    )
  }

  if (is.null(model)) {
    cat("\n❌ [", outcome_name, "] El modelo multivariante falló:", warning_message, "\n")
    next
  }

  # --- Tabla de resultados (HR, IC95, p Wald por nivel) ---
  multivariate_results_all[[outcome_name]] <- tidy(
    model, exponentiate = TRUE, conf.int = TRUE
  ) %>%
    mutate(
      outcome = outcome_name,
      HR = estimate,
      CI95 = paste0(round(conf.low, 3), " - ", round(conf.high, 3)),
      p_value_level = p.value,
      N = model$n,
      N_events = model$nevent,
      .before = 1
    ) %>%
    dplyr::select(outcome, term, N, N_events, HR, CI95, p_value_level)

  # --- Casos completos realmente usados por el modelo (listwise deletion
  #     sobre time/event + TODAS las covariables del modelo multivariante) ---
  complete_rows <- data_outcome %>%
    dplyr::select(all_of(c(time_col, event_col, covs))) %>%
    tidyr::drop_na()

  n_complete <- nrow(complete_rows)
  n_events_complete <- sum(complete_rows[[event_col]] == 1)

  # --- N por nivel de cada covariable categórica, sobre ese subconjunto,
  #     para poder detectar categorías con muy pocos casos/eventos una vez
  #     ajustadas todas las variables entre sí (riesgo de cuasi-separación) ---
  level_counts <- list()

  for (var in covs) {
    if (is.factor(complete_rows[[var]]) || is.character(complete_rows[[var]])) {
      tab_n <- table(complete_rows[[var]], useNA = "no")
      tab_events <- tapply(
        complete_rows[[event_col]],
        complete_rows[[var]],
        function(x) sum(x == 1)
      )

      level_counts[[var]] <- data.frame(
        outcome = outcome_name,
        variable = var,
        level = names(tab_n),
        N = as.integer(tab_n),
        N_events = as.integer(tab_events[names(tab_n)])
      )
    }
  }

  if (length(level_counts) > 0) {
    level_counts_all[[outcome_name]] <- bind_rows(level_counts)
  }

  # --- N / eventos, resumen por outcome ---
  n_parameters <- length(coef(model))
  events_per_parameter <- if (n_parameters > 0) model$nevent / n_parameters else NA_real_

  model_summary_all[[outcome_name]] <- data.frame(
    outcome = outcome_name,
    N_eligible = nrow(data_outcome),
    N_model = model$n,
    N_events_model = model$nevent,
    N_complete_cases_check = n_complete,
    N_events_complete_cases_check = n_events_complete,
    N_covariates = length(covs),
    N_parameters = n_parameters,
    events_per_parameter = round(events_per_parameter, 2),
    EPV_ok_10 = if_else(events_per_parameter >= 10, "Yes", "No"),
    covariates = paste(covs, collapse = ", ")
  )

  # --- Riesgos proporcionales (cox.zph) ---
  zph_test <- tryCatch(cox.zph(model), error = function(e) NULL)

  if (!is.null(zph_test)) {
    zph_table <- as.data.frame(zph_test$table)
    zph_table$term <- rownames(zph_table)
    zph_table$outcome <- outcome_name

    zph_results_all[[outcome_name]] <- zph_table %>%
      dplyr::select(outcome, term, chisq, df, p)
  }

    # --- VIF (solo tiene sentido con >= 2 covariables; con 1 no aplica) ---
  if (length(covs) >= 2) {
    vif_error_message <- NA_character_

    vif_vals <- tryCatch(
      car::vif(model),
      error = function(e) {
        vif_error_message <<- conditionMessage(e)
        NULL
      }
    )

    if (!is.null(vif_vals)) {
      if (is.matrix(vif_vals)) {
        vif_df <- data.frame(
          outcome = outcome_name,
          term = rownames(vif_vals),
          VIF_adjusted = vif_vals[, "GVIF^(1/(2*Df))"],
          note = NA_character_
        )
      } else {
        vif_df <- data.frame(
          outcome = outcome_name,
          term = names(vif_vals),
          VIF_adjusted = as.numeric(vif_vals),
          note = NA_character_
        )
      }

      vif_results_all[[outcome_name]] <- vif_df
    } else {
      vif_results_all[[outcome_name]] <- data.frame(
        outcome = outcome_name,
        term = NA_character_,
        VIF_adjusted = NA_real_,
        note = paste0("VIF failed: ", vif_error_message)
      )
    }
  } else {
    vif_results_all[[outcome_name]] <- data.frame(
      outcome = outcome_name,
      term = NA_character_,
      VIF_adjusted = NA_real_,
      note = "VIF not applicable: fewer than 2 covariates in the model"
    )
  }

  cat(
    "\n✅ [", outcome_name, "] Modelo ajustado. N =", model$n,
    "| eventos =", model$nevent,
    "| parametros =", n_parameters,
    "| eventos/parametro =", round(events_per_parameter, 2),
    "| covariables:", paste(covs, collapse = ", "), "\n"
  )
}

# ==========================
# Exportar
# ==========================

write_xlsx(
  list(
    multivariate_results = bind_rows(multivariate_results_all),
    proportional_hazards = bind_rows(zph_results_all),
    vif_diagnostics = bind_rows(vif_results_all),
    vif_notes = vif_note,
    level_counts_full_model = bind_rows(level_counts_all),
    model_summary = bind_rows(model_summary_all),
    model_warnings = bind_rows(model_warnings_all)
  ),
  output_file
)

cat("\nArchivo creado:\n")
cat(output_file, "\n")
