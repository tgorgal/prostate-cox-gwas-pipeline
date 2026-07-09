# Modelos multivariantes finales de supervivencia
# Usa el conjunto de covariables seleccionado en el script 08
# (final_covariate_candidates) para ajustar un único modelo Cox
# por outcome, con todas las variables juntas y ajustadas entre sí.
#
# Diagnósticos incluidos:
#   - Riesgos proporcionales (cox.zph)
#   - Colinealidad residual (VIF)
#   - N / eventos por modelo

library(readxl)
library(dplyr)
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

binary_no_yes_vars <- c(
  "DM_r_label", "RA_r_label", "HTA_r_label", "HC_r_label",
  "CardDis_r_label", "TUR_r_label", "HRR_r_label", "HT_Conc_label"
)

Pr <- Pr %>%
  mutate(
    TStage_imputed = factor(TStage_imputed, levels = T_levels_full),
    Smoker_r_label = factor(Smoker_r_label, levels = c("no", "ex-smoker", "yes")),
    EAU_Risk_Score = factor(EAU_Risk_Score, levels = eau_risk_levels),
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

  # --- N / eventos, resumen por outcome ---
  model_summary_all[[outcome_name]] <- data.frame(
    outcome = outcome_name,
    N = model$n,
    N_events = model$nevent,
    N_covariates = length(covs),
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

  # --- VIF (solo si hay >= 2 covariables; con 1 no aplica) ---
  if (length(covs) >= 2) {
    vif_vals <- tryCatch(car::vif(model), error = function(e) NULL)

    if (!is.null(vif_vals)) {
      if (is.matrix(vif_vals)) {
        # Variables categóricas con >1 grado de libertad devuelven
        # GVIF, Df, GVIF^(1/(2*Df)) — se usa esta última columna,
        # comparable directamente al VIF clásico independientemente
        # de los grados de libertad de cada término.
        vif_df <- data.frame(
          outcome = outcome_name,
          term = rownames(vif_vals),
          VIF_adjusted = vif_vals[, "GVIF^(1/(2*Df))"]
        )
      } else {
        vif_df <- data.frame(
          outcome = outcome_name,
          term = names(vif_vals),
          VIF_adjusted = as.numeric(vif_vals)
        )
      }

      vif_results_all[[outcome_name]] <- vif_df
    }
  }

  cat(
    "\n✅ [", outcome_name, "] Modelo ajustado. N =", model$n,
    "| eventos =", model$nevent,
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
    model_summary = bind_rows(model_summary_all),
    model_warnings = bind_rows(model_warnings_all)
  ),
  output_file
)

cat("\nArchivo creado:\n")
cat(output_file, "\n")
