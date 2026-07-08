# Modelos univariantes de supervivencia
library(readxl)
library(dplyr)
library(survival)
library(broom)
library(writexl)

input_file <- "results/06_survival_dataset.xlsx"
output_file <- "results/07_univariate_models.xlsx"

Pr <- read_excel(input_file, sheet = "survival_dataset")

# Reimponer el orden de niveles de TStage_imputed
# (se pierde al pasar por Excel entre el script 04 y este)
T_levels_full <- c("Tx","T1","T1b","T1c","T2","T2a","T2b","T2c",
                    "T3","T3a","T3b","T3c","T4")

Pr <- Pr %>%
  mutate(TStage_imputed = factor(TStage_imputed, levels = T_levels_full))

covariates <- c(
  "Edad_r",
  "PSA_r",
  "TStage_imputed",
  "Gl_Score_Diag",
  "ISUP_Grade",
  "EAU_Risk_Score",
  "Smoker_r",
  "DM_r",
  "RA_r",
  "HTA_r",
  "HC_r",
  "CardDis_r",
  "TUR_r",
  "HRR_r",
  "PTV1_r",
  "dose_fx_r",
  "fx_r",
  "PTV3_r",
  "HT_Conc"
)

run_univariate_cox <- function(data, time_col, event_col, covariates, outcome_name) {
  results <- list()
  warnings_list <- list()

  for (var in covariates) {
    formula <- as.formula(
      paste0("Surv(", time_col, ", ", event_col, ") ~ ", var)
    )
    warning_message <- NA_character_

    model <- tryCatch(
      withCallingHandlers(
        coxph(formula, data = data),
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

    if (!is.null(model)) {
      # --- N total y eventos ---
      n_total  <- model$n
      n_events <- model$nevent

      # --- N por categoría (solo para factores/caracteres) ---
      # calculado sobre las filas realmente usadas por el modelo,
      # para que sea consistente con n_total/n_events
      if (is.factor(data[[var]]) || is.character(data[[var]])) {
        complete_data <- data %>%
          filter(
            !is.na(.data[[time_col]]),
            !is.na(.data[[event_col]]),
            !is.na(.data[[var]])
          )
        n_by_level <- table(complete_data[[var]], useNA = "no")
        n_detail <- paste(
          paste0(names(n_by_level), ": ", as.integer(n_by_level)),
          collapse = " | "
        )
      } else {
        n_detail <- NA_character_  # continua: no aplica desglose
      }

      # p-valor a nivel de variable (LR test), único por variable,
      # independiente del número de niveles/categorías
      lr_table <- anova(model)
      p_value_variable <- lr_table[["Pr(>|Chi|)"]][nrow(lr_table)]

      results[[var]] <- tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
        mutate(
          outcome       = outcome_name,
          variable      = var,
          HR            = estimate,
          CI95          = paste0(round(conf.low, 3), " - ", round(conf.high, 3)),
          p_value_level = p.value,           # Wald, por nivel/coeficiente
          p_value       = p_value_variable,  # LR, a nivel de variable
          significant   = if_else(p_value < 0.05, "Yes", "No"),
          N             = n_total,
          N_events      = n_events,
          N_by_level    = n_detail
        ) %>%
        dplyr::select(outcome, variable, N, N_events, N_by_level,
                       HR, CI95, p_value_level, p_value, significant)
    }

    if (!is.na(warning_message)) {
      warnings_list[[var]] <- data.frame(
        outcome  = outcome_name,
        variable = var,
        warning  = warning_message
      )
    }
  }

  results_df <- bind_rows(results)

  # FDR calculado una vez por variable (no una vez por nivel/categoría)
  variable_pvals <- results_df %>%
    distinct(variable, p_value) %>%
    mutate(p_value_FDR = p.adjust(p_value, method = "BH"))

  results_df <- results_df %>%
    left_join(variable_pvals, by = c("variable", "p_value")) %>%
    mutate(significant_FDR = if_else(p_value_FDR < 0.05, "Yes", "No"))

  list(
    results  = results_df,
    warnings = bind_rows(warnings_list)
  )
}

os      <- run_univariate_cox(Pr, "OS_time_months",      "OS_event",      covariates, "OS")
bcr     <- run_univariate_cox(Pr, "BCR_time_months",     "BCR_event",     covariates, "BCR")
local   <- run_univariate_cox(Pr, "Local_time_months",   "Local_event",   covariates, "Local")
pelvic  <- run_univariate_cox(Pr, "Pelvic_time_months",  "Pelvic_event",  covariates, "Pelvic")
distant <- run_univariate_cox(Pr, "Distant_time_months", "Distant_event", covariates, "Distant")

write_xlsx(
  list(
    OS = os$results,
    BCR = bcr$results,
    Local = local$results,
    Pelvic = pelvic$results,
    Distant = distant$results,
    model_warnings = bind_rows(
      os$warnings,
      bcr$warnings,
      local$warnings,
      pelvic$warnings,
      distant$warnings
    )
  ),
  output_file
)

cat("Archivo creado:\n")
cat(output_file, "\n")
