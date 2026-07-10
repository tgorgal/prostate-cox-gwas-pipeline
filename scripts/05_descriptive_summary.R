# Tabla descriptiva de la cohorte

library(readxl)
library(dplyr)
library(writexl)

input_file <- "results/04_model_dataset.xlsx"
output_file <- "results/05_table1_descriptive.xlsx"

Pr <- read_excel(input_file, sheet = "gwas_derived")

continuous_vars <- c(
  "Edad_r",
  "PSA_r",
  "Gl_FG_Diag",
  "Gl_SC_Diag",
  "Gl_Score_Diag",
  "PTV1_r",
  "dose_fx_r",
  "fx_r",
  "PTV3_r"
)

categorical_vars <- c(
  "TStage_Diag_rec",
  "NStage_Diag",
  "T_r_label",
  "ISUP_Grade",
  "EAU_Risk_Score",
  "EAU_Extent",
  "EAU_Risk_Group",
  "Smoker_r_label",
  "DM_r_label",
  "RA_r_label",
  "SLW_r_label",
  "HTA_r_label",
  "HC_r_label",
  "CardDis_r_label",
  "TUR_r_label",
  "HRR_r_label",
  "HT_Conc_label",
  "Vital_status",
  "Biochemical_rec",
  "Local_rec",
  "Pelvic_rec",
  "Distant_rec"
)

continuous_summary <- lapply(continuous_vars, function(var) {
  x <- Pr[[var]]

  data.frame(
    variable = var,
    n = sum(!is.na(x)),
    missing = sum(is.na(x)),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    p25 = quantile(x, 0.25, na.rm = TRUE),
    p75 = quantile(x, 0.75, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
}) %>%
  bind_rows()

categorical_summary <- lapply(categorical_vars, function(var) {
  Pr %>%
    count(.data[[var]], name = "n") %>%
    mutate(
      variable = var,
      category = as.character(.data[[var]]),
      percent = round(n / sum(n) * 100, 2)
    ) %>%
    dplyr::select(variable, category, n, percent)
}) %>%
  bind_rows()

write_xlsx(
  list(
    continuous_summary = continuous_summary,
    categorical_summary = categorical_summary
  ),
  output_file
)

cat("Archivo creado:\n")
cat(output_file, "\n")
