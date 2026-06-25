library(readxl)
library(dplyr)
library(mice)
library(MASS)
library(broom)
library(glmnet)
library(writexl)


# Preparamos input y output

input_file <- "results/03_clean_dataset.xlsx"

output_model <- "results/04_model_dataset.xlsx"
output_gwas <- "results/04_gwas_covariates.tsv"

set.seed(1)

# Cargar datos

Pr <- read_excel(input_file)

## Recodificación para modelos en R
# Variables como factores

Pr_model <- Pr %>%
  mutate(
    Edad_r = Age_RT_Start,
    PSA_r = PSA_Diag,

    T_r = case_when(
      TStage_Diag_rec %in% c("T1", "T1b", "T1c", "T1C") ~ "T1",
      TStage_Diag_rec %in% c("T2", "T2a", "T2a-b", "T2b", "T2c") ~ "T2",
      TStage_Diag_rec %in% c("T3", "T3a", "T3b", "T3bc", "T3c", "T3-4") ~ "T3",
      TStage_Diag_rec == "T4" ~ "T4",
      TRUE ~ NA_character_
    ),

    Gleason_r = case_when(
      Gl_Score_Diag < 8 ~ "<8",
      Gl_Score_Diag >= 8 ~ "≥8",
      TRUE ~ NA_character_
    ),

    Smoker_r = Smoker,

    PTV1_r = case_when(
      PTV1_Dose < 7000 ~ "<70Gy",
      PTV1_Dose >= 7000 & PTV1_Dose < 7400 ~ "70-73.9Gy",
      PTV1_Dose >= 7400 ~ "≥74Gy",
      TRUE ~ NA_character_
    ),

    dose_fx_r = case_when(
      Dose_fract == 180 ~ "180",
      Dose_fract == 200 ~ "200",
      TRUE ~ NA_character_
    ),

    fx_r = case_when(
      N_Doses < 35 ~ "<35",
      N_Doses >= 35 & N_Doses < 38 ~ "35-37",
      N_Doses >= 38 ~ "≥38",
      TRUE ~ NA_character_
    ),

    PTV3_r = case_when(
      PTV3_Dose == 0 ~ "no",
      PTV3_Dose > 0 ~ "yes",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    T_r = factor(T_r, levels = c("T1", "T2", "T3", "T4")),
    Gleason_r = factor(Gleason_r, levels = c("<8", "≥8")),
    Smoker_r = factor(Smoker_r, levels = c("no", "ex-smoker", "yes")),

    DM_r = factor(DM, levels = c("no", "yes")),
    RA_r = factor(RA, levels = c("no", "yes")),
    SLW_r = factor(SLW, levels = c("no", "yes")),
    HTA_r = factor(HTA, levels = c("no", "yes")),
    HC_r = factor(HC, levels = c("no", "yes")),
    CardDis_r = factor(CardDis, levels = c("no", "yes")),
    TUR_r = factor(TUR, levels = c("no", "yes")),
    HRR_r = factor(HRR, levels = c("no", "yes")),

    PTV1_r = factor(PTV1_r, levels = c("<70Gy", "70-73.9Gy", "≥74Gy")),
    dose_fx_r = factor(dose_fx_r, levels = c("180", "200")),
    fx_r = factor(fx_r, levels = c("<35", "35-37", "≥38")),
    PTV3_r = factor(PTV3_r, levels = c("no", "yes")),
    HT_Conc = factor(HT_Conc, levels = c("no", "yes"))
  )

# Dataset numérico para GWAS

Pr_gwas <- Pr_model %>%
  transmute(
    ID = Sample_ID,

    Edad_r = Edad_r,
    PSA_r = PSA_r,

    T_r = as.numeric(T_r),
    Gleason_r = as.numeric(Gleason_r),
    Smoker_r = as.numeric(Smoker_r) - 1,

    DM_r = as.numeric(DM_r) - 1,
    RA_r = as.numeric(RA_r) - 1,
    SLW_r = as.numeric(SLW_r) - 1,
    HTA_r = as.numeric(HTA_r) - 1,
    HC_r = as.numeric(HC_r) - 1,
    CardDis_r = as.numeric(CardDis_r) - 1,
    TUR_r = as.numeric(TUR_r) - 1,
    HRR_r = as.numeric(HRR_r) - 1,

    PTV1_r = as.numeric(PTV1_r),
    dose_fx_r = as.numeric(dose_fx_r),
    fx_r = as.numeric(fx_r),
    PTV3_r = as.numeric(PTV3_r) - 1,
    HT_Conc = as.numeric(HT_Conc) - 1
  )

# Imputación MICE

covsPr <- c(
  "Edad_r", "PSA_r", "T_r", "Gleason_r", "Smoker_r",
  "DM_r", "RA_r", "SLW_r", "HTA_r", "HC_r", "CardDis_r",
  "TUR_r", "HRR_r", "PTV1_r", "dose_fx_r", "fx_r",
  "PTV3_r", "HT_Conc"
)

Pr_gwas_i <- mice(
  Pr_gwas[, covsPr],
  m = 5,
  seed = 1,
  printFlag = FALSE
)

Pr_gwas_imputed <- Pr_gwas
Pr_gwas_imputed[, covsPr] <- complete(Pr_gwas_i)

# Exportar

write_xlsx(
  list(
    model_dataset = Pr_model,
    gwas_numeric = Pr_gwas,
    gwas_imputed = Pr_gwas_imputed
  ),
  output_model
)

write.table(
  Pr_gwas_imputed,
  output_gwas,
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

cat("Archivos creados:\n")
cat(output_model, "\n")
cat(output_gwas, "\n")
cat("Filas:", nrow(Pr_gwas_imputed), "\n")
