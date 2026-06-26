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
      TStage_Diag_rec == "T1" ~ "T1",
      TStage_Diag_rec == "T1b" ~ "T1b",
      TStage_Diag_rec == "T1c" ~ "T1c",
      TStage_Diag_rec %in% c("T2", "T2a-b") ~ "T2",
      TStage_Diag_rec == "T2a" ~ "T2a",
      TStage_Diag_rec == "T2b" ~ "T2b",
      TStage_Diag_rec == "T2c" ~ "T2c",
      TStage_Diag_rec %in% c("T3", "T3bc") ~ "T3",
      TStage_Diag_rec == "T3a" ~ "T3a",
      TStage_Diag_rec == "T3b" ~ "T3b",
      TStage_Diag_rec == "T3c" ~ "T3c",
      TStage_Diag_rec %in% c("T3-4", "T4") ~ "T4",
      TRUE ~ NA_character_
    ),

    Gleason_r = Gl_Score_Diag,
    Smoker_r = Smoker,
    PTV1_r = PTV1_Dose,
    dose_fx_r = Dose_fract,
    fx_r = N_Doses,
    PTV3_r = PTV3_Dose
  ) %>%
  mutate(
    T_r = factor(T_r, levels = c("T1","T1b","T1c",
                                "T2","T2a","T2b","T2c",
                                "T3","T3a","T3b","T3c",
                                "T4")),
    Smoker_r = factor(Smoker_r, levels = c("no", "ex-smoker", "yes")),

    DM_r = factor(DM, levels = c("no", "yes")),
    RA_r = factor(RA, levels = c("no", "yes")),
    SLW_r = factor(SLW, levels = c("no", "yes")),
    HTA_r = factor(HTA, levels = c("no", "yes")),
    HC_r = factor(HC, levels = c("no", "yes")),
    CardDis_r = factor(CardDis, levels = c("no", "yes")),
    TUR_r = factor(TUR, levels = c("no", "yes")),
    HRR_r = factor(HRR, levels = c("no", "yes")),
    #dose_fx_r = factor(dose_fx_r, levels = c("180", "200")),
    HT_Conc = factor(HT_Conc, levels = c("no", "yes"))
  )

# Dataset numérico para GWAS

Pr_gwas <- Pr_model %>%
  transmute(
    ID = Sample_ID,

    Edad_r = as.numeric(Edad_r),
    PSA_r = as.numeric(PSA_r),

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
    PTV3_r = as.numeric(PTV3_r),
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
