# Construcción de variables de supervivencia
library(readxl)
library(dplyr)
library(lubridate)
library(openxlsx)

input_file <- "results/04_model_dataset.xlsx"
output_file <- "results/06_survival_dataset.xlsx"
warning_file <- "results/warnings/06_negative_survival_times.xlsx"
dir.create("results/warnings", showWarnings = FALSE, recursive = TRUE)

Pr <- read_excel(input_file, sheet = "gwas_derived")

parse_date <- function(x) {
  dmy(x)
}

Pr_surv <- Pr %>%
  mutate(
    Date_RT_End = parse_date(Date_RT_End),
    Last_Last_FU = parse_date(Last_Last_FU),
    Date_exitus = parse_date(Date_exitus),
    Biochemical_rec_date = parse_date(Biochemical_rec_date),
    Local_rec_date = parse_date(Local_rec_date),
    Pelvic_rec_date = parse_date(Pelvic_rec_date),
    Distant_rec_date = parse_date(Distant_rec_date),

    # Supervivencia global (OS)
    OS_event = case_when(
      Vital_status == "dead" ~ 1,
      Vital_status == "alive" ~ 0,
      TRUE ~ NA_real_
    ),
    OS_time_days = case_when(
      OS_event == 1 ~ as.numeric(Date_exitus - Date_RT_End),
      OS_event == 0 ~ as.numeric(Last_Last_FU - Date_RT_End),
      TRUE ~ NA_real_
    ),
    OS_time_months = OS_time_days / 30.44,

    # Supervivencia libre de recurrencia bioquímica (BCR)
    BCR_event = case_when(
      Biochemical_rec == "yes" ~ 1,
      Biochemical_rec == "no" ~ 0,
      TRUE ~ NA_real_
    ),
    BCR_time_days = case_when(
      BCR_event == 1 ~ as.numeric(Biochemical_rec_date - Date_RT_End),
      BCR_event == 0 ~ as.numeric(Last_Last_FU - Date_RT_End),
      TRUE ~ NA_real_
    ),
    BCR_time_months = BCR_time_days / 30.44,

    # Supervivencia local (Local)
    Local_event = case_when(
      Local_rec == "yes" ~ 1,
      Local_rec == "no" ~ 0,
      TRUE ~ NA_real_
    ),
    Local_time_days = case_when(
      Local_event == 1 ~ as.numeric(Local_rec_date - Date_RT_End),
      Local_event == 0 ~ as.numeric(Last_Last_FU - Date_RT_End),
      TRUE ~ NA_real_
    ),
    Local_time_months = Local_time_days / 30.44,

    # Supervivencia pélvica (Pelvic)
    Pelvic_event = case_when(
      Pelvic_rec == "yes" ~ 1,
      Pelvic_rec == "no" ~ 0,
      TRUE ~ NA_real_
    ),
    Pelvic_time_days = case_when(
      Pelvic_event == 1 ~ as.numeric(Pelvic_rec_date - Date_RT_End),
      Pelvic_event == 0 ~ as.numeric(Last_Last_FU - Date_RT_End),
      TRUE ~ NA_real_
    ),
    Pelvic_time_months = Pelvic_time_days / 30.44,

    # Supervivencia distante (Distant)
    Distant_event = case_when(
      Distant_rec == "yes" ~ 1,
      Distant_rec == "no" ~ 0,
      TRUE ~ NA_real_
    ),
    Distant_time_days = case_when(
      Distant_event == 1 ~ as.numeric(Distant_rec_date - Date_RT_End),
      Distant_event == 0 ~ as.numeric(Last_Last_FU - Date_RT_End),
      TRUE ~ NA_real_
    ),
    Distant_time_months = Distant_time_days / 30.44
  )

# ==========================
# Detectar y excluir pacientes con tiempos negativos
#
# Un tiempo negativo en cualquier endpoint indica, en esta cohorte,
# que el paciente ha recibido un segundo curso de radioterapia (RT).
# Estos pacientes aparecen como "salvage" en RT_Intent de Tx.
# En estos casos, Date_RT_End no representa de forma fiable el origen
# temporal del seguimiento, y las fechas de recidiva/último contacto
# generan ruido también en el resto de variables derivadas de fecha.
# Por ello, el paciente se excluye COMPLETAMENTE de Pr_surv (no solo
# del endpoint puntual que disparó el tiempo negativo).
# ==========================

time_cols <- c(
  "OS_time_days", "BCR_time_days", "Local_time_days",
  "Pelvic_time_days", "Distant_time_days"
)

is_negative_row <- Pr_surv %>%
  transmute(across(all_of(time_cols), ~ !is.na(.x) & .x < 0)) %>%
  rowSums() > 0

# Detalle de qué endpoint(s) dispararon la exclusión, por paciente
neg_detail <- Pr_surv %>%
  filter(is_negative_row) %>%
  rowwise() %>%
  mutate(
    negative_endpoints = paste(
      time_cols[c_across(all_of(time_cols)) < 0 & !is.na(c_across(all_of(time_cols)))],
      collapse = ", "
    )
  ) %>%
  ungroup() %>%
  dplyr::select(
    ID, Vital_status, negative_endpoints,
    Date_RT_End, Last_Last_FU, Date_exitus,
    Biochemical_rec_date, Local_rec_date, Pelvic_rec_date, Distant_rec_date,
    all_of(time_cols)
  ) %>%
  mutate(
    likely_reason = "Probable segundo curso de RT (retratamiento): Date_RT_End no representa el origen real del seguimiento para este paciente",
    .after = negative_endpoints
  )

if (nrow(neg_detail) > 0) {
  cat("⚠️ ", nrow(neg_detail),
      " pacientes excluidos por completo del dataset de supervivencia",
      " (tiempo negativo en al menos un endpoint; probable retratamiento con RT):\n", sep = "")
  print(neg_detail, width = Inf)

  write.xlsx(neg_detail, warning_file, overwrite = TRUE)
} else {
  cat("✅ Sin pacientes con tiempos de supervivencia negativos.\n")
}

# Excluir del dataset final a los pacientes marcados
Pr_surv <- Pr_surv %>%
  filter(!is_negative_row)

# ==========================
# Exportar con formato de fecha dd/mm/yyyy
# ==========================

date_cols <- c(
  "Date_RT_End",
  "Last_Last_FU",
  "Date_exitus",
  "Biochemical_rec_date",
  "Local_rec_date",
  "Pelvic_rec_date",
  "Distant_rec_date"
)

wb <- createWorkbook()
addWorksheet(wb, "survival_dataset")
writeData(wb, "survival_dataset", Pr_surv)

date_style <- createStyle(numFmt = "dd/mm/yyyy")

for (col_name in date_cols) {
  col_idx <- which(names(Pr_surv) == col_name)
  addStyle(
    wb,
    sheet = "survival_dataset",
    style = date_style,
    rows = 2:(nrow(Pr_surv) + 1),
    cols = col_idx,
    gridExpand = TRUE
  )
}

saveWorkbook(wb, output_file, overwrite = TRUE)

cat("Archivo creado:\n")
cat(output_file, "\n")
cat("Filas:", nrow(Pr_surv), "\n")
