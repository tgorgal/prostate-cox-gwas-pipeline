# Construcción de variables de supervivencia
library(readxl)
library(dplyr)
library(lubridate)
library(openxlsx)

input_file <- "results/04_model_dataset.xlsx"
output_file <- "results/06_survival_dataset.xlsx"

Pr <- read_excel(input_file, sheet = "gwas_derived")

parse_date <- function(x) {
  dmy(x)
}

Pr_surv <- Pr %>%
  mutate(
    Date_RT_Start = parse_date(Date_RT_Start),
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
      OS_event == 1 ~ as.numeric(Date_exitus - Date_RT_Start),
      OS_event == 0 ~ as.numeric(Last_Last_FU - Date_RT_Start),
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
      BCR_event == 1 ~ as.numeric(Biochemical_rec_date - Date_RT_Start),
      BCR_event == 0 ~ as.numeric(Last_Last_FU - Date_RT_Start),
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
      Local_event == 1 ~ as.numeric(Local_rec_date - Date_RT_Start),
      Local_event == 0 ~ as.numeric(Last_Last_FU - Date_RT_Start),
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
      Pelvic_event == 1 ~ as.numeric(Pelvic_rec_date - Date_RT_Start),
      Pelvic_event == 0 ~ as.numeric(Last_Last_FU - Date_RT_Start),
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
      Distant_event == 1 ~ as.numeric(Distant_rec_date - Date_RT_Start),
      Distant_event == 0 ~ as.numeric(Last_Last_FU - Date_RT_Start),
      TRUE ~ NA_real_
    ),
    Distant_time_months = Distant_time_days / 30.44
  )

# ==========================
# Aviso de tiempos negativos (control de calidad)
# ==========================

neg_times <- Pr_surv %>%
  filter(if_any(ends_with("_time_days"), ~ .x < 0)) %>%
  select(ID, Vital_status, ends_with("_time_days"))

if (nrow(neg_times) > 0) {
  cat("⚠️  Aviso:", nrow(neg_times), "filas con tiempos de supervivencia negativos:\n")
  print(neg_times)
} else {
  cat("✅ Sin tiempos de supervivencia negativos.\n")
}

# ==========================
# Exportar con formato de fecha dd/mm/yyyy
# ==========================

date_cols <- c(
  "Date_RT_Start",
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
