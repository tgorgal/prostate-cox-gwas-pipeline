# Crear un dataset base a partir de las hojas del fichero Excel original

from pathlib import Path

import pandas as pd

INPUT_FILE = Path("data/2026_03_18-Ca.Prostata_update_MA_v3.0.xlsx")
OUTPUT_FILE = Path("results/01_base_dataset.xlsx")

OUTPUT_FILE.parent.mkdir(parents=True, exist_ok=True)

# Leer hojas
patients = pd.read_excel(INPUT_FILE, sheet_name="Patients")
tx = pd.read_excel(INPUT_FILE, sheet_name="Tx")
clinical = pd.read_excel(INPUT_FILE, sheet_name="Clinical")
status = pd.read_excel(INPUT_FILE, sheet_name="Status-FU")
late = pd.read_excel(INPUT_FILE, sheet_name="Late")
late2 = pd.read_excel(INPUT_FILE, sheet_name="Late2")
psa_sheet = pd.read_excel(INPUT_FILE, sheet_name="PSA")

# Normalizar nombre Id -> ID para la salida
patients = patients.rename(columns={"Id": "ID"})
tx = tx.rename(columns={"Id": "ID"})
clinical = clinical.rename(columns={"Id": "ID"})
status = status.rename(columns={"Id": "ID"})
late = late.rename(columns={"Id": "ID"})
late2 = late2.rename(columns={"Id": "ID"})
psa_sheet = psa_sheet.rename(columns={"Id": "ID"})

# Selección de columnas
patients_cols = [
    "ID",
    "Sample_ID",
    "NHC",
    "Born_Date",
    "Smoker",
    "DM",
    "RA",
    "SLW",
    "HTA",
    "HC",
    "CardDis",
    "TUR",
    "HRR",
]

tx_cols = [
    "Sample_ID",
    "Date_RT_Start",
    "Date_RT_End",
    "Date_HT",
    "PTV1_Dose",
    "Dose_fract",
    "N_Doses",
    "PTV3_Dose",
    "HT_Conc",
]

clinical_cols = [
    "Sample_ID",
    "Diag_Date",
    "PSA_Diag",
    "TStage_Diag_rec",
    "NStage_Diag",
    "Gl_FG_Diag",
    "Gl_SC_Diag",
    "Gl_Score_Diag",
]

status_cols = [
    "ID",
    "NHC",
    "Vital_status",
    "Last_Last_FU",
    "Date_last_FU",
    "Date_exitus",
    "Biochemical_rec",
    "Local_rec",
    "Pelvic_rec",
    "Distant_rec",
    "Biochemical_rec_date",
    "Local_rec_date",
    "Pelvic_rec_date",
    "Distant_rec_date",
    "Date_second_tumor",
]


patients_sub = patients[patients_cols].copy()
tx_sub = tx[tx_cols].copy()
clinical_sub = clinical[clinical_cols].copy()
status_sub = status[status_cols].copy()

late_cols = ["ID", "NHC", "Last-FU"]
late2_cols = ["ID", "NHC", "Last-FU_Late2"]
psa_cols = ["ID", "NHC", "Last_PSA"]

# Renombramos
late_sub = late[late_cols].rename(columns={"Last-FU": "Last_FU_Late"}).copy()
late2_sub = late2[late2_cols].rename(columns={"Last-FU_Late2": "Last_FU_Late2"}).copy()
psa_sub = psa_sheet[psa_cols].rename(columns={"Last_PSA": "Last_PSA_date"}).copy()


# Merge usando Sample_ID como clave
df = patients_sub.merge(tx_sub, on="Sample_ID", how="left")
df = df.merge(clinical_sub, on="Sample_ID", how="left")
df = df.merge(status_sub, on=["ID", "NHC"], how="left")
df = df.merge(late_sub, on=["ID", "NHC"], how="left")
df = df.merge(late2_sub, on=["ID", "NHC"], how="left")
df = df.merge(psa_sub, on=["ID", "NHC"], how="left")

# Orden final de columnas
final_cols = [
    "ID",
    "Sample_ID",
    "NHC",
    "Born_Date",
    "Diag_Date",
    "Date_RT_Start",
    "Date_RT_End",
    "Date_HT",
    "PSA_Diag",
    "TStage_Diag_rec",
    "NStage_Diag",
    "Gl_FG_Diag",
    "Gl_SC_Diag",
    "Gl_Score_Diag",
    "Smoker",
    "DM",
    "RA",
    "SLW",
    "HTA",
    "HC",
    "CardDis",
    "TUR",
    "HRR",
    "PTV1_Dose",
    "Dose_fract",
    "N_Doses",
    "PTV3_Dose",
    "HT_Conc",
    "Vital_status",
    "Last_Last_FU",
    "Date_last_FU",
    "Date_exitus",
    "Biochemical_rec",
    "Biochemical_rec_date",
    "Local_rec",
    "Local_rec_date",
    "Pelvic_rec",
    "Pelvic_rec_date",
    "Distant_rec",
    "Distant_rec_date",
    "Date_second_tumor",
    "Last_FU_Late",
    "Last_FU_Late2",
    "Last_PSA_date",
]

df = df[final_cols]

date_cols = [
    "Born_Date",
    "Diag_Date",
    "Date_RT_Start",
    "Date_RT_End",
    "Date_HT",
    "Last_Last_FU",
    "Date_last_FU",
    "Date_exitus",
    "Biochemical_rec_date",
    "Local_rec_date",
    "Pelvic_rec_date",
    "Distant_rec_date",
    "Date_second_tumor",
    "Last_FU_Late",
    "Last_FU_Late2",
    "Last_PSA_date",
]

with pd.ExcelWriter(OUTPUT_FILE, engine="openpyxl") as writer:
    df.to_excel(writer, index=False, sheet_name="base_data")

    ws = writer.book["base_data"]

    for col_name in date_cols:
        if col_name in df.columns:
            col_idx = list(df.columns).index(col_name) + 1

            for row in range(2, len(df) + 2):
                cell = ws.cell(row=row, column=col_idx)  # type: ignore

                if cell.value in (-9, "-9", "-9.0"):
                    continue

                cell.number_format = "DD/MM/YYYY"

print(f"Archivo creado: {OUTPUT_FILE}")
print(f"Filas: {len(df)}")
print(f"Columnas: {len(df.columns)}")
print(df.head())
