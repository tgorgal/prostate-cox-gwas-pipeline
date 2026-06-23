from pathlib import Path

import pandas as pd

INPUT_FILE = Path("data/2026_03_18-Ca.Prostata_update_MA_v3.0.xlsx")
OUTPUT_FILE = Path("results/01_base_dataset.xlsx")

OUTPUT_FILE.parent.mkdir(parents=True, exist_ok=True)

# Leer hojas
patients = pd.read_excel(INPUT_FILE, sheet_name="Patients")
tx = pd.read_excel(INPUT_FILE, sheet_name="Tx")
clinical = pd.read_excel(INPUT_FILE, sheet_name="Clinical")

# Normalizar nombre Id -> ID para la salida
patients = patients.rename(columns={"Id": "ID"})
tx = tx.rename(columns={"Id": "ID"})
clinical = clinical.rename(columns={"Id": "ID"})

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
    "PTV1_Dose",
    "Dose_fract",
    "N_Doses",
    "PTV3_Dose",
    "HT_Conc",
]

clinical_cols = [
    "Sample_ID",
    "PSA_Diag",
    "TStage_Diag_rec",
    "Gl_Score_Diag",
]

patients_sub = patients[patients_cols].copy()
tx_sub = tx[tx_cols].copy()
clinical_sub = clinical[clinical_cols].copy()

# Merge usando Sample_ID como clave
df = patients_sub.merge(tx_sub, on="Sample_ID", how="left")
df = df.merge(clinical_sub, on="Sample_ID", how="left")

# Fechas
df["Born_Date"] = pd.to_datetime(df["Born_Date"], errors="coerce")
df["Date_RT_Start"] = pd.to_datetime(df["Date_RT_Start"], errors="coerce")

# Edad al inicio de RT en años
df["Age_RT_Start"] = ((df["Date_RT_Start"] - df["Born_Date"]).dt.days / 365.25).round(1)

# Orden final de columnas
final_cols = [
    "ID",
    "Sample_ID",
    "NHC",
    "Born_Date",
    "Date_RT_Start",
    "Age_RT_Start",
    "PSA_Diag",
    "TStage_Diag_rec",
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
]

df = df[final_cols]

# Exportar
with pd.ExcelWriter(OUTPUT_FILE, engine="openpyxl") as writer:
    df.to_excel(writer, index=False, sheet_name="data")

    ws = writer.book["data"]

    # Formato de fecha para columnas concretas
    date_columns = ["Born_Date", "Date_RT_Start"]

    for col_name in date_columns:
        col_idx = df.columns.get_loc(col_name) + 1

        for row in range(2, len(df) + 2):  # desde fila 2 porque fila 1 es cabecera
            ws.cell(row=row, column=col_idx).number_format = "DD/MM/YYYY"

print(f"Archivo creado: {OUTPUT_FILE}")
print(f"Filas: {len(df)}")
print(f"Columnas: {len(df.columns)}")
print(df.head())
