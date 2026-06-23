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

# Orden final de columnas
final_cols = [
    "ID",
    "Sample_ID",
    "NHC",
    "Born_Date",
    "Date_RT_Start",
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

df.to_excel(OUTPUT_FILE, index=False)

print(f"Archivo creado: {OUTPUT_FILE}")
print(f"Filas: {len(df)}")
print(f"Columnas: {len(df.columns)}")
print(df.head())
