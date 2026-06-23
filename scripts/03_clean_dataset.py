from pathlib import Path

import pandas as pd

INPUT_FILE = Path("results/01_base_dataset.xlsx")
OUTPUT_FILE = Path("results/03_clean_dataset.xlsx")
WARNINGS_FILE = Path("results/cleaning/03_cleaning_warnings.xlsx")

OUTPUT_FILE.parent.mkdir(parents=True, exist_ok=True)
WARNINGS_FILE.parent.mkdir(parents=True, exist_ok=True)

df = pd.read_excel(INPUT_FILE, dtype=object)

id_cols = ["ID", "Sample_ID", "NHC"]
date_cols = ["Born_Date", "Date_RT_Start"]

# ==========================
# Funciones auxiliares
# ==========================


def strip_text_columns(data):
    for col in data.columns:
        if col not in id_cols:
            data[col] = data[col].apply(
                lambda x: x.strip().strip('"').strip("'") if isinstance(x, str) else x
            )
    return data


def convert_excel_serial_date(x):
    if pd.isna(x) or str(x).strip() == "-9":
        return pd.NA

    if isinstance(x, (int, float)) and not isinstance(x, bool):
        return pd.to_datetime(x, unit="D", origin="1899-12-30", errors="coerce")

    s = str(x).strip()

    if s.replace(".0", "").isdigit() and len(s.replace(".0", "")) == 5:
        return pd.to_datetime(float(s), unit="D", origin="1899-12-30", errors="coerce")

    return pd.to_datetime(s, errors="coerce")


# ==========================
# Auditoría antes de limpiar
# ==========================

warnings = {}

# PSA no numérico
psa_raw = df["PSA_Diag"].astype(str).str.strip()
psa_num = pd.to_numeric(psa_raw, errors="coerce")

warnings["psa_non_numeric"] = df[
    psa_raw.notna() & (psa_raw != "") & (psa_raw != "-9") & psa_num.isna()
][["ID", "Sample_ID", "NHC", "PSA_Diag"]]

# Fechas seriales
for col in date_cols:
    s = df[col].astype(str).str.strip()
    warnings[f"{col}_serial_like"] = df[s.str.fullmatch(r"\d+(\.0)?", na=False)][
        ["ID", "Sample_ID", "NHC", col]
    ]

# ==========================
# Limpieza
# ==========================

df = strip_text_columns(df)

# Corregir valores conocidos
df["HT_Conc"] = df["HT_Conc"].replace({"No": "no"})

df["Smoker"] = df["Smoker"].replace(
    {
        "ex-moker": "ex-smoker",
        "smoker": "yes",
        0: -9,
        "0": -9,
    }
)

df["TStage_Diag_rec"] = df["TStage_Diag_rec"].replace(
    {
        "T1C": "T1c",
    }
)

# Vacíos a -9 en columnas no identificadoras
for col in df.columns:
    if col not in id_cols:
        df[col] = df[col].replace(["", " ", "nan", "NaN"], -9)
        df[col] = df[col].fillna(-9)

# Convertir -9 a NA
for col in df.columns:
    if col not in id_cols:
        df[col] = df[col].replace([-9, "-9", "-9.0"], pd.NA)

# Fechas
df["Born_Date"] = pd.to_datetime(
    df["Born_Date"].apply(convert_excel_serial_date),
    errors="coerce",
)

df["Date_RT_Start"] = pd.to_datetime(
    df["Date_RT_Start"].apply(convert_excel_serial_date),
    errors="coerce",
)

# Eliminar filas con PSA_Diag no numérico distinto de missing
psa_raw_clean = df["PSA_Diag"].astype(str).str.strip()
psa_num_clean = pd.to_numeric(psa_raw_clean, errors="coerce")

rows_psa_non_numeric = (
    psa_raw_clean.notna()
    & (psa_raw_clean != "")
    & (psa_raw_clean != "-9")
    & psa_num_clean.isna()
)

df = df.loc[~rows_psa_non_numeric].copy()

# PSA numérico
df["PSA_Diag"] = pd.to_numeric(df["PSA_Diag"], errors="coerce")

# Edad al inicio de RT
df["Age_RT_Start"] = ((df["Date_RT_Start"] - df["Born_Date"]).dt.days / 365.25).round(1)

# Insertar Age_RT_Start antes de PSA_Diag
cols = list(df.columns)
cols.remove("Age_RT_Start")
psa_pos = cols.index("PSA_Diag")
cols = cols[:psa_pos] + ["Age_RT_Start"] + cols[psa_pos:]
df = df[cols]

# Detectar categorías minoritarias (<10%)
categorical_cols = [
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
    "HT_Conc",
]

rare_rows = []

for col in categorical_cols:
    counts = df[col].value_counts(dropna=False)
    total = counts.sum()

    for category, count in counts.items():
        percent = count / total * 100

        if percent < 10:
            rare_rows.append(
                {
                    "variable": col,
                    "category": category,
                    "n": count,
                    "percent": round(percent, 2),
                }
            )

warnings["categ_under_10_percent"] = pd.DataFrame(rare_rows)

# Resumen completo de categorías
categorical_rows = []

for col in categorical_cols:
    counts = df[col].fillna("<NA>").value_counts(dropna=False)
    total = counts.sum()

    for category, count in counts.items():
        categorical_rows.append(
            {
                "variable": col,
                "category": category,
                "n": count,
                "percent": round(count / total * 100, 2),
            }
        )

warnings["categorical_summary"] = pd.DataFrame(categorical_rows)


# ==========================
# Exportar
# ==========================

with pd.ExcelWriter(OUTPUT_FILE, engine="openpyxl") as writer:
    df.to_excel(writer, index=False, sheet_name="clean_data")

    ws = writer.book["clean_data"]

    for col_name in ["Born_Date", "Date_RT_Start"]:
        col_idx = df.columns.get_loc(col_name) + 1
        for row in range(2, len(df) + 2):
            ws.cell(row=row, column=col_idx).number_format = "DD/MM/YYYY"

with pd.ExcelWriter(WARNINGS_FILE, engine="openpyxl") as writer:
    for name, table in warnings.items():
        table.to_excel(writer, index=False, sheet_name=name[:31])

print(f"Archivo limpio creado: {OUTPUT_FILE}")
print(f"Archivo de avisos creado: {WARNINGS_FILE}")
print(f"Filas: {len(df)}")
print(f"Columnas: {len(df.columns)}")

for name, table in warnings.items():
    print(f"{name}: {len(table)} casos")
