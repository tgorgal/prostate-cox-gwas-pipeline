# Script para limpiar el dataset base y generar un dataset limpio,
# junto con un archivo de advertencias:
#    - limpia espacios y comillas
#    - corrige categorías mal escritas
#    - convierte -9 en valores perdidos
#    - convierte fechas
#    - cambio fecha errónea
#    - elimina filas con PSA no numérico
#    - convierte PSA a número
#    - calcula edad al inicio de radioterapia
#    - limpia variables de seguimiento y eventos
#    - detecta categorías poco frecuentes
#    - corrige Last_Last_FU respecto a otras fechas de seguimiento
#    - guarda el dataset limpio y los avisos.

import re
from pathlib import Path

import pandas as pd

INPUT_FILE = Path("results/01_base_dataset.xlsx")
OUTPUT_FILE = Path("results/03_clean_dataset.xlsx")
WARNINGS_FILE = Path("results/cleaning/03_cleaning_warnings.xlsx")

OUTPUT_FILE.parent.mkdir(parents=True, exist_ok=True)
WARNINGS_FILE.parent.mkdir(parents=True, exist_ok=True)

df = pd.read_excel(INPUT_FILE, dtype=object)

id_cols = ["ID", "Sample_ID", "NHC"]
date_cols = [
    "Born_Date",
    "Diag_Date",
    "Date_RT_Start",
    "Date_RT_End",
    "Last_Last_FU",
    "Date_last_FU",
    "Date_exitus",
    "Biochemical_rec_date",
    "Local_rec_date",
    "Pelvic_rec_date",
    "Distant_rec_date",
    "Date_second_tumor",
]

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
    """Convierte un valor a fecha. Devuelve pd.NaT si es nulo o -9."""
    if pd.isna(x):
        return pd.NaT

    if isinstance(x, pd.Timestamp):
        return x

    s = str(x).strip()

    if s in {"", "-9", "-9.0", "unknown", "desconocido", "nan", "NaN", "<NA>"}:
        return pd.NaT

    if isinstance(x, (int, float)) and not isinstance(x, bool):
        return pd.to_datetime(x, unit="D", origin="1899-12-30", errors="coerce")

    if s.replace(".0", "").isdigit() and len(s.replace(".0", "")) == 5:
        return pd.to_datetime(float(s), unit="D", origin="1899-12-30", errors="coerce")

    if re.match(r"^\d{4}-\d{2}-\d{2}", s):
        return pd.to_datetime(s, errors="coerce")

    return pd.to_datetime(s, format="%d/%m/%Y", errors="coerce")


def extract_first_date(x):
    """Extrae la primera fecha en formato dd/mm/yyyy de una cadena.
    Devuelve pd.NaT si no se encuentra ninguna fecha válida."""
    if pd.isna(x):
        return pd.NaT

    s = str(x).strip()

    if s in {"", "-9", "-9.0", "unknown", "desconocido", "nan", "NaN", "<NA>"}:
        return pd.NaT

    match = re.search(r"\b\d{1,2}/\d{1,2}/\d{4}\b", s)

    if match:
        return pd.to_datetime(match.group(0), dayfirst=True, errors="coerce")

    return convert_excel_serial_date(s)


# ==========================
# Limpieza inicial de texto
# ==========================

df = strip_text_columns(df)

# Corregir valores conocidos antes de cualquier auditoría o conversión
df["HT_Conc"] = df["HT_Conc"].replace({"No": "no"})

df["Smoker"] = df["Smoker"].replace(
    {
        "ex-moker": "ex-smoker",
        "smoker": "yes",
        0: -9,
        "0": -9,
    }
)

df["TStage_Diag_rec"] = df["TStage_Diag_rec"].replace({"T1C": "T1c"})

df["Vital_status"] = df["Vital_status"].replace({"unknown": -9})

df["Date_exitus"] = df["Date_exitus"].replace({"unknown": -9, "": -9, " ": -9})

df["Biochemical_rec"] = df["Biochemical_rec"].replace({"si": "yes", "yer": "yes"})

df["Local_rec"] = df["Local_rec"].replace({"desconocido": -9, "si": "yes"})

df["Pelvic_rec"] = df["Pelvic_rec"].replace({"desconocido": -9, "si": "yes"})

df["Distant_rec"] = df["Distant_rec"].replace({"desconocido": -9})

df["Date_second_tumor"] = df["Date_second_tumor"].replace({"desconocido": -9})

# Convertir Gleason a numérico (necesario para la auditoría)
for col in ["Gl_FG_Diag", "Gl_SC_Diag", "Gl_Score_Diag"]:
    df[col] = pd.to_numeric(df[col], errors="coerce")

# ==========================
# Auditoría antes de convertir -9 a NA
# (sobre datos ya corregidos textualmente, pero con -9 aún presentes)
# ==========================

warnings = {}

# PSA no numérico (excluyendo missing explícitos)
psa_raw = df["PSA_Diag"].astype(str).str.strip()
psa_num = pd.to_numeric(psa_raw, errors="coerce")

mask_psa_non_numeric = (
    psa_raw.notna()
    & (psa_raw != "")
    & (~psa_raw.isin(["-9", "-9.0", "nan", "NaN", "<NA>"]))
    & psa_num.isna()
)

warnings["psa_non_numeric"] = df.loc[
    mask_psa_non_numeric, ["ID", "Sample_ID", "NHC", "PSA_Diag"]
]

# Inconsistencias en Gleason
# Se exige >= 1 en los tres componentes para excluir -9 aún no convertidos a NA
# y para aplicar el límite clínico válido (ningún componente puede ser 0 o negativo)
warnings["gleason_inconsistent"] = df[
    df["Gl_FG_Diag"].notna()
    & df["Gl_SC_Diag"].notna()
    & df["Gl_Score_Diag"].notna()
    & (df["Gl_FG_Diag"] >= 1)
    & (df["Gl_SC_Diag"] >= 1)
    & (df["Gl_Score_Diag"] >= 1)
    & ((df["Gl_FG_Diag"] + df["Gl_SC_Diag"]) != df["Gl_Score_Diag"])
][["ID", "Sample_ID", "NHC", "Gl_FG_Diag", "Gl_SC_Diag", "Gl_Score_Diag"]]


# Gleason 7 sin desglose (quedará ISUP_Grade = NA)
# Cubre tanto NA como valores inválidos (<= 0, p.ej. -9 aún no convertido)
def _is_missing_or_invalid(s: pd.Series) -> pd.Series:
    return s.isna() | (s <= 0)


warnings["gleason7_no_breakdown"] = df[
    (df["Gl_Score_Diag"] == 7)
    & (
        _is_missing_or_invalid(df["Gl_FG_Diag"])
        | _is_missing_or_invalid(df["Gl_SC_Diag"])
    )
][["ID", "Sample_ID", "NHC", "Gl_FG_Diag", "Gl_SC_Diag", "Gl_Score_Diag"]]

# Fechas con formato serial de Excel
# (excluye -9 y -9.0 para evitar falsos positivos)
SERIAL_PATTERN = r"^\d+(\.0)?$"
SERIAL_EXCLUDE = {"-9", "-9.0"}

for col in date_cols:
    if col in df.columns:
        s = df[col].astype(str).str.strip()
        mask_serial = s.str.fullmatch(SERIAL_PATTERN, na=False) & ~s.isin(
            SERIAL_EXCLUDE
        )
        warnings[f"{col}_serial_like"] = df.loc[
            mask_serial, ["ID", "Sample_ID", "NHC", col]
        ]

# Date_second_tumor con varias fechas
if "Date_second_tumor" in df.columns:
    s = df["Date_second_tumor"].astype(str).str.strip()
    warnings["Date_second_tumor_multiple"] = df[
        s.str.count(r"\b\d{1,2}/\d{1,2}/\d{4}\b") > 1
    ][["ID", "Sample_ID", "NHC", "Date_second_tumor"]]

# Guardar índices de filas con PSA no numérico para eliminarlas después
idx_psa_non_numeric = df.index[mask_psa_non_numeric]

# ==========================
# Conversión de missing
# ==========================

# Rellenar vacíos con -9 solo en columnas no identificadoras y no de fecha
non_date_non_id_cols = [
    c for c in df.columns if c not in id_cols and c not in date_cols
]

for col in non_date_non_id_cols:
    df[col] = df[col].replace(["", " ", "nan", "NaN"], -9)
    df[col] = df[col].fillna(-9)

# Convertir -9 a NA en columnas no identificadoras (incluye fechas)
for col in df.columns:
    if col not in id_cols:
        df[col] = df[col].replace([-9, "-9", "-9.0"], pd.NA)

# ==========================
# Conversión de fechas
# (después de sustituir -9 por NA para que las funciones
# reciban NA directamente y devuelvan NaT)
# ==========================

df["Born_Date"] = pd.Series(
    [convert_excel_serial_date(x) for x in df["Born_Date"]],
    index=df.index,
)

df["Diag_Date"] = pd.Series(
    [convert_excel_serial_date(x) for x in df["Diag_Date"]],
    index=df.index,
)

df["Date_RT_Start"] = pd.Series(
    [convert_excel_serial_date(x) for x in df["Date_RT_Start"]],
    index=df.index,
)

df["Date_RT_End"] = pd.Series(
    [convert_excel_serial_date(x) for x in df["Date_RT_End"]],
    index=df.index,
)

df["Last_Last_FU"] = pd.Series(
    [convert_excel_serial_date(x) for x in df["Last_Last_FU"]],
    index=df.index,
)

df["Date_last_FU"] = pd.Series(
    [convert_excel_serial_date(x) for x in df["Date_last_FU"]],
    index=df.index,
)

df["Date_exitus"] = pd.Series(
    [convert_excel_serial_date(x) for x in df["Date_exitus"]],
    index=df.index,
)

df["Biochemical_rec_date"] = pd.Series(
    [convert_excel_serial_date(x) for x in df["Biochemical_rec_date"]],
    index=df.index,
)

df["Local_rec_date"] = pd.Series(
    [convert_excel_serial_date(x) for x in df["Local_rec_date"]],
    index=df.index,
)

df["Pelvic_rec_date"] = pd.Series(
    [convert_excel_serial_date(x) for x in df["Pelvic_rec_date"]],
    index=df.index,
)

df["Distant_rec_date"] = pd.Series(
    [convert_excel_serial_date(x) for x in df["Distant_rec_date"]],
    index=df.index,
)

df["Date_second_tumor"] = pd.Series(
    [extract_first_date(x) for x in df["Date_second_tumor"]],
    index=df.index,
)

for col in date_cols:
    if col in df.columns:
        df[col] = pd.to_datetime(df[col], errors="coerce")

# ==========================
# Corrección de Last_Last_FU respecto a otras fechas de seguimiento
# ==========================

other_fu_cols = [
    "Diag_Date",
    "Date_RT_Start",
    "Date_RT_End",
    "Date_last_FU",
    "Biochemical_rec_date",
    "Local_rec_date",
    "Pelvic_rec_date",
    "Distant_rec_date",
    "Date_second_tumor",
]
other_fu_cols = [c for c in other_fu_cols if c in df.columns]

max_others = df[other_fu_cols].max(axis=1, skipna=True)

exitus_conflict_rows = []
last_fu_correction_rows = []

new_last_last_fu = df["Last_Last_FU"].copy()

for idx in df.index:
    exitus = df.at[idx, "Date_exitus"]
    lfu = df.at[idx, "Last_Last_FU"]
    mx = max_others.at[idx]

    exitus_has_conflict = False

    # --- Chequeo de exitus: nunca se corrige automáticamente ---
    if pd.notna(exitus):
        if (pd.notna(mx) and exitus < mx) or (pd.notna(lfu) and exitus < lfu):
            exitus_has_conflict = True
            exitus_conflict_rows.append(
                {
                    "ID": df.at[idx, "ID"],
                    "Sample_ID": df.at[idx, "Sample_ID"],
                    "NHC": df.at[idx, "NHC"],
                    "Date_exitus": exitus,
                    "Last_Last_FU": lfu,
                    "max_other_dates": mx,
                }
            )

    # --- Corrección de Last_Last_FU, solo si no hay conflicto de exitus ---
    if not exitus_has_conflict:
        if pd.isna(lfu):
            new_value = (
                mx if pd.isna(exitus) else (exitus if pd.isna(mx) else max(mx, exitus))
            )
            if pd.notna(new_value):
                new_last_last_fu.at[idx] = new_value
                last_fu_correction_rows.append(
                    {
                        "ID": df.at[idx, "ID"],
                        "Sample_ID": df.at[idx, "Sample_ID"],
                        "NHC": df.at[idx, "NHC"],
                        "old_Last_Last_FU": pd.NA,
                        "new_Last_Last_FU": new_value,
                        "reason": "Last_Last_FU vacío, rellenado con la fecha más reciente disponible",
                    }
                )
        elif pd.notna(mx) and lfu < mx:
            new_last_last_fu.at[idx] = mx
            last_fu_correction_rows.append(
                {
                    "ID": df.at[idx, "ID"],
                    "Sample_ID": df.at[idx, "Sample_ID"],
                    "NHC": df.at[idx, "NHC"],
                    "old_Last_Last_FU": lfu,
                    "new_Last_Last_FU": mx,
                    "reason": "Last_Last_FU anterior a otra fecha de seguimiento",
                }
            )
        elif pd.notna(exitus) and lfu > exitus:
            new_last_last_fu.at[idx] = exitus
            last_fu_correction_rows.append(
                {
                    "ID": df.at[idx, "ID"],
                    "Sample_ID": df.at[idx, "Sample_ID"],
                    "NHC": df.at[idx, "NHC"],
                    "old_Last_Last_FU": lfu,
                    "new_Last_Last_FU": exitus,
                    "reason": "Last_Last_FU posterior a la fecha de éxitus, recortado",
                }
            )

df["Last_Last_FU"] = new_last_last_fu

warnings["exitus_date_conflict"] = pd.DataFrame(exitus_conflict_rows)
warnings["last_last_fu_corrections"] = pd.DataFrame(last_fu_correction_rows)


# ==========================
# Corrección manual de fecha de nacimiento errónea
# ==========================

df.loc[df["Sample_ID"] == "4E008", "Born_Date"] = pd.to_datetime(
    "23/12/1934", dayfirst=True
)

# ==========================
# Eliminar filas con PSA_Diag no numérico
# ==========================

df = df.drop(index=idx_psa_non_numeric).copy()

# PSA a numérico
df["PSA_Diag"] = pd.to_numeric(df["PSA_Diag"], errors="coerce")

# ==========================
# Reordenación de columnas
# ==========================

# Edad al inicio de RT
df["Age_RT_Start"] = ((df["Date_RT_Start"] - df["Born_Date"]).dt.days / 365.25).round(1)

# Age_RT_Start antes de PSA_Diag
cols = list(df.columns)
cols.remove("Age_RT_Start")
psa_pos = cols.index("PSA_Diag")
cols = cols[:psa_pos] + ["Age_RT_Start"] + cols[psa_pos:]
df = df[cols]

# ==========================
# Auditoría de categorías
# ==========================

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
    "Vital_status",
    "Biochemical_rec",
    "Local_rec",
    "Pelvic_rec",
    "Distant_rec",
]

# Categorías con menos del 10 %
rare_rows = []

for col in categorical_cols:
    if col not in df.columns:
        continue
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
    if col not in df.columns:
        continue
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

    for col_name in date_cols:
        if col_name in df.columns:
            col_idx = df.columns.get_loc(col_name) + 1  # type: ignore
            for row in range(2, len(df) + 2):
                ws.cell(row=row, column=col_idx).number_format = "DD/MM/YYYY"  # type: ignore

with pd.ExcelWriter(WARNINGS_FILE, engine="openpyxl") as writer:
    for name, table in warnings.items():
        table.to_excel(writer, index=False, sheet_name=name[:31])

print(f"Archivo limpio creado: {OUTPUT_FILE}")
print(f"Archivo de avisos creado: {WARNINGS_FILE}")
print(f"Filas: {len(df)}")
print(f"Columnas: {len(df.columns)}")

for name, table in warnings.items():
    print(f"  {name}: {len(table)} casos")
