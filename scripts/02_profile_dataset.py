# Script para perfilar el dataset base y generar un informe de perfilado.

from pathlib import Path

import pandas as pd

INPUT_FILE = Path("results/01_base_dataset.xlsx")
OUTPUT_DIR = Path("results/profile")
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

df = pd.read_excel(INPUT_FILE, dtype=str)

report_lines = []


def add(text=""):
    print(text)
    report_lines.append(text)


add("=" * 80)
add("PERFILADO DEL DATASET BASE")
add("=" * 80)
add(f"Archivo: {INPUT_FILE}")
add(f"Filas: {len(df)}")
add(f"Columnas: {len(df.columns)}")

# 1. Valores únicos por columna
add("\n" + "=" * 80)
add("VALORES ÚNICOS POR COLUMNA")
add("=" * 80)

for col in df.columns:
    add(f"\n--- {col} ---")
    add(f"N valores únicos: {df[col].nunique(dropna=False)}")
    vc = df[col].fillna("<EMPTY/NA>").value_counts(dropna=False).head(40)
    add(vc.to_string())

# 2. Detectar empty / NA / -9
add("\n" + "=" * 80)
add("RESUMEN DE VALORES PERDIDOS O SOSPECHOSOS")
add("=" * 80)

for col in df.columns:
    s = df[col].astype(str).str.strip()

    n_empty = df[col].isna().sum() + (s == "").sum()
    n_minus9 = (s == "-9").sum()
    n_zero = (s == "0").sum()

    add(f"{col}: empty={n_empty}, -9={n_minus9}, 0={n_zero}")

# 3. Revisar columnas de fecha
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

add("\n" + "=" * 80)
add("REVISIÓN DE FECHAS")
add("=" * 80)

for col in date_cols:
    add(f"\n--- {col} ---")

    s = df[col].astype(str).str.strip()

    parsed = pd.to_datetime(s, errors="coerce", dayfirst=True)

    suspicious = df[s.notna() & (s != "") & (s != "-9") & parsed.isna()][
        ["ID", "Sample_ID", "NHC", col]
    ]

    numeric_like = df[s.str.fullmatch(r"\d+(\.0)?", na=False)][
        ["ID", "Sample_ID", "NHC", col]
    ]

    add(f"Fechas no interpretables excluyendo -9/empty: {len(suspicious)}")
    add(f"Valores numéricos tipo serial detectados: {len(numeric_like)}")

    if len(suspicious) > 0:
        add("\nEjemplos de fechas no interpretables:")
        add(suspicious.head(20).to_string(index=False))

    if len(numeric_like) > 0:
        add("\nEjemplos de valores numéricos tipo serial:")
        add(numeric_like.head(20).to_string(index=False))

# 4. PSA: valores no numéricos
add("\n" + "=" * 80)
add("REVISIÓN DE PSA_Diag")
add("=" * 80)

psa = df["PSA_Diag"].astype(str).str.strip()
psa_num = pd.to_numeric(psa, errors="coerce")

psa_non_numeric = df[psa.notna() & (psa != "") & (psa != "-9") & psa_num.isna()][
    ["ID", "Sample_ID", "NHC", "PSA_Diag"]
]

add(f"PSA no numéricos excluyendo -9/empty: {len(psa_non_numeric)}")

if len(psa_non_numeric) > 0:
    add(psa_non_numeric.to_string(index=False))

add(f"PSA mínimo numérico: {psa_num.min()}")
add(f"PSA máximo numérico: {psa_num.max()}")

# 5. Revisión específica de textos
text_cols = [
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

add("\n" + "=" * 80)
add("REVISIÓN DE VARIABLES CATEGÓRICAS/TEXTO")
add("=" * 80)

for col in text_cols:
    add(f"\n--- {col} ---")
    s = df[col].astype(str)

    has_spaces = df[s != s.str.strip()][["ID", "Sample_ID", "NHC", col]]
    has_quotes = df[s.str.contains("\"|'", regex=True, na=False)][
        ["ID", "Sample_ID", "NHC", col]
    ]

    add(f"Valores con espacios iniciales/finales: {len(has_spaces)}")
    add(f"Valores con comillas: {len(has_quotes)}")

    add("\nFrecuencias:")
    add(df[col].fillna("<EMPTY/NA>").value_counts(dropna=False).head(40).to_string())

    if len(has_spaces) > 0:
        add("\nEjemplos con espacios:")
        add(has_spaces.head(10).to_string(index=False))

    if len(has_quotes) > 0:
        add("\nEjemplos con comillas:")
        add(has_quotes.head(10).to_string(index=False))

# Guardar informe
report_file = OUTPUT_DIR / "02_profile_report.txt"
report_file.write_text("\n".join(report_lines), encoding="utf-8")

# Guardar valores únicos completos en Excel
unique_rows = []

for col in df.columns:
    counts = df[col].fillna("<EMPTY/NA>").value_counts(dropna=False)
    for value, count in counts.items():
        unique_rows.append(
            {
                "column": col,
                "value": value,
                "count": count,
            }
        )

unique_df = pd.DataFrame(unique_rows)
unique_df.to_excel(OUTPUT_DIR / "02_unique_values.xlsx", index=False)

print("\nInforme guardado en:")
print(report_file)
print(OUTPUT_DIR / "02_unique_values.xlsx")
