# Comprobación de las claves de los ficheros Excel

from pathlib import Path

import pandas as pd

EXCEL_FILE = Path("data/2026_03_18-Ca.Prostata_update_MA_v3.0.xlsx")

# Claves esperadas por hoja (algunas no tienen Sample_ID)
SHEET_KEYS = {
    "Patients": ["Id", "Sample_ID", "NHC"],
    "Tx": ["Id", "Sample_ID", "NHC"],
    "Clinical": ["Id", "Sample_ID", "NHC"],
    "Late": ["Id", "NHC"],
    "Late2": ["Id", "NHC"],
    "PSA": ["Id", "NHC"],
}

# Hojas donde se espera una única fila por paciente (clave ID+NHC única)
ONE_ROW_PER_PATIENT = ["Late", "Late2", "PSA"]

# Códigos que se consideran equivalentes a "valor perdido"
MISSING_CODES = {"-9", "-9.0", "0", "nan", "none", "<na>", ""}

SHEETS = list(SHEET_KEYS.keys())

dfs = {sheet: pd.read_excel(EXCEL_FILE, sheet_name=sheet) for sheet in SHEETS}


def normalize(value):
    """Convierte cualquier valor a string de forma segura, incluso si es NaN o float."""
    if pd.isna(value):
        return "nan"
    return str(value).strip().lower()


print("=" * 70)
print("COMPROBACIÓN DE COLUMNAS")
print("=" * 70)

for sheet, df in dfs.items():
    print(f"\nHoja: {sheet}")
    expected_keys = SHEET_KEYS[sheet]
    n = len(expected_keys)
    first_cols = list(df.columns[:n])

    if first_cols == expected_keys:
        print("✅ Primeras columnas correctas:", first_cols)
    else:
        print("❌ Las primeras columnas no coinciden.")
        print(f"Esperado: {expected_keys}")
        print(f"Encontrado: {first_cols}")

print("\n" + "=" * 70)
print("COMPROBACIÓN DE CLAVES")
print("=" * 70)

reference_keys = SHEET_KEYS["Patients"]
reference = dfs["Patients"][reference_keys]

for sheet in [s for s in SHEETS if s != "Patients"]:
    common_keys = [k for k in SHEET_KEYS[sheet] if k in reference_keys]

    ref_subset = reference[common_keys]
    current = dfs[sheet][common_keys]

    print(f"\nPatients vs {sheet}  (comparando por: {common_keys})")
    print(f"Número de filas: {len(ref_subset)} vs {len(current)}")

    n = min(len(ref_subset), len(current))
    encoding_diff_rows = []
    real_diff_rows = []
    exact_match_count = 0

    for i in range(n):
        row_ref = ref_subset.iloc[i]
        row_cur = current.iloc[i]

        is_exact_match = True
        is_encoding_diff = True

        for key in common_keys:
            v_ref = normalize(row_ref[key])
            v_cur = normalize(row_cur[key])

            if v_ref != v_cur:
                is_exact_match = False

                if not (v_ref in MISSING_CODES and v_cur in MISSING_CODES):
                    is_encoding_diff = False

        if is_exact_match:
            exact_match_count += 1
        elif is_encoding_diff:
            encoding_diff_rows.append(i)
        else:
            real_diff_rows.append(i)

    if exact_match_count == n:
        print("✅ Todas las claves coinciden exactamente.")
        continue

    print(f"✅ Filas idénticas: {exact_match_count}")
    print(
        f"⚠️  Diferencias solo por codificación de missing (-9/0/vacío): {len(encoding_diff_rows)}"
    )
    print(f"❌ Diferencias reales de identidad: {len(real_diff_rows)}")

    if real_diff_rows:
        print("\nDetalle de diferencias reales:")
        for i in real_diff_rows[:20]:
            print(f"\nFila {i}")
            print("Patients")
            print(ref_subset.iloc[i])
            print(f"\n{sheet}")
            print(current.iloc[i])
        if len(real_diff_rows) > 20:
            print(f"\n... y {len(real_diff_rows) - 20} más.")

print("\n" + "=" * 70)
print("COMPROBACIÓN DE DUPLICADOS (ID + NHC)")
print("=" * 70)

for sheet in ONE_ROW_PER_PATIENT:
    df = dfs[sheet]
    dup_mask = df[["Id", "NHC"]].astype(str).duplicated(keep=False)
    n_dup = dup_mask.sum()

    print(f"\nHoja: {sheet}")

    if n_dup == 0:
        print("✅ Sin duplicados de ID + NHC.")
    else:
        print(f"❌ {n_dup} filas implicadas en duplicados de ID + NHC.")
        print(
            df.loc[dup_mask, ["Id", "NHC"]]
            .sort_values(["Id", "NHC"])
            .to_string(index=False)
        )

print("\nFin de la comprobación.")
