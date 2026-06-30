# Comprobación de las claves de los ficheros Excel

from pathlib import Path

import pandas as pd

EXCEL_FILE = Path("data/2026_03_18-Ca.Prostata_update_MA_v3.0.xlsx")

SHEETS = ["Patients", "Tx", "Clinical"]
KEYS = ["Id", "Sample_ID", "NHC"]

dfs = {sheet: pd.read_excel(EXCEL_FILE, sheet_name=sheet) for sheet in SHEETS}

print("=" * 70)
print("COMPROBACIÓN DE COLUMNAS")
print("=" * 70)

for sheet, df in dfs.items():
    print(f"\nHoja: {sheet}")

    first_three = list(df.columns[:3])

    if first_three == KEYS:
        print("✅ Primeras columnas correctas:", first_three)
    else:
        print("❌ Las tres primeras columnas no coinciden.")
        print(first_three)

print("\n" + "=" * 70)
print("COMPROBACIÓN DE CLAVES")
print("=" * 70)

reference = dfs["Patients"][KEYS].astype(str)

for sheet in ["Tx", "Clinical"]:
    current = dfs[sheet][KEYS].astype(str)

    print(f"\nPatients vs {sheet}")

    print(f"Número de filas: {len(reference)} vs {len(current)}")

    if reference.equals(current):
        print("✅ Todas las claves coinciden exactamente.")
    else:
        print("❌ Existen diferencias.")

        n = min(len(reference), len(current))

        for i in range(n):
            if not reference.iloc[i].equals(current.iloc[i]):
                print(f"\nPrimera diferencia en la fila {i}")
                print("\nPatients")
                print(reference.iloc[i])
                print(f"\n{sheet}")
                print(current.iloc[i])
                break

print("\nFin de la comprobación.")
