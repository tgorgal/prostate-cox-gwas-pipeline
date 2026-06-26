# Prostate Cox-GWAS Pipeline

Pipeline for preprocessing clinical data and preparing covariates for Cox proportional hazards genome-wide association studies (Cox-GWAS) in prostate cancer.

## Overview

This repository contains a reproducible workflow to:

* Extract relevant clinical variables from a multi-sheet Excel database.
* Validate patient identifiers across sheets.
* Generate a unified clinical dataset.
* Profile and clean clinical variables.
* Recode variables for statistical analyses.
* Perform missing data imputation using MICE.
* Prepare covariates for downstream Cox-GWAS analyses.

## Project structure

```text
.
├── data/              # Input data (ignored by Git)
├── results/           # Generated datasets and reports (ignored by Git)
├── scripts/
│   ├── 00_check_keys.py
│   ├── 01_create_base_excel.py
│   ├── 02_profile_dataset.py
│   ├── 03_clean_dataset.py
│   └── 04_imputation_variable_selection.R
├── README.md
└── pyproject.toml
```

## Pipeline

1. **00_check_keys.py**

   * Validates that patient identifiers match across Excel sheets.

2. **01_create_base_excel.py**

   * Extracts selected variables from the original Excel workbook.

3. **02_profile_dataset.py**

   * Generates descriptive reports to identify inconsistencies and unusual values.

4. **03_clean_dataset.py**

   * Cleans dates and categorical variables.
   * Standardizes missing values.
   * Removes invalid records.
   * Generates a cleaning report.

5. **04_imputation_variable_selection.R**

   * Creates datasets for statistical modelling.
   * Performs MICE imputation.
   * Exports covariates ready for Cox-GWAS analyses.

## Requirements

### Python

* Python ≥ 3.12
* uv

Install dependencies with:

```bash
uv sync
```

### R

Required packages:

* readxl
* dplyr
* mice
* MASS
* broom
* glmnet
* writexl

## Data

Clinical data are **not included** in this repository because they contain sensitive patient information.

The `data/` and `results/` directories are excluded from version control.

## Author

Tania Gorgal
