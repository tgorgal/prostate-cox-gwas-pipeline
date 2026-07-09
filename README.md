# Prostate Survival & Cox-GWAS Pipeline

Pipeline for preprocessing clinical data, performing survival analyses, and preparing covariates for Cox proportional hazards genome-wide association studies (Cox-GWAS) in prostate cancer.

## Main objective

The aim of this project is to identify clinical and genetic factors associated with disease progression in prostate cancer patients treated with radiotherapy.

The pipeline integrates, cleans, and analyzes clinical data to answer questions such as:

- Which patients are at higher risk of recurrence?
- Which clinical characteristics are associated with poorer outcomes?
- Are there genetic variants associated with recurrence or survival after treatment?

Unlike traditional analyses that only consider whether an event occurs, this pipeline also incorporates **when** the event occurs, allowing robust time-to-event (survival) analyses.

Ultimately, the processed clinical data are used to build adjusted Cox regression models and to generate covariates for downstream genome-wide association studies (GWAS).

---

## Overview

This repository contains a reproducible workflow to:

- Extract relevant clinical variables from a multi-sheet Excel database.
- Validate patient identifiers across sheets.
- Generate a unified clinical dataset.
- Profile and clean clinical variables.
- Standardize dates and missing values.
- Calculate derived clinical variables.
- Perform missing data imputation using MICE.
- Generate descriptive statistics.
- Build survival datasets.
- Perform univariate Cox proportional hazards analyses.
- Select candidate covariates for multivariable survival models.
- Prepare covariates for downstream Cox-GWAS analyses.

---

## Project structure

```text
.
├── data/                     # Input data (ignored by Git)
├── results/                  # Generated datasets and reports (ignored by Git)
├── scripts/
│   ├── 00_check_keys.py
│   ├── 01_create_base_excel.py
│   ├── 02_profile_dataset.py
│   ├── 03_clean_dataset.py
│   ├── 04_imputation_variable_selection.R
│   ├── 05_descriptive_summary.R
│   ├── 06_survival_variables.R
│   ├── 07_univariate_models.R
│   └── 08_variable_selection.R
├── README.md
└── pyproject.toml
```

---

## Pipeline

### 00_check_keys.py

- Validates that patient identifiers match across Excel sheets.

### 01_create_base_excel.py

- Extracts selected variables from the original Excel workbook.
- Merges all clinical information into a unified dataset.

### 02_profile_dataset.py

- Generates descriptive reports.
- Identifies inconsistencies, missing values, and unusual distributions.

### 03_clean_dataset.py

- Cleans dates and categorical variables.
- Standardizes missing values.
- Removes invalid records.
- Calculates derived variables (e.g. Age at RT start).
- Generates a cleaning report.

### 04_imputation_variable_selection.R

- Recodes variables for statistical analyses.
- Performs multiple imputation using MICE.
- Calculates derived variables after imputation:
  - ISUP Grade
  - EAU Risk Score
- Generates datasets for downstream analyses.

### 05_descriptive_summary.R

- Generates descriptive statistics of the study cohort.
- Summarizes continuous and categorical variables.
- Reports missing data.

### 06_survival_variables.R

- Creates time-to-event variables.
- Calculates follow-up times.
- Generates event indicators for:
  - Overall survival
  - Biochemical recurrence
  - Local recurrence
  - Pelvic recurrence
  - Distant recurrence

### 07_univariate_models.R

- Performs univariate Cox proportional hazards regression.
- Estimates hazard ratios (HR), confidence intervals, and p-values.
- Reports model warnings.

### 08_variable_selection.R *(under development)*

- Removes low-frequency binary variables.
- Evaluates clinical relevance.
- Compares variable selection using:
  - Univariate Cox models
  - StepAIC
  - LASSO
- Identifies candidate covariates for multivariable analyses.

---

## Current pipeline

```text
Clinical database
        │
        ▼
Data preprocessing
        │
        ▼
Clinical variable derivation
        │
        ▼
Multiple imputation
        │
        ▼
Descriptive analysis
        │
        ▼
Survival dataset generation
        │
        ▼
Univariate Cox models
        │
        ▼
Variable selection
        │
        ▼
Multivariable Cox models (planned)
        │
        ▼
Cox-GWAS
```
---

## Alternative multiple-imputation workflow

The main pipeline currently uses a completed imputed dataset after MICE. An alternative branch/workflow keeps all five imputed datasets through downstream analyses to allow pooled inference across imputations.

---

## Requirements

### Python

- Python ≥ 3.12
- uv

Install dependencies with:

```bash
uv sync
```

### R

Required packages:

- readxl
- dplyr
- mice
- MASS
- broom
- glmnet
- survival
- writexl
- openxlsx

---

## Data

Clinical data are not included in this repository because they contain sensitive patient information.

The `data/` and `results/` directories are excluded from version control.

---

## Author

**Tania Gorgal**
