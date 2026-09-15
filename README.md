# Longitudinal BIA analysis after bariatric surgery

This repository contains the reproducible R workflow for the manuscript comparing longitudinal body-composition trajectories after sleeve gastrectomy (SG), one-anastomosis gastric bypass (OAGB), and Roux-en-Y gastric bypass (RYGB).

## Analysis overview

The workflow:

1. imports and validates the analysis dataset;
2. harmonizes follow-up periods and procedure labels;
3. creates the baseline characteristics table;
4. summarizes crude trajectories;
5. fits separate linear mixed-effects models for eight outcomes;
6. estimates adjusted marginal means;
7. compares procedure-specific changes from the preoperative period using Tukey-adjusted contrasts;
8. generates manuscript tables, figures, diagnostics, and aggregated source data.

The mixed models include categorical follow-up period, procedure, their interaction, age, sex, diabetes, hypertension, and hypothyroidism as fixed effects, with a patient-specific random intercept. Inference from `emmeans` uses asymptotic z-tests.

## Project structure

```text
BIA-analysis/
├── README.md
├── BIA-analysis.Rproj
├── run_analysis.R
├── R/
│   ├── config.R
│   ├── data_preparation.R
│   ├── modeling.R
│   ├── contrasts.R
│   ├── tables.R
│   ├── figures.R
│   └── exports.R
├── data/
│   └── README.md
└── outputs/               # Created when the analysis runs
```

## Required software

Use a recent version of R and install:

```r
install.packages(c(
  "tidyverse", "haven", "lme4", "emmeans", "broom.mixed",
  "gtsummary", "flextable", "officer", "openxlsx", "patchwork"
))
```

For strict package-version reproducibility, initialize `renv` locally after confirming that the analysis runs correctly:

```r
install.packages("renv")
renv::init()
renv::snapshot()
```

## Running the analysis

Place the confidential dataset at `data/Final Clean Data.dta` and run the main script. You may source it from the project directory:

```r
source("run_analysis.R")
```

You may also source it using its complete path from any working directory:

```r
source("E:/D/Silver/misrc/BIA/BIA-analysis-modular/BIA-analysis/run_analysis.R")
```

The script detects the project directory automatically, so `setwd()` is not required.

Alternatively, define custom locations before running R:

```r
Sys.setenv(
  BIA_DATA_PATH = "path/to/Final Clean Data.dta",
  BIA_OUTPUT_DIR = "outputs"
)
source("run_analysis.R")
```

## Reproducible outputs

The workflow creates publication-ready tables and figures plus aggregated source-data files under `outputs/`. The source-data files are suitable for public sharing after institutional review because they contain group summaries or model estimates rather than patient-level records.

## Data availability

The patient-level dataset is not included because it may contain protected or confidential clinical information. Do not commit the `.dta` file, exported patient-level records, fitted model objects, credentials, or local absolute paths.

