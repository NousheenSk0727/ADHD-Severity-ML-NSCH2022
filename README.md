# ADHD-Severity-ML-NSCH2022

## Predicting ADHD Severity in U.S. Children - A Multidomain Machine Learning Analysis

ADHD affects roughly 9–11% of U.S. children, but **severity** - not diagnosis alone - determines how much a child struggles in school, at home, and in social life. This project asks a straightforward question: which aspects of a child's life — their individual characteristics, family environment, community, daily behavior, or healthcare access — best predict whether their ADHD is moderate-to-severe?

Using the 2022 National Survey of Children's Health (NSCH), I built a machine learning pipeline in R that models ADHD severity across five domains and compares three modeling approaches: Logistic Regression, LASSO, and Random Forest.

---

## Dataset

**Source:** 2022 National Survey of Children's Health (NSCH)  
**Provider:** Data Resource Center for Child and Adolescent Health - [childhealthdata.org](https://www.childhealthdata.org)  
**Sample:** ~34,000 U.S. children aged 6–17 (caregiver-reported, nationally representative)  
**Outcome:** Binary - Moderate/Severe ADHD vs. None/Mild (~8% prevalence)  

> The dataset is not included in this repository. Download instructions are in `DATA/README_data.md`.

---

## Domain Framework

Rather than throwing all variables into one model, I organized 51 predictors into 5 socioecological domains and modeled each domain separately before combining them. This tells you not just *what* predicts severity, but *which part of a child's life* carries the most signal.

| Domain | Variables | Examples |
|--------|-----------|---------|
| D1 — Individual | 14 | Age, sex, comorbidities (anxiety, ASD, depression), memory difficulties, self-regulation |
| D2 — Family & Household | 9 | Parental mental health, parental aggravation, ACE count, poverty level, food security |
| D3 — Community | 6 | Neighborhood safety, bullying frequency, difficulty making friends, community ACEs |
| D4 — Behavioral | 8 | School engagement, difficulty of care, screen time, sleep, missed school days |
| D5 — Healthcare Access | 5 | Family-centred care, unmet needs, frustration frequency, care coordination |

---

## Models

Three supervised learning models were trained and compared for each domain and combined:

**Logistic Regression** - baseline model; interpretable coefficients and odds ratios; assumes linear relationships between predictors and outcome.

**LASSO (L1-regularized Logistic Regression)** — same as logistic but adds a penalty that shrinks weak predictors to exactly zero, performing automatic feature selection. Reveals which variables genuinely carry signal vs. which are redundant.

**Random Forest** - ensemble of 400 decision trees; captures nonlinear relationships and interactions between variables that logistic models cannot. Importance scores reflect how much each variable reduces prediction error across all trees.

All models used an 80/20 stratified train/test split. PR-AUC was prioritized alongside ROC-AUC given the class imbalance (~8% moderate/severe outcome).

---

## Results

### Domain-Specific Model Performance

| Domain | Best Model | ROC-AUC | PR-AUC | Sensitivity | Specificity |
|--------|-----------|---------|--------|-------------|-------------|
| D1 — Individual | LASSO | 0.126 | 0.815 | 0.991 | 0.193 |
| D2 — Family | Logistic | 0.754 | 0.283 | 0.098 | 0.992 |
| D3 — Community | Logistic/RF | 0.760 | 0.278 | 0.079 | 0.993 |
| D4 — Behavioral | Logistic | 0.845 | 0.344 | 0.168 | 0.985 |
| **D5 — Healthcare** | **RF** | **0.984** | **0.778** | **0.698** | **0.979** |

### Combined Model Performance (Primary — Access Variables Only)

| Model | ROC-AUC | PR-AUC | Accuracy | Sensitivity | Specificity |
|-------|---------|--------|----------|-------------|-------------|
| Logistic | 0.923 | 0.550 | 0.930 | 0.438 | 0.973 |
| LASSO | 0.925 | 0.545 | 0.927 | 0.331 | 0.978 |
| RF | 0.924 | 0.553 | 0.931 | 0.304 | 0.986 |

### Sensitivity Analysis (Access + Treatment Variables)

| Model | ROC-AUC | PR-AUC | Accuracy | Sensitivity | Specificity |
|-------|---------|--------|----------|-------------|-------------|
| Logistic | 0.957 | 0.687 | 0.948 | 0.592 | 0.980 |
| LASSO | 0.956 | 0.684 | 0.948 | 0.540 | 0.985 |
| RF | 0.961 | 0.746 | 0.950 | 0.583 | 0.984 |

---

## Key Findings

**Healthcare access was the strongest single domain** (RF ROC-AUC = 0.984) - children with more severe ADHD show consistently higher rates of unmet needs, care coordination difficulties, and service frustration.

**Individual characteristics were second** - memory/concentration difficulty (MEMORYCOND) was the single strongest predictor across every model and domain combination, followed by task persistence and learning disability.

**Behavioral functioning added meaningful signal** - difficulty of care and school disengagement were top behavioral predictors, reflecting how severely ADHD disrupts daily functioning.

**Family adversity contributed beyond individual factors** - parental aggravation showed an odds ratio of ~8x in the family domain logistic model, and household ACE count showed a dose-response pattern with severity.

**Community factors were the weakest domain** - peer difficulties and bullying frequency carried signal, but neighborhood-level variables added comparatively little.

**Combined model outperformed all single domains** - integrating all 5 domains improved PR-AUC from 0.344 (behavioral alone) to 0.553, supporting the multidimensional nature of ADHD severity.

---

## An Important Methodological Note

ADHD medication and behavioral treatment variables were **excluded from the primary model**. Here is why:

A doctor does not give a child ADHD medication because they might have severe ADHD - they give it because the child already has severe ADHD. So medication follows severity, not the other way around. In the NSCH survey, a child being on medication is essentially already telling you that child has severe ADHD. Including it would let the model cheat — using the answer to predict the answer.

The sensitivity analysis confirms this: adding treatment variables boosts ROC-AUC from ~0.924 to ~0.961. That jump is the cheat - not genuine signal about risk factors.

This distinction matters for real-world use. A screening tool built to identify children at risk should use upstream factors (family environment, behavioral functioning, access barriers) - not downstream consequences of already having severe ADHD.

---

## Limitations

- **Cross-sectional design** - no causal inference possible. Strong predictors are associated with severity but cannot be confirmed as causes without longitudinal data.
- **Caregiver-reported measures** - all variables are parent-reported, subject to recall and reporting bias.
- **Class imbalance** - ~8% outcome prevalence; addressed with stratified splitting and PR-AUC focus.
- **LASSO penalty fixed** - penalty = 0.01 was not cross-validated; tuning via `tune_grid()` + `vfold_cv()` would be more rigorous.
- **Survey weights not applied** - NSCH provides complex-survey weights for population-representative estimates; prevalence figures should be interpreted accordingly.

---

## Repository Structure

```
ADHD-Severity-ML-NSCH2022/
├── R/
│   ├── 00_setup.R                   # load subset, recode all variables, save RDS
│   ├── 01_extract_subset.R          # extract 51 variables from raw NSCH CSV
│   ├── 02_data_quality.R            # missingness audit, special code analysis
│   ├── Exploratory_Data_Analysis.R  # EDA plots
│   ├── model.R                      # domain-specific models (D1-D5)
│   └── Combinedmodel.R              # combined models + sensitivity analysis
├── DATA/
│   └── README_data.md               # download instructions (data not included)
├── OUTPUT/
│   ├── All_Domains_Model_Performance.csv
│   ├── Combined_Model_Performance.csv
│   └── Plots/                       # all figures
├── docs/
│   └── Final_Report.pdf
└── README.md
```

---

## How to Reproduce

### 1. Get the data
Download the NSCH 2022 CSV from [childhealthdata.org](https://www.childhealthdata.org) and place it in `DATA/`. See `DATA/README_data.md` for details.

### 2. Install packages
```r
install.packages(c(
  "data.table", "dplyr", "tidyr", "ggplot2", "scales",
  "corrplot", "gtsummary", "tidymodels", "ranger", "glmnet",
  "pROC", "PRROC", "broom", "forcats"
))
```

### 3. Run scripts in order
```r
setwd("/path/to/ADHD-Severity-ML-NSCH2022")

source("R/00_setup.R")                   # run once
source("R/02_data_quality.R")            # missingness audit
source("R/Exploratory_Data_Analysis.R")  # EDA plots
source("R/model.R")                      # domain models
source("R/Combinedmodel.R")              # combined models
```

---

## Tools

**Language:** R  
**Modeling:** tidymodels · glmnet · ranger  
**Visualization:** ggplot2 · corrplot · PRROC · pROC  
**Data:** gtsummary · data.table · dplyr  

---

## Related Projects

- [16S Vaginal Microbiome — Mother-Daughter Pairs](https://github.com/nousheen-shaik/16S-vaginal-microbiome-mother-daughter)
- [DNA Methylation Aging Clock](https://github.com/nousheen-shaik/methylation-aging-clock)
