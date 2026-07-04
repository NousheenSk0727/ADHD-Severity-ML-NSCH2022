# explores the data visually before modeling
# - outcome distribution (how many kids in each severity group)
# - prevalence of moderate/severe ADHD by age
# - comorbidity patterns across severity groups
# - spearman correlation matrix (domain 1 variables)
# - ACE burden by severity (domain 2)
# - healthcare access patterns by severity (domain 5)

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(corrplot)
library(tibble)
library(forcats)

# load clean recoded dataset
# UPDATE PATH if running on a different machine

dt_6_17 <- readRDS("/Users/nousheenjahanshaik/Documents/BigDataAnalytics/NACHFINALPROJECT/DATA/NSCH2022_ADHD_clean.rds")
cat("rows loaded:", nrow(dt_6_17), "\n")

# helper functions needed for EDA


# finds most common value - used for mode imputation in plots
mode_value <- function(x) {
  ux <- x[!is.na(x)]
  if (length(ux) == 0) return(NA)
  tab <- table(ux)
  names(tab)[which.max(tab)]
}

# recodes categorical variables - replaces special codes with NA
clean_cat <- function(x, missing_values = c("NA", "Missing", "", "95", "99")) {
  x_chr <- as.character(x)
  x_chr[x_chr %in% missing_values] <- NA
  fct_na_value_to_level(factor(x_chr), level = "Missing")
}

# recodes numeric variables - replaces special codes with NA
clean_num <- function(x, missing_codes = c(95, 99)) {
  x_num <- suppressWarnings(as.numeric(x))
  x_num[x_num %in% missing_codes] <- NA
  x_num
}

# DOMAIN 1 EDA

domain1_vars <- c(
  "SC_AGE_YEARS", "SC_SEX", "SC_RACE_R",
  "K2Q30B", "K2Q33B", "K2Q32B", "K2Q35B", "K2Q36B", "K2Q37B", "K2Q42A",
  "MEMORYCOND", "GENETIC_ANY",
  "resil6to17_22_ord", "finishes_22_ord"
)

# just the comorbidity variables - for dot plot and correlation
comorbidity_vars_all <- c(
  "K2Q30B", "K2Q33B", "K2Q32B", "K2Q35B",
  "K2Q36B", "K2Q37B", "K2Q42A", "MEMORYCOND", "GENETIC_ANY"
)

# working copy for domain 1 EDA
# converting severity to numeric (0/1/2) for correlation analysis
# filtering to rows with valid outcome only
dt1 <- dt_6_17 %>%
  mutate(ADHDSev_num = as.integer(ADHDSev_22_ord) - 1) %>%
  select(ADHDSev_22_ord, ADHDSev_num, all_of(domain1_vars)) %>%
  filter(!is.na(ADHDSev_22_ord))

# re-checking outcome coding is consistent
dt_6_17 <- dt_6_17 %>%
  mutate(
    ADHDSev_22_ord = case_when(
      ADHDSev_22 %in% c(1, 2, 3) ~ ADHDSev_22,
      ADHDSev_22 %in% c(95, 99)  ~ NA_real_,
      TRUE                        ~ NA_real_
    ) %>% factor(
      levels = c(1, 2, 3), ordered = TRUE,
      labels = c("None", "Mild", "Moderate/Severe")
    )
  )

# mode imputation for EDA purposes only
# just filling gaps so plots work cleanly
# NOT using this imputed version for modeling - models handle missing separately
for (v in domain1_vars) {
  dt1[[paste0(v, "_miss")]] <- as.integer(is.na(dt1[[v]]))  # flag which were missing
  mv <- mode_value(dt1[[v]])
  dt1[[v]] <- ifelse(is.na(dt1[[v]]), mv, dt1[[v]])
}

# ---- plot 1: outcome distribution ----
# just checking how many kids fall into each severity category
# expecting most to be "none" since moderate/severe is only ~8%
dt1 %>%
  count(ADHDSev_22_ord) %>%
  mutate(pct = percent(n / sum(n))) %>%
  print()

# ---- plot 2: prevalence of moderate/severe ADHD by age ----
# want to see if certain ages have higher rates
# based on literature i expected a peak around 9-11 (school age when demands increase)
age_prev <- dt1 %>%
  mutate(
    age    = as.numeric(SC_AGE_YEARS),
    modsev = as.integer(ADHDSev_22_ord == "Moderate/Severe")
  ) %>%
  group_by(age) %>%
  summarise(prev_modsev = mean(modsev), n = n(), .groups = "drop")

print(
  ggplot(age_prev, aes(age, prev_modsev)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(labels = percent_format()) +
    labs(title = "Prevalence of Moderate/Severe ADHD by Age",
         x = "Age (years)", y = "Prevalence") +
    theme_minimal()
)

# ---- plot 3: comorbidity prevalence by severity ----
# checking if comorbidities (anxiety, ASD, depression etc) increase with severity
# expecting yes - kids with more severe ADHD tend to have more co-occurring conditions
prev_by_sev <- dt1 %>%
  select(ADHDSev_22_ord, all_of(comorbidity_vars_all)) %>%
  pivot_longer(-ADHDSev_22_ord, names_to = "comorb", values_to = "val") %>%
  mutate(val = as.numeric(val)) %>%
  group_by(ADHDSev_22_ord, comorb) %>%
  summarise(prev = mean(val == 1), .groups = "drop")

print(
  ggplot(prev_by_sev, aes(prev, comorb)) +
    geom_point() +
    facet_wrap(~ADHDSev_22_ord) +
    scale_x_continuous(labels = percent_format()) +
    labs(title = "Comorbidity Prevalence by ADHD Severity",
         x = "Prevalence", y = "Comorbidity") +
    theme_minimal()
)

# ---- plot 4: spearman correlation matrix ----
# using spearman because variables are ordinal not continuous
# want to see how severity correlates with age, self-regulation, and comorbidities
# also checking if comorbidities are correlated with each other (they often are)
corr_vars <- dt1 %>%
  transmute(
    ADHDSev_num,
    SC_AGE_YEARS = as.numeric(SC_AGE_YEARS),
    resil        = as.numeric(resil6to17_22_ord),
    finishes     = as.numeric(finishes_22_ord),
    across(all_of(comorbidity_vars_all), ~ as.numeric(.))
  )

corr_mat <- cor(corr_vars, use = "pairwise.complete.obs", method = "spearman")

corrplot(
  corr_mat,
  method = "color", type = "lower",
  tl.col = "black", addCoef.col = "black", number.cex = 0.7
)

# DOMAIN 2 EDA - family and household


domain2_vars <- c(
  "MotherMH_22_ord", "FatherMH_22_ord", "ParAggrav_22_bin", "EmSupport_22_bin",
  "FoodSit_22_ord", "povlev4_22_ord", "famstruct5_22_fac", "AdultEduc_22_ord",
  "TOTKIDS_R_ord", "ACE6ctHH_22_num", "FamCount_22_ord", "smoking_22_bin"
)

# working copy for domain 2
dt2 <- dt_6_17 %>%
  select(ADHDSev_22_ord, all_of(domain2_vars)) %>%
  filter(!is.na(ADHDSev_22_ord)) %>%
  mutate(
    ADHDSev_num       = as.integer(ADHDSev_22_ord) - 1,
    # using clean_cat helper to handle remaining special codes
    povlev4_22_ord    = clean_cat(povlev4_22_ord),
    FoodSit_22_ord    = clean_cat(FoodSit_22_ord),
    famstruct5_22_fac = clean_cat(famstruct5_22_fac),
    AdultEduc_22_ord  = clean_cat(AdultEduc_22_ord),
    TOTKIDS_R_ord     = clean_cat(TOTKIDS_R_ord),
    FamCount_22_ord   = clean_cat(FamCount_22_ord),
    MotherMH_22_ord   = clean_cat(MotherMH_22_ord),
    FatherMH_22_ord   = clean_cat(FatherMH_22_ord),
    ParAggrav_22_bin  = clean_cat(ParAggrav_22_bin),
    EmSupport_22_bin  = clean_cat(EmSupport_22_bin),
    smoking_22_bin    = clean_cat(smoking_22_bin),
    ACE6ctHH_22_num   = clean_num(ACE6ctHH_22_num)
  )

# checking missingness in domain 2 before plotting
miss_domain2 <- tibble(
  variable    = domain2_vars,
  missing_n   = sapply(domain2_vars, \(v) sum(is.na(dt2[[v]]))),
  missing_pct = round(100 * missing_n / nrow(dt2), 2)
) %>% arrange(desc(missing_pct))

cat("\nmissingness in domain 2 variables:\n")
print(miss_domain2)

# imputing ACE count with median for plotting only
# creating a flag column first so we know which were imputed
dt2 <- dt2 %>%
  mutate(
    ACE6ctHH_22_num_miss = as.integer(is.na(ACE6ctHH_22_num)),
    ACE6ctHH_22_num = ifelse(
      is.na(ACE6ctHH_22_num),
      median(ACE6ctHH_22_num, na.rm = TRUE),
      ACE6ctHH_22_num
    )
  )

# ---- plot 5: ACE burden by ADHD severity ----
# violin + boxplot to show distribution shape and median
# expecting ACE count to increase with severity
print(
  ggplot(dt2, aes(x = ADHDSev_22_ord, y = ACE6ctHH_22_num)) +
    geom_violin(alpha = 0.35) +
    geom_boxplot(width = 0.15, outlier.shape = NA) +
    labs(title = "Household ACE Burden Across ADHD Severity",
         subtitle = "higher ACE count associated with greater severity",
         x = "ADHD Severity", y = "Household ACE count") +
    theme_minimal()
)

# DOMAIN 5 EDA - healthcare access

# looking at how healthcare access and treatment differ by severity
# kids with more severe ADHD should have more healthcare contact
# NOTE: medication and behavioral treatment are downstream of severity
# (kids get treated because they have severe ADHD, not the other way around)
# thats why these are in sensitivity analysis only and not the main model
healthcare_vars <- c(
  "famcent_bin_quality",
  "unmetfrust_bin",
  "helpcoord_bin_universe",
  "adhd_med_bin_among_adhd",
  "adhd_behtx_bin_among_adhd"
)

healthcare_long <- dt_6_17 %>%
  select(ADHDSev_22_ord, all_of(healthcare_vars)) %>%
  pivot_longer(cols = all_of(healthcare_vars), names_to = "factor", values_to = "value") %>%
  filter(!is.na(ADHDSev_22_ord)) %>%
  group_by(ADHDSev_22_ord, factor) %>%
  summarise(pct = mean(value == 1, na.rm = TRUE), .groups = "drop")

# ---- plot 6: healthcare access by severity ----
print(
  ggplot(healthcare_long, aes(x = pct, y = factor, fill = ADHDSev_22_ord)) +
    geom_col(position = "dodge") +
    scale_x_continuous(labels = percent_format()) +
    labs(title = "Healthcare Access & Coordination Risk Factors by ADHD Severity",
         x = "Percent with risk factor", y = "Risk factor",
         fill = "ADHD Severity") +
    theme_minimal()
)

cat("\ndone - EDA complete\n")
cat("next: run model.R\n")
