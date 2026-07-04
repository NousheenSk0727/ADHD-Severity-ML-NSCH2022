# 1. loads the subset csv (raw variables, not yet recoded)
# 2. defines helper functions for checking missingness and special codes (90/95/99)
# 3. audits the full dataset - which variables have too many special codes (>30% threshold)
# 4. restricts sample to ages 6-17 only (ADHD severity not measured outside this range)
# 5. repeats the missingness audit on the age-restricted sample
# note: this script works on the RAW subset to audit missingness BEFORE recoding
# run 00_setup.R separately to recode variables and save the clean RDS

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
  library(scales)
  library(forcats)
  library(gtsummary)
})

# load raw subset
# this is the subset CSV - raw NSCH variables, not yet recoded

DATA_PATH <- "/Users/nousheenjahanshaik/Documents/BigDataAnalytics/NACHFINALPROJECT/DATA/NSCH_Subset.csv"
dt <- fread(DATA_PATH, showProgress = TRUE)

cat("Rows:", nrow(dt), "\n")
cat("Cols:", ncol(dt), "\n")

# quick check - making sure outcome variable is actually in the dataset
if (!("ADHDSev_22" %in% names(dt))) stop("ERROR: ADHDSev_22 not found in dataset")
cat("outcome variable ADHDSev_22 found\n")
cat("unique values in outcome:", sort(unique(dt$ADHDSev_22)), "\n")
# 1 = none, 2 = mild, 3 = moderate/severe, 95/99 = missing

# helper functions
# defined here once, used throughout the script

SPECIAL_CODES <- c(90, 95, 99)
# 90 = not in universe (question didnt apply)
# 95 = legitimate skip (screener was no so follow up wasnt asked)
# 99 = missing in error

# counts true NAs per variable
summarize_missing_na <- function(df) {
  tibble(
    variable    = names(df),
    n_missing   = sapply(df, function(x) sum(is.na(x))),
    pct_missing = round(sapply(df, function(x) mean(is.na(x)) * 100), 2)
  ) %>% arrange(desc(pct_missing))
}

# counts special codes (90/95/99) vs usable responses per variable
# more useful than just checking NAs because NSCH uses special codes
# instead of leaving things blank
make_var_summary <- function(df, special_codes = SPECIAL_CODES) {
  tibble(variable = names(df)) %>%
    mutate(
      total_n    = sapply(df, \(x) sum(!is.na(x))),
      special_n  = sapply(df, \(x) sum(!is.na(x) & x %in% special_codes)),
      special_pct = round(100 * special_n / total_n, 2),
      usable_n   = total_n - special_n,
      usable_pct = round(100 * usable_n / total_n, 2)
    ) %>%
    arrange(desc(special_pct))
}

# bar chart - % special codes per variable
# dashed line at 30% = threshold i used to flag problematic variables
plot_special_pct <- function(var_summary_df, title = "Special-code percentage (90/95/99) by variable") {
  print(
    ggplot(var_summary_df, aes(x = reorder(variable, special_pct), y = special_pct)) +
      geom_col() +
      coord_flip() +
      geom_hline(yintercept = 30, linetype = "dashed") +
      scale_y_continuous(labels = percent_format(scale = 1)) +
      labs(title = title, subtitle = "Dashed line = 30% threshold for flagging",
           x = "Variable", y = "% special codes") +
      theme_minimal(base_size = 12)
  )
}

# bar chart - usable vs special coded counts side by side
plot_usable_vs_special <- function(var_summary_df, title = "Usable vs special-coded counts by variable") {
  plot_df <- var_summary_df %>%
    select(variable, usable_n, special_n) %>%
    pivot_longer(cols = c(usable_n, special_n), names_to = "type", values_to = "count") %>%
    mutate(type = recode(type, usable_n = "Usable", special_n = "Special (90/95/99)"))

  print(
    ggplot(plot_df, aes(x = reorder(variable, count), y = count, fill = type)) +
      geom_col() +
      coord_flip() +
      labs(title = title, x = "Variable", y = "Count") +
      theme_minimal(base_size = 12)
  )
}

# finds most common value - used later for mode imputation in EDA
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

# missingness audit on full dataset

# checking true NAs first
print(summarize_missing_na(dt))

# checking special codes - more important check for NSCH data
var_summary <- make_var_summary(dt)
print(var_summary)

# plots wrapped in print() so they show up when running from script
plot_special_pct(var_summary)
plot_usable_vs_special(var_summary)

# flagging variables above 30% special codes threshold
# these need careful handling in preprocessing
flag_30 <- var_summary %>% filter(special_pct > 30)
cat("\nvariables with >30% special codes:\n")
print(flag_30)

# restrict to ages 6-17

# ADHD severity is only assessed for children aged 6-17 in NSCH
# dropping everyone outside this range
dt_6_17 <- dt %>% filter(SC_AGE_YEARS >= 6, SC_AGE_YEARS <= 17)
cat("original rows:", nrow(dt), "\n")
cat("after age filter (6-17):", nrow(dt_6_17), "\n")

# repeating missingness audit on age-restricted sample
# patterns may differ slightly after filtering
var_summary_6_17 <- make_var_summary(dt_6_17)
print(var_summary_6_17)
plot_special_pct(var_summary_6_17, "Special-code % (Age 6-17 only)")
plot_usable_vs_special(var_summary_6_17, "Usable vs special-coded (Age 6-17 only)")

cat("\ndone - missingness audit complete\n")
cat("next: run 00_setup.R to recode variables and save clean RDS\n")
cat("then: run Exploratory_Data_Analysis.R\n")
