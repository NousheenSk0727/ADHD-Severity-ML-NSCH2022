# Combined model - all domains together
# 1. compiles performance table from all domain models
# 2. runs combined model (primary) - all domains, access variables only
# 3. runs combined model (sensitivity) - all domains + treatment variables
# 4. produces ROC, PR curves, feature importance, domain contribution weights
# why two combined models:
# primary = access only (fair comparison, no leakage)
# sensitivity = includes treatment (medication, behavioral tx) to show how much
# performance inflates when you include downstream variables
# NOTE: requires res_all, res_domain2, res_domain3, res_domain4, res_domain5
# from previous domain scripts to be in your environment
# AND dt_6_17 loaded from RDS

# ============================================================
# libraries
# ============================================================
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tidymodels)
  library(yardstick)
  library(broom)
  library(ggplot2)
  library(stringr)
  library(glmnet)
  library(ranger)
})

tidymodels_prefer()
set.seed(123)

# load clean dataset
# UPDATE PATH if running on a different machine

dt_6_17 <- readRDS("/Users/nousheenjahanshaik/Documents/BigDataAnalytics/NACHFINALPROJECT/DATA/NSCH2022_ADHD_clean.rds")
cat("rows loaded:", nrow(dt_6_17), "\n")

# PART 1 - compile performance table from all domain models
# requires res_all, res_domain2, res_domain3, res_domain4, res_domain5
# to already be in your environment from running domain scripts

cat("\n--- compiling domain results table ---\n")

# check which result objects exist
cat("result objects in environment:", paste(ls(pattern = "res_"), collapse = ", "), "\n")

all_domains_long <- bind_rows(
  res_all     %>% mutate(domain = "Domain 1 - Individual"),
  res_domain2 %>% mutate(domain = "Domain 2 - Family"),
  res_domain3 %>% mutate(domain = "Domain 3 - Community"),
  res_domain4 %>% mutate(domain = "Domain 4 - Behavioral"),
  res_domain5 %>% mutate(domain = "Domain 5 - Healthcare")
)

# wide format table - one row per domain/model combination
final_performance_table <- all_domains_long %>%
  filter(.metric %in% c("accuracy", "roc_auc", "pr_auc", "sens", "spec", "f_meas")) %>%
  mutate(
    metric = recode(.metric,
                    accuracy = "Accuracy",
                    roc_auc  = "ROC_AUC",
                    pr_auc   = "PR_AUC",
                    sens     = "Sensitivity",
                    spec     = "Specificity",
                    f_meas   = "F1_Score"
    )
  ) %>%
  select(domain, model, metric, .estimate) %>%
  pivot_wider(names_from = metric, values_from = .estimate) %>%
  arrange(domain, model) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

cat("\n===== DOMAIN-SPECIFIC PERFORMANCE TABLE =====\n")
print(final_performance_table)

# save to csv
write.csv(
  final_performance_table,
  "All_Domains_Model_Performance.csv",
  row.names = FALSE
)
cat("saved: All_Domains_Model_Performance.csv\n")

# domain variable lists
# needed for combined models

domain1_vars <- c(
  "SC_AGE_YEARS", "SC_SEX", "SC_RACE_R",
  "K2Q30B", "K2Q33B", "K2Q32B", "K2Q35B", "K2Q36B", "K2Q37B", "K2Q42A",
  "MEMORYCOND", "GENETIC_ANY", "resil6to17_22_ord", "finishes_22_ord"
)

domain2_vars <- c(
  "MotherMH_22_ord", "FatherMH_22_ord", "ParAggrav_22_bin", "EmSupport_22_bin",
  "FoodSit_22_ord", "povlev4_22_ord", "AdultEduc_22_ord", "ACE6ctHH_22_num",
  "smoking_22_bin"
)

domain3_vars <- c(
  "nbhd_safe_ord", "nbhd_help_ord", "ACE4ctCom_22_num",
  "school_safe_ord", "bullied_freq_ord", "friend_diff_ord"
)

domain4_vars <- c(
  "schl_engage_ord", "physact_ord", "missed_school_cat",
  "diffcare_ord", "sports_bin", "aftschact_bin", "screentime_ord", "hrssleep_bin"
)

# domain 5 split into access (primary) and treatment (sensitivity only)
# treatment variables are downstream of severity - kids get medication BECAUSE
# they have severe ADHD, not the other way around
domain5_access_vars <- c(
  "famcent_bin_quality", "unmetfrust_bin", "frust_freq_ord",
  "helpcoord_bin_universe", "allextrahelp_ord"
)

domain5_treat_vars <- c("adhd_med_bin_among_adhd", "adhd_behtx_bin_among_adhd")

# helper functions


# check all variables exist before running
check_vars_exist <- function(df, vars, label = "") {
  missing <- setdiff(vars, names(df))
  if (length(missing) > 0) {
    stop(label, " missing vars: ", paste(missing, collapse = ", "))
  }
}

# create binary outcome
make_binary_outcome <- function(df) {
  df %>%
    filter(!is.na(ADHDSev_22_ord)) %>%
    mutate(
      ADHD_ModSev = factor(
        if_else(ADHDSev_22_ord == "Moderate/Severe", "Yes", "No"),
        levels = c("No", "Yes")
      )
    )
}

# get predictions
get_preds <- function(fit_obj, test_df) {
  predict(fit_obj, test_df, type = "prob") %>%
    bind_cols(predict(fit_obj, test_df, type = "class")) %>%
    bind_cols(test_df %>% select(ADHD_ModSev))
}

# evaluate metrics
eval_metrics <- function(p_df) {
  metric_set(roc_auc, pr_auc, accuracy, sens, spec, f_meas)(
    p_df,
    truth       = ADHD_ModSev,
    estimate    = .pred_class,
    .pred_Yes,
    event_level = "second"
  )
}

# ROC and PR curve plots
plot_roc_pr <- function(p_list_named, title_prefix = "Model") {
  roc_df <- imap_dfr(p_list_named,
                     ~ roc_curve(.x, ADHD_ModSev, .pred_Yes, event_level = "second") %>%
                       mutate(model = .y))
  pr_df <- imap_dfr(p_list_named,
                    ~ pr_curve(.x, ADHD_ModSev, .pred_Yes, event_level = "second") %>%
                      mutate(model = .y))

  p_roc <- ggplot(roc_df, aes(1 - specificity, sensitivity, color = model)) +
    geom_path(linewidth = 1) +
    geom_abline(linetype = "dashed") +
    coord_equal() +
    theme_minimal() +
    labs(title = paste0(title_prefix, " - ROC Curve"),
         x = "False Positive Rate", y = "True Positive Rate")

  p_pr <- ggplot(pr_df, aes(recall, precision, color = model)) +
    geom_path(linewidth = 1) +
    theme_minimal() +
    labs(title = paste0(title_prefix, " - Precision-Recall Curve"),
         x = "Recall", y = "Precision")

  list(roc_plot = p_roc, pr_plot = p_pr)
}

# LASSO feature importance
lasso_importance <- function(fit_lasso, top_n = 25, title = "LASSO Feature Importance") {
  imp <- tidy(extract_fit_parsnip(fit_lasso)) %>%
    filter(term != "(Intercept)", estimate != 0) %>%
    mutate(abs_est = abs(estimate)) %>%
    arrange(desc(abs_est)) %>%
    slice_head(n = top_n)

  g <- ggplot(imp, aes(abs_est, reorder(term, abs_est))) +
    geom_col(fill = "#2c7fb8") +
    theme_minimal() +
    labs(title = title, x = "|Coefficient|", y = "Feature")

  list(data = imp, plot = g)
}

# RF feature importance
rf_importance <- function(fit_rf, top_n = 25, title = "RF Feature Importance (Impurity)") {
  rf_fit <- extract_fit_parsnip(fit_rf)$fit
  imp <- tibble(
    term       = names(rf_fit$variable.importance),
    importance = as.numeric(rf_fit$variable.importance)
  ) %>%
    arrange(desc(importance)) %>%
    slice_head(n = top_n)

  g <- ggplot(imp, aes(importance, reorder(term, importance))) +
    geom_col(fill = "#d95f02") +
    theme_minimal() +
    labs(title = title, x = "Importance", y = "Feature")

  list(data = imp, plot = g)
}

# domain contribution weights from LASSO
# shows which domain contributed most to predictions
domain_weights_from_lasso <- function(fit_lasso, d1, d2, d3, d4, d5,
                                      title = "Domain Contribution Weights (LASSO)") {
  assign_domain <- function(term) {
    if (str_detect(term, paste0("^(", paste(d1, collapse = "|"), ")"))) return("Individual")
    if (str_detect(term, paste0("^(", paste(d2, collapse = "|"), ")"))) return("Family")
    if (str_detect(term, paste0("^(", paste(d3, collapse = "|"), ")"))) return("Community")
    if (str_detect(term, paste0("^(", paste(d4, collapse = "|"), ")"))) return("Behavior")
    if (str_detect(term, paste0("^(", paste(d5, collapse = "|"), ")"))) return("Healthcare")
    return(NA_character_)
  }

  w <- tidy(extract_fit_parsnip(fit_lasso)) %>%
    filter(term != "(Intercept)", estimate != 0) %>%
    mutate(
      domain  = map_chr(term, assign_domain),
      abs_est = abs(estimate)
    ) %>%
    filter(!is.na(domain)) %>%
    group_by(domain) %>%
    summarise(weight = sum(abs_est), .groups = "drop") %>%
    mutate(prop = weight / sum(weight)) %>%
    arrange(desc(prop))

  g <- ggplot(w, aes(prop, reorder(domain, prop))) +
    geom_col(fill = "#4dac26") +
    theme_minimal() +
    labs(title = title, x = "Proportion of total |coef|", y = "Domain")

  list(data = w, plot = g)
}

# main runner function - trains all 3 models on combined variable set
run_combined <- function(dt, combined_vars, title_tag = "Combined") {
  check_vars_exist(dt, combined_vars, label = paste0("[", title_tag, "]"))

  d_all <- dt %>%
    select(ADHDSev_22_ord, all_of(combined_vars)) %>%
    make_binary_outcome()

  cat("rows:", nrow(d_all), "| moderate/severe:", sum(d_all$ADHD_ModSev == "Yes"), "\n")

  split_all <- initial_split(d_all, prop = 0.80, strata = ADHD_ModSev)
  train_all <- training(split_all)
  test_all  <- testing(split_all)

  rec_all <- recipe(ADHD_ModSev ~ ., data = train_all) %>%
    step_rm(ADHDSev_22_ord) %>%
    step_impute_mode(all_nominal_predictors()) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors())

  mod_log <- logistic_reg() %>% set_engine("glm")    %>% set_mode("classification")
  mod_las <- logistic_reg(penalty = 0.01, mixture = 1) %>% set_engine("glmnet") %>% set_mode("classification")
  mod_rf  <- rand_forest(trees = 400, mtry = 10, min_n = 10) %>%
    set_engine("ranger", importance = "impurity", num.threads = parallel::detectCores()) %>%
    set_mode("classification")

  cat("fitting logistic...\n")
  fit_log <- workflow() %>% add_recipe(rec_all) %>% add_model(mod_log) %>% fit(train_all)
  cat("fitting LASSO...\n")
  fit_las <- workflow() %>% add_recipe(rec_all) %>% add_model(mod_las) %>% fit(train_all)
  cat("fitting random forest (this takes a minute)...\n")
  fit_rf  <- workflow() %>% add_recipe(rec_all) %>% add_model(mod_rf)  %>% fit(train_all)

  p_log <- get_preds(fit_log, test_all)
  p_las <- get_preds(fit_las, test_all)
  p_rf  <- get_preds(fit_rf,  test_all)

  results <- bind_rows(
    eval_metrics(p_log) %>% mutate(model = "Logistic"),
    eval_metrics(p_las) %>% mutate(model = "LASSO"),
    eval_metrics(p_rf)  %>% mutate(model = "RF")
  ) %>%
    mutate(domain = title_tag) %>%
    select(domain, model, .metric, .estimate) %>%
    arrange(.metric, desc(.estimate))

  list(fit_log = fit_log, fit_lasso = fit_las, fit_rf = fit_rf,
       p_log = p_log, p_lasso = p_las, p_rf = p_rf, results = results)
}

# PART 2 - PRIMARY combined model (access variables only)
# this is the main result - combining all domains

cat("\n--- COMBINED MODEL PRIMARY (Access only) ---\n")

combined_vars_access <- c(
  domain1_vars, domain2_vars, domain3_vars,
  domain4_vars, domain5_access_vars
)

out_access <- run_combined(dt_6_17, combined_vars_access, "Combined D1-D5 (Access only)")

cat("\nprimary combined model results:\n")
print(out_access$results)

# plots
curves_access <- plot_roc_pr(
  list(Logistic = out_access$p_log, LASSO = out_access$p_lasso, RF = out_access$p_rf),
  title_prefix = "Combined D1-D5 (Access only)"
)
print(curves_access$roc_plot)
print(curves_access$pr_plot)

lasso_imp_access <- lasso_importance(
  out_access$fit_lasso, top_n = 25,
  title = "Combined D1-D5 (Access only) LASSO Feature Importance"
)
print(lasso_imp_access$plot)

rf_imp_access <- rf_importance(
  out_access$fit_rf, top_n = 25,
  title = "Combined D1-D5 (Access only) RF Feature Importance"
)
print(rf_imp_access$plot)

# domain contribution weights - which domain drove predictions most
dw_access <- domain_weights_from_lasso(
  out_access$fit_lasso,
  d1 = domain1_vars, d2 = domain2_vars, d3 = domain3_vars,
  d4 = domain4_vars, d5 = domain5_access_vars,
  title = "Domain Contribution Weights (LASSO) - Access only"
)
cat("\ndomain weights:\n")
print(dw_access$data)
print(dw_access$plot)

# ============================================================
# PART 3 - SENSITIVITY combined model (access + treatment)
# showing this separately to be transparent about leakage
# performance will be higher here because treatment is a consequence of severity
# ============================================================
cat("\n--- COMBINED MODEL SENSITIVITY (Access + Treatment) ---\n")

combined_vars_treat <- c(
  domain1_vars, domain2_vars, domain3_vars,
  domain4_vars, domain5_access_vars, domain5_treat_vars
)

out_treat <- run_combined(dt_6_17, combined_vars_treat, "Combined D1-D5 (Access + Treatment)")

cat("\nsensitivity combined model results:\n")
print(out_treat$results)

curves_treat <- plot_roc_pr(
  list(Logistic = out_treat$p_log, LASSO = out_treat$p_lasso, RF = out_treat$p_rf),
  title_prefix = "Combined D1-D5 (Access + Treatment)"
)
print(curves_treat$roc_plot)
print(curves_treat$pr_plot)

lasso_imp_treat <- lasso_importance(
  out_treat$fit_lasso, top_n = 25,
  title = "Combined D1-D5 (Access + Treatment) LASSO Feature Importance"
)
print(lasso_imp_treat$plot)

rf_imp_treat <- rf_importance(
  out_treat$fit_rf, top_n = 25,
  title = "Combined D1-D5 (Access + Treatment) RF Feature Importance"
)
print(rf_imp_treat$plot)

dw_treat <- domain_weights_from_lasso(
  out_treat$fit_lasso,
  d1 = domain1_vars, d2 = domain2_vars, d3 = domain3_vars,
  d4 = domain4_vars, d5 = c(domain5_access_vars, domain5_treat_vars),
  title = "Domain Contribution Weights (LASSO) - Access + Treatment"
)
cat("\ndomain weights (sensitivity):\n")
print(dw_treat$data)
print(dw_treat$plot)

# PART 4 - final combined performance table

final_combined_table <- bind_rows(
  out_access$results,
  out_treat$results
) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(domain, model)

cat("\n===== COMBINED MODEL PERFORMANCE TABLE =====\n")
print(final_combined_table)

write.csv(
  final_combined_table,
  "Combined_Model_Performance.csv",
  row.names = FALSE
)
cat("saved: Combined_Model_Performance.csv\n")
cat("\ndone - all modeling complete\n")
