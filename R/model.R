# trains and evaluates 3 models (logistic, LASSO, random forest)
# on each of the 5 domains separately
# produces ROC curves, PR curves, and feature importance plots per domain
# models:
# - logistic regression (baseline, interpretable)
# - LASSO (regularized, does feature selection)
# - random forest (captures nonlinear relationships)
# evaluation metrics:
# - ROC-AUC (overall discrimination)
# - PR-AUC (better than ROC when outcome is imbalanced - ~8% moderate/severe here)
# - sensitivity, specificity, F1, accuracy


# libraries

suppressPackageStartupMessages({
  library(dplyr)
  library(tidymodels)
  library(yardstick)
  library(broom)
  library(pROC)
  library(PRROC)
  library(ggplot2)
  library(glmnet)
  library(ranger)
})

tidymodels_prefer()
set.seed(123)


# load clean recoded dataset
# UPDATE PATH if running on a different machine

dt_6_17 <- readRDS("/Users/nousheenjahanshaik/Documents/BigDataAnalytics/NACHFINALPROJECT/DATA/NSCH2022_ADHD_clean.rds")
cat("rows loaded:", nrow(dt_6_17), "\n")
cat("moderate/severe cases:", sum(dt_6_17$ADHDSev_22_ord == "Moderate/Severe", na.rm = TRUE), "\n")


# shared helper functions - defined once, used for all domains


# get predicted probabilities and class labels
eval_probs <- function(fit_obj, test_df) {
  predict(fit_obj, test_df, type = "prob") %>%
    bind_cols(predict(fit_obj, test_df, type = "class")) %>%
    bind_cols(test_df %>% select(ADHD_ModSev))
}

# calculate all evaluation metrics
# event_level = "second" because "Yes" is second level in factor
eval_metrics <- function(p_df) {
  metric_set(roc_auc, pr_auc, accuracy, sens, spec, f_meas)(
    p_df,
    truth       = ADHD_ModSev,
    estimate    = .pred_class,
    .pred_Yes,
    event_level = "second"
  )
}

# standard recipe - same for all domains
# imputes missing, dummy codes categoricals, removes zero variance predictors
make_recipe <- function(train_df) {
  recipe(ADHD_ModSev ~ ., data = train_df) %>%
    step_rm(ADHDSev_22_ord) %>%
    step_impute_mode(all_nominal_predictors()) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors())
}

# creates binary outcome and selects domain variables
make_domain_data <- function(dt, vars) {
  dt %>%
    select(ADHDSev_22_ord, all_of(vars)) %>%
    filter(!is.na(ADHDSev_22_ord)) %>%
    mutate(
      ADHD_ModSev = factor(
        if_else(ADHDSev_22_ord == "Moderate/Severe", "Yes", "No"),
        levels = c("No", "Yes")
      )
    )
}


# shared model specifications


mod_log <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

# penalty fixed at 0.01 - for a more rigorous analysis
# tune this using tune_grid() + vfold_cv()
mod_lasso <- logistic_reg(penalty = 0.01, mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

mod_rf <- rand_forest(trees = 400, mtry = 8, min_n = 10) %>%
  set_engine("ranger",
             importance  = "impurity",
             num.threads = parallel::detectCores()) %>%
  set_mode("classification")


# DOMAIN 1 - Individual characteristics
# demographics, comorbidities, self-regulation

cat("\n--- DOMAIN 1: Individual ---\n")

domain1_vars <- c(
  "SC_AGE_YEARS", "SC_SEX", "SC_RACE_R",
  "K2Q30B", "K2Q33B", "K2Q32B", "K2Q35B", "K2Q36B", "K2Q37B", "K2Q42A",
  "MEMORYCOND", "GENETIC_ANY", "resil6to17_22_ord", "finishes_22_ord"
)

d1     <- make_domain_data(dt_6_17, domain1_vars)
split1 <- initial_split(d1, prop = 0.80, strata = ADHD_ModSev)
train1 <- training(split1); test1 <- testing(split1)
rec1   <- make_recipe(train1)

fit_log1   <- workflow() %>% add_recipe(rec1) %>% add_model(mod_log)   %>% fit(train1)
fit_lasso1 <- workflow() %>% add_recipe(rec1) %>% add_model(mod_lasso) %>% fit(train1)
fit_rf1    <- workflow() %>% add_recipe(rec1) %>% add_model(mod_rf)    %>% fit(train1)

p_log1   <- eval_probs(fit_log1,   test1)
p_lasso1 <- eval_probs(fit_lasso1, test1)
p_rf1    <- eval_probs(fit_rf1,    test1)

res_all <- bind_rows(
  eval_metrics(p_log1)   %>% mutate(domain = "Individual", model = "Logistic"),
  eval_metrics(p_lasso1) %>% mutate(domain = "Individual", model = "LASSO"),
  eval_metrics(p_rf1)    %>% mutate(domain = "Individual", model = "RF")
)
cat("Domain 1 results:\n")
print(res_all %>% select(model, .metric, .estimate) %>% arrange(.metric))

# ROC curves
roc_log1   <- roc(test1$ADHD_ModSev, p_log1$.pred_Yes,   levels = c("No","Yes"), direction = "<")
roc_lasso1 <- roc(test1$ADHD_ModSev, p_lasso1$.pred_Yes, levels = c("No","Yes"), direction = "<")
roc_rf1    <- roc(test1$ADHD_ModSev, p_rf1$.pred_Yes,    levels = c("No","Yes"), direction = "<")

plot(roc_log1, col = "blue", lwd = 2, main = "ROC Curves - Domain 1 (Individual)")
lines(roc_lasso1, col = "darkgreen", lwd = 2)
lines(roc_rf1,    col = "red",       lwd = 2)
legend("bottomright",
       legend = c(paste0("Logistic (AUC=", round(auc(roc_log1),   3), ")"),
                  paste0("LASSO (AUC=",    round(auc(roc_lasso1), 3), ")"),
                  paste0("RF (AUC=",       round(auc(roc_rf1),    3), ")")),
       col = c("blue", "darkgreen", "red"), lwd = 2)

# PR curves
pr_fn <- function(fit_obj, test_df, col) {
  probs  <- predict(fit_obj, test_df, type = "prob")$.pred_Yes
  labels <- test_df$ADHD_ModSev == "Yes"
  pr <- pr.curve(scores.class0 = probs[labels], scores.class1 = probs[!labels], curve = TRUE)
  lines(pr$curve[, 1], pr$curve[, 2], col = col, lwd = 2)
}
plot(NULL, xlim = c(0,1), ylim = c(0,1),
     xlab = "Recall", ylab = "Precision", main = "PR Curves - Domain 1")
pr_fn(fit_log1,   test1, "blue")
pr_fn(fit_lasso1, test1, "darkgreen")
pr_fn(fit_rf1,    test1, "red")
legend("topright", legend = c("Logistic","LASSO","RF"),
       col = c("blue","darkgreen","red"), lwd = 2)

# LASSO feature importance
lasso1_coefs <- tidy(extract_fit_parsnip(fit_lasso1)) %>%
  filter(term != "(Intercept)", abs(estimate) > 0.05) %>%
  mutate(term = reorder(term, abs(estimate)))

print(ggplot(lasso1_coefs, aes(estimate, term)) +
        geom_col(fill = "#2c7fb8") +
        labs(title = "LASSO Feature Importance - Domain 1", x = "Coefficient", y = "Predictor") +
        theme_minimal())

# RF feature importance
rf1_fit <- extract_fit_parsnip(fit_rf1)$fit
rf1_imp <- data.frame(variable = names(rf1_fit$variable.importance),
                      importance = as.numeric(rf1_fit$variable.importance)) %>%
  slice_max(importance, n = 15)

print(ggplot(rf1_imp, aes(importance, reorder(variable, importance))) +
        geom_col(fill = "#d95f02") +
        labs(title = "RF Variable Importance - Domain 1", x = "Importance", y = "Predictor") +
        theme_minimal())


# DOMAIN 2 - Family & Household
# parental mental health, SES, ACEs, household structure

cat("\n--- DOMAIN 2: Family & Household ---\n")

domain2_vars <- c(
  "MotherMH_22_ord", "FatherMH_22_ord", "ParAggrav_22_bin", "EmSupport_22_bin",
  "FoodSit_22_ord", "povlev4_22_ord", "AdultEduc_22_ord", "ACE6ctHH_22_num",
  "smoking_22_bin"
)

d2     <- make_domain_data(dt_6_17, domain2_vars)
split2 <- initial_split(d2, prop = 0.80, strata = ADHD_ModSev)
train2 <- training(split2); test2 <- testing(split2)
rec2   <- make_recipe(train2)

fit_log2   <- workflow() %>% add_recipe(rec2) %>% add_model(mod_log)   %>% fit(train2)
fit_lasso2 <- workflow() %>% add_recipe(rec2) %>% add_model(mod_lasso) %>% fit(train2)

p_log2   <- eval_probs(fit_log2,   test2)
p_lasso2 <- eval_probs(fit_lasso2, test2)

res_domain2 <- bind_rows(
  eval_metrics(p_log2)   %>% mutate(domain = "Family", model = "Logistic"),
  eval_metrics(p_lasso2) %>% mutate(domain = "Family", model = "LASSO")
)
cat("Domain 2 results:\n")
print(res_domain2 %>% select(model, .metric, .estimate) %>% arrange(.metric))

# ROC
roc_log2   <- roc(p_log2$ADHD_ModSev,   p_log2$.pred_Yes,   levels = c("No","Yes"), direction = "<")
roc_lasso2 <- roc(p_lasso2$ADHD_ModSev, p_lasso2$.pred_Yes, levels = c("No","Yes"), direction = "<")

plot(roc_log2, col = "blue", legacy.axes = TRUE, main = "ROC Curves - Domain 2 (Family)")
lines(roc_lasso2, col = "darkgreen", lwd = 2)
legend("bottomright",
       legend = c(paste0("Logistic (AUC=", round(auc(roc_log2),   3), ")"),
                  paste0("LASSO (AUC=",    round(auc(roc_lasso2), 3), ")")),
       col = c("blue","darkgreen"), lwd = 2)

# PR
pr_log2   <- pr.curve(scores.class0 = p_log2$.pred_Yes[p_log2$ADHD_ModSev=="Yes"],
                      scores.class1 = p_log2$.pred_Yes[p_log2$ADHD_ModSev=="No"], curve = TRUE)
pr_lasso2 <- pr.curve(scores.class0 = p_lasso2$.pred_Yes[p_lasso2$ADHD_ModSev=="Yes"],
                      scores.class1 = p_lasso2$.pred_Yes[p_lasso2$ADHD_ModSev=="No"], curve = TRUE)

plot(pr_log2$curve[,1],   pr_log2$curve[,2],   type="l", col="blue",      lwd=2,
     xlab="Recall", ylab="Precision", main="PR Curves - Domain 2")
lines(pr_lasso2$curve[,1], pr_lasso2$curve[,2], col="darkgreen", lwd=2)
legend("topright",
       legend = c(paste0("Logistic (PR-AUC=", round(pr_log2$auc.integral,   3), ")"),
                  paste0("LASSO (PR-AUC=",    round(pr_lasso2$auc.integral, 3), ")")),
       col = c("blue","darkgreen"), lwd = 2)

# LASSO importance
lasso2_coefs <- tidy(extract_fit_parsnip(fit_lasso2)) %>%
  filter(term != "(Intercept)", estimate != 0) %>%
  arrange(desc(abs(estimate))) %>%
  mutate(term = gsub("_ord_|_bin", " ", term), term = gsub("_", " ", term))

print(ggplot(lasso2_coefs, aes(reorder(term, estimate), estimate)) +
        geom_col(fill = "#2c7fb8") + coord_flip() +
        labs(title = "LASSO Feature Importance - Domain 2", x = "Predictor", y = "Coefficient") +
        theme_minimal())

# Odds ratios
log2_or <- tidy(extract_fit_parsnip(fit_log2), exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>% arrange(desc(estimate)) %>% slice(1:8)

print(ggplot(log2_or, aes(estimate, reorder(term, estimate))) +
        geom_point(size = 3) +
        geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
        geom_vline(xintercept = 1, linetype = "dashed") +
        labs(title = "Odds Ratios - Domain 2 Logistic", x = "Odds Ratio (95% CI)", y = "Predictor") +
        theme_minimal())


# DOMAIN 3 - Community & Environment
# neighborhood safety, bullying, peer difficulties

cat("\n--- DOMAIN 3: Community ---\n")

domain3_vars <- c(
  "nbhd_safe_ord", "nbhd_help_ord", "ACE4ctCom_22_num",
  "school_safe_ord", "bullied_freq_ord", "friend_diff_ord"
)

d3     <- make_domain_data(dt_6_17, domain3_vars)
split3 <- initial_split(d3, prop = 0.80, strata = ADHD_ModSev)
train3 <- training(split3); test3 <- testing(split3)
rec3   <- make_recipe(train3)

mod_rf3 <- rand_forest(trees = 300, mtry = 3, min_n = 10) %>%
  set_engine("ranger", importance = "impurity", num.threads = parallel::detectCores()) %>%
  set_mode("classification")

fit_log3   <- workflow() %>% add_recipe(rec3) %>% add_model(mod_log)   %>% fit(train3)
fit_lasso3 <- workflow() %>% add_recipe(rec3) %>% add_model(mod_lasso) %>% fit(train3)
fit_rf3    <- workflow() %>% add_recipe(rec3) %>% add_model(mod_rf3)   %>% fit(train3)

p_log3   <- eval_probs(fit_log3,   test3)
p_lasso3 <- eval_probs(fit_lasso3, test3)
p_rf3    <- eval_probs(fit_rf3,    test3)

res_domain3 <- bind_rows(
  eval_metrics(p_log3)   %>% mutate(domain = "Community", model = "Logistic"),
  eval_metrics(p_lasso3) %>% mutate(domain = "Community", model = "LASSO"),
  eval_metrics(p_rf3)    %>% mutate(domain = "Community", model = "RF")
)
cat("Domain 3 results:\n")
print(res_domain3 %>% select(model, .metric, .estimate) %>% arrange(.metric))

# ROC + PR using tidymodels (cleaner for 3-model comparison)
roc_df3 <- bind_rows(
  roc_curve(p_log3,   ADHD_ModSev, .pred_Yes, event_level="second") %>% mutate(model="Logistic"),
  roc_curve(p_lasso3, ADHD_ModSev, .pred_Yes, event_level="second") %>% mutate(model="LASSO"),
  roc_curve(p_rf3,    ADHD_ModSev, .pred_Yes, event_level="second") %>% mutate(model="RF")
)
print(ggplot(roc_df3, aes(1-specificity, sensitivity, color=model)) +
        geom_path(linewidth=1) + geom_abline(linetype="dashed") + coord_equal() +
        labs(title="Domain 3 ROC Curve", x="False Positive Rate", y="True Positive Rate") +
        theme_minimal())

pr_df3 <- bind_rows(
  pr_curve(p_log3,   ADHD_ModSev, .pred_Yes, event_level="second") %>% mutate(model="Logistic"),
  pr_curve(p_lasso3, ADHD_ModSev, .pred_Yes, event_level="second") %>% mutate(model="LASSO"),
  pr_curve(p_rf3,    ADHD_ModSev, .pred_Yes, event_level="second") %>% mutate(model="RF")
)
print(ggplot(pr_df3, aes(recall, precision, color=model)) +
        geom_path(linewidth=1) +
        labs(title="Domain 3 PR Curve", x="Recall", y="Precision") + theme_minimal())

# Feature importance
lasso3_imp <- tidy(extract_fit_parsnip(fit_lasso3)) %>%
  filter(term != "(Intercept)", estimate != 0) %>%
  mutate(abs_est = abs(estimate)) %>% arrange(desc(abs_est))
print(ggplot(lasso3_imp, aes(abs_est, reorder(term, abs_est))) +
        geom_col() + labs(title="Domain 3 LASSO Importance", x="|Coefficient|", y="Feature") +
        theme_minimal())

rf3_imp <- tibble(term = names(extract_fit_parsnip(fit_rf3)$fit$variable.importance),
                  importance = as.numeric(extract_fit_parsnip(fit_rf3)$fit$variable.importance)) %>%
  arrange(desc(importance)) %>% slice_head(n=20)
print(ggplot(rf3_imp, aes(importance, reorder(term, importance))) +
        geom_col() + labs(title="Domain 3 RF Importance", x="Importance", y="Feature") +
        theme_minimal())


# DOMAIN 4 - Behavioral & Functional

cat("\n--- DOMAIN 4: Behavioral ---\n")

domain4_vars <- c(
  "schl_engage_ord", "physact_ord", "missed_school_cat",
  "diffcare_ord", "sports_bin", "aftschact_bin",
  "screentime_ord", "hrssleep_bin"
)

d4     <- make_domain_data(dt_6_17, domain4_vars)
split4 <- initial_split(d4, prop = 0.80, strata = ADHD_ModSev)
train4 <- training(split4); test4 <- testing(split4)
rec4   <- make_recipe(train4)

mod_rf4 <- rand_forest(trees = 400, mtry = 5, min_n = 10) %>%
  set_engine("ranger", importance = "impurity", num.threads = parallel::detectCores()) %>%
  set_mode("classification")

cat("fitting domain 4 models...\n")
fit_log4   <- workflow() %>% add_recipe(rec4) %>% add_model(mod_log)   %>% fit(train4)
fit_lasso4 <- workflow() %>% add_recipe(rec4) %>% add_model(mod_lasso) %>% fit(train4)
fit_rf4    <- workflow() %>% add_recipe(rec4) %>% add_model(mod_rf4)   %>% fit(train4)

p_log4   <- eval_probs(fit_log4,   test4)
p_lasso4 <- eval_probs(fit_lasso4, test4)
p_rf4    <- eval_probs(fit_rf4,    test4)

res_domain4 <- bind_rows(
  eval_metrics(p_log4)   %>% mutate(domain = "Behavioral", model = "Logistic"),
  eval_metrics(p_lasso4) %>% mutate(domain = "Behavioral", model = "LASSO"),
  eval_metrics(p_rf4)    %>% mutate(domain = "Behavioral", model = "RF")
)
cat("Domain 4 results:\n")
print(res_domain4 %>% select(model, .metric, .estimate) %>% arrange(.metric))

roc_df4 <- bind_rows(
  roc_curve(p_log4,   ADHD_ModSev, .pred_Yes, event_level="second") %>% mutate(model="Logistic"),
  roc_curve(p_lasso4, ADHD_ModSev, .pred_Yes, event_level="second") %>% mutate(model="LASSO"),
  roc_curve(p_rf4,    ADHD_ModSev, .pred_Yes, event_level="second") %>% mutate(model="RF")
)
print(ggplot(roc_df4, aes(1-specificity, sensitivity, color=model)) +
        geom_path(linewidth=1) + geom_abline(linetype="dashed") + coord_equal() +
        labs(title="Domain 4 ROC Curve", x="False Positive Rate", y="True Positive Rate") +
        theme_minimal())

pr_df4 <- bind_rows(
  pr_curve(p_log4,   ADHD_ModSev, .pred_Yes, event_level="second") %>% mutate(model="Logistic"),
  pr_curve(p_lasso4, ADHD_ModSev, .pred_Yes, event_level="second") %>% mutate(model="LASSO"),
  pr_curve(p_rf4,    ADHD_ModSev, .pred_Yes, event_level="second") %>% mutate(model="RF")
)
print(ggplot(pr_df4, aes(recall, precision, color=model)) +
        geom_path(linewidth=1) +
        labs(title="Domain 4 PR Curve", x="Recall", y="Precision") + theme_minimal())

lasso4_imp <- tidy(extract_fit_parsnip(fit_lasso4)) %>%
  filter(term != "(Intercept)", estimate != 0) %>%
  mutate(abs_est = abs(estimate)) %>% arrange(desc(abs_est))
print(ggplot(lasso4_imp, aes(abs_est, reorder(term, abs_est))) +
        geom_col(fill="#2c7fb8") +
        labs(title="Domain 4 LASSO Importance", x="|Coefficient|", y="Feature") + theme_minimal())

rf4_imp <- tibble(term = names(extract_fit_parsnip(fit_rf4)$fit$variable.importance),
                  importance = as.numeric(extract_fit_parsnip(fit_rf4)$fit$variable.importance)) %>%
  arrange(desc(importance)) %>% slice_head(n=20)
print(ggplot(rf4_imp, aes(importance, reorder(term, importance))) +
        geom_col(fill="#d95f02") +
        labs(title="Domain 4 RF Importance", x="Importance", y="Feature") + theme_minimal())


# DOMAIN 5 - Healthcare Access (primary model)
# family centered care, unmet needs, care coordination
# NOT including treatment variables - they are downstream of severity
# kids get medication BECAUSE they have severe ADHD, not the other way around
# treatment variables used in sensitivity model in Combinedmodel.R

cat("\n--- DOMAIN 5: Healthcare Access ---\n")

domain5_vars <- c(
  "famcent_bin_quality",    # family centred care quality
  "unmetfrust_bin",         # unmet needs + frustrated
  "frust_freq_ord",         # frequency of frustration
  "helpcoord_bin_universe", # got care coordination help
  "allextrahelp_ord"        # received all extra help needed
)

d5     <- make_domain_data(dt_6_17, domain5_vars)
split5 <- initial_split(d5, prop = 0.80, strata = ADHD_ModSev)
train5 <- training(split5); test5 <- testing(split5)
rec5   <- make_recipe(train5)

mod_rf5 <- rand_forest(trees = 400, mtry = 4, min_n = 10) %>%
  set_engine("ranger", importance = "impurity", num.threads = parallel::detectCores()) %>%
  set_mode("classification")

fit_log5   <- workflow() %>% add_recipe(rec5) %>% add_model(mod_log)   %>% fit(train5)
fit_lasso5 <- workflow() %>% add_recipe(rec5) %>% add_model(mod_lasso) %>% fit(train5)
fit_rf5    <- workflow() %>% add_recipe(rec5) %>% add_model(mod_rf5)   %>% fit(train5)

p_log5   <- eval_probs(fit_log5,   test5)
p_lasso5 <- eval_probs(fit_lasso5, test5)
p_rf5    <- eval_probs(fit_rf5,    test5)

res_domain5 <- bind_rows(
  eval_metrics(p_log5)   %>% mutate(domain = "Healthcare", model = "Logistic"),
  eval_metrics(p_lasso5) %>% mutate(domain = "Healthcare", model = "LASSO"),
  eval_metrics(p_rf5)    %>% mutate(domain = "Healthcare", model = "RF")
)
cat("Domain 5 results:\n")
print(res_domain5 %>% select(model, .metric, .estimate) %>% arrange(.metric))

roc_df5 <- bind_rows(
  roc_curve(p_log5,   ADHD_ModSev, .pred_Yes, event_level="second") %>% mutate(model="Logistic"),
  roc_curve(p_lasso5, ADHD_ModSev, .pred_Yes, event_level="second") %>% mutate(model="LASSO"),
  roc_curve(p_rf5,    ADHD_ModSev, .pred_Yes, event_level="second") %>% mutate(model="RF")
)
print(ggplot(roc_df5, aes(1-specificity, sensitivity, color=model)) +
        geom_path(linewidth=1) + geom_abline(linetype="dashed") + coord_equal() +
        labs(title="Domain 5 ROC Curve", x="False Positive Rate", y="True Positive Rate") +
        theme_minimal())

pr_df5 <- bind_rows(
  pr_curve(p_log5,   ADHD_ModSev, .pred_Yes, event_level="second") %>% mutate(model="Logistic"),
  pr_curve(p_lasso5, ADHD_ModSev, .pred_Yes, event_level="second") %>% mutate(model="LASSO"),
  pr_curve(p_rf5,    ADHD_ModSev, .pred_Yes, event_level="second") %>% mutate(model="RF")
)
print(ggplot(pr_df5, aes(recall, precision, color=model)) +
        geom_path(linewidth=1) +
        labs(title="Domain 5 PR Curve", x="Recall", y="Precision") + theme_minimal())

lasso5_imp <- tidy(extract_fit_parsnip(fit_lasso5)) %>%
  filter(term != "(Intercept)", estimate != 0) %>%
  mutate(abs_est = abs(estimate)) %>% arrange(desc(abs_est))
print(ggplot(lasso5_imp, aes(abs_est, reorder(term, abs_est))) +
        geom_col() + labs(title="Domain 5 LASSO Importance", x="|Coefficient|", y="Feature") +
        theme_minimal())

rf5_imp <- tibble(term = names(extract_fit_parsnip(fit_rf5)$fit$variable.importance),
                  importance = as.numeric(extract_fit_parsnip(fit_rf5)$fit$variable.importance)) %>%
  arrange(desc(importance))
print(ggplot(rf5_imp, aes(importance, reorder(term, importance))) +
        geom_col() + labs(title="Domain 5 RF Importance", x="Importance", y="Feature") +
        theme_minimal())

cat("\ndone - all domain models complete\n")
cat("result objects created: res_all, res_domain2, res_domain3, res_domain4, res_domain5\n")
cat("next: run Combinedmodel.R\n")
