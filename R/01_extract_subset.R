# the full dataset has 400+ variables, i only need ~51 so pulling those out first
# and saving as a smaller csv so i dont have to load the big file every time

library(data.table)
library(dplyr)

DATA_FILE <- "/Users/nousheenjahanshaik/Documents/BigDataAnalytics/NACHFINALPROJECT/DATA/NSCH_2022e.csv"
outcome_vars <- c("ADHDSev_22")

# predictor variables - grouped by domain
# i organized these into 5 domains based on the socioecological model
# domain 1 = individual, 2 = family, 3 = community, 4 = behavioral, 5 = healthcare

vars <- list(

  # just keeping HHID and state for ID purposes, not using in models
  `0_identifiers` = c("HHID", "FIPSST"),

  # domain 1 - individual level
  # basic demographics
  `1a_core_demographics` = c("SC_AGE_YEARS", "SC_SEX", "SC_RACE_R"),

  # current comorbidities - these are all yes/no (1/2 in NSCH coding)
  # 95 = legitimate skip meaning parent said no to screener so follow up wasnt asked
  # treating 95 as "no condition" in preprocessing
  `1b_current_comorbidities` = c(
    "K2Q30B",  # learning disability
    "K2Q33B",  # anxiety
    "K2Q32B",  # depression
    "K2Q35B",  # autism/ASD
    "K2Q36B",  # developmental delay
    "K2Q42A",  # seizure disorder
    "K2Q37B"   # speech disorder
  ),

  # cognitive/health profile
  `1c_general_bio_cognitive_profile` = c(
    "K2Q01",        # general health rating
    "GENETIC_DESC", # genetic condition - will recode in next script
    "MEMORYCOND"    # serious difficulty concentrating/remembering
  ),

  # self regulation - how well does the child manage emotions and finish tasks
  `1d_functional_self_regulation` = c(
    "K7Q85_R",  # emotional regulation
    "K7Q84_R"   # task persistence/finishing things
  ),

  # domain 2 - family and household
  # parental mental health and stress - included because parental wellbeing
  # is strongly linked to child outcomes in literature
  `2a_parent_mental_health_stress_support` = c(
    "MotherMH_22",   # mother mental health
    "FatherMH_22",   # father mental health
    "ParAggrav_22",  # parental aggravation
    "EmSupport_22"   # emotional support available to parent
  ),

  # SES and material hardship
  `2b_socioeconomic_material_hardship` = c(
    "povlev4_22",   # poverty level (% federal poverty level)
    "AdultEduc_22", # highest adult education in household
    "smoking_22",   # smoking in household
    "FoodSit_22",   # food security
    "ACE6ctHH_22"   # household ACE count (adverse childhood experiences, 0-6)
  ),

  # household structure
  `2c_household_structure_composition` = c(
    "famstruct5_22", # family structure (married, single parent etc)
    "FamCount_22",   # number of people in household
    "TOTKIDS_R"      # total number of kids
  ),

  # domain 3 - community and environment
  # neighborhood context and peer relationships
  `3a_neighborhood_peer_context` = c(
    "K10Q40_R",     # neighborhood safety
    "K10Q30",       # neighborhood support/helpfulness
    "ACE4ctCom_22", # community ACE count (discrimination, violence etc)
    "K10Q41_R",     # child feels safe at school
    "BULLIED_R",    # bullied or excluded
    "MAKEFRIEND"    # difficulty making/keeping friends
  ),

  # domain 4 - behavioral and functional
  # how ADHD is showing up in daily life
  `4a_school_activity_behavior_sleep` = c(
    "SchlEngage_22", # school engagement (cares about school, does homework)
    "PHYSACTIV",     # physical activity (days per week)
    "K7Q02R_R",      # missed school days due to illness/injury
    "DiffCare_22",   # child is harder to care for than other kids
    "sports_22",     # participates in sports
    "AftSchAct_22",  # participates in after school activities
    "ScreenTime_22", # daily screen time
    "HrsSleep_22"    # gets recommended hours of sleep
  ),

  # domain 5 - healthcare access and treatment
  # split into access (primary model) and treatment (sensitivity only)
  # NOTE: medication and behavioral treatment are downstream of severity
  # meaning kids get treatment BECAUSE they have severe ADHD
  # so including them inflates model performance artificially
  # keeping them separate and only using in sensitivity analysis
  `5a_healthcare_access_coordination_treatment` = c(
    "FamCent_22",       # family centered care (medical home quality)
    "UnmetFrust_22",    # had unmet healthcare needs + frustrated
    "frustrated_22",    # how often frustrated trying to get services
    "HelpCoord_22",     # got help coordinating care
    "AllExtraHelp_22",  # received all extra help they needed
    "ADHDMed_22",       # currently on ADHD medication -- sensitivity only
    "ADHDBehTreat_22"   # received behavioral treatment -- sensitivity only
  )
)

# combine everything into one list of column names to pull
all_predictors <- unique(unlist(vars, use.names = FALSE))
all_vars <- unique(c(outcome_vars, all_predictors))

cat("pulling", length(all_vars), "variables from raw NSCH file\n")

# fread with select is much faster than reading the whole file

cat("loading data...\n")
dt <- fread(DATA_FILE, select = all_vars, showProgress = TRUE)

# quick check - make sure all variables actually exist in the dataset
missing_cols <- setdiff(all_vars, names(dt))
if (length(missing_cols) > 0) {
  stop("these columns were not found: ", paste(missing_cols, collapse = ", "))
}

# preview
cat("\n--- variables per domain ---\n")
print(sapply(vars, length))

cat("\n--- first 5 rows ---\n")
print(head(dt, 5))

cat("\n--- structure ---\n")
str(dt)

# save the subset

write.csv(dt, "/Users/nousheenjahanshaik/Documents/BigDataAnalytics/NACHFINALPROJECT/DATA/NSCH_Subset.csv", row.names = FALSE)
cat("\ndone - saved NSCH_Subset.csv\n")
cat("next: run 02_preprocess.R\n")
