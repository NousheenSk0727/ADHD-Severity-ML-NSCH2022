# loads the subset csv, recodes ALL variables across all 5 domains,
# and saves a complete clean dataset as RDS
#
# run this ONCE at the start of every new R session
# after this, every other script just loads the RDS and goes straight to analysis

library(data.table)
library(dplyr)

# load subset and age filter
dt_6_17 <- fread("/Users/nousheenjahanshaik/Documents/BigDataAnalytics/NACHFINALPROJECT/DATA/NSCH_Subset.csv")

dt_6_17 <- dt_6_17 %>% filter(SC_AGE_YEARS >= 6, SC_AGE_YEARS <= 17)
cat("rows after age filter:", nrow(dt_6_17), "\n")

# outcome variable

dt_6_17 <- dt_6_17 %>%
  mutate(
    ADHDSev_22_ord = factor(
      case_when(
        ADHDSev_22 %in% c(95, 99) ~ NA_real_,
        ADHDSev_22 %in% 1:3       ~ as.numeric(ADHDSev_22)
      ),
      levels = 1:3, ordered = TRUE,
      labels = c("None", "Mild", "Moderate/Severe")
    )
  )

cat("outcome distribution:\n")
print(table(dt_6_17$ADHDSev_22_ord, useNA = "ifany"))

# DOMAIN 1 - Individual characteristics

dt_6_17 <- dt_6_17 %>%
  mutate(
    # sex
    SC_SEX = factor(case_when(
      SC_SEX == 1 ~ "Male",
      SC_SEX == 2 ~ "Female",
      TRUE        ~ NA_character_
    ), levels = c("Male", "Female")),

    # race
    SC_RACE_R = factor(SC_RACE_R),

    # comorbidities - 1=yes, 2=no, 95=legitimate skip (no condition), 99=missing
    K2Q30B = case_when(K2Q30B == 1 ~ 1, K2Q30B == 2 ~ 0, K2Q30B == 95 ~ 0, TRUE ~ NA_real_),
    K2Q33B = case_when(K2Q33B == 1 ~ 1, K2Q33B == 2 ~ 0, K2Q33B == 95 ~ 0, TRUE ~ NA_real_),
    K2Q32B = case_when(K2Q32B == 1 ~ 1, K2Q32B == 2 ~ 0, K2Q32B == 95 ~ 0, TRUE ~ NA_real_),
    K2Q35B = case_when(K2Q35B == 1 ~ 1, K2Q35B == 2 ~ 0, K2Q35B == 95 ~ 0, TRUE ~ NA_real_),
    K2Q36B = case_when(K2Q36B == 1 ~ 1, K2Q36B == 2 ~ 0, K2Q36B == 95 ~ 0, TRUE ~ NA_real_),
    K2Q37B = case_when(K2Q37B == 1 ~ 1, K2Q37B == 2 ~ 0, K2Q37B == 95 ~ 0, TRUE ~ NA_real_),
    K2Q42A = case_when(K2Q42A == 1 ~ 1, K2Q42A == 2 ~ 0, K2Q42A == 99 ~ NA_real_, TRUE ~ NA_real_),

    # memory/concentration difficulty
    MEMORYCOND = case_when(MEMORYCOND == 1 ~ 1, MEMORYCOND == 2 ~ 0, TRUE ~ NA_real_),

    # genetic condition - any vs none
    GENETIC_ANY = case_when(
      GENETIC_DESC %in% c(1, 2, 3) ~ 1,
      GENETIC_DESC == 95            ~ 0,
      TRUE                          ~ NA_real_
    ),

    # self regulation
    resil6to17_22_ord = factor(
      case_when(K7Q85_R %in% 1:4 ~ K7Q85_R, TRUE ~ NA_real_),
      levels = 1:4, ordered = TRUE,
      labels = c("Always", "Usually", "Sometimes", "Never")
    ),
    finishes_22_ord = factor(
      case_when(K7Q84_R %in% 1:4 ~ K7Q84_R, TRUE ~ NA_real_),
      levels = 1:4, ordered = TRUE,
      labels = c("Always", "Usually", "Sometimes", "Never")
    )
  )

cat("domain 1 recoded\n")

# DOMAIN 2 - Family & Household

dt_6_17 <- dt_6_17 %>%
  mutate(
    # parental mental health - 1=excellent/very good, 2=good, 3=fair/poor
    MotherMH_22_ord = factor(
      case_when(
        MotherMH_22 %in% 1:3       ~ as.numeric(MotherMH_22),
        MotherMH_22 %in% c(95, 99) ~ NA_real_,
        TRUE                        ~ NA_real_
      ),
      levels = 1:3, ordered = TRUE,
      labels = c("Excellent/Very good", "Good", "Fair/Poor")
    ),
    FatherMH_22_ord = factor(
      case_when(
        FatherMH_22 %in% 1:3       ~ as.numeric(FatherMH_22),
        FatherMH_22 %in% c(95, 99) ~ NA_real_,
        TRUE                        ~ NA_real_
      ),
      levels = 1:3, ordered = TRUE,
      labels = c("Excellent/Very good", "Good", "Fair/Poor")
    ),

    # parental aggravation and emotional support
    ParAggrav_22_bin = case_when(
      ParAggrav_22 == 1 ~ 1, ParAggrav_22 == 2 ~ 0, TRUE ~ NA_real_
    ),
    EmSupport_22_bin = case_when(
      EmSupport_22 == 1 ~ 1, EmSupport_22 == 2 ~ 0, TRUE ~ NA_real_
    ),

    # poverty level
    povlev4_22_ord = factor(
      case_when(povlev4_22 %in% 1:4 ~ as.numeric(povlev4_22), TRUE ~ NA_real_),
      levels = 1:4, ordered = TRUE,
      labels = c("0-99% FPL", "100-199% FPL", "200-399% FPL", "400%+ FPL")
    ),

    # adult education
    AdultEduc_22_ord = factor(
      case_when(
        AdultEduc_22 %in% 1:4 ~ as.numeric(AdultEduc_22),
        AdultEduc_22 == 99    ~ NA_real_,
        TRUE                   ~ NA_real_
      ),
      levels = 1:4, ordered = TRUE,
      labels = c("Less than HS", "HS/GED", "Some college", "College degree+")
    ),

    # smoking, food security, ACE count
    smoking_22_bin = case_when(
      smoking_22 == 1 ~ 1, smoking_22 == 2 ~ 0, TRUE ~ NA_real_
    ),
    FoodSit_22_ord = factor(
      case_when(FoodSit_22 %in% 1:4 ~ as.numeric(FoodSit_22), TRUE ~ NA_real_),
      levels = 1:4, ordered = TRUE,
      labels = c("Always nutritious", "Enough but not right kinds",
                 "Sometimes not enough", "Often not enough")
    ),
    ACE6ctHH_22_num = case_when(
      ACE6ctHH_22 %in% 0:6 ~ as.numeric(ACE6ctHH_22), TRUE ~ NA_real_
    ),

    # household structure and size
    famstruct5_22_fac = factor(
      case_when(famstruct5_22 %in% 1:5 ~ as.numeric(famstruct5_22), TRUE ~ NA_real_),
      levels = 1:5,
      labels = c("Two parents married", "Two parents not married",
                 "Single parent", "Grandparent HH", "Other")
    ),
    FamCount_22_ord = factor(
      case_when(FamCount_22 %in% 1:5 ~ as.numeric(FamCount_22), TRUE ~ NA_real_),
      levels = 1:5, ordered = TRUE,
      labels = c("1-2", "3", "4", "5", "6+")
    ),
    TOTKIDS_R_ord = factor(
      case_when(TOTKIDS_R %in% 1:4 ~ as.numeric(TOTKIDS_R), TRUE ~ NA_real_),
      levels = 1:4, ordered = TRUE,
      labels = c("1 child", "2 children", "3 children", "4+ children")
    )
  )

cat("domain 2 recoded\n")

# DOMAIN 3 - Community & Environment

dt_6_17 <- dt_6_17 %>%
  mutate(
    nbhd_safe_ord = factor(
      case_when(K10Q40_R %in% 1:4 ~ K10Q40_R, TRUE ~ NA_real_),
      levels = 1:4, ordered = TRUE,
      labels = c("Definitely agree", "Somewhat agree",
                 "Somewhat disagree", "Definitely disagree")
    ),
    nbhd_help_ord = factor(
      case_when(K10Q30 %in% 1:4 ~ K10Q30, TRUE ~ NA_real_),
      levels = 1:4, ordered = TRUE,
      labels = c("Definitely agree", "Somewhat agree",
                 "Somewhat disagree", "Definitely disagree")
    ),
    ACE4ctCom_22_num = case_when(
      ACE4ctCom_22 %in% 0:4 ~ as.numeric(ACE4ctCom_22), TRUE ~ NA_real_
    ),
    school_safe_ord = factor(
      case_when(K10Q41_R %in% 1:4 ~ K10Q41_R, TRUE ~ NA_real_),
      levels = 1:4, ordered = TRUE,
      labels = c("Definitely agree", "Somewhat agree",
                 "Somewhat disagree", "Definitely disagree")
    ),
    bullied_freq_ord = factor(
      case_when(BULLIED_R %in% 1:5 ~ BULLIED_R, TRUE ~ NA_real_),
      levels = 1:5, ordered = TRUE,
      labels = c("Never", "1-2 times", "1-2/month", "1-2/week", "Almost every day")
    ),
    bullied_any_bin = case_when(
      BULLIED_R == 99     ~ NA_real_,
      BULLIED_R == 1      ~ 0,
      BULLIED_R %in% 2:5 ~ 1,
      TRUE                ~ NA_real_
    ),
    friend_diff_ord = factor(
      case_when(MAKEFRIEND %in% 1:3 ~ MAKEFRIEND, TRUE ~ NA_real_),
      levels = 1:3, ordered = TRUE,
      labels = c("No difficulty", "A little difficulty", "A lot of difficulty")
    ),
    friend_diff_any_bin = case_when(
      MAKEFRIEND == 99        ~ NA_real_,
      MAKEFRIEND == 1         ~ 0,
      MAKEFRIEND %in% c(2, 3) ~ 1,
      TRUE                    ~ NA_real_
    )
  )

cat("domain 3 recoded\n")

# DOMAIN 4 - Behavioral & Functional

dt_6_17 <- dt_6_17 %>%
  mutate(
    schl_engage_ord = factor(
      case_when(SchlEngage_22 %in% 1:3 ~ SchlEngage_22, TRUE ~ NA_real_),
      levels = 1:3, ordered = TRUE,
      labels = c("Always to both items",
                 "Always/usually to one OR usually to both",
                 "Sometimes/never to both OR any item")
    ),
    physact_ord = factor(
      case_when(PHYSACTIV %in% 1:4 ~ PHYSACTIV, TRUE ~ NA_real_),
      levels = 1:4, ordered = TRUE,
      labels = c("0 days", "1-3 days", "4-6 days", "Every day")
    ),
    missed_school_cat = factor(
      case_when(K7Q02R_R %in% 1:6 ~ K7Q02R_R, TRUE ~ NA_real_),
      levels = 1:6, ordered = TRUE,
      labels = c("No missed days", "1-3 days", "4-6 days",
                 "7-10 days", "11+ days", "Not enrolled")
    ),
    diffcare_ord = factor(
      case_when(DiffCare_22 %in% 1:4 ~ DiffCare_22, TRUE ~ NA_real_),
      levels = 1:4, ordered = TRUE,
      labels = c("Never", "Rarely", "Sometimes", "Usually/always")
    ),
    sports_bin = case_when(
      sports_22 %in% c(90, 99) ~ NA_real_,
      sports_22 == 1            ~ 1,
      sports_22 == 2            ~ 0,
      TRUE                      ~ NA_real_
    ),
    aftschact_bin = case_when(
      AftSchAct_22 %in% c(90, 99) ~ NA_real_,
      AftSchAct_22 == 1            ~ 1,
      AftSchAct_22 == 2            ~ 0,
      TRUE                         ~ NA_real_
    ),
    screentime_ord = factor(
      case_when(ScreenTime_22 %in% 1:5 ~ ScreenTime_22, TRUE ~ NA_real_),
      levels = 1:5, ordered = TRUE,
      labels = c("<1 hr", "1 hr", "2 hrs", "3 hrs", "4+ hrs")
    ),
    hrssleep_bin = case_when(
      HrsSleep_22 %in% c(95, 99) ~ NA_real_,
      HrsSleep_22 == 1            ~ 1,
      HrsSleep_22 == 2            ~ 0,
      TRUE                        ~ NA_real_
    )
  )

cat("domain 4 recoded\n")

# DOMAIN 5 - Healthcare Access & Treatment

dt_6_17 <- dt_6_17 %>%
  mutate(
    # family centred care - 0=no visit, 1=yes, 2=no
    famcent_bin_quality = case_when(
      FamCent_22 %in% c(0, 99) ~ NA_real_,
      FamCent_22 == 1           ~ 1,
      FamCent_22 == 2           ~ 0,
      TRUE                      ~ NA_real_
    ),
    # unmet needs + frustrated
    unmetfrust_bin = case_when(
      UnmetFrust_22 == 99 ~ NA_real_,
      UnmetFrust_22 == 1  ~ 0,
      UnmetFrust_22 == 2  ~ 1,
      TRUE                ~ NA_real_
    ),
    # frustration frequency
    frust_freq_ord = factor(
      case_when(frustrated_22 %in% 1:3 ~ frustrated_22, TRUE ~ NA_real_),
      levels = 1:3, ordered = TRUE,
      labels = c("Never", "Sometimes", "Always/usually")
    ),
    # care coordination help
    helpcoord_bin_universe = case_when(
      HelpCoord_22 %in% c(95, 99) ~ NA_real_,
      HelpCoord_22 == 1            ~ 1,
      HelpCoord_22 == 2            ~ 0,
      TRUE                         ~ NA_real_
    ),
    # received all extra help needed
    allextrahelp_ord = factor(
      case_when(
        AllExtraHelp_22 %in% c(95, 99) ~ NA_real_,
        AllExtraHelp_22 %in% 1:3       ~ as.numeric(AllExtraHelp_22),
        TRUE                            ~ NA_real_
      ),
      levels = 1:3, ordered = TRUE,
      labels = c("Usually got help", "Sometimes got help", "Never got help")
    ),
    # treatment variables - downstream of severity, sensitivity analysis only
    # kids get medication BECAUSE they have severe ADHD, not the other way around
    adhd_med_bin_among_adhd = case_when(
      ADHDMed_22 %in% c(95, 99) ~ NA_real_,
      ADHDMed_22 == 1            ~ 1,
      ADHDMed_22 == 2            ~ 0,
      ADHDMed_22 == 3            ~ NA_real_,
      TRUE                       ~ NA_real_
    ),
    adhd_behtx_bin_among_adhd = case_when(
      ADHDBehTreat_22 %in% c(95, 99) ~ NA_real_,
      ADHDBehTreat_22 == 1            ~ 1,
      ADHDBehTreat_22 == 2            ~ 0,
      ADHDBehTreat_22 == 3            ~ NA_real_,
      TRUE                            ~ NA_real_
    )
  )

cat("domain 5 recoded\n")

# save complete clean dataset
# all scripts load from this RDS from now on

saveRDS(
  dt_6_17,
  "/Users/nousheenjahanshaik/Documents/BigDataAnalytics/NACHFINALPROJECT/DATA/NSCH2022_ADHD_clean.rds"
)

cat("\ndone - saved complete clean dataset\n")
cat("columns in clean dataset:", ncol(dt_6_17), "\n")
cat("all future scripts just need: dt_6_17 <- readRDS('NSCH2022_ADHD_clean.rds')\n")
