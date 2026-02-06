# =============================================================================
# 04_regression_main.R - Main DID Regression Analysis
# =============================================================================
# Project: Nepal Civil Conflict and International Migration
# Author: Ramesh Dulal
#
# REQUIRES: 02_data_cleaning.R must be run first
#
# OUTCOMES:
# 1. international_migrant
# 2. international_absentee_only
# 3. present_ind_migrant
#
# TREATMENT MEASURES:
# A. Continuous: mwar_own_any (months of war)
# B. Continuous: cas_own_any (casualties)
# C. Binary: high_conflict_q3_binary (war-based)
# D. Binary: high_conflict_casualty_binary (casualty-based)
#
# OUTPUTS:
# - Table_1A through Table_3D (regression tables)
# =============================================================================

# Load helper functions
source(file.path(code_path, "functions/Helper_functions.R"))

cat("Running main regression analysis...\n")

# Standard coefficient dictionary for all tables
coef_dict <- get_coef_dict()
coef_order <- get_coef_order()

# =============================================================================
# OUTCOME 1: INTERNATIONAL_MIGRANT
# =============================================================================

cat("  Outcome 1: International Migrant...\n")

# ---------- 1A: MONTHS OF WAR (CONTINUOUS) ----------

model_1a <- feols(international_migrant ~ treatment * mwar_own_any, 
                  data = nlss_conflict_data, cluster = ~dist)

model_1b <- feols(international_migrant ~ treatment * mwar_own_any + 
                    sex + age + I(age^2), 
                  data = nlss_conflict_data, cluster = ~dist)

model_1c <- feols(international_migrant ~ treatment * mwar_own_any + 
                    sex + age + I(age^2) + factor(education_category), 
                  data = nlss_conflict_data, cluster = ~dist)

model_1d <- feols(international_migrant ~ treatment * mwar_own_any + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity), 
                  data = nlss_conflict_data, cluster = ~dist)

model_1e <- feols(international_migrant ~ treatment * mwar_own_any + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity) | dist, 
                  data = nlss_conflict_data, cluster = ~dist)

# Display in console
etable(model_1a, model_1b, model_1c, model_1d, model_1e,
       title = "International Migrant ~ Treatment × Months of War",
       headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
       cluster = ~dist)

# Export to LaTeX
etable(model_1a, model_1b, model_1c, model_1d, model_1e,
       title = "International Migrant ~ Treatment × Months of War",
       headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
       dict = coef_dict, order = coef_order,
       tex = TRUE,
       file = file.path(output_path, "Table_1A_IntMigrant_War.tex"),
       replace = TRUE)


# ---------- 1B: CASUALTIES (CONTINUOUS) ----------

model_1k <- feols(international_migrant ~ treatment * cas_own_any, 
                  data = nlss_conflict_data, cluster = ~dist)

model_1l <- feols(international_migrant ~ treatment * cas_own_any + 
                    sex + age + I(age^2), 
                  data = nlss_conflict_data, cluster = ~dist)

model_1m <- feols(international_migrant ~ treatment * cas_own_any + 
                    sex + age + I(age^2) + factor(education_category), 
                  data = nlss_conflict_data, cluster = ~dist)

model_1n <- feols(international_migrant ~ treatment * cas_own_any + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity), 
                  data = nlss_conflict_data, cluster = ~dist)

model_1o <- feols(international_migrant ~ treatment * cas_own_any + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity) | dist, 
                  data = nlss_conflict_data, cluster = ~dist)

etable(model_1k, model_1l, model_1m, model_1n, model_1o,
       title = "International Migrant ~ Treatment × Casualties",
       headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
       dict = coef_dict, order = coef_order,
       tex = TRUE,
       file = file.path(output_path, "Table_1B_IntMigrant_Casualty.tex"),
       replace = TRUE)


# ---------- 1C: HIGH CONFLICT BINARY (WAR) ----------

model_1f <- feols(international_migrant ~ treatment * high_conflict_q3_binary, 
                  data = nlss_conflict_data, cluster = ~dist)

model_1g <- feols(international_migrant ~ treatment * high_conflict_q3_binary + 
                    sex + age + I(age^2), 
                  data = nlss_conflict_data, cluster = ~dist)

model_1h <- feols(international_migrant ~ treatment * high_conflict_q3_binary + 
                    sex + age + I(age^2) + factor(education_category), 
                  data = nlss_conflict_data, cluster = ~dist)

model_1i <- feols(international_migrant ~ treatment * high_conflict_q3_binary + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity), 
                  data = nlss_conflict_data, cluster = ~dist)

model_1j <- feols(international_migrant ~ treatment * high_conflict_q3_binary + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity) | dist, 
                  data = nlss_conflict_data, cluster = ~dist)

etable(model_1f, model_1g, model_1h, model_1i, model_1j,
       title = "International Migrant ~ Treatment × High Conflict Binary (War)",
       headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
       dict = coef_dict, order = coef_order,
       tex = TRUE,
       file = file.path(output_path, "Table_1C_IntMigrant_HighConflict_War.tex"),
       replace = TRUE)


# ---------- 1D: HIGH CONFLICT BINARY (CASUALTY) ----------

model_1p <- feols(international_migrant ~ treatment * high_conflict_casualty_binary, 
                  data = nlss_conflict_data, cluster = ~dist)

model_1q <- feols(international_migrant ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2), 
                  data = nlss_conflict_data, cluster = ~dist)

model_1r <- feols(international_migrant ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2) + factor(education_category), 
                  data = nlss_conflict_data, cluster = ~dist)

model_1s <- feols(international_migrant ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity), 
                  data = nlss_conflict_data, cluster = ~dist)

model_1t <- feols(international_migrant ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity) | dist, 
                  data = nlss_conflict_data, cluster = ~dist)

etable(model_1p, model_1q, model_1r, model_1s, model_1t,
       title = "International Migrant ~ Treatment × High Conflict Binary (Casualty)",
       headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
       dict = coef_dict, order = coef_order,
       tex = TRUE,
       file = file.path(output_path, "Table_1D_IntMigrant_HighConflict_Casualty.tex"),
       replace = TRUE)


# =============================================================================
# OUTCOME 2: INTERNATIONAL_ABSENTEE_ONLY
# =============================================================================

cat("  Outcome 2: International Absentee Only...\n")

# ---------- 2A: MONTHS OF WAR ----------

model_2a <- feols(international_absentee_only ~ treatment * mwar_own_any, 
                  data = nlss_conflict_data, cluster = ~dist)

model_2b <- feols(international_absentee_only ~ treatment * mwar_own_any + 
                    sex + age + I(age^2), 
                  data = nlss_conflict_data, cluster = ~dist)

model_2c <- feols(international_absentee_only ~ treatment * mwar_own_any + 
                    sex + age + I(age^2) + factor(education_category), 
                  data = nlss_conflict_data, cluster = ~dist)

model_2d <- feols(international_absentee_only ~ treatment * mwar_own_any + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity), 
                  data = nlss_conflict_data, cluster = ~dist)

model_2e <- feols(international_absentee_only ~ treatment * mwar_own_any + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity) | dist, 
                  data = nlss_conflict_data, cluster = ~dist)

etable(model_2a, model_2b, model_2c, model_2d, model_2e,
       title = "Absentee Only ~ Treatment × Months of War",
       headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
       dict = coef_dict, order = coef_order,
       tex = TRUE,
       file = file.path(output_path, "Table_2A_Absentee_War.tex"),
       replace = TRUE)


# ---------- 2B: CASUALTIES ----------

model_2k <- feols(international_absentee_only ~ treatment * cas_own_any, 
                  data = nlss_conflict_data, cluster = ~dist)

model_2l <- feols(international_absentee_only ~ treatment * cas_own_any + 
                    sex + age + I(age^2), 
                  data = nlss_conflict_data, cluster = ~dist)

model_2m <- feols(international_absentee_only ~ treatment * cas_own_any + 
                    sex + age + I(age^2) + factor(education_category), 
                  data = nlss_conflict_data, cluster = ~dist)

model_2n <- feols(international_absentee_only ~ treatment * cas_own_any + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity), 
                  data = nlss_conflict_data, cluster = ~dist)

model_2o <- feols(international_absentee_only ~ treatment * cas_own_any + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity) | dist, 
                  data = nlss_conflict_data, cluster = ~dist)

etable(model_2k, model_2l, model_2m, model_2n, model_2o,
       title = "Absentee Only ~ Treatment × Casualties",
       headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
       dict = coef_dict, order = coef_order,
       tex = TRUE,
       file = file.path(output_path, "Table_2B_Absentee_Casualty.tex"),
       replace = TRUE)


# ---------- 2C: HIGH CONFLICT (WAR) ----------

model_2f <- feols(international_absentee_only ~ treatment * high_conflict_q3_binary, 
                  data = nlss_conflict_data, cluster = ~dist)

model_2g <- feols(international_absentee_only ~ treatment * high_conflict_q3_binary + 
                    sex + age + I(age^2), 
                  data = nlss_conflict_data, cluster = ~dist)

model_2h <- feols(international_absentee_only ~ treatment * high_conflict_q3_binary + 
                    sex + age + I(age^2) + factor(education_category), 
                  data = nlss_conflict_data, cluster = ~dist)

model_2i <- feols(international_absentee_only ~ treatment * high_conflict_q3_binary + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity), 
                  data = nlss_conflict_data, cluster = ~dist)

model_2j <- feols(international_absentee_only ~ treatment * high_conflict_q3_binary + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity) | dist, 
                  data = nlss_conflict_data, cluster = ~dist)

etable(model_2f, model_2g, model_2h, model_2i, model_2j,
       title = "Absentee Only ~ Treatment × High Conflict Binary (War)",
       headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
       dict = coef_dict, order = coef_order,
       tex = TRUE,
       file = file.path(output_path, "Table_2C_Absentee_HighConflict_War.tex"),
       replace = TRUE)


# ---------- 2D: HIGH CONFLICT (CASUALTY) ----------

model_2p <- feols(international_absentee_only ~ treatment * high_conflict_casualty_binary, 
                  data = nlss_conflict_data, cluster = ~dist)

model_2q <- feols(international_absentee_only ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2), 
                  data = nlss_conflict_data, cluster = ~dist)

model_2r <- feols(international_absentee_only ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2) + factor(education_category), 
                  data = nlss_conflict_data, cluster = ~dist)

model_2s <- feols(international_absentee_only ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity), 
                  data = nlss_conflict_data, cluster = ~dist)

model_2t <- feols(international_absentee_only ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity) | dist, 
                  data = nlss_conflict_data, cluster = ~dist)

etable(model_2p, model_2q, model_2r, model_2s, model_2t,
       title = "Absentee Only ~ Treatment × High Conflict Binary (Casualty)",
       headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
       dict = coef_dict, order = coef_order,
       tex = TRUE,
       file = file.path(output_path, "Table_2D_Absentee_HighConflict_Casualty.tex"),
       replace = TRUE)


# =============================================================================
# OUTCOME 3: PRESENT_IND_MIGRANT (Return Migrant)
# =============================================================================

cat("  Outcome 3: Return Migrant...\n")

# ---------- 3D: HIGH CONFLICT (CASUALTY) - MAIN SPECIFICATION ----------

model_3p <- feols(present_ind_migrant ~ treatment * high_conflict_casualty_binary, 
                  data = nlss_conflict_data, cluster = ~dist)

model_3q <- feols(present_ind_migrant ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2), 
                  data = nlss_conflict_data, cluster = ~dist)

model_3r <- feols(present_ind_migrant ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2) + factor(education_category), 
                  data = nlss_conflict_data, cluster = ~dist)

model_3s <- feols(present_ind_migrant ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity), 
                  data = nlss_conflict_data, cluster = ~dist)

model_3t <- feols(present_ind_migrant ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity) | dist, 
                  data = nlss_conflict_data, cluster = ~dist)

etable(model_3p, model_3q, model_3r, model_3s, model_3t,
       title = "Return Migrant ~ Treatment × High Conflict Binary (Casualty)",
       headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
       dict = coef_dict, order = coef_order,
       tex = TRUE,
       file = file.path(output_path, "Table_3D_ReturnMigrant_HighConflict_Casualty.tex"),
       replace = TRUE)


# =============================================================================
# COMBINED TABLE: MAIN RESULTS (ALL 3 OUTCOMES, PREFERRED SPECIFICATION)
# =============================================================================

cat("  Creating combined main results table...\n")

# Preferred specification: Full controls + District FE
etable(model_1t, model_2t, model_3t,
       title = "Main Results: Conflict Effects on Migration (Full Model)",
       headers = c("Int. Migrant", "Absentee", "Return"),
       dict = coef_dict, order = coef_order,
       tex = TRUE,
       file = file.path(output_path, "Table_Main_Results.tex"),
       replace = TRUE)

cat("\n✓ Main regression analysis complete!\n")