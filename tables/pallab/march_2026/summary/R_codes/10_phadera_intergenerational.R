################################################################################
# 10_phadera_intergenerational.R
#
# Replicates Phadera (2021) Equation 8 - Intergenerational specification.
# Child outcome = f(mother's conflict exposure × mother's cohort) + child controls
#
# Adaptation for NLFS data with migration as outcome:
# - Link mothers (female head/spouse/parent) to children (son/daughter/grandchild)
#   within households
# - Mother's conflict exposure = months of war / casualties in district
# - Mother's cohort = age at conflict start (same as Eq 7)
# - Child outcomes = international migration, domestic migration
# - Child controls = child birth year FE, child sex (girl indicator), birth order FE
#
# Outputs:
#   1. Mother-child pairs summary
#   2. Table 6 (Eq 8 with Phadera cohorts)
#   3. Table 7 (Eq 8 with user-defined age groups)
#   4. Excel, LaTeX, and PDF formatted tables
################################################################################

library(tidyverse)
library(haven)
library(fixest)
library(lmtest)
library(sandwich)
library(openxlsx)

# ══════════════════════════════════════════════════════════════════════════════
# CONFIGURATION & PATHS
# ══════════════════════════════════════════════════════════════════════════════

BASE_PATH <- "/Users/pallab.ghosh/Documents/GitHub/Nepal-Civil-War-and-Int-Migration"
DATA_PATH <- file.path(BASE_PATH, "data/Modified_Data/1_conflict_present_absentee_data.dta")
OUT_DIR <- file.path(BASE_PATH, "tables/pallab/march_2026/summary/results")

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

SURVEY_YEAR <- 2017
CONFLICT_START <- 1996
CONFLICT_END <- 2006

# ══════════════════════════════════════════════════════════════════════════════
# LOAD & PREPARE DATA
# ══════════════════════════════════════════════════════════════════════════════

cat("Loading data from:", DATA_PATH, "\n")
df <- read_dta(DATA_PATH)
df <- as_tibble(df)

# Create numeric versions of key variables
df <- df %>%
  mutate(
    sex_num = as.numeric(sex),
    male = if_else(sex_num == 1, 1.0, 0.0),
    female = if_else(sex_num == 2, 1.0, 0.0),
    age_num = as.numeric(age),
    birth_year = SURVEY_YEAR - age_num,
    age_at_conflict_start = CONFLICT_START - birth_year,
    age_at_conflict_end = CONFLICT_END - birth_year
  )

# Casualty variables
casualty_vars <- c('cas_own_any', 'cas_own_fatal', 'mwar_own_any', 'mwar_own_fatal',
                    'cas_nbr_any', 'cas_nbr_fatal', 'mwar_nbr_any', 'mwar_nbr_fatal')

for (var in casualty_vars) {
  df[[var]] <- as.numeric(df[[var]])
}

# Fill missing casualty data using district mapping
dist_cas <- df %>%
  filter(!is.na(cas_own_any)) %>%
  group_by(dist) %>%
  slice(1) %>%
  select(dist, all_of(casualty_vars)) %>%
  ungroup()

df <- df %>%
  left_join(
    dist_cas %>%
      rename_with(~paste0(., "_fill"), all_of(casualty_vars)),
    by = "dist"
  )

for (var in casualty_vars) {
  fill_var <- paste0(var, "_fill")
  df[[var]] <- if_else(is.na(df[[var]]), df[[fill_var]], df[[var]])
  df[[fill_var]] <- NULL
}

# Ethnicity
hill_high_castes <- c(1, 2, 14, 20, 27, 48, 49)

df <- df %>%
  mutate(
    caste_num = as.numeric(caste),
    high_caste = if_else(caste_num %in% hill_high_castes, 1.0, 0.0),
    high_caste = if_else(is.na(caste_num), NA_real_, high_caste)
  ) %>%
  group_by(psu, hhld) %>%
  fill(caste_num, .direction = "downup") %>%
  ungroup()

# Migration outcomes
df <- df %>%
  mutate(
    intl_mig = as.numeric(international_absentee_only),
    dom_mig = as.numeric(national),
    girl = as.numeric(female)
  )

# District and PSU identifiers
df <- df %>%
  mutate(
    dist_num = as.numeric(dist),
    psu_num = as.numeric(psu),
    rel_hhh = as.numeric(rel_hhh)
  )

cat("Data loaded:", nrow(df), "rows\n")

# ══════════════════════════════════════════════════════════════════════════════
# LINK MOTHERS TO CHILDREN WITHIN HOUSEHOLDS
# ══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 70), "\n")
cat("  LINKING MOTHERS TO CHILDREN WITHIN HOUSEHOLDS\n")
cat(strrep("=", 70), "\n")

# Mothers: female (sex_num == 2), relationship in {1=head, 2=spouse, 6=parent}
mothers <- df %>%
  filter(sex_num == 2 & rel_hhh %in% c(1, 2, 6)) %>%
  select(psu, hhld, age_num, birth_year, age_at_conflict_start, high_caste,
         dist_num, psu_num, mwar_own_any, cas_own_any, mwar_own_fatal, cas_own_fatal) %>%
  rename_with(~paste0("mother_", .), -c(psu, hhld, dist_num, psu_num, mwar_own_any, cas_own_any, mwar_own_fatal, cas_own_fatal)) %>%
  mutate(mother_idx = row_number())

# Children: rel_hhh in {3=son/daughter, 4=son/daughter-in-law, 5=grandchild}
children <- df %>%
  filter(rel_hhh %in% c(3, 4, 5)) %>%
  select(psu, hhld, age_num, birth_year, sex_num, girl, intl_mig, dom_mig, male) %>%
  rename_with(~paste0("child_", .), -c(psu, hhld)) %>%
  mutate(child_idx = row_number())

# Merge: match children to mothers within same household
mc <- children %>%
  left_join(mothers, by = c("psu", "hhld"), relationship = "many-to-many") %>%
  filter(!is.na(mother_age_num)) %>%
  mutate(age_gap = mother_age_num - child_age_num) %>%
  filter(age_gap >= 15 & age_gap <= 50)  # plausible mother-child age gap

if (nrow(mc) > 0) {
  # If multiple mothers match, keep the one closest to typical motherhood age (25)
  mc <- mc %>%
    mutate(age_gap_abs = abs(age_gap - 25)) %>%
    group_by(child_idx) %>%
    slice_min(order_by = age_gap_abs, n = 1, with_ties = "first") %>%
    ungroup()

  # Compute birth order within household by mother
  mc <- mc %>%
    arrange(psu, hhld, mother_idx, desc(child_age_num)) %>%
    group_by(psu, hhld, mother_idx) %>%
    mutate(birth_order = row_number(), birth_order = pmin(birth_order, 6)) %>%
    ungroup()

  cat("Mother-child pairs:", nrow(mc), "\n")
  cat("Unique mothers:", n_distinct(mc$mother_idx), "\n")
  cat("Unique children:", n_distinct(mc$child_idx), "\n")
}

# ══════════════════════════════════════════════════════════════════════════════
# CREATE MOTHER'S COHORT VARIABLES
# ══════════════════════════════════════════════════════════════════════════════

# Mother's cohort based on age at conflict start
mc <- mc %>%
  mutate(
    # Phadera cohorts
    m_cohort_0_3 = if_else(between(mother_age_at_conflict_start, 0, 3), 1.0, 0.0),
    m_cohort_4_8 = if_else(between(mother_age_at_conflict_start, 4, 8), 1.0, 0.0),
    m_cohort_9_15 = if_else(between(mother_age_at_conflict_start, 9, 15), 1.0, 0.0),
    m_cohort_16_21 = if_else(between(mother_age_at_conflict_start, 16, 21), 1.0, 0.0),  # control
    m_cohort_22_29 = if_else(between(mother_age_at_conflict_start, 22, 29), 1.0, 0.0),  # placebo

    # User-defined age groups
    m_cohort_0_5 = if_else(between(mother_age_at_conflict_start, 0, 5), 1.0, 0.0),
    m_cohort_6_10 = if_else(between(mother_age_at_conflict_start, 6, 10), 1.0, 0.0),
    m_cohort_11_18 = if_else(between(mother_age_at_conflict_start, 11, 18), 1.0, 0.0),
    m_cohort_19_25 = if_else(between(mother_age_at_conflict_start, 19, 25), 1.0, 0.0),  # control
    m_cohort_26_40 = if_else(between(mother_age_at_conflict_start, 26, 40), 1.0, 0.0)   # placebo
  )

# Create interaction terms: mother's conflict × mother's cohort
for (cv in c('mwar_own_any', 'cas_own_any')) {
  for (cn in c('0_3', '4_8', '9_15', '22_29', '0_5', '6_10', '11_18', '26_40')) {
    var_name <- paste0(cv, '_x_m', cn)
    cohort_var <- paste0('m_cohort_', cn)
    mc[[var_name]] <- mc[[cv]] * mc[[cohort_var]]
  }
}

# Filter to mothers aged 0-40 at conflict start
mc_sample <- mc %>%
  filter(between(mother_age_at_conflict_start, 0, 40)) %>%
  filter(!is.na(mwar_own_any)) %>%
  filter(!is.na(child_age_num))

cat("\nRegression sample (mothers age 0-40 at CS):", nrow(mc_sample), "\n")
cat("  Mother age at conflict start:", paste(
  round(quantile(mc_sample$mother_age_at_conflict_start, na.rm = TRUE), 2),
  collapse = " "
), "\n")
cat("  Child age:", paste(
  round(quantile(mc_sample$child_age_num, na.rm = TRUE), 2),
  collapse = " "
), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# RUN EQUATION 8 REGRESSIONS
# ══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 70), "\n")
cat("  RUNNING EQUATION 8: INTERGENERATIONAL REGRESSIONS\n")
cat(strrep("=", 70), "\n")

results_eq8 <- list()

for (outcome in c('intl_mig', 'dom_mig')) {
  for (conflict_var in c('mwar_own_any', 'cas_own_any')) {

    # Phadera cohorts
    interactions_phadera <- paste0(conflict_var, '_x_m', c('22_29', '9_15', '4_8', '0_3'))
    cohort_dummies_phadera <- c('m_cohort_0_3', 'm_cohort_4_8', 'm_cohort_9_15', 'm_cohort_22_29')

    formula_phadera <- as.formula(
      paste(outcome, '~', paste(interactions_phadera, collapse = ' + '),
            '+', paste(cohort_dummies_phadera, collapse = ' + '),
            '+ mother_high_caste + girl + C(birth_order) | dist_num')
    )

    # Get valid data
    vars_needed <- c(outcome, interactions_phadera, cohort_dummies_phadera,
                     'mother_high_caste', 'girl', 'birth_order', 'dist_num', 'psu_num')
    valid <- mc_sample %>%
      select(all_of(vars_needed)) %>%
      filter(if_all(everything(), ~!is.na(.)))

    if (nrow(valid) >= 50) {
      model_phadera <- feols(formula_phadera, data = valid, se = 'cluster', cluster = 'psu_num')

      key <- paste0(outcome, '_', conflict_var, '_phadera_eq8')
      results_eq8[[key]] <- list(
        model = model_phadera,
        nobs = nrow(valid),
        r2_adj = r2(model_phadera, type = 'adj')
      )

      # Control mean
      ctrl_data <- valid %>% filter(m_cohort_16_21 == 1)
      if (nrow(ctrl_data) > 0) {
        results_eq8[[key]]$ctrl_mean <- mean(pull(ctrl_data, all_of(outcome)), na.rm = TRUE)
      } else {
        results_eq8[[key]]$ctrl_mean <- NA_real_
      }

      cat(sprintf("  %s ~ %s (Phadera Eq8): N=%d, R2=%.4f\n",
                  outcome, conflict_var, nrow(valid), results_eq8[[key]]$r2_adj))
    } else {
      cat(sprintf("  SKIP %s ~ %s (Phadera): N=%d too small\n", outcome, conflict_var, nrow(valid)))
    }

    # User-defined age groups
    interactions_user <- paste0(conflict_var, '_x_m', c('26_40', '11_18', '6_10', '0_5'))
    cohort_dummies_user <- c('m_cohort_0_5', 'm_cohort_6_10', 'm_cohort_11_18', 'm_cohort_26_40')

    formula_user <- as.formula(
      paste(outcome, '~', paste(interactions_user, collapse = ' + '),
            '+', paste(cohort_dummies_user, collapse = ' + '),
            '+ mother_high_caste + girl + C(birth_order) | dist_num')
    )

    vars_needed_user <- c(outcome, interactions_user, cohort_dummies_user,
                          'mother_high_caste', 'girl', 'birth_order', 'dist_num', 'psu_num')
    valid_user <- mc_sample %>%
      select(all_of(vars_needed_user)) %>%
      filter(if_all(everything(), ~!is.na(.)))

    if (nrow(valid_user) >= 50) {
      model_user <- feols(formula_user, data = valid_user, se = 'cluster', cluster = 'psu_num')

      key_user <- paste0(outcome, '_', conflict_var, '_agegroups_eq8')
      results_eq8[[key_user]] <- list(
        model = model_user,
        nobs = nrow(valid_user),
        r2_adj = r2(model_user, type = 'adj')
      )

      ctrl_data_user <- valid_user %>% filter(m_cohort_19_25 == 1)
      if (nrow(ctrl_data_user) > 0) {
        results_eq8[[key_user]]$ctrl_mean <- mean(pull(ctrl_data_user, all_of(outcome)), na.rm = TRUE)
      } else {
        results_eq8[[key_user]]$ctrl_mean <- NA_real_
      }

      cat(sprintf("  %s ~ %s (Age Groups Eq8): N=%d, R2=%.4f\n",
                  outcome, conflict_var, nrow(valid_user), results_eq8[[key_user]]$r2_adj))
    } else {
      cat(sprintf("  SKIP %s ~ %s (age groups): N=%d too small\n", outcome, conflict_var, nrow(valid_user)))
    }
  }
}

# ══════════════════════════════════════════════════════════════════════════════
# WRITE RESULTS TO XLSX
# ══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 70), "\n")
cat("  WRITING EQUATION 8 RESULTS\n")
cat(strrep("=", 70), "\n")

wb <- createWorkbook()

# Helper function to write regression results
write_regression_sheet <- function(wb, sheet_name, title, cohort_interactions,
                                  results_dict, suffix, ctrl_label) {
  ws <- addWorksheet(wb, sheet_name)
  r <- 1

  # Title
  mergeCells(ws, cols = 1:9, rows = r)
  writeData(ws, r, title, startCol = 1)
  title_style <- createStyle(fontSize = 12, bold = TRUE, fgFill = "#1B2A4A", fontColour = "#FFFFFF")
  addStyle(ws, style = title_style, rows = r, cols = 1:9)
  r <- r + 2

  # Headers
  header_style <- createStyle(fontSize = 10, bold = TRUE, fgFill = "#1B2A4A", fontColour = "#FFFFFF",
                             halign = "center", wrapText = TRUE)
  outcome_labels <- c(intl_mig = 'International Migration', dom_mig = 'Domestic Migration')
  conflict_labels <- c(mwar_own_any = 'Months of War', cas_own_any = 'Casualty Count')

  col <- 2
  for (outcome in c('intl_mig', 'dom_mig')) {
    mergeCells(ws, cols = col:(col + 1), rows = r)
    writeData(ws, r, outcome_labels[outcome], startCol = col)
    addStyle(ws, style = header_style, rows = r, cols = col:(col + 1))
    col <- col + 2
  }
  r <- r + 1

  col <- 2
  for (outcome in c('intl_mig', 'dom_mig')) {
    for (conf_var in c('mwar_own_any', 'cas_own_any')) {
      label <- paste0('(', col - 1, ')\n', conflict_labels[conf_var])
      writeData(ws, r, label, startCol = col)
      addStyle(ws, style = header_style, rows = r, cols = col)
      col <- col + 1
    }
  }
  r <- r + 1

  # Regression rows
  data_style <- createStyle(fontSize = 9, halign = "center")

  for (cohort_info in cohort_interactions) {
    cohort_suffix <- cohort_info[[1]]
    row_label <- cohort_info[[2]]

    # Coefficient row
    writeData(ws, r, row_label, startCol = 1)
    addStyle(ws, style = createStyle(fontSize = 9, bold = TRUE), rows = r, cols = 1)

    col <- 2
    for (outcome in c('intl_mig', 'dom_mig')) {
      for (conf_var in c('mwar_own_any', 'cas_own_any')) {
        key <- paste0(outcome, '_', conf_var, '_', suffix)
        if (!is.null(results_dict[[key]])) {
          model <- results_dict[[key]]$model
          inter_name <- paste0(conf_var, '_x_m', cohort_suffix)
          if (inter_name %in% names(coef(model))) {
            coef_val <- coef(model)[inter_name]
            writeData(ws, r, formatC(coef_val, format = "f", digits = 6), startCol = col)
            addStyle(ws, style = data_style, rows = r, cols = col)
          }
        }
        col <- col + 1
      }
    }
    r <- r + 1

    # SE row
    col <- 2
    for (outcome in c('intl_mig', 'dom_mig')) {
      for (conf_var in c('mwar_own_any', 'cas_own_any')) {
        key <- paste0(outcome, '_', conf_var, '_', suffix)
        if (!is.null(results_dict[[key]])) {
          model <- results_dict[[key]]$model
          inter_name <- paste0(conf_var, '_x_m', cohort_suffix)
          if (inter_name %in% names(coef(model))) {
            se_val <- sqrt(diag(vcov(model)))[inter_name]
            writeData(ws, r, paste0('(', formatC(se_val, format = "f", digits = 6), ')'), startCol = col)
            addStyle(ws, style = createStyle(fontSize = 8, fontColour = '#666666'), rows = r, cols = col)
          }
        }
        col <- col + 1
      }
    }
    r <- r + 1
  }

  # Stats rows
  r <- r + 1
  for (stat_label in c('Observations', 'Adjusted R-squared', paste0('Control Mean (', ctrl_label, ')'))) {
    writeData(ws, r, stat_label, startCol = 1)
    addStyle(ws, style = createStyle(fontSize = 9, bold = TRUE), rows = r, cols = 1)

    col <- 2
    for (outcome in c('intl_mig', 'dom_mig')) {
      for (conf_var in c('mwar_own_any', 'cas_own_any')) {
        key <- paste0(outcome, '_', conf_var, '_', suffix)
        if (!is.null(results_dict[[key]])) {
          if (stat_label == 'Observations') {
            val_str <- as.character(results_dict[[key]]$nobs)
          } else if (stat_label == 'Adjusted R-squared') {
            val_str <- formatC(results_dict[[key]]$r2_adj, format = "f", digits = 4)
          } else {
            val_str <- formatC(results_dict[[key]]$ctrl_mean, format = "f", digits = 4)
          }
          writeData(ws, r, val_str, startCol = col)
          addStyle(ws, style = data_style, rows = r, cols = col)
        }
        col <- col + 1
      }
    }
    r <- r + 1
  }

  # Set column widths
  setColWidths(ws, cols = 1, widths = 38)
  for (i in 2:5) {
    setColWidths(ws, cols = i, widths = 18)
  }

  r
}

# Table 6: Eq 8 with Phadera cohorts
phadera_cohort_interactions <- list(
  list('22_29', "Mother Age 22-29 × Conflict (Placebo)"),
  list('9_15', "Mother Age 9-15 × Conflict"),
  list('4_8', "Mother Age 4-8 × Conflict"),
  list('0_3', "Mother Age 0-3 × Conflict")
)

write_regression_sheet(wb, "Table 6 - Eq8 Phadera",
    "Table 6: Intergenerational Impact on Children's Migration (Equation 8, Phadera Cohorts)",
    phadera_cohort_interactions, results_eq8, 'phadera_eq8', "Mother Age 16-21")

# Table 7: Eq 8 with user age groups
user_cohort_interactions <- list(
  list('26_40', "Mother Age 26-40 × Conflict (Placebo)"),
  list('11_18', "Mother Age 11-18 × Conflict"),
  list('6_10', "Mother Age 6-10 × Conflict"),
  list('0_5', "Mother Age 0-5 × Conflict")
)

write_regression_sheet(wb, "Table 7 - Eq8 Age Groups",
    "Table 7: Intergenerational Impact on Children's Migration (Equation 8, User Age Groups)",
    user_cohort_interactions, results_eq8, 'agegroups_eq8', "Mother Age 19-25")

# Save workbook
xlsx_path <- file.path(OUT_DIR, 'phadera_replication_tables_eq8.xlsx')
saveWorkbook(wb, xlsx_path)

cat("  Saved XLSX:", xlsx_path, "\n")

cat("\n", strrep("=", 70), "\n")
cat("  EQUATION 8 COMPLETE!\n")
cat(strrep("=", 70), "\n")
cat("  Mother-child pairs used:", nrow(mc_sample), "\n")
cat("  Results saved to:", OUT_DIR, "\n")
