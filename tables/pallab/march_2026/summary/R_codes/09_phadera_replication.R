################################################################################
# 09_phadera_replication.R
#
# Replicates the empirical strategy from Phadera (2021) "Unfortunate Moms and
# Unfortunate Children" using NLFS data with MIGRATION as the outcome variable
# instead of height.
#
# Key adaptation:
#   - Original paper: Y = adult height (women), using NDHS 2016
#   - This replication: Y = international migration / domestic migration, using NLFS
#   - Same DID structure: conflict_var × cohort (Equation 7)
#   - Same intergenerational framework: Equation 8
#
# Outputs:
#   1. Summary statistics tables
#   2. Figure 4 (cohort diagram)
#   3. Figure 5 (migration rates by conflict intensity)
#   4. Table 4 style DID regression results (Eq 7) for migration outcomes
#   5. Results for both Phadera cohorts (0-3, 4-8, 9-15, 16-21, 22-29)
#      and user-defined age groups (0-5, 6-10, 11-18, 19-25, 26-40)
################################################################################

library(tidyverse)
library(haven)
library(fixest)
library(lmtest)
library(sandwich)
library(openxlsx)
library(ggplot2)
library(gridExtra)

# ══════════════════════════════════════════════════════════════════════════════
# CONFIGURATION & PATHS
# ══════════════════════════════════════════════════════════════════════════════

BASE_PATH <- "/Users/pallab.ghosh/Documents/GitHub/Nepal-Civil-War-and-Int-Migration"
DATA_PATH <- file.path(BASE_PATH, "data/Modified_Data/1_conflict_present_absentee_data.dta")
OUT_DIR <- file.path(BASE_PATH, "tables/pallab/march_2026/summary/results")
FIG_DIR <- file.path(BASE_PATH, "tables/pallab/march_2026/summary/figures")

dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

SURVEY_YEAR <- 2017
CONFLICT_START <- 1996
CONFLICT_END <- 2006

# ══════════════════════════════════════════════════════════════════════════════
# LOAD & PREPARE DATA
# ══════════════════════════════════════════════════════════════════════════════

cat("Loading data from:", DATA_PATH, "\n")
df <- read_dta(DATA_PATH)

# Convert to tibble for easier manipulation
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

# Casualty variables - convert to numeric
casualty_vars <- c('cas_own_any', 'cas_own_fatal', 'mwar_own_any', 'mwar_own_fatal',
                    'cas_nbr_any', 'cas_nbr_fatal', 'mwar_nbr_any', 'mwar_nbr_fatal')

for (var in casualty_vars) {
  df[[var]] <- as.numeric(df[[var]])
}

# Fill missing casualty data for absentees using district-level mapping
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

# Ethnicity and high caste coding
# Hill high castes = {1, 2, 14, 20, 27, 48, 49}
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

# Migration outcomes (binary)
df <- df %>%
  mutate(
    intl_mig = as.numeric(international_absentee_only),
    dom_mig = as.numeric(national)
  )

# In utero indicator
df <- df %>%
  mutate(
    in_utero = if_else(birth_year >= 1996 & birth_year <= 1997, 1.0, 0.0)
  )

# District and PSU numeric identifiers
df <- df %>%
  mutate(
    dist_num = as.numeric(dist),
    psu_num = as.numeric(psu),
    rel_hhh = as.numeric(rel_hhh)
  )

cat("Data loaded:", nrow(df), "rows,", ncol(df), "columns\n")
cat("Age at conflict start range:", round(min(df$age_at_conflict_start, na.rm = TRUE), 0),
    "to", round(max(df$age_at_conflict_start, na.rm = TRUE), 0), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# CREATE COHORT VARIABLES (EQUATION 7)
# ══════════════════════════════════════════════════════════════════════════════

# Phadera-style cohorts
df <- df %>%
  mutate(
    # Phadera cohorts
    cohort_0_3 = if_else(between(age_at_conflict_start, 0, 3), 1.0, 0.0),
    cohort_4_8 = if_else(between(age_at_conflict_start, 4, 8), 1.0, 0.0),
    cohort_9_15 = if_else(between(age_at_conflict_start, 9, 15), 1.0, 0.0),
    cohort_16_21 = if_else(between(age_at_conflict_start, 16, 21), 1.0, 0.0),  # control
    cohort_22_29 = if_else(between(age_at_conflict_start, 22, 29), 1.0, 0.0),  # placebo

    # User-defined age groups
    cohort_0_5 = if_else(between(age_at_conflict_start, 0, 5), 1.0, 0.0),
    cohort_6_10 = if_else(between(age_at_conflict_start, 6, 10), 1.0, 0.0),
    cohort_11_18 = if_else(between(age_at_conflict_start, 11, 18), 1.0, 0.0),
    cohort_19_25 = if_else(between(age_at_conflict_start, 19, 25), 1.0, 0.0),  # control
    cohort_26_40 = if_else(between(age_at_conflict_start, 26, 40), 1.0, 0.0)   # placebo
  )

# Create interaction terms: conflict × cohort
conflict_vars <- c('mwar_own_any', 'cas_own_any')
cohort_names_phadera <- c('0_3', '4_8', '9_15', '22_29')
cohort_names_user <- c('0_5', '6_10', '11_18', '26_40')

for (cv in conflict_vars) {
  for (cn in c(cohort_names_phadera, cohort_names_user)) {
    var_name <- paste0(cv, '_x_', cn)
    cohort_var <- paste0('cohort_', cn)
    df[[var_name]] <- df[[cv]] * df[[cohort_var]]
  }
}

# Fill high_caste NAs with 0
df <- df %>%
  mutate(high_caste = replace_na(high_caste, 0))

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 1: SUMMARY STATISTICS - ALL INDIVIDUALS
# ══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 70), "\n")
cat("  GENERATING TABLE 1: Summary Statistics (All Individuals)\n")
cat(strrep("=", 70), "\n")

sample_t1 <- df %>%
  filter(between(age_at_conflict_start, 0, 40))

treat_t1 <- sample_t1 %>%
  filter(between(age_at_conflict_start, 0, 17))

ctrl_t1 <- sample_t1 %>%
  filter(between(age_at_conflict_start, 18, 40))

# Function to compute summary row
compute_summary_row <- function(sub_all, sub_treat, sub_ctrl, var, label) {
  all_vals <- pull(sub_all, all_of(var)) %>% na.omit()
  treat_vals <- pull(sub_treat, all_of(var)) %>% na.omit()
  ctrl_vals <- pull(sub_ctrl, all_of(var)) %>% na.omit()

  all_mean <- if (length(all_vals) > 0) mean(all_vals, na.rm = TRUE) else NA_real_
  all_sd <- if (length(all_vals) > 0) sd(all_vals, na.rm = TRUE) else NA_real_
  treat_mean <- if (length(treat_vals) > 0) mean(treat_vals, na.rm = TRUE) else NA_real_
  treat_sd <- if (length(treat_vals) > 0) sd(treat_vals, na.rm = TRUE) else NA_real_
  ctrl_mean <- if (length(ctrl_vals) > 0) mean(ctrl_vals, na.rm = TRUE) else NA_real_
  ctrl_sd <- if (length(ctrl_vals) > 0) sd(ctrl_vals, na.rm = TRUE) else NA_real_

  diff <- treat_mean - ctrl_mean

  # t-test
  stars <- ''
  se <- NA_real_
  if (length(treat_vals) >= 2 && length(ctrl_vals) >= 2) {
    t_test <- t.test(treat_vals, ctrl_vals, var.equal = FALSE)
    p_val <- t_test$p.value
    se <- sqrt(var(treat_vals, na.rm = TRUE) / length(treat_vals) +
               var(ctrl_vals, na.rm = TRUE) / length(ctrl_vals))
    if (p_val < 0.01) stars <- '***'
    else if (p_val < 0.05) stars <- '**'
    else if (p_val < 0.10) stars <- '*'
  }

  list(
    label = label,
    all_mean = all_mean, all_sd = all_sd,
    treat_mean = treat_mean, treat_sd = treat_sd,
    ctrl_mean = ctrl_mean, ctrl_sd = ctrl_sd,
    diff = diff, se = se, stars = stars
  )
}

# Panel A: Outcomes
panel_a <- list(
  compute_summary_row(sample_t1, treat_t1, ctrl_t1, 'intl_mig', 'International Migration (=1)'),
  compute_summary_row(sample_t1, treat_t1, ctrl_t1, 'dom_mig', 'Domestic Migration (=1)')
)

# Panel B: Exposure
panel_b <- list(
  compute_summary_row(sample_t1, treat_t1, ctrl_t1, 'mwar_own_any', 'Months of War (own village)'),
  compute_summary_row(sample_t1, treat_t1, ctrl_t1, 'cas_own_any', 'Number of Casualties (own village)'),
  compute_summary_row(sample_t1, treat_t1, ctrl_t1, 'mwar_nbr_any', 'Months of War (incl. neighbors)'),
  compute_summary_row(sample_t1, treat_t1, ctrl_t1, 'cas_nbr_any', 'Casualties (incl. neighbors)')
)

# Panel C: Controls
panel_c <- list(
  compute_summary_row(sample_t1, treat_t1, ctrl_t1, 'age_num', 'Current Age'),
  compute_summary_row(sample_t1, treat_t1, ctrl_t1, 'male', 'Male (=1)'),
  compute_summary_row(sample_t1, treat_t1, ctrl_t1, 'high_caste', 'High Caste (=1)')
)

cat("Table 1 generated with", nrow(sample_t1), "observations\n")

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 4: COHORT DIAGRAM
# ══════════════════════════════════════════════════════════════════════════════

cat("  GENERATING FIGURE 4: Cohort Diagram\n")

cohort_fig_data <- tibble(
  label = c(
    'In Utero\n(born after 1996)',
    'Age 0-5\nat conflict start',
    'Age 6-10\nat conflict start',
    'Age 11-18\nat conflict start',
    'Age 19-25\nat conflict start\n(Control 1)',
    'Age 26-40\nat conflict start\n(Control 2)'
  ),
  lo = c(-3, 0, 6, 11, 19, 26),
  hi = c(0, 5, 10, 18, 25, 40),
  color = c('#FFB3BA', '#FF6B6B', '#FF8E53', '#FFC107', '#4CAF50', '#2196F3'),
  y_pos = seq(0, 5)
)

p4 <- cohort_fig_data %>%
  mutate(width = hi - lo + 1) %>%
  ggplot(aes(x = lo, y = y_pos, width = width, fill = color)) +
  geom_tile(height = 0.6, color = 'black', size = 0.2) +
  geom_text(aes(x = lo + width/2, label = label), size = 2.5, fontface = 'bold') +
  scale_fill_identity() +
  scale_x_continuous(limits = c(-5, 42)) +
  scale_y_continuous(limits = c(-0.5, 5.5)) +
  labs(
    x = 'Age at Start of Civil War (February 1996)',
    title = 'Figure 4: Cohorts by Age at the Start of the War in 1996\n(Adapted from Phadera, 2021)'
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.y = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 11, face = 'bold')
  ) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'red', size = 0.5, alpha = 0.7)

ggsave(file.path(FIG_DIR, 'figure_4_cohort_diagram.png'), p4, dpi = 300, width = 12, height = 5)
ggsave(file.path(FIG_DIR, 'figure_4_cohort_diagram.pdf'), p4, width = 12, height = 5)

cat("  Saved: figure_4_cohort_diagram.png/pdf\n")

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 5: MIGRATION RATES BY CONFLICT INTENSITY
# ══════════════════════════════════════════════════════════════════════════════

cat("  GENERATING FIGURE 5: Migration Rates by Conflict Intensity\n")

age_bins <- list(c(0, 5), c(6, 10), c(11, 18), c(19, 25), c(26, 40))
age_labels <- c('0-5', '6-10', '11-18', '19-25', '26-40')

med_mwar <- median(df$mwar_own_any, na.rm = TRUE)

fig5_data <- tibble()

for (i in seq_along(age_bins)) {
  lo <- age_bins[[i]][1]
  hi <- age_bins[[i]][2]

  sub <- df %>% filter(between(age_at_conflict_start, lo, hi))
  t <- sub %>% filter(mwar_own_any > med_mwar)
  c <- sub %>% filter(mwar_own_any <= med_mwar)

  for (outcome in c('intl_mig', 'dom_mig')) {
    t_rate <- if (nrow(t) > 0) mean(pull(t, all_of(outcome)), na.rm = TRUE) * 100 else 0
    c_rate <- if (nrow(c) > 0) mean(pull(c, all_of(outcome)), na.rm = TRUE) * 100 else 0

    fig5_data <- bind_rows(fig5_data,
      tibble(
        age_group = age_labels[i],
        outcome = outcome,
        treatment = t_rate,
        control = c_rate
      )
    )
  }
}

fig5_data <- fig5_data %>%
  pivot_longer(c(treatment, control), names_to = 'group', values_to = 'rate') %>%
  mutate(
    age_group = factor(age_group, levels = age_labels),
    group = factor(group, levels = c('control', 'treatment'),
                   labels = c('Low Conflict (Control)', 'High Conflict (Treatment)'))
  )

p5 <- ggplot(fig5_data, aes(x = age_group, y = rate, fill = group)) +
  facet_wrap(~outcome, labeller = labeller(outcome = c(
    intl_mig = 'International Migration',
    dom_mig = 'Domestic Migration'
  ))) +
  geom_col(position = 'dodge', color = 'black', size = 0.2) +
  scale_fill_manual(values = c('High Conflict (Treatment)' = '#E74C3C',
                               'Low Conflict (Control)' = '#3498DB')) +
  labs(
    x = 'Age at Conflict Start (1996)',
    y = 'Migration Rate (%)',
    fill = '',
    title = paste0('Figure 5: Migration Rates by Age at Conflict Start and Conflict Intensity\n',
                   '(Treatment: Months of War > median (', round(med_mwar, 0), '); Control: <= median)')
  ) +
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    plot.title = element_text(hjust = 0.5, size = 11, face = 'bold'),
    strip.text = element_text(face = 'bold')
  )

ggsave(file.path(FIG_DIR, 'figure_5_migration_by_conflict.png'), p5, dpi = 300, width = 14, height = 6)
ggsave(file.path(FIG_DIR, 'figure_5_migration_by_conflict.pdf'), p5, width = 14, height = 6)

cat("  Saved: figure_5_migration_by_conflict.png/pdf\n")

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 4: DID REGRESSION - EQUATION 7
# ══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 70), "\n")
cat("  GENERATING TABLE 4: DID Regressions (Equation 7)\n")
cat(strrep("=", 70), "\n")

# Prepare regression sample
reg_sample <- df %>%
  filter(between(age_at_conflict_start, 0, 40)) %>%
  filter(!is.na(mwar_own_any)) %>%
  filter(!is.na(age_num))

# Store results
results_eq7 <- list()

# Run regressions for each outcome and conflict variable
for (outcome in c('intl_mig', 'dom_mig')) {
  for (conflict_var in c('mwar_own_any', 'cas_own_any')) {

    # Phadera cohorts
    interactions_phadera <- paste0(conflict_var, '_x_', c('22_29', '9_15', '4_8', '0_3'))
    cohort_dummies <- c('cohort_0_3', 'cohort_4_8', 'cohort_9_15', 'cohort_22_29')

    formula_phadera <- as.formula(
      paste(outcome, '~', paste(interactions_phadera, collapse = ' + '),
            '+', paste(cohort_dummies, collapse = ' + '),
            '+ high_caste | dist_num')
    )

    # Filter valid data
    vars_needed <- c(outcome, interactions_phadera, cohort_dummies, 'high_caste', 'dist_num', 'psu_num')
    valid <- reg_sample %>%
      select(all_of(vars_needed)) %>%
      filter(if_all(everything(), ~!is.na(.)))

    if (nrow(valid) >= 50) {
      # Fit model with district FE using feols
      model_phadera <- feols(formula_phadera, data = valid, se = 'cluster', cluster = 'psu_num')

      key <- paste0(outcome, '_', conflict_var, '_phadera')
      results_eq7[[key]] <- list(
        model = model_phadera,
        nobs = nrow(valid),
        r2_adj = r2(model_phadera, type = 'adj'),
        ctrl_mean = NA_real_
      )

      # Compute control mean
      ctrl_data <- valid %>% filter(get(paste0('cohort_16_21')) == 1)
      if (nrow(ctrl_data) > 0) {
        results_eq7[[key]]$ctrl_mean <- mean(pull(ctrl_data, all_of(outcome)), na.rm = TRUE)
      }

      cat(sprintf("  %s ~ %s (Phadera cohorts): N=%d, R2=%.4f\n",
                  outcome, conflict_var, nrow(valid), results_eq7[[key]]$r2_adj))
    } else {
      cat(sprintf("  SKIP %s ~ %s (Phadera): N=%d too small\n", outcome, conflict_var, nrow(valid)))
    }

    # User-defined age groups
    interactions_user <- paste0(conflict_var, '_x_', c('26_40', '11_18', '6_10', '0_5'))
    cohort_dummies_user <- c('cohort_0_5', 'cohort_6_10', 'cohort_11_18', 'cohort_26_40')

    formula_user <- as.formula(
      paste(outcome, '~', paste(interactions_user, collapse = ' + '),
            '+', paste(cohort_dummies_user, collapse = ' + '),
            '+ high_caste | dist_num')
    )

    vars_needed_user <- c(outcome, interactions_user, cohort_dummies_user, 'high_caste', 'dist_num', 'psu_num')
    valid_user <- reg_sample %>%
      select(all_of(vars_needed_user)) %>%
      filter(if_all(everything(), ~!is.na(.)))

    if (nrow(valid_user) >= 50) {
      model_user <- feols(formula_user, data = valid_user, se = 'cluster', cluster = 'psu_num')

      key_user <- paste0(outcome, '_', conflict_var, '_agegroups')
      results_eq7[[key_user]] <- list(
        model = model_user,
        nobs = nrow(valid_user),
        r2_adj = r2(model_user, type = 'adj'),
        ctrl_mean = NA_real_
      )

      ctrl_data_user <- valid_user %>% filter(get(paste0('cohort_19_25')) == 1)
      if (nrow(ctrl_data_user) > 0) {
        results_eq7[[key_user]]$ctrl_mean <- mean(pull(ctrl_data_user, all_of(outcome)), na.rm = TRUE)
      }

      cat(sprintf("  %s ~ %s (Age groups): N=%d, R2=%.4f\n",
                  outcome, conflict_var, nrow(valid_user), results_eq7[[key_user]]$r2_adj))
    } else {
      cat(sprintf("  SKIP %s ~ %s (age groups): N=%d too small\n", outcome, conflict_var, nrow(valid_user)))
    }
  }
}

# ══════════════════════════════════════════════════════════════════════════════
# WRITE RESULTS TO XLSX
# ══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 70), "\n")
cat("  WRITING EXCEL OUTPUT\n")
cat(strrep("=", 70), "\n")

wb <- createWorkbook()

# Sheet 1: Table 1 - Summary Statistics
ws1 <- addWorksheet(wb, "Table 1 - Summary Stats")

# Helper function to write summary table
write_summary_table_xlsx <- function(wb, ws, title, panels, col_headers) {
  r <- 1

  # Title
  mergeCells(ws, cols = 1:9, rows = r)
  writeData(ws, r, title, startCol = 1)

  # Style title
  title_style <- createStyle(fontSize = 12, bold = TRUE, fgFill = "#1B2A4A", fontColour = "#FFFFFF")
  addStyle(ws, style = title_style, rows = r, cols = 1:9)
  r <- r + 2

  # Headers
  header_style <- createStyle(fontSize = 10, bold = TRUE, fgFill = "#1B2A4A", fontColour = "#FFFFFF",
                             halign = "center", wrapText = TRUE)

  for (i in seq_along(col_headers)) {
    writeData(ws, r, col_headers[i], startCol = i + 1)
    addStyle(ws, style = header_style, rows = r, cols = i + 1)
  }
  addStyle(ws, style = header_style, rows = r, cols = 1)
  r <- r + 1

  # Data rows
  data_style <- createStyle(fontSize = 9, halign = "center")
  alt_style <- createStyle(fontSize = 9, halign = "center", fgFill = "#F5F5F5")

  alt <- FALSE
  for (panel in panels) {
    for (row_data in panel) {
      row_style <- if (alt) alt_style else data_style

      # Label
      writeData(ws, r, row_data$label, startCol = 1)
      addStyle(ws, style = row_style, rows = r, cols = 1)

      # Values
      vals <- c(
        formatC(row_data$all_mean, format = "f", digits = 2),
        paste0("[", formatC(row_data$all_sd, format = "f", digits = 2), "]"),
        formatC(row_data$treat_mean, format = "f", digits = 2),
        paste0("[", formatC(row_data$treat_sd, format = "f", digits = 2), "]"),
        formatC(row_data$ctrl_mean, format = "f", digits = 2),
        paste0("[", formatC(row_data$ctrl_sd, format = "f", digits = 2), "]"),
        paste0(formatC(row_data$diff, format = "f", digits = 2), row_data$stars),
        paste0("(", formatC(row_data$se, format = "f", digits = 2), ")")
      )

      for (i in seq_along(vals)) {
        if (!is.na(vals[i]) && vals[i] != "NA") {
          writeData(ws, r, vals[i], startCol = i + 1)
          addStyle(ws, style = row_style, rows = r, cols = i + 1)
        }
      }

      r <- r + 1
      alt <- !alt
    }
  }

  # Set column widths
  setColWidths(ws, cols = 1, widths = 30)
  for (i in 2:9) {
    setColWidths(ws, cols = i, widths = 14)
  }

  r
}

col_hdrs_t1 <- c('All\nMean', 'All\n[SD]', 'Treatment\n(Age 0-17)\nMean', 'Treatment\n[SD]',
                  'Control\n(Age 18-40)\nMean', 'Control\n[SD]', 'Difference', '(SE)')

r <- write_summary_table_xlsx(wb, ws1, 'Table 1: Summary Statistics of Individuals (Age 0-40 at Conflict Start)',
                              list(panel_a, panel_b, panel_c), col_hdrs_t1)

# Notes
note_style <- createStyle(fontSize = 8, italic = TRUE, fontColour = "#666666")
writeData(ws1, r + 2, "Note: Treatment = aged 0-17 at conflict start. Control = aged 18-40. Standard deviations in brackets, standard errors in parentheses.")
addStyle(ws1, style = note_style, rows = r + 2, cols = 1, gridExpand = TRUE)

# Sheet 2: Table 4 - DID Phadera Cohorts
ws4 <- addWorksheet(wb, "Table 4 - DID Phadera Cohorts")

r4 <- 1
mergeCells(ws4, cols = 1:5, rows = r4)
writeData(ws4, r4, 'Table 4: Impact on Migration by Age at Start of the Civil War (Equation 7)', startCol = 1)
title_style <- createStyle(fontSize = 12, bold = TRUE, fgFill = "#1B2A4A", fontColour = "#FFFFFF")
addStyle(ws4, style = title_style, rows = r4, cols = 1:5)
r4 <- r4 + 2

# Column headers for Equation 7
header_style <- createStyle(fontSize = 10, bold = TRUE, fgFill = "#1B2A4A", fontColour = "#FFFFFF",
                           halign = "center", wrapText = TRUE)
data_style <- createStyle(fontSize = 9, halign = "center")

outcome_labels <- c(intl_mig = 'International Migration', dom_mig = 'Domestic Migration')
conflict_labels <- c(mwar_own_any = 'Months of War', cas_own_any = 'Casualty Count')

col <- 2
for (outcome in c('intl_mig', 'dom_mig')) {
  mergeCells(ws4, cols = col:(col + 1), rows = r4)
  writeData(ws4, r4, outcome_labels[outcome], startCol = col)
  addStyle(ws4, style = header_style, rows = r4, cols = col:(col + 1))
  col <- col + 2
}
r4 <- r4 + 1

col <- 2
for (outcome in c('intl_mig', 'dom_mig')) {
  for (conf_var in c('mwar_own_any', 'cas_own_any')) {
    label <- paste0('(', col - 1, ')\n', conflict_labels[conf_var])
    writeData(ws4, r4, label, startCol = col)
    addStyle(ws4, style = header_style, rows = r4, cols = col)
    col <- col + 1
  }
}
r4 <- r4 + 1

# Fill in coefficients and SEs
cohort_interactions_phadera <- list(
  list(suffix = '22_29', label = 'Age 22-29 × Conflict'),
  list(suffix = '9_15', label = 'Age 9-15 × Conflict'),
  list(suffix = '4_8', label = 'Age 4-8 × Conflict'),
  list(suffix = '0_3', label = 'Age 0-3 × Conflict')
)

for (ci in cohort_interactions_phadera) {
  writeData(ws4, r4, ci$label, startCol = 1)
  addStyle(ws4, style = createStyle(fontSize = 9, bold = TRUE), rows = r4, cols = 1)

  col <- 2
  for (outcome in c('intl_mig', 'dom_mig')) {
    for (conf_var in c('mwar_own_any', 'cas_own_any')) {
      key <- paste0(outcome, '_', conf_var, '_phadera')
      if (!is.null(results_eq7[[key]])) {
        model <- results_eq7[[key]]$model
        inter_name <- paste0(conf_var, '_x_', ci$suffix)
        if (inter_name %in% names(coef(model))) {
          coef_val <- coef(model)[inter_name]
          se_val <- sqrt(diag(vcov(model)))[inter_name]
          stars <- if (abs(coef_val) > 0) '***' else ''

          writeData(ws4, r4, formatC(coef_val, format = "f", digits = 6), startCol = col)
          addStyle(ws4, style = data_style, rows = r4, cols = col)
        }
      }
      col <- col + 1
    }
  }
  r4 <- r4 + 1

  # SE row
  col <- 2
  for (outcome in c('intl_mig', 'dom_mig')) {
    for (conf_var in c('mwar_own_any', 'cas_own_any')) {
      key <- paste0(outcome, '_', conf_var, '_phadera')
      if (!is.null(results_eq7[[key]])) {
        model <- results_eq7[[key]]$model
        inter_name <- paste0(conf_var, '_x_', ci$suffix)
        if (inter_name %in% names(coef(model))) {
          se_val <- sqrt(diag(vcov(model)))[inter_name]
          writeData(ws4, r4, paste0('(', formatC(se_val, format = "f", digits = 6), ')'), startCol = col)
          addStyle(ws4, style = createStyle(fontSize = 8, fontColour = '#666666'), rows = r4, cols = col)
        }
      }
      col <- col + 1
    }
  }
  r4 <- r4 + 1
}

# Stats rows
r4 <- r4 + 1
for (stat_label in c('Observations', 'Adjusted R-squared', 'Control Mean (Age 16-21)')) {
  writeData(ws4, r4, stat_label, startCol = 1)
  addStyle(ws4, style = createStyle(fontSize = 9, bold = TRUE), rows = r4, cols = 1)

  col <- 2
  for (outcome in c('intl_mig', 'dom_mig')) {
    for (conf_var in c('mwar_own_any', 'cas_own_any')) {
      key <- paste0(outcome, '_', conf_var, '_phadera')
      if (!is.null(results_eq7[[key]])) {
        if (stat_label == 'Observations') {
          val_str <- as.character(results_eq7[[key]]$nobs)
        } else if (stat_label == 'Adjusted R-squared') {
          val_str <- formatC(results_eq7[[key]]$r2_adj, format = "f", digits = 4)
        } else {
          val_str <- formatC(results_eq7[[key]]$ctrl_mean, format = "f", digits = 4)
        }
        writeData(ws4, r4, val_str, startCol = col)
        addStyle(ws4, style = data_style, rows = r4, cols = col)
      }
      col <- col + 1
    }
  }
  r4 <- r4 + 1
}

# Set column widths
setColWidths(ws4, cols = 1, widths = 30)
for (i in 2:5) {
  setColWidths(ws4, cols = i, widths = 18)
}

# Save workbook
xlsx_path <- file.path(OUT_DIR, 'phadera_replication_tables.xlsx')
saveWorkbook(wb, xlsx_path)

cat("  Saved XLSX:", xlsx_path, "\n")

cat("\n", strrep("=", 70), "\n")
cat("  EQUATION 7 COMPLETE!\n")
cat(strrep("=", 70), "\n")
cat("  Results saved to:", OUT_DIR, "\n")
