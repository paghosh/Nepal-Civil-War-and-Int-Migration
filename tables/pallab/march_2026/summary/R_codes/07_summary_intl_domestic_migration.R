# =============================================================================
# 07_summary_intl_domestic_migration.R
# Summary Statistics: International vs. Domestic Migration Channels
# With Treatment-Control Difference and Welch's t-test p-values
# =============================================================================
# Project: Nepal Civil Conflict and International Migration
# Author: Pallab Ghosh
# Date: March 2026
#
# REQUIRES: 02_data_cleaning.R must be run first (nlss_conflict_data loaded)
#
# OUTPUTS:
#   - Table_A_International_Migration_Summary.xlsx  (with Diff T-C column)
#   - Table_B_Domestic_Migration_Summary.xlsx       (with Diff T-C column)
#   - Table_A_International_Migration_Summary.tex
#   - Table_B_Domestic_Migration_Summary.tex
#
# KEY FINDING:
#   International absentees are older (mean age 28), overwhelmingly male (90%),
#   and concentrated in low-skilled and service work abroad.
#   National absentees are younger (mean age ~22), more gender-balanced,
#   better educated (10% tertiary vs. 4%), and more likely in high-skilled
#   occupations or armed forces — suggesting two distinct migration channels.
# =============================================================================

# ── Paths ────────────────────────────────────────────────────────────────────

# USER: Pallab
dropbox_path <- "/Users/pallab.ghosh/Library/CloudStorage/Dropbox/Papers_with_Coauthors/PhD_Students/Ramesh/Nepal Civil Conflict"
output_path  <- "/Users/pallab.ghosh/Documents/GitHub/Nepal-Civil-War-and-Int-Migration/tables/pallab/march_2026/summary_stats"
code_path    <- "/Users/pallab.ghosh/Documents/GitHub/Nepal-Civil-War-and-Int-Migration/source_code"

setwd(dropbox_path)

# ── Load packages ────────────────────────────────────────────────────────────

library(haven)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(openxlsx)

# ── Load data ────────────────────────────────────────────────────────────────

# If not already loaded from 02_data_cleaning.R:
# nlss_conflict_data <- read_dta("nlfs_conflict_data.dta")

# ── Helper: compute group summary ───────────────────────────────────────────

compute_group_summary <- function(df) {
  df %>%
    summarise(
      N = n(),

      # Age
      Age_Mean        = round(mean(age, na.rm = TRUE), 2),
      Age_SD          = round(sd(age, na.rm = TRUE), 2),
      Age_Conf_Mean   = round(mean(age_at_conflict_start, na.rm = TRUE), 2),
      Age_Conf_SD     = round(sd(age_at_conflict_start, na.rm = TRUE), 2),

      # Gender
      Male_Pct = round(mean(sex == 1, na.rm = TRUE) * 100, 2),

      # Education
      No_Edu_Pct    = round(mean(education_category == "No Education", na.rm = TRUE) * 100, 2),
      Primary_Pct   = round(mean(education_category == "Primary (1-5)", na.rm = TRUE) * 100, 2),
      Secondary_Pct = round(mean(education_category == "Secondary (6-12)", na.rm = TRUE) * 100, 2),
      Tertiary_Pct  = round(mean(education_category == "Tertiary", na.rm = TRUE) * 100, 2),

      # Ethnicity
      Hill_High_Pct = round(mean(Ethnicity == "Hill High Caste", na.rm = TRUE) * 100, 2),
      Janajati_Pct  = round(mean(Ethnicity == "Hill Janajati", na.rm = TRUE) * 100, 2),
      Terai_Pct     = round(mean(Ethnicity == "Terai/Madhesi", na.rm = TRUE) * 100, 2),
      Dalit_Pct     = round(mean(Ethnicity == "Dalit", na.rm = TRUE) * 100, 2),
      Muslim_Pct    = round(mean(Ethnicity == "Muslim", na.rm = TRUE) * 100, 2),

      # Occupation
      Agri_Pct       = round(mean(occupation_category == "Agriculture", na.rm = TRUE) * 100, 2),
      HighSkill_Pct  = round(mean(occupation_category == "High Skilled", na.rm = TRUE) * 100, 2),
      Service_Pct    = round(mean(occupation_category == "Service & Clerical", na.rm = TRUE) * 100, 2),
      Craft_Pct      = round(mean(occupation_category == "Craft & Manufacturing", na.rm = TRUE) * 100, 2),
      Elementary_Pct = round(mean(occupation_category == "Elementary/Low Skilled", na.rm = TRUE) * 100, 2),
      Armed_Pct      = round(mean(occupation_category == "Armed Forces", na.rm = TRUE) * 100, 2),

      .groups = "drop"
    )
}

# ── Helper: Welch's t-test for Treatment vs. Control ────────────────────────
#    Returns "diff (p=X.XXX)" string for each variable

compute_ttest_diff <- function(treat_df, ctrl_df, var, is_continuous = FALSE) {
  t_vals <- treat_df[[var]]
  c_vals <- ctrl_df[[var]]
  t_vals <- t_vals[!is.na(t_vals)]
  c_vals <- c_vals[!is.na(c_vals)]

  if (length(t_vals) < 2 | length(c_vals) < 2) return("")

  test <- t.test(t_vals, c_vals, var.equal = FALSE)
  diff  <- mean(t_vals) - mean(c_vals)

  if (is_continuous) {
    return(paste0(round(diff, 2), " (p=", formatC(test$p.value, format = "f", digits = 3), ")"))
  } else {
    return(paste0(round(diff * 100, 2), " (p=", formatC(test$p.value, format = "f", digits = 3), ")"))
  }
}

# ── Helper: format summary table with Diff column ───────────────────────────

format_summary_table <- function(baseline, migrant, treatment, control,
                                  migrant_label, treat_df, ctrl_df) {

  fmt <- function(mean, sd) paste0(mean, " (", sd, ")")
  v   <- function(df, col) df[[col]]

  # Compute t-test differences for each variable
  diff_age      <- compute_ttest_diff(treat_df, ctrl_df, "age", is_continuous = TRUE)
  diff_age_conf <- compute_ttest_diff(treat_df, ctrl_df, "age_at_conflict_start", is_continuous = TRUE)
  diff_male     <- compute_ttest_diff(treat_df, ctrl_df, "male")
  diff_no_edu   <- compute_ttest_diff(treat_df, ctrl_df, "edu_no_education")
  diff_primary  <- compute_ttest_diff(treat_df, ctrl_df, "edu_primary")
  diff_second   <- compute_ttest_diff(treat_df, ctrl_df, "edu_secondary")
  diff_tertiary <- compute_ttest_diff(treat_df, ctrl_df, "edu_tertiary")
  diff_hill     <- compute_ttest_diff(treat_df, ctrl_df, "eth_hill_high")
  diff_jan      <- compute_ttest_diff(treat_df, ctrl_df, "eth_janajati")
  diff_terai    <- compute_ttest_diff(treat_df, ctrl_df, "eth_terai")
  diff_dalit    <- compute_ttest_diff(treat_df, ctrl_df, "eth_dalit")
  diff_muslim   <- compute_ttest_diff(treat_df, ctrl_df, "eth_muslim")
  diff_agri     <- compute_ttest_diff(treat_df, ctrl_df, "occ_agriculture")
  diff_high     <- compute_ttest_diff(treat_df, ctrl_df, "occ_high_skilled")
  diff_service  <- compute_ttest_diff(treat_df, ctrl_df, "occ_service")
  diff_craft    <- compute_ttest_diff(treat_df, ctrl_df, "occ_craft")
  diff_elem     <- compute_ttest_diff(treat_df, ctrl_df, "occ_elementary")
  diff_armed    <- compute_ttest_diff(treat_df, ctrl_df, "occ_armed")

  data.frame(
    Variable = c(
      "Sample Size", "",
      "Age:",
      "Age in 2017", "",
      "Age at Conflict Start", "",
      "Male (%)", "",
      "Education Distribution (%):",
      "No Education", "Primary (1-5)", "Secondary (6-12)", "Tertiary", "",
      "Ethnicity Distribution (%):",
      "Hill High Caste", "Hill Janajati", "Terai/Madhesi", "Dalit", "Muslim", "",
      "Occupation Type (%):",
      "Agriculture", "High Skilled", "Service & Clerical",
      "Craft & Manufacturing", "Elementary/Low Skilled", "Armed Forces"
    ),
    Baseline = c(
      format(v(baseline,"N"), big.mark=","), "",
      "", fmt(v(baseline,"Age_Mean"), v(baseline,"Age_SD")), "",
      fmt(v(baseline,"Age_Conf_Mean"), v(baseline,"Age_Conf_SD")), "",
      v(baseline,"Male_Pct"), "",
      "", v(baseline,"No_Edu_Pct"), v(baseline,"Primary_Pct"),
      v(baseline,"Secondary_Pct"), v(baseline,"Tertiary_Pct"), "",
      "", v(baseline,"Hill_High_Pct"), v(baseline,"Janajati_Pct"),
      v(baseline,"Terai_Pct"), v(baseline,"Dalit_Pct"), v(baseline,"Muslim_Pct"), "",
      "", v(baseline,"Agri_Pct"), v(baseline,"HighSkill_Pct"),
      v(baseline,"Service_Pct"), v(baseline,"Craft_Pct"),
      v(baseline,"Elementary_Pct"), v(baseline,"Armed_Pct")
    ),
    Migrant = c(
      format(v(migrant,"N"), big.mark=","), "",
      "", fmt(v(migrant,"Age_Mean"), v(migrant,"Age_SD")), "",
      fmt(v(migrant,"Age_Conf_Mean"), v(migrant,"Age_Conf_SD")), "",
      v(migrant,"Male_Pct"), "",
      "", v(migrant,"No_Edu_Pct"), v(migrant,"Primary_Pct"),
      v(migrant,"Secondary_Pct"), v(migrant,"Tertiary_Pct"), "",
      "", v(migrant,"Hill_High_Pct"), v(migrant,"Janajati_Pct"),
      v(migrant,"Terai_Pct"), v(migrant,"Dalit_Pct"), v(migrant,"Muslim_Pct"), "",
      "", v(migrant,"Agri_Pct"), v(migrant,"HighSkill_Pct"),
      v(migrant,"Service_Pct"), v(migrant,"Craft_Pct"),
      v(migrant,"Elementary_Pct"), v(migrant,"Armed_Pct")
    ),
    Treatment = c(
      format(v(treatment,"N"), big.mark=","), "",
      "", fmt(v(treatment,"Age_Mean"), v(treatment,"Age_SD")), "",
      fmt(v(treatment,"Age_Conf_Mean"), v(treatment,"Age_Conf_SD")), "",
      v(treatment,"Male_Pct"), "",
      "", v(treatment,"No_Edu_Pct"), v(treatment,"Primary_Pct"),
      v(treatment,"Secondary_Pct"), v(treatment,"Tertiary_Pct"), "",
      "", v(treatment,"Hill_High_Pct"), v(treatment,"Janajati_Pct"),
      v(treatment,"Terai_Pct"), v(treatment,"Dalit_Pct"), v(treatment,"Muslim_Pct"), "",
      "", v(treatment,"Agri_Pct"), v(treatment,"HighSkill_Pct"),
      v(treatment,"Service_Pct"), v(treatment,"Craft_Pct"),
      v(treatment,"Elementary_Pct"), v(treatment,"Armed_Pct")
    ),
    Control = c(
      format(v(control,"N"), big.mark=","), "",
      "", fmt(v(control,"Age_Mean"), v(control,"Age_SD")), "",
      fmt(v(control,"Age_Conf_Mean"), v(control,"Age_Conf_SD")), "",
      v(control,"Male_Pct"), "",
      "", v(control,"No_Edu_Pct"), v(control,"Primary_Pct"),
      v(control,"Secondary_Pct"), v(control,"Tertiary_Pct"), "",
      "", v(control,"Hill_High_Pct"), v(control,"Janajati_Pct"),
      v(control,"Terai_Pct"), v(control,"Dalit_Pct"), v(control,"Muslim_Pct"), "",
      "", v(control,"Agri_Pct"), v(control,"HighSkill_Pct"),
      v(control,"Service_Pct"), v(control,"Craft_Pct"),
      v(control,"Elementary_Pct"), v(control,"Armed_Pct")
    ),
    Diff_TC = c(
      "", "",
      "",
      diff_age, "",
      diff_age_conf, "",
      diff_male, "",
      "",
      diff_no_edu, diff_primary, diff_second, diff_tertiary, "",
      "",
      diff_hill, diff_jan, diff_terai, diff_dalit, diff_muslim, "",
      "",
      diff_agri, diff_high, diff_service, diff_craft, diff_elem, diff_armed
    ),
    stringsAsFactors = FALSE
  ) %>%
    setNames(c("Variable", "Baseline", migrant_label, "Treatment", "Control", "Diff (T-C) (p-value)"))
}

# =============================================================================
# PREPARE DATA FOR T-TESTS
# =============================================================================

# Create binary dummies needed for t-tests
nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    male = ifelse(sex == 1, 1, 0),

    # Education dummies
    edu_no_education = ifelse(education_category == "No Education", 1, 0),
    edu_primary      = ifelse(education_category == "Primary (1-5)", 1, 0),
    edu_secondary    = ifelse(education_category == "Secondary (6-12)", 1, 0),
    edu_tertiary     = ifelse(education_category == "Tertiary", 1, 0),

    # Ethnicity dummies
    eth_hill_high = ifelse(Ethnicity == "Hill High Caste", 1, 0),
    eth_janajati  = ifelse(Ethnicity == "Hill Janajati", 1, 0),
    eth_terai     = ifelse(Ethnicity == "Terai/Madhesi", 1, 0),
    eth_dalit     = ifelse(Ethnicity == "Dalit", 1, 0),
    eth_muslim    = ifelse(Ethnicity == "Muslim", 1, 0),

    # Occupation dummies
    occ_agriculture  = ifelse(occupation_category == "Agriculture", 1, 0),
    occ_high_skilled = ifelse(occupation_category == "High Skilled", 1, 0),
    occ_service      = ifelse(occupation_category == "Service & Clerical", 1, 0),
    occ_craft        = ifelse(occupation_category == "Craft & Manufacturing", 1, 0),
    occ_elementary   = ifelse(occupation_category == "Elementary/Low Skilled", 1, 0),
    occ_armed        = ifelse(occupation_category == "Armed Forces", 1, 0)
  )

# Define Treatment and Control subsets
treat_df <- nlss_conflict_data %>% filter(treatment == 1)
ctrl_df  <- nlss_conflict_data %>%
  filter(treatment == 0, age_at_conflict_start >= 18, age_at_conflict_start <= 40)

# =============================================================================
# COMPUTE SUMMARIES BY GROUP
# =============================================================================

baseline_data      <- nlss_conflict_data %>% filter(baseline == 1) %>% compute_group_summary()
intl_absentee_data <- nlss_conflict_data %>% filter(international_absentee_only == 1) %>% compute_group_summary()
national_data      <- nlss_conflict_data %>% filter(national == 1) %>% compute_group_summary()
treatment_data     <- treat_df %>% compute_group_summary()
control_data       <- ctrl_df %>% compute_group_summary()

# =============================================================================
# TABLE A: INTERNATIONAL MIGRATION (with T-C Diff column)
# =============================================================================

table_intl <- format_summary_table(
  baseline_data, intl_absentee_data, treatment_data, control_data,
  "Intl. Absentee", treat_df, ctrl_df
)

# ── Export LaTeX ─────────────────────────────────────────────────────────────

latex_intl <- kable(table_intl,
  format    = "latex",
  booktabs  = TRUE,
  caption   = "International Migration: Summary Statistics with Treatment-Control Difference",
  label     = "tab:intl_migration_summary",
  escape    = FALSE,
  align     = c("l", "r", "r", "r", "r", "r")
) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size     = 10
  ) %>%
  footnote(
    general = c(
      "Standard deviations in parentheses for continuous variables.",
      "Diff (T-C) column: Treatment minus Control with Welch's t-test p-value.",
      "Baseline: present at survey and never migrated. Intl. Absentee: absent and currently abroad.",
      "Treatment: aged 0--17 at conflict start (1996). Control: aged 18--40 at conflict start (1996)."
    ),
    general_title     = "Notes:",
    footnote_as_chunk = FALSE,
    escape            = FALSE
  )

writeLines(as.character(latex_intl),
  file.path(output_path, "Table_A_International_Migration_Summary.tex"))

# ── Export Excel ─────────────────────────────────────────────────────────────

write.xlsx(table_intl,
  file      = file.path(output_path, "Table_A_International_Migration_Summary.xlsx"),
  sheetName = "Intl Migration",
  rowNames  = FALSE
)

# =============================================================================
# TABLE B: DOMESTIC (NATIONAL) MIGRATION (with T-C Diff column)
# =============================================================================

table_domestic <- format_summary_table(
  baseline_data, national_data, treatment_data, control_data,
  "National Absent", treat_df, ctrl_df
)

# ── Export LaTeX ─────────────────────────────────────────────────────────────

latex_domestic <- kable(table_domestic,
  format    = "latex",
  booktabs  = TRUE,
  caption   = "Domestic (National) Migration: Summary Statistics with Treatment-Control Difference",
  label     = "tab:domestic_migration_summary",
  escape    = FALSE,
  align     = c("l", "r", "r", "r", "r", "r")
) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size     = 10
  ) %>%
  footnote(
    general = c(
      "Standard deviations in parentheses for continuous variables.",
      "Diff (T-C) column: Treatment minus Control with Welch's t-test p-value.",
      "Baseline: present at survey and never migrated. National Absent: absent and inside Nepal.",
      "Treatment: aged 0--17 at conflict start (1996). Control: aged 18--40 at conflict start (1996)."
    ),
    general_title     = "Notes:",
    footnote_as_chunk = FALSE,
    escape            = FALSE
  )

writeLines(as.character(latex_domestic),
  file.path(output_path, "Table_B_Domestic_Migration_Summary.tex"))

# ── Export Excel ─────────────────────────────────────────────────────────────

write.xlsx(table_domestic,
  file      = file.path(output_path, "Table_B_Domestic_Migration_Summary.xlsx"),
  sheetName = "Domestic Migration",
  rowNames  = FALSE
)

cat("\n✓ Table A (International) saved to:", output_path, "\n")
cat("✓ Table B (Domestic) saved to:", output_path, "\n")
cat("✓ Both tables include Diff (T-C) column with Welch's t-test p-values\n")
