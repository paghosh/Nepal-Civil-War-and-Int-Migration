# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Setup - Packages and Global Settings and Helper Function
# Last Updated  : April 2026
# ==============================================================================


# ==============================================================================
# SECTION 1: PACKAGE INSTALLATION
# (Run once, then comment out)
# ==============================================================================

# install.packages(c(
#   "haven",        # Import Stata files
#   "dplyr",        # Data manipulation
#   "tidyr",        # Data reshaping
#   "ggplot2",      # Visualization
#   "labelled",     # Variable labels
#   "stringr",      # String manipulation
#   "knitr",        # Tables for LaTeX
#   "kableExtra",   # Enhanced tables
#   "writexl",      # Export to Excel
#   "fixest",       # Fixed effects regression
#   "stargazer",    # Regression tables
#   "modelsummary", # Model summary tables
#   "tinytex",      # LaTeX compilation
#   "tinytable",    # Tables
#   "webshot2"      # Save HTML as PNG
# ))

# For LaTeX packages (run once)
# tinytex::install_tinytex()
# tinytex::tlmgr_install(c("booktabs", "float", "colortbl", "xcolor"))


# ==============================================================================
# SECTION 2: LOAD PACKAGES
# ==============================================================================

# Data import/export
library(haven)
library(writexl)

# Data manipulation
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)

# Visualization
library(ggplot2)

# Tables and output
library(knitr)
library(kableExtra)
library(tinytex)
library(tinytable)
library(webshot2)

# Regression analysis
library(fixest)
library(stargazer)
library(modelsummary)


# ==============================================================================
# SECTION 3: GLOBAL SETTINGS
# ==============================================================================

# Survey year for age calculations
SURVEY_YEAR <- 2017

# Conflict period
CONFLICT_START <- 1996
CONFLICT_END   <- 2006

# ==============================================================================
# SECTION 4: SHARED HELPER FUNCTIONS----
# ==============================================================================

# Reusable function to compute group-level summary stats
compute_group_stats <- function(data_subset) {
  data_subset %>%
    summarise(
      N = n(),
      Age_Mean          = round(mean(age,                   na.rm = TRUE), 2),
      Age_SD            = round(sd(age,                     na.rm = TRUE), 2),
      Age_Conflict_Mean = round(mean(age_at_conflict_start, na.rm = TRUE), 2),
      Age_Conflict_SD   = round(sd(age_at_conflict_start,   na.rm = TRUE), 2),
      Male_Pct          = round(mean(sex == 1,              na.rm = TRUE) * 100, 2),
      No_Edu_Pct        = round(mean(education_category == "No Education",     na.rm = TRUE) * 100, 2),
      Primary_Pct       = round(mean(education_category == "Primary (1-5)",    na.rm = TRUE) * 100, 2),
      Secondary_Pct     = round(mean(education_category == "Secondary (6-12)", na.rm = TRUE) * 100, 2),
      Tertiary_Pct      = round(mean(education_category == "Tertiary",         na.rm = TRUE) * 100, 2),
      High_Caste_Pct    = round(mean(Ethnicity == "Hill High Caste", na.rm = TRUE) * 100, 2),
      Janajati_Pct      = round(mean(Ethnicity == "Hill Janajati",   na.rm = TRUE) * 100, 2),
      Terai_Pct         = round(mean(Ethnicity == "Terai/Madhesi",   na.rm = TRUE) * 100, 2),
      Dalit_Pct         = round(mean(Ethnicity == "Dalit",           na.rm = TRUE) * 100, 2),
      Muslim_Pct        = round(mean(Ethnicity == "Muslim",          na.rm = TRUE) * 100, 2),
      Agri_Pct          = round(mean(occupation_category == "Agriculture",            na.rm = TRUE) * 100, 2),
      HighSkill_Pct     = round(mean(occupation_category == "High Skilled",           na.rm = TRUE) * 100, 2),
      Service_Pct       = round(mean(occupation_category == "Service & Clerical",     na.rm = TRUE) * 100, 2),
      Craft_Pct         = round(mean(occupation_category == "Craft & Manufacturing",  na.rm = TRUE) * 100, 2),
      Elementary_Pct    = round(mean(occupation_category == "Elementary/Low Skilled", na.rm = TRUE) * 100, 2),
      Armed_Pct         = round(mean(occupation_category == "Armed Forces",           na.rm = TRUE) * 100, 2),
      .groups = "drop"
    )
}

# Shortcut: pull one stat from a stats data frame
g <- function(stats_df, col) stats_df[[col]]



# ==============================================================================
# SECTION 5: CORRECTING THE LATEX OUTPUT-----
# ==============================================================================

# Sanitize strings for LaTeX output
sanitize_latex <- function(x) {
  x %>%
    str_replace_all("&",  "\\\\&") %>%   # & → \&
    str_replace_all("%",  "\\\\%")        # % → \%
}



# ==============================================================================
# SECTION 6: BUILDING STANDART COVARIATE SUMMARY TABLE-----
# ==============================================================================

# Format mean (SD) — latex = TRUE puts SD on new line using \makecell
format_mean_sd <- function(mean_val, sd_val, latex = FALSE) {
  if (latex) {
    paste0("\\makecell[c]{", mean_val, " \\\\ (", sd_val, ")}")
  } else {
    paste0(mean_val, " (", sd_val, ")")
  }
}

# Format mean (SE) — same pattern
format_mean_se <- function(mean_val, se_val, latex = FALSE) {
  if (latex) {
    paste0("\\makecell[c]{", mean_val, " \\\\ (", se_val, ")}")
  } else {
    paste0(mean_val, " (", se_val, ")")
  }
}

build_covariate_table <- function(groups, latex = FALSE) {
  
  # Standard Variable column (same for every summary table)
  var_col <- c(
    "Sample Size",
    "",
    "Age",
    "  Age in 2017",
    "",
    "  Age at Conflict Start",
    "",
    "Male (%)",
    "",
    "Education Distribution (%):",
    "  No Education",
    "  Primary (1-5)",
    "  Secondary (6-12)",
    "  Tertiary",
    "",
    "Ethnicity Distribution (%):",
    "  Hill High Caste",
    "  Hill Janajati",
    "  Terai/Madhesi",
    "  Dalit",
    "  Muslim",
    "",
    "Occupation Type (%):",
    "  Agriculture",
    "  High Skilled",
    "  Service & Clerical",
    "  Craft & Manufacturing",
    "  Elementary/Low Skilled",
    "  Armed Forces"
  )
  
  # Build one data column per group using its value-puller function
  group_cols <- lapply(groups, function(val) {
    c(
      as.character(val("N")),
      "",
      "",
      format_mean_sd(val("Age_Mean"),          val("Age_SD"),          latex = latex),  # ← passed here
      "",
      format_mean_sd(val("Age_Conflict_Mean"), val("Age_Conflict_SD"), latex = latex),  # ← and here
      "",
      as.character(val("Male_Pct")),
      "",
      "",
      as.character(val("No_Edu_Pct")),
      as.character(val("Primary_Pct")),
      as.character(val("Secondary_Pct")),
      as.character(val("Tertiary_Pct")),
      "",
      "",
      as.character(val("High_Caste_Pct")),
      as.character(val("Janajati_Pct")),
      as.character(val("Terai_Pct")),
      as.character(val("Dalit_Pct")),
      as.character(val("Muslim_Pct")),
      "",
      "",
      as.character(val("Agri_Pct")),
      as.character(val("HighSkill_Pct")),
      as.character(val("Service_Pct")),
      as.character(val("Craft_Pct")),
      as.character(val("Elementary_Pct")),
      as.character(val("Armed_Pct"))
    )
  })
  
  # Combine Variable column with all group columns
  result <- data.frame(Variable = var_col, stringsAsFactors = FALSE)
  for (col_name in names(group_cols)) {
    result[[col_name]] <- group_cols[[col_name]]
  }
  
  return(result)
}


# ==============================================================================
# SECTION 7: BUILDING ttest BALANCE TABLE-----
# ==============================================================================

# Welch's t-test: Treatment minus Control with stars and p-value
compute_ttest_diff <- function(treat_df, ctrl_df, var,
                               is_continuous = FALSE,
                               latex         = FALSE) {
  
  t_vals <- treat_df[[var]][!is.na(treat_df[[var]])]
  c_vals <- ctrl_df[[var]][!is.na(ctrl_df[[var]])]
  
  if (length(t_vals) < 2 | length(c_vals) < 2) return("")
  
  test <- t.test(t_vals, c_vals, var.equal = FALSE)
  
  diff <- if (is_continuous) {
    round(mean(t_vals) - mean(c_vals), 2)
  } else {
    round((mean(t_vals) - mean(c_vals)) * 100, 2)
  }
  
  stars <- case_when(
    test$p.value < 0.01 ~ "***",
    test$p.value < 0.05 ~ "**",
    test$p.value < 0.10 ~ "*",
    TRUE                ~ ""
  )
  
  pval <- paste0("(p=", formatC(test$p.value, format = "f", digits = 3), ")")
  
  # LaTeX: stars on estimate, p-value on new line via \makecell----
  if (latex) {
    paste0("\\makecell[c]{", diff, stars, " \\\\ ", pval, "}")
  } else {
    paste0(diff, stars, " ", pval)
  }
}


# Build the Diff (T-C) column — row order matches build_covariate_table()
build_diff_column <- function(treat_df, ctrl_df, latex = FALSE) {
  d <- function(var, continuous = FALSE) {
    compute_ttest_diff(treat_df, ctrl_df, var,
                       is_continuous = continuous,
                       latex         = latex)
  }
  
  c(
    "",   # Sample Size
    "",
    "",   # Age header
    d("age",                   continuous = TRUE),
    "",
    d("age_at_conflict_start", continuous = TRUE),
    "",
    d("male"),
    "",
    "",   # Education header
    d("edu_no_education"),
    d("edu_primary"),
    d("edu_secondary"),
    d("edu_tertiary"),
    "",
    "",   # Ethnicity header
    d("eth_hill_high"),
    d("eth_janajati"),
    d("eth_terai"),
    d("eth_dalit"),
    d("eth_muslim"),
    "",
    "",   # Occupation header
    d("occ_agriculture"),
    d("occ_high_skilled"),
    d("occ_service"),
    d("occ_craft"),
    d("occ_elementary"),
    d("occ_armed")
  )
}