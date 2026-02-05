# =============================================================================
# functions/helper_functions.R - Reusable Functions
# =============================================================================
# Project: Nepal Civil Conflict and International Migration
# Author: Ramesh Dulal
#
# USAGE: source("functions/helper_functions.R")
# =============================================================================

# -----------------------------------------------------------------------------
# FORMATTING FUNCTIONS
# -----------------------------------------------------------------------------

#' Format mean with standard deviation in parentheses
#' @param mean_val Numeric mean value
#' @param sd_val Numeric standard deviation
#' @return Character string like "5.23 (1.45)"
format_mean_sd <- function(mean_val, sd_val) {
  paste0(round(mean_val, 2), " (", round(sd_val, 2), ")")
}

#' Format mean with standard error in parentheses
format_mean_se <- function(mean_val, se_val) {
  paste0(round(mean_val, 2), " (", round(se_val, 3), ")")
}

#' Add significance stars based on p-value
#' @param p_value P-value from statistical test
#' @return Character string with stars (*** p<0.01, ** p<0.05, * p<0.10)
add_stars <- function(p_value) {
  case_when(
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.10 ~ "*",
    TRUE ~ ""
  )
}

# -----------------------------------------------------------------------------
# SUMMARY STATISTICS FUNCTIONS
# -----------------------------------------------------------------------------

#' Create summary statistics for a set of variables
#' @param data Data frame
#' @param var_list Character vector of variable names
#' @param group_var Optional grouping variable
#' @return Data frame with summary statistics
create_summary_stats <- function(data, var_list, group_var = NULL) {
  if (is.null(group_var)) {
    summary_data <- data %>%
      summarise(across(all_of(var_list), 
                       list(
                         N = ~sum(!is.na(.)),
                         Mean = ~mean(., na.rm = TRUE),
                         SD = ~sd(., na.rm = TRUE),
                         Min = ~min(., na.rm = TRUE),
                         Max = ~max(., na.rm = TRUE)
                       ),
                       .names = "{.col}_{.fn}"))
  } else {
    summary_data <- data %>%
      group_by(across(all_of(group_var))) %>%
      summarise(across(all_of(var_list), 
                       list(
                         N = ~sum(!is.na(.)),
                         Mean = ~mean(., na.rm = TRUE),
                         SD = ~sd(., na.rm = TRUE),
                         Min = ~min(., na.rm = TRUE),
                         Max = ~max(., na.rm = TRUE)
                       ),
                       .names = "{.col}_{.fn}"),
                .groups = 'drop')
  }
  return(summary_data)
}

#' Clean variable names for display in tables
clean_var_names <- function(var) {
  case_when(
    var == "mwar_own_any" ~ "Months of War (any)",
    var == "mwar_own_fatal" ~ "Months of War (fatal)",
    var == "cas_own_any" ~ "Casualties (any)",
    var == "cas_own_fatal" ~ "Casualties (fatal)",
    var == "age" ~ "Age in 2017",
    var == "age_at_conflict_start" ~ "Age at Conflict Start",
    var == "grade_comp" ~ "Years of Education",
    var == "international_migrant" ~ "International Migrant (%)",
    var == "international_absentee_only" ~ "Currently Abroad (%)",
    var == "present_ind_migrant" ~ "Return Migrant (%)",
    var == "treatment" ~ "Treatment Cohort (%)",
    var == "absent" ~ "Absent from Household (%)",
    TRUE ~ var
  )
}

# -----------------------------------------------------------------------------
# TABLE EXPORT FUNCTIONS
# -----------------------------------------------------------------------------

#' Save table as both LaTeX and PNG (for GitHub)
#' @param kable_obj A kable object
#' @param filename Base filename without extension
#' @param path Output directory path
save_table_both_formats <- function(kable_obj, filename, path) {
  # LaTeX version
  latex_table <- kable_obj %>%
    kable_styling(latex_options = c("hold_position", "scale_down"))
  writeLines(as.character(latex_table), file.path(path, paste0(filename, ".tex")))
  
  # HTML/PNG version for GitHub
  html_table <- kable_obj %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = FALSE)
  html_table %>%
    save_kable(file.path(path, paste0(filename, ".png")))
  
  cat("  Saved:", filename, "(both .tex and .png)\n")
}

# -----------------------------------------------------------------------------
# REGRESSION TABLE FUNCTIONS
# -----------------------------------------------------------------------------

#' Standard coefficient mapping for etable
#' Returns a named vector for use in etable dict parameter
get_coef_dict <- function() {
  c(
    "treatment:high_conflict_q3_binary"      = "Treatment $\\times$ High Conflict",
    "treatment:high_conflict_casualty_binary" = "Treatment $\\times$ High Conflict",
    "treatment:mwar_own_any"                 = "Treatment $\\times$ Months of War",
    "treatment:cas_own_any"                  = "Treatment $\\times$ Casualties",
    "treatment"                              = "Treatment",
    "high_conflict_q3_binary"                = "High Conflict (War)",
    "high_conflict_casualty_binary"          = "High Conflict (Casualty)",
    "mwar_own_any"                           = "Months of War",
    "cas_own_any"                            = "Casualties",
    "sex"                                    = "Male",
    "age"                                    = "Age",
    "I(I(age^2))"                            = "Age$^2$",
    "factor(education_category)Primary(1-5)"    = "Primary (1-5)",
    "factor(education_category)Secondary(6-12)" = "Secondary (6-12)",
    "factor(education_category)Tertiary"        = "Tertiary",
    "factor(Ethnicity)Hill High Caste"          = "Hill High Caste",
    "factor(Ethnicity)Hill Janajati"            = "Hill Janajati",
    "factor(Ethnicity)Muslim"                   = "Muslim",
    "factor(Ethnicity)Terai/Madhesi"            = "Terai/Madhesi"
  )
}

#' Standard coefficient ordering for etable
get_coef_order <- function() {
  c(
    "%treatment:high_conflict",
    "%treatment:mwar",
    "%treatment:cas",
    "%treatment",
    "%high_conflict",
    "%mwar",
    "%cas",
    "%sex",
    "%age",
    "%I(I(age^2))",
    "%factor(education_category)",
    "%factor(Ethnicity)"
  )
}

# -----------------------------------------------------------------------------
# BALANCE TEST FUNCTION
# -----------------------------------------------------------------------------

#' Run balance test between two groups
#' @param data Data frame
#' @param var Variable to test
#' @param group_var Grouping variable (treatment/control)
#' @return Data frame with difference and p-value
run_balance_test <- function(data, var, group_var = "treatment_label") {
  control <- data %>% filter(.data[[group_var]] == "Control") %>% pull({{var}})
  treatment <- data %>% filter(.data[[group_var]] == "Treatment") %>% pull({{var}})
  
  test_result <- t.test(treatment, control)
  
  data.frame(
    Variable = var,
    Control_Mean = mean(control, na.rm = TRUE),
    Treatment_Mean = mean(treatment, na.rm = TRUE),
    Difference = mean(treatment, na.rm = TRUE) - mean(control, na.rm = TRUE),
    P_Value = test_result$p.value,
    Significant = add_stars(test_result$p.value)
  )
}

cat("✓ Helper functions loaded.\n")