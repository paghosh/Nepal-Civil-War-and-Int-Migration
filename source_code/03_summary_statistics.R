# =============================================================================
# 03_summary_statistics.R - Descriptive Statistics & Balance Tables
# =============================================================================
# Project: Nepal Civil Conflict and International Migration
# Author: Ramesh Dulal
#
# REQUIRES: 02_data_cleaning.R must be run first
#
# OUTPUTS:
# - Table1_Overall_Summary.tex/png
# - Table2_Summary_TreatmentControl.tex/png
# - Table3_Summary_by_Cohort.tex/png
# - Table4_Summary_by_Conflict.tex/png
# - Table5_Balance_Check.tex/png
# - Table6_DID_Framework.tex/png
# =============================================================================


# Load helper functions
source(file.path(code_path, "functions/Helper_functions.R"))


# =============================================================================
# TABLE 1: OVERALL DESCRIPTIVE STATISTICS
# =============================================================================



# Define variable groups
continuous_vars <- c("mwar_own_any", "mwar_own_fatal", "cas_own_any", "cas_own_fatal", 
                     "age", "age_at_conflict_start", "grade_comp")

binary_vars <- c("international_migrant", "international_absentee_only", 
                 "present_ind_migrant", "treatment", "absent")

# Continuous variables
table1_continuous <- nlss_conflict_data %>%
  summarise(
    across(all_of(continuous_vars),
           list(
             N = ~sum(!is.na(.)),
             Mean = ~round(mean(., na.rm = TRUE), 2),
             SD = ~round(sd(., na.rm = TRUE), 2),
             Min = ~round(min(., na.rm = TRUE), 2),
             Max = ~round(max(., na.rm = TRUE), 2)
           ),
           .names = "{.col}_{.fn}")
  ) %>%
  pivot_longer(everything(),
               names_to = c("Variable", ".value"),
               names_pattern = "(.+)_(N|Mean|SD|Min|Max)") %>%
  mutate(Variable = clean_var_names(Variable))

# Binary variables (as percentages)
table1_binary <- nlss_conflict_data %>%
  summarise(
    across(all_of(binary_vars),
           list(
             N = ~sum(!is.na(.)),
             Percent = ~round(mean(. == 1, na.rm = TRUE) * 100, 2)
           ),
           .names = "{.col}_{.fn}")
  ) %>%
  pivot_longer(everything(),
               names_to = c("Variable", ".value"),
               names_pattern = "(.+)_(N|Percent)") %>%
  mutate(Variable = clean_var_names(Variable),
         Mean = Percent, SD = NA, Min = NA, Max = NA) %>%
  select(Variable, N, Mean, SD, Min, Max)

# Education distribution
table1_education <- nlss_conflict_data %>%
  filter(!is.na(education_category)) %>%
  count(education_category) %>%
  mutate(
    Variable = paste("  ", education_category),
    N = n,
    Mean = round(n / sum(n) * 100, 2),
    SD = NA, Min = NA, Max = NA
  ) %>%
  select(Variable, N, Mean, SD, Min, Max)

# Ethnicity distribution
table1_ethnicity <- nlss_conflict_data %>%
  filter(!is.na(Ethnicity)) %>%
  count(Ethnicity) %>%
  mutate(
    Variable = paste("  ", Ethnicity),
    N = n,
    Mean = round(n / sum(n) * 100, 2),
    SD = NA, Min = NA, Max = NA
  ) %>%
  select(Variable, N, Mean, SD, Min, Max)

# Cohort Distribution
table1_cohort <- nlss_conflict_data %>%
  filter(!is.na(cohort_group)) %>%
  count(cohort_group) %>%
  mutate(
    Variable = paste("  ", cohort_group),
    N = n,
    Mean = round(n / sum(n) * 100, 2),
    SD = NA, Min = NA, Max = NA
  ) %>%
  select(Variable, N, Mean, SD, Min, Max)


# Clean variable names
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

# Convert all data frames to have character columns
table1_continuous <- table1_continuous %>%
  mutate(across(c(N, Mean, SD, Min, Max), as.character))

table1_binary <- table1_binary %>%
  mutate(across(c(N, Mean, SD, Min, Max), as.character))

table1_education <- table1_education %>%
  mutate(across(c(N, Mean, SD, Min, Max), as.character))

table1_ethnicity <- table1_ethnicity %>%
  mutate(across(c(N, Mean, SD, Min, Max), as.character))

table1_cohort <- table1_cohort %>%
  mutate(across(c(N, Mean, SD, Min, Max), as.character))

# Combine all
table1_overall <- bind_rows(
  data.frame(Variable = "Continuous Variables", N = NA, Mean = NA, SD = NA, Min = NA, Max = NA),
  table1_continuous,
  data.frame(Variable = "", N = NA, Mean = NA, SD = NA, Min = NA, Max = NA),
  data.frame(Variable = "Binary Variables (%)", N = NA, Mean = NA, SD = NA, Min = NA, Max = NA),
  table1_binary,
  data.frame(Variable = "", N = NA, Mean = NA, SD = NA, Min = NA, Max = NA),
  data.frame(Variable = "Education Distribution (%)", N = NA, Mean = NA, SD = NA, Min = NA, Max = NA),
  table1_education,
  data.frame(Variable = "", N = NA, Mean = NA, SD = NA, Min = NA, Max = NA),
  data.frame(Variable = "Ethnicity Distribution (%)", N = NA, Mean = NA, SD = NA, Min = NA, Max = NA),
  table1_ethnicity,
  data.frame(Variable = "Cohort Distribution (%)", N = "", Mean = "", SD = "", Min = "", Max = ""),
  table1_cohort
) %>%
  mutate(across(everything(), ~ifelse(is.na(.), "", as.character(.))))

# Export LaTeX
latex_table1 <- kable(table1_overall,
                      format = "latex",
                      booktabs = TRUE,
                      caption = "Descriptive Statistics: Overall Sample",
                      label = "tab:overall_summary",
                      col.names = c("Variable", "N", "Mean/\\%", "SD", "Min", "Max"),
                      escape = FALSE,
                      align = c("l", "r", "r", "r", "r", "r")) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"),
                font_size = 10)

writeLines(as.character(latex_table1), file.path(output_path, "Table1_Overall_Summary.tex"))

# Export PNG
html_table1 <- kable(table1_overall,
                     format = "html",
                     col.names = c("Variable", "N", "Mean/%", "SD", "Min", "Max"),
                     caption = "Descriptive Statistics: Overall Sample") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE)
html_table1 %>% save_kable(file.path(output_path, "Table1_Overall_Summary.png"))


# =============================================================================
# TABLE 2: SUMMARY BY TREATMENT/CONTROL
# =============================================================================

cat("  Creating Table 2: Treatment vs Control Summary...\n")

table2_data <- nlss_conflict_data %>%
  filter(treatment_label %in% c("Treatment", "Control")) %>%
  group_by(treatment_label) %>%
  summarise(
    N = n(),
    
    # Migration outcomes
    Int_Migrant_Mean = round(mean(international_migrant == 1, na.rm = TRUE) * 100, 2),
    Int_Migrant_SD = round(sd(international_migrant, na.rm = TRUE) * 100, 2),
    
    Absentee_Mean = round(mean(international_absentee_only == 1, na.rm = TRUE) * 100, 2),
    Absentee_SD = round(sd(international_absentee_only, na.rm = TRUE) * 100, 2),
    
    Return_Mean = round(mean(present_ind_migrant == 1, na.rm = TRUE) * 100, 2),
    Return_SD = round(sd(present_ind_migrant, na.rm = TRUE) * 100, 2),
    
    # Conflict exposure
    Months_War_Mean = round(mean(mwar_own_any, na.rm = TRUE), 2),
    Months_War_SD = round(sd(mwar_own_any, na.rm = TRUE), 2),
    
    Casualties_Mean = round(mean(cas_own_any, na.rm = TRUE), 2),
    Casualties_SD = round(sd(cas_own_any, na.rm = TRUE), 2),
    
    # Demographics
    Age_Mean = round(mean(age, na.rm = TRUE), 2),
    Age_SD = round(sd(age, na.rm = TRUE), 2),
    
    Age_Conflict_Mean = round(mean(age_at_conflict_start, na.rm = TRUE), 2),
    Age_Conflict_SD = round(sd(age_at_conflict_start, na.rm = TRUE), 2),
    
    Male_Pct = round(mean(sex == 1, na.rm = TRUE) * 100, 2),
    
    # Education
    No_Edu_Pct = round(mean(education_category == "No Education", na.rm = TRUE) * 100, 2),
    Primary_Pct = round(mean(education_category == "Primary (1-5)", na.rm = TRUE) * 100, 2),
    Secondary_Pct = round(mean(education_category == "Secondary (6-12)", na.rm = TRUE) * 100, 2),
    Tertiary_Pct = round(mean(education_category == "Tertiary", na.rm = TRUE) * 100, 2),
    
    # Ethnicity
    High_Caste_Pct = round(mean(Ethnicity == "Hill High Caste", na.rm = TRUE) * 100, 2),
    Janajati_Pct = round(mean(Ethnicity == "Hill Janajati", na.rm = TRUE) * 100, 2),
    Terai_Pct = round(mean(Ethnicity == "Terai/Madhesi", na.rm = TRUE) * 100, 2),
    Dalit_Pct = round(mean(Ethnicity == "Dalit", na.rm = TRUE) * 100, 2),
    Muslim_Pct = round(mean(Ethnicity == "Muslim", na.rm = TRUE) * 100, 2),
    
    .groups = 'drop'
  )

# Format with SD in parentheses
format_mean_sd <- function(mean_val, sd_val) {
  if (is.na(sd_val)) {
    return(as.character(mean_val))
  } else {
    return(paste0(mean_val, "\n(", sd_val, ")"))
  }
}

# Format for display
table2_formatted <- data.frame(
  Variable = c(
    "Sample Size",
    "",
    "Outcome Variables:",
    "  International Migrant (%)",
    "  Currently Abroad (%)",
    "  Return Migrant (%)",
    "",
    "Conflict Exposure:",
    "  Months of War",
    "  Casualties",
    "",
    "Demographics:",
    "  Age in 2017",
    "  Age at Conflict Start",
    "  Male (%)",
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
    "  Muslim"
  ),
  
  Control = c(
    as.character(table2_data$N[table2_data$treatment_label == "Control"]),
    "",
    "",
    format_mean_sd(table2_data$Int_Migrant_Mean[table2_data$treatment_label == "Control"],
                   table2_data$Int_Migrant_SD[table2_data$treatment_label == "Control"]),
    format_mean_sd(table2_data$Absentee_Mean[table2_data$treatment_label == "Control"],
                   table2_data$Absentee_SD[table2_data$treatment_label == "Control"]),
    format_mean_sd(table2_data$Return_Mean[table2_data$treatment_label == "Control"],
                   table2_data$Return_SD[table2_data$treatment_label == "Control"]),
    "",
    "",
    format_mean_sd(table2_data$Months_War_Mean[table2_data$treatment_label == "Control"],
                   table2_data$Months_War_SD[table2_data$treatment_label == "Control"]),
    format_mean_sd(table2_data$Casualties_Mean[table2_data$treatment_label == "Control"],
                   table2_data$Casualties_SD[table2_data$treatment_label == "Control"]),
    "",
    "",
    format_mean_sd(table2_data$Age_Mean[table2_data$treatment_label == "Control"],
                   table2_data$Age_SD[table2_data$treatment_label == "Control"]),
    format_mean_sd(table2_data$Age_Conflict_Mean[table2_data$treatment_label == "Control"],
                   table2_data$Age_Conflict_SD[table2_data$treatment_label == "Control"]),
    as.character(table2_data$Male_Pct[table2_data$treatment_label == "Control"]),
    "",
    "",
    as.character(table2_data$No_Edu_Pct[table2_data$treatment_label == "Control"]),
    as.character(table2_data$Primary_Pct[table2_data$treatment_label == "Control"]),
    as.character(table2_data$Secondary_Pct[table2_data$treatment_label == "Control"]),
    as.character(table2_data$Tertiary_Pct[table2_data$treatment_label == "Control"]),
    "",
    "",
    as.character(table2_data$High_Caste_Pct[table2_data$treatment_label == "Control"]),
    as.character(table2_data$Janajati_Pct[table2_data$treatment_label == "Control"]),
    as.character(table2_data$Terai_Pct[table2_data$treatment_label == "Control"]),
    as.character(table2_data$Dalit_Pct[table2_data$treatment_label == "Control"]),
    as.character(table2_data$Muslim_Pct[table2_data$treatment_label == "Control"])
  ),
  
  Treatment = c(
    as.character(table2_data$N[table2_data$treatment_label == "Treatment"]),
    "",
    "",
    format_mean_sd(table2_data$Int_Migrant_Mean[table2_data$treatment_label == "Treatment"],
                   table2_data$Int_Migrant_SD[table2_data$treatment_label == "Treatment"]),
    format_mean_sd(table2_data$Absentee_Mean[table2_data$treatment_label == "Treatment"],
                   table2_data$Absentee_SD[table2_data$treatment_label == "Treatment"]),
    format_mean_sd(table2_data$Return_Mean[table2_data$treatment_label == "Treatment"],
                   table2_data$Return_SD[table2_data$treatment_label == "Treatment"]),
    "",
    "",
    format_mean_sd(table2_data$Months_War_Mean[table2_data$treatment_label == "Treatment"],
                   table2_data$Months_War_SD[table2_data$treatment_label == "Treatment"]),
    format_mean_sd(table2_data$Casualties_Mean[table2_data$treatment_label == "Treatment"],
                   table2_data$Casualties_SD[table2_data$treatment_label == "Treatment"]),
    "",
    "",
    format_mean_sd(table2_data$Age_Mean[table2_data$treatment_label == "Treatment"],
                   table2_data$Age_SD[table2_data$treatment_label == "Treatment"]),
    format_mean_sd(table2_data$Age_Conflict_Mean[table2_data$treatment_label == "Treatment"],
                   table2_data$Age_Conflict_SD[table2_data$treatment_label == "Treatment"]),
    as.character(table2_data$Male_Pct[table2_data$treatment_label == "Treatment"]),
    "",
    "",
    as.character(table2_data$No_Edu_Pct[table2_data$treatment_label == "Treatment"]),
    as.character(table2_data$Primary_Pct[table2_data$treatment_label == "Treatment"]),
    as.character(table2_data$Secondary_Pct[table2_data$treatment_label == "Treatment"]),
    as.character(table2_data$Tertiary_Pct[table2_data$treatment_label == "Treatment"]),
    "",
    "",
    as.character(table2_data$High_Caste_Pct[table2_data$treatment_label == "Treatment"]),
    as.character(table2_data$Janajati_Pct[table2_data$treatment_label == "Treatment"]),
    as.character(table2_data$Terai_Pct[table2_data$treatment_label == "Treatment"]),
    as.character(table2_data$Dalit_Pct[table2_data$treatment_label == "Treatment"]),
    as.character(table2_data$Muslim_Pct[table2_data$treatment_label == "Treatment"])
  ),
  
  stringsAsFactors = FALSE
)

# Export
latex_table2 <- kable(table2_formatted,
                      format = "latex",
                      booktabs = TRUE,
                      caption = "Summary Statistics by Treatment Status",
                      label = "tab:treat_control",
                      col.names = c("Variable", "Control", "Treatment"),
                      escape = FALSE) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
  footnote(general = "Standard deviations in parentheses.",
           footnote_as_chunk = TRUE)

writeLines(as.character(latex_table2), file.path(output_path, "Table2_Summary_TreatmentControl.tex"))

html_table2 <- kable(table2_formatted, format = "html",
                     col.names = c("Variable", "Control", "Treatment"),
                     caption = "Summary Statistics by Treatment Status") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
html_table2 %>% save_kable(file.path(output_path, "Table2_Summary_TreatmentControl.png"))


# =============================================================================
# TABLE 5: BALANCE CHECK
# =============================================================================

cat("  Creating Table 5: Balance Check...\n")

# Variables to test
balance_vars <- c("international_migrant", "international_absentee_only",
                  "mwar_own_any", "cas_own_any", "age", "age_at_conflict_start")

# Run balance tests
balance_results <- lapply(balance_vars, function(v) {
  run_balance_test(nlss_conflict_data %>% 
                     filter(treatment_label %in% c("Treatment", "Control")), 
                   var = v)
}) %>% bind_rows()

# Create and export balance table
balance_table <- balance_results %>%
  mutate(
    Variable = clean_var_names(Variable),
    Control = round(Control_Mean, 3),
    Treatment = round(Treatment_Mean, 3),
    Diff = paste0(round(Difference, 3), Significant)
  ) %>%
  select(Variable, Control, Treatment, Diff)

latex_table5 <- kable(balance_table,
                      format = "latex",
                      booktabs = TRUE,
                      caption = "Balance Check: Treatment vs Control Groups",
                      label = "tab:balance",
                      col.names = c("Variable", "Control", "Treatment", "Difference"),
                      escape = FALSE) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
  footnote(general = "*** p<0.01, ** p<0.05, * p<0.10",
           footnote_as_chunk = TRUE)

writeLines(as.character(latex_table5), file.path(output_path, "Table5_Balance_Check.tex"))

html_table5 <- kable(balance_table, format = "html",
                     col.names = c("Variable", "Control", "Treatment", "Difference"),
                     caption = "Balance Check: Treatment vs Control Groups") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
html_table5 %>% save_kable(file.path(output_path, "Table5_Balance_Check.png"))


# =============================================================================
# TABLE 6: DID FRAMEWORK (2x2)
# =============================================================================

cat("  Creating Table 6: DID Framework...\n")

did_data <- nlss_conflict_data %>%
  filter(!is.na(high_conflict_casualty_label) & 
           treatment_label %in% c("Treatment", "Control")) %>%
  group_by(treatment_label, high_conflict_casualty_label) %>%
  summarise(
    N = n(),
    Int_Migrant_Pct = round(mean(international_migrant == 1, na.rm = TRUE) * 100, 2),
    .groups = 'drop'
  )

# Calculate DID
control_low <- did_data$Int_Migrant_Pct[did_data$treatment_label == "Control" & 
                                          did_data$high_conflict_casualty_label == "Low Conflict"]
control_high <- did_data$Int_Migrant_Pct[did_data$treatment_label == "Control" & 
                                           did_data$high_conflict_casualty_label == "High Conflict"]
treat_low <- did_data$Int_Migrant_Pct[did_data$treatment_label == "Treatment" & 
                                        did_data$high_conflict_casualty_label == "Low Conflict"]
treat_high <- did_data$Int_Migrant_Pct[did_data$treatment_label == "Treatment" & 
                                         did_data$high_conflict_casualty_label == "High Conflict"]

did_estimate <- round((treat_high - treat_low) - (control_high - control_low), 2)

cat("    DID Estimate:", did_estimate, "percentage points\n")

# Format table
table6_formatted <- data.frame(
  Group = c("Control", "Treatment", "Difference (T-C)"),
  Low_Conflict = c(paste0(control_low, "%"), paste0(treat_low, "%"), 
                   paste0(round(treat_low - control_low, 2), " pp")),
  High_Conflict = c(paste0(control_high, "%"), paste0(treat_high, "%"),
                    paste0(round(treat_high - control_high, 2), " pp")),
  Difference = c(paste0(round(control_high - control_low, 2), " pp"),
                 paste0(round(treat_high - treat_low, 2), " pp"),
                 paste0("DID: ", did_estimate, " pp"))
)

latex_table6 <- kable(table6_formatted,
                      format = "latex",
                      booktabs = TRUE,
                      caption = "DID Framework: International Migration",
                      label = "tab:did",
                      col.names = c("", "Low Conflict", "High Conflict", "Diff (H-L)"),
                      escape = FALSE) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
  footnote(general = "pp = percentage points. Based on casualty-defined conflict intensity.",
           footnote_as_chunk = TRUE)

writeLines(as.character(latex_table6), file.path(output_path, "Table6_DID_Framework.tex"))

html_table6 <- kable(table6_formatted, format = "html",
                     col.names = c("", "Low Conflict", "High Conflict", "Diff (H-L)"),
                     caption = "DID Framework: International Migration") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
html_table6 %>% save_kable(file.path(output_path, "Table6_DID_Framework.png"))


cat("\n✓ Summary statistics tables complete!\n")