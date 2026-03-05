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

binary_vars <- c("international_migrant", "international_absentee_only", "national", 
                 "present_ind_migrant", "treatment", "absent", "baseline", "male")

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

# Marital Status distribution

table1_marital <- nlss_conflict_data %>%
  filter(!is.na(marital)) %>%
  count(marital_label) %>%
  mutate(
    Variable = paste(" ", marital_label),
    N = n,
    Mean = round(n / sum(n)*100, 2),
    SD = NA, Min = NA, Max = NA
  ) %>%
  select(Variable, N, Mean, SD, Min, Max)

# Occupation type distribution

table1_occupation <- nlss_conflict_data %>%
  filter(!is.na(occupation_category)) %>%
  count(occupation_category) %>%
  mutate(
    Variable = paste(" ", occupation_category),
    N = n,
    Mean = round(n / sum(n)*100, 2),
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
    var == "national" ~ "Internal Migrant (%)",
    var == "treatment" ~ "Treatment Cohort (%)",
    var == "absent" ~ "Absent from Household (%)",
    var == "baseline" ~ "Non-Migrant",
    var == "male" ~ "Male (%)",
    var == "marital" ~ "Marital Status",
    var == "nsco_major" ~ "Occupation Type",
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

table1_marital <- table1_marital %>%
  mutate(across(c(N, Mean, SD, Min, Max), as.character))

table1_occupation <- table1_occupation %>%
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
  data.frame(Variable = "Occupation Type (%)", N = NA, Mean = NA, SD = NA, Min = NA, Max = NA),
  table1_occupation,
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

writeLines(as.character(latex_table1), file.path(output_path, "1.Overall_Summary.tex"))


# Export PNG
html_table1 <- kable(table1_overall,
                     format = "html",
                     col.names = c("Variable", "N", "Mean/%", "SD", "Min", "Max"),
                     caption = "Descriptive Statistics: Overall Sample") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  footnote(
    general = c(
      "- International Migrant includes individuals abroad at time of survey and individual who had been abroad ever for at least for 3 months in the past",
      "- Currently Abroad includes individuals abroad at the time of survey",
      "- Internal Migrant includes individuals migrating inside the country at the time of survey",
      "- Return Migrant includes only the individuals who travelled abroad ever for at least for 3 months ",
      "- Absent from Household includes all the absent individuals at the time of survey."
    ),
    general_title = "Notes:",
    footnote_as_chunk = FALSE
  )

html_table1 %>% save_kable(file.path(output_path, "1.Overall_Summary.png"),
                              zoom = 2,
                              vwidth = 900,
                              vheight = 600
)




#===============================================================================
# TABLE 2: SUMMARY BY ABSENT/NON-ABSENT-----------------------------------------
#===============================================================================

table_absent_data <- nlss_conflict_data %>%
  group_by(absent_label) %>%
  summarise(
    N = n(),
    
    # Age
    Age_Mean = round(mean(age, na.rm = TRUE), 2),
    Age_SD = round(sd(age, na.rm = TRUE), 2),
    
    Age_Conflict_Mean = round(mean(age_at_conflict_start, na.rm = TRUE), 2),
    Age_Conflict_SD = round(sd(age_at_conflict_start, na.rm = TRUE), 2),
    
    # Sex
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
    
    # Occupation
    Agri_Pct        = round(mean(occupation_category == "Agriculture",            na.rm = TRUE) * 100, 2),
    HighSkill_Pct   = round(mean(occupation_category == "High Skilled",           na.rm = TRUE) * 100, 2),
    Service_Pct     = round(mean(occupation_category == "Service & Clerical",     na.rm = TRUE) * 100, 2),
    Craft_Pct       = round(mean(occupation_category == "Craft & Manufacturing",  na.rm = TRUE) * 100, 2),
    Elementary_Pct  = round(mean(occupation_category == "Elementary/Low Skilled", na.rm = TRUE) * 100, 2),
    Armed_Pct       = round(mean(occupation_category == "Armed Forces",           na.rm = TRUE) * 100, 2),
    
    .groups = "drop"
  )

# Shortcut helpers
absent_val    <- function(col) table_absent_data[[col]][table_absent_data$absent_label == "Absent"]
nonabsent_val <- function(col) table_absent_data[[col]][table_absent_data$absent_label == "Non-Absent"]


table_absent_formatted <- data.frame(
  Variable = c(
    "Sample Size",
    "",
    "Age:",
    "Age in 2017",
    "",
    "Age at Conflict Start",
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
  ),
  
  Absent = c(
    as.character(absent_val("N")),
    "",
    "",
    format_mean_sd(absent_val("Age_Mean"), absent_val("Age_SD")),
    "",
    format_mean_sd(absent_val("Age_Conflict_Mean"), absent_val("Age_Conflict_SD")),
    "",
    as.character(absent_val("Male_Pct")),
    "",
    "",
    as.character(absent_val("No_Edu_Pct")),
    as.character(absent_val("Primary_Pct")),
    as.character(absent_val("Secondary_Pct")),
    as.character(absent_val("Tertiary_Pct")),
    "",
    "",
    as.character(absent_val("High_Caste_Pct")),
    as.character(absent_val("Janajati_Pct")),
    as.character(absent_val("Terai_Pct")),
    as.character(absent_val("Dalit_Pct")),
    as.character(absent_val("Muslim_Pct")),
    "",
    "",
    as.character(absent_val("Agri_Pct")),
    as.character(absent_val("HighSkill_Pct")),
    as.character(absent_val("Service_Pct")),
    as.character(absent_val("Craft_Pct")),
    as.character(absent_val("Elementary_Pct")),
    as.character(absent_val("Armed_Pct"))
  ),
  
  Non_Absent = c(
    as.character(nonabsent_val("N")),
    "",
    "",
    format_mean_sd(nonabsent_val("Age_Mean"), nonabsent_val("Age_SD")),
    "",
    format_mean_sd(nonabsent_val("Age_Conflict_Mean"), nonabsent_val("Age_Conflict_SD")),
    "",
    as.character(nonabsent_val("Male_Pct")),
    "",
    "",
    as.character(nonabsent_val("No_Edu_Pct")),
    as.character(nonabsent_val("Primary_Pct")),
    as.character(nonabsent_val("Secondary_Pct")),
    as.character(nonabsent_val("Tertiary_Pct")),
    "",
    "",
    as.character(nonabsent_val("High_Caste_Pct")),
    as.character(nonabsent_val("Janajati_Pct")),
    as.character(nonabsent_val("Terai_Pct")),
    as.character(nonabsent_val("Dalit_Pct")),
    as.character(nonabsent_val("Muslim_Pct")),
    "",
    "",
    as.character(nonabsent_val("Agri_Pct")),
    as.character(nonabsent_val("HighSkill_Pct")),
    as.character(nonabsent_val("Service_Pct")),
    as.character(nonabsent_val("Craft_Pct")),
    as.character(nonabsent_val("Elementary_Pct")),
    as.character(nonabsent_val("Armed_Pct"))
  ),
  
  stringsAsFactors = FALSE
)

# --- Export LaTeX ---
latex_absent <- kable(table_absent_formatted,
                      format    = "latex",
                      booktabs  = TRUE,
                      caption   = "Covariate Summary by Absent Status",
                      label     = "tab:absent_summary",
                      col.names = c("Variable", "Absent", "Non-Absent"),
                      escape    = FALSE) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
  footnote(general = "Standard deviations in parentheses for continuous variables.",
           footnote_as_chunk = TRUE)

writeLines(as.character(latex_absent), file.path(output_path, "2.Covariate_Summary_Absent.tex"))

# --- Export Markdown ---
md_absent <- kable(table_absent_formatted,
                   format    = "markdown",
                   col.names = c("Variable", "Absent", "Non-Absent"),
                   caption   = "Covariate Summary by Absent Status")

md_absent_full <- c(
  md_absent,
  "",
  "*Notes:*",
  "- Standard deviations in parentheses for continuous variables.",
  "- Absent from Household includes all the absent individuals at the time of survey."
)

writeLines(md_absent_full, file.path(output_path, "2.Covariate_Summary_Absent.md"))


#===============================================================================
# TABLE 3: SUMMARY BY INTERNATIONAL MIGRANT STATUS  ---------------------------
#===============================================================================

table_migrant_data <- nlss_conflict_data %>%
  filter(!is.na(international_absentee_only_label)) %>%
  group_by(migrant_label) %>%
  summarise(
    N = n(),
    
    # Age
    Age_Mean = round(mean(age, na.rm = TRUE), 2),
    Age_SD   = round(sd(age, na.rm = TRUE), 2),
    
    Age_Conflict_Mean = round(mean(age_at_conflict_start, na.rm = TRUE), 2),
    Age_Conflict_SD   = round(sd(age_at_conflict_start, na.rm = TRUE), 2),
    
    # Sex
    Male_Pct = round(mean(sex == 1, na.rm = TRUE) * 100, 2),
    
    # Education
    No_Edu_Pct    = round(mean(education_category == "No Education",     na.rm = TRUE) * 100, 2),
    Primary_Pct   = round(mean(education_category == "Primary (1-5)",    na.rm = TRUE) * 100, 2),
    Secondary_Pct = round(mean(education_category == "Secondary (6-12)", na.rm = TRUE) * 100, 2),
    Tertiary_Pct  = round(mean(education_category == "Tertiary",         na.rm = TRUE) * 100, 2),
    
    # Ethnicity
    High_Caste_Pct = round(mean(Ethnicity == "Hill High Caste", na.rm = TRUE) * 100, 2),
    Janajati_Pct   = round(mean(Ethnicity == "Hill Janajati",   na.rm = TRUE) * 100, 2),
    Terai_Pct      = round(mean(Ethnicity == "Terai/Madhesi",   na.rm = TRUE) * 100, 2),
    Dalit_Pct      = round(mean(Ethnicity == "Dalit",           na.rm = TRUE) * 100, 2),
    Muslim_Pct     = round(mean(Ethnicity == "Muslim",          na.rm = TRUE) * 100, 2),
    
    # Occupation
    Agri_Pct       = round(mean(occupation_category == "Agriculture",            na.rm = TRUE) * 100, 2),
    HighSkill_Pct  = round(mean(occupation_category == "High Skilled",           na.rm = TRUE) * 100, 2),
    Service_Pct    = round(mean(occupation_category == "Service & Clerical",     na.rm = TRUE) * 100, 2),
    Craft_Pct      = round(mean(occupation_category == "Craft & Manufacturing",  na.rm = TRUE) * 100, 2),
    Elementary_Pct = round(mean(occupation_category == "Elementary/Low Skilled", na.rm = TRUE) * 100, 2),
    Armed_Pct      = round(mean(occupation_category == "Armed Forces",           na.rm = TRUE) * 100, 2),
    
    .groups = "drop"
  )

# Shortcut helpers
migrant_val    <- function(col) table_migrant_data[[col]][table_migrant_data$migrant_label == "Migrant"]
nonmigrant_val <- function(col) table_migrant_data[[col]][table_migrant_data$migrant_label == "Non-Migrant"]

table_migrant_formatted <- data.frame(
  Variable = c(
    "Sample Size",
    "",
    "Age:",
    "  Age in 2017",
    "",
    "  Age at Conflict Start",
    "",
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
    "  Muslim",
    "",
    "Occupation Type (%):",
    "  Agriculture",
    "  High Skilled",
    "  Service & Clerical",
    "  Craft & Manufacturing",
    "  Elementary/Low Skilled",
    "  Armed Forces"
  ),
  
  Migrant = c(
    as.character(migrant_val("N")),
    "",
    "",
    format_mean_sd(migrant_val("Age_Mean"), migrant_val("Age_SD")),
    "",
    format_mean_sd(migrant_val("Age_Conflict_Mean"), migrant_val("Age_Conflict_SD")),
    "",
    as.character(migrant_val("Male_Pct")),
    "",
    "",
    as.character(migrant_val("No_Edu_Pct")),
    as.character(migrant_val("Primary_Pct")),
    as.character(migrant_val("Secondary_Pct")),
    as.character(migrant_val("Tertiary_Pct")),
    "",
    "",
    as.character(migrant_val("High_Caste_Pct")),
    as.character(migrant_val("Janajati_Pct")),
    as.character(migrant_val("Terai_Pct")),
    as.character(migrant_val("Dalit_Pct")),
    as.character(migrant_val("Muslim_Pct")),
    "",
    "",
    as.character(migrant_val("Agri_Pct")),
    as.character(migrant_val("HighSkill_Pct")),
    as.character(migrant_val("Service_Pct")),
    as.character(migrant_val("Craft_Pct")),
    as.character(migrant_val("Elementary_Pct")),
    as.character(migrant_val("Armed_Pct"))
  ),
  
  Non_Migrant = c(
    as.character(nonmigrant_val("N")),
    "",
    "",
    format_mean_sd(nonmigrant_val("Age_Mean"), nonmigrant_val("Age_SD")),
    "",
    format_mean_sd(nonmigrant_val("Age_Conflict_Mean"), nonmigrant_val("Age_Conflict_SD")),
    "",
    as.character(nonmigrant_val("Male_Pct")),
    "",
    "",
    as.character(nonmigrant_val("No_Edu_Pct")),
    as.character(nonmigrant_val("Primary_Pct")),
    as.character(nonmigrant_val("Secondary_Pct")),
    as.character(nonmigrant_val("Tertiary_Pct")),
    "",
    "",
    as.character(nonmigrant_val("High_Caste_Pct")),
    as.character(nonmigrant_val("Janajati_Pct")),
    as.character(nonmigrant_val("Terai_Pct")),
    as.character(nonmigrant_val("Dalit_Pct")),
    as.character(nonmigrant_val("Muslim_Pct")),
    "",
    "",
    as.character(nonmigrant_val("Agri_Pct")),
    as.character(nonmigrant_val("HighSkill_Pct")),
    as.character(nonmigrant_val("Service_Pct")),
    as.character(nonmigrant_val("Craft_Pct")),
    as.character(nonmigrant_val("Elementary_Pct")),
    as.character(nonmigrant_val("Armed_Pct"))
  ),
  
  stringsAsFactors = FALSE
)

# --- Export LaTeX ---
latex_migrant <- kable(table_migrant_formatted,
                       format    = "latex",
                       booktabs  = TRUE,
                       caption   = "Covariate Summary by International Migrant Status",
                       label     = "tab:migrant_summary",
                       col.names = c("Variable", "Migrant", "Non-Migrant"),
                       escape    = FALSE) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
  footnote(general = "Standard deviations in parentheses for continuous variables.",
           footnote_as_chunk = TRUE)

writeLines(as.character(latex_migrant), file.path(output_path, "3.Covariate_Summary_Migrant.tex"))

# --- Export Markdown ---
md_migrant <- kable(table_migrant_formatted,
                    format    = "markdown",
                    col.names = c("Variable", "Migrant", "Non-Migrant"),
                    caption   = "Covariate Summary by International Migrant Status")

md_migrant_full <- c(
  md_migrant,
  "",
  "*Notes:*",
  "- Standard deviations in parentheses for continuous variables.",
  "- Migrant includes individuals abroad at time of survey and individual who had been abroad ever at least for 3 months in the past."
)

writeLines(md_migrant_full, file.path(output_path, "3.Covariate_Summary_Migrant.md"))


#===============================================================================
# TABLE 4: SUMMARY BY INTERNATIONAL ABSENTEE ONLY STATUS  ----------------------
#===============================================================================

table_international_absentee_data <- nlss_conflict_data %>%
  filter(!is.na(international_absentee_only_label)) %>%
  group_by(international_absentee_only_label) %>%
  summarise(
    N = n(),
    Age_Mean = round(mean(age, na.rm = TRUE), 2),
    Age_SD   = round(sd(age, na.rm = TRUE), 2),
    Age_Conflict_Mean = round(mean(age_at_conflict_start, na.rm = TRUE), 2),
    Age_Conflict_SD   = round(sd(age_at_conflict_start, na.rm = TRUE), 2),
    Male_Pct = round(mean(sex == 1, na.rm = TRUE) * 100, 2),
    No_Edu_Pct    = round(mean(education_category == "No Education",     na.rm = TRUE) * 100, 2),
    Primary_Pct   = round(mean(education_category == "Primary (1-5)",    na.rm = TRUE) * 100, 2),
    Secondary_Pct = round(mean(education_category == "Secondary (6-12)", na.rm = TRUE) * 100, 2),
    Tertiary_Pct  = round(mean(education_category == "Tertiary",         na.rm = TRUE) * 100, 2),
    High_Caste_Pct = round(mean(Ethnicity == "Hill High Caste", na.rm = TRUE) * 100, 2),
    Janajati_Pct   = round(mean(Ethnicity == "Hill Janajati",   na.rm = TRUE) * 100, 2),
    Terai_Pct      = round(mean(Ethnicity == "Terai/Madhesi",   na.rm = TRUE) * 100, 2),
    Dalit_Pct      = round(mean(Ethnicity == "Dalit",           na.rm = TRUE) * 100, 2),
    Muslim_Pct     = round(mean(Ethnicity == "Muslim",          na.rm = TRUE) * 100, 2),
    Agri_Pct       = round(mean(occupation_category == "Agriculture",            na.rm = TRUE) * 100, 2),
    HighSkill_Pct  = round(mean(occupation_category == "High Skilled",           na.rm = TRUE) * 100, 2),
    Service_Pct    = round(mean(occupation_category == "Service & Clerical",     na.rm = TRUE) * 100, 2),
    Craft_Pct      = round(mean(occupation_category == "Craft & Manufacturing",  na.rm = TRUE) * 100, 2),
    Elementary_Pct = round(mean(occupation_category == "Elementary/Low Skilled", na.rm = TRUE) * 100, 2),
    Armed_Pct      = round(mean(occupation_category == "Armed Forces",           na.rm = TRUE) * 100, 2),
    .groups = "drop"
  )

# Shortcut helpers
international_absentee_val     <- function(col) table_international_absentee_data[[col]][table_international_absentee_data$international_absentee_only_label == "International Absentee"]
non_international_absentee_val <- function(col) table_international_absentee_data[[col]][table_international_absentee_data$international_absentee_only_label == "Non-International Absentee"]

table_migrant_formatted <- data.frame(
  Variable = c(
    "Sample Size",
    "",
    "Age:",
    "  Age in 2017",
    "",
    "  Age at Conflict Start",
    "",
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
    "  Muslim",
    "",
    "Occupation Type (%):",
    "  Agriculture",
    "  High Skilled",
    "  Service & Clerical",
    "  Craft & Manufacturing",
    "  Elementary/Low Skilled",
    "  Armed Forces"
  ),
  
  International_Absentee = c(
    as.character(international_absentee_val("N")),
    "",
    "",
    format_mean_sd(international_absentee_val("Age_Mean"), international_absentee_val("Age_SD")),
    "",
    format_mean_sd(international_absentee_val("Age_Conflict_Mean"), international_absentee_val("Age_Conflict_SD")),
    "",
    as.character(international_absentee_val("Male_Pct")),
    "",
    "",
    as.character(international_absentee_val("No_Edu_Pct")),
    as.character(international_absentee_val("Primary_Pct")),
    as.character(international_absentee_val("Secondary_Pct")),
    as.character(international_absentee_val("Tertiary_Pct")),
    "",
    "",
    as.character(international_absentee_val("High_Caste_Pct")),
    as.character(international_absentee_val("Janajati_Pct")),
    as.character(international_absentee_val("Terai_Pct")),
    as.character(international_absentee_val("Dalit_Pct")),
    as.character(international_absentee_val("Muslim_Pct")),
    "",
    "",
    as.character(international_absentee_val("Agri_Pct")),
    as.character(international_absentee_val("HighSkill_Pct")),
    as.character(international_absentee_val("Service_Pct")),
    as.character(international_absentee_val("Craft_Pct")),
    as.character(international_absentee_val("Elementary_Pct")),
    as.character(international_absentee_val("Armed_Pct"))
  ),
  
  Non_International_Absentee = c(
    as.character(non_international_absentee_val("N")),
    "",
    "",
    format_mean_sd(non_international_absentee_val("Age_Mean"), non_international_absentee_val("Age_SD")),
    "",
    format_mean_sd(non_international_absentee_val("Age_Conflict_Mean"), non_international_absentee_val("Age_Conflict_SD")),
    "",
    as.character(non_international_absentee_val("Male_Pct")),
    "",
    "",
    as.character(non_international_absentee_val("No_Edu_Pct")),
    as.character(non_international_absentee_val("Primary_Pct")),
    as.character(non_international_absentee_val("Secondary_Pct")),
    as.character(non_international_absentee_val("Tertiary_Pct")),
    "",
    "",
    as.character(non_international_absentee_val("High_Caste_Pct")),
    as.character(non_international_absentee_val("Janajati_Pct")),
    as.character(non_international_absentee_val("Terai_Pct")),
    as.character(non_international_absentee_val("Dalit_Pct")),
    as.character(non_international_absentee_val("Muslim_Pct")),
    "",
    "",
    as.character(non_international_absentee_val("Agri_Pct")),
    as.character(non_international_absentee_val("HighSkill_Pct")),
    as.character(non_international_absentee_val("Service_Pct")),
    as.character(non_international_absentee_val("Craft_Pct")),
    as.character(non_international_absentee_val("Elementary_Pct")),
    as.character(non_international_absentee_val("Armed_Pct"))
  ),
  
  stringsAsFactors = FALSE
)

# --- Export LaTeX ---
latex_migrant <- kable(table_migrant_formatted,
                       format    = "latex",
                       booktabs  = TRUE,
                       caption   = "Covariate Summary by International Absentee Status",
                       label     = "tab:migrant_summary",
                       col.names = c("Variable", "International Absentee", "Non-International Absentee"),
                       escape    = FALSE) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
  footnote(general = "Standard deviations in parentheses for continuous variables.",
           footnote_as_chunk = TRUE)

writeLines(as.character(latex_migrant), file.path(output_path, "4.Covariate_Summary_International_Absentee.tex"))

# --- Export Markdown ---
md_migrant <- kable(table_migrant_formatted,
                    format    = "markdown",
                    col.names = c("Variable", "International Absentee", "Non-International Absentee"),
                    caption   = "Covariate Summary by International Absentee Status")

md_migrant_full <- c(
  md_migrant,
  "",
  "*Notes:*",
  "- Standard deviations in parentheses for continuous variables.",
  "- International Absentee includes individuals abroad at time of survey."
)

writeLines(md_migrant_full, file.path(output_path, "4.Covariate_Summary_International_Absentee.md"))

#===============================================================================
# TABLE 5: SUMMARY BY RESPONDENT MIGRANT/RESPONDENT NON-MIGRANT ONLY STATUS  ----
#===============================================================================

table_present_ind_migrant_data <- nlss_conflict_data %>%
  filter(!is.na(present_ind_migrant_label)) %>%  # prevent NA group
  group_by(present_ind_migrant_label) %>%
  summarise(
    N = n(),
    Age_Mean = round(mean(age, na.rm = TRUE), 2),
    Age_SD   = round(sd(age, na.rm = TRUE), 2),
    Age_Conflict_Mean = round(mean(age_at_conflict_start, na.rm = TRUE), 2),
    Age_Conflict_SD   = round(sd(age_at_conflict_start, na.rm = TRUE), 2),
    Male_Pct = round(mean(sex == 1, na.rm = TRUE) * 100, 2),
    No_Edu_Pct    = round(mean(education_category == "No Education",     na.rm = TRUE) * 100, 2),
    Primary_Pct   = round(mean(education_category == "Primary (1-5)",    na.rm = TRUE) * 100, 2),
    Secondary_Pct = round(mean(education_category == "Secondary (6-12)", na.rm = TRUE) * 100, 2),
    Tertiary_Pct  = round(mean(education_category == "Tertiary",         na.rm = TRUE) * 100, 2),
    High_Caste_Pct = round(mean(Ethnicity == "Hill High Caste", na.rm = TRUE) * 100, 2),
    Janajati_Pct   = round(mean(Ethnicity == "Hill Janajati",   na.rm = TRUE) * 100, 2),
    Terai_Pct      = round(mean(Ethnicity == "Terai/Madhesi",   na.rm = TRUE) * 100, 2),
    Dalit_Pct      = round(mean(Ethnicity == "Dalit",           na.rm = TRUE) * 100, 2),
    Muslim_Pct     = round(mean(Ethnicity == "Muslim",          na.rm = TRUE) * 100, 2),
    Agri_Pct       = round(mean(occupation_category == "Agriculture",            na.rm = TRUE) * 100, 2),
    HighSkill_Pct  = round(mean(occupation_category == "High Skilled",           na.rm = TRUE) * 100, 2),
    Service_Pct    = round(mean(occupation_category == "Service & Clerical",     na.rm = TRUE) * 100, 2),
    Craft_Pct      = round(mean(occupation_category == "Craft & Manufacturing",  na.rm = TRUE) * 100, 2),
    Elementary_Pct = round(mean(occupation_category == "Elementary/Low Skilled", na.rm = TRUE) * 100, 2),
    Armed_Pct      = round(mean(occupation_category == "Armed Forces",           na.rm = TRUE) * 100, 2),
    .groups = "drop"
  )

# Shortcut helpers
present_ind_migrant_val             <- function(col) table_present_ind_migrant_data[[col]][table_present_ind_migrant_data$present_ind_migrant_label == "Respondent Migrant"]
present_ind_migrant_nonrespondent_val <- function(col) table_present_ind_migrant_data[[col]][table_present_ind_migrant_data$present_ind_migrant_label == "Respondent Non-Migrant"]

table_migrant_formatted <- data.frame(
  Variable = c(
    "Sample Size",
    "",
    "Age:",
    "  Age in 2017",
    "",
    "  Age at Conflict Start",
    "",
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
    "  Muslim",
    "",
    "Occupation Type (%):",
    "  Agriculture",
    "  High Skilled",
    "  Service & Clerical",
    "  Craft & Manufacturing",
    "  Elementary/Low Skilled",
    "  Armed Forces"
  ),
  
  Respondent_Migrant = c(
    as.character(present_ind_migrant_val("N")),
    "",
    "",
    format_mean_sd(present_ind_migrant_val("Age_Mean"), present_ind_migrant_val("Age_SD")),
    "",
    format_mean_sd(present_ind_migrant_val("Age_Conflict_Mean"), present_ind_migrant_val("Age_Conflict_SD")),
    "",
    as.character(present_ind_migrant_val("Male_Pct")),
    "",
    "",
    as.character(present_ind_migrant_val("No_Edu_Pct")),
    as.character(present_ind_migrant_val("Primary_Pct")),
    as.character(present_ind_migrant_val("Secondary_Pct")),
    as.character(present_ind_migrant_val("Tertiary_Pct")),
    "",
    "",
    as.character(present_ind_migrant_val("High_Caste_Pct")),
    as.character(present_ind_migrant_val("Janajati_Pct")),
    as.character(present_ind_migrant_val("Terai_Pct")),
    as.character(present_ind_migrant_val("Dalit_Pct")),
    as.character(present_ind_migrant_val("Muslim_Pct")),
    "",
    "",
    as.character(present_ind_migrant_val("Agri_Pct")),
    as.character(present_ind_migrant_val("HighSkill_Pct")),
    as.character(present_ind_migrant_val("Service_Pct")),
    as.character(present_ind_migrant_val("Craft_Pct")),
    as.character(present_ind_migrant_val("Elementary_Pct")),
    as.character(present_ind_migrant_val("Armed_Pct"))
  ),
  
  Respondent_Non_Migrant = c(
    as.character(present_ind_migrant_nonrespondent_val("N")),
    "",
    "",
    format_mean_sd(present_ind_migrant_nonrespondent_val("Age_Mean"), present_ind_migrant_nonrespondent_val("Age_SD")),
    "",
    format_mean_sd(present_ind_migrant_nonrespondent_val("Age_Conflict_Mean"), present_ind_migrant_nonrespondent_val("Age_Conflict_SD")),
    "",
    as.character(present_ind_migrant_nonrespondent_val("Male_Pct")),
    "",
    "",
    as.character(present_ind_migrant_nonrespondent_val("No_Edu_Pct")),
    as.character(present_ind_migrant_nonrespondent_val("Primary_Pct")),
    as.character(present_ind_migrant_nonrespondent_val("Secondary_Pct")),
    as.character(present_ind_migrant_nonrespondent_val("Tertiary_Pct")),
    "",
    "",
    as.character(present_ind_migrant_nonrespondent_val("High_Caste_Pct")),
    as.character(present_ind_migrant_nonrespondent_val("Janajati_Pct")),
    as.character(present_ind_migrant_nonrespondent_val("Terai_Pct")),
    as.character(present_ind_migrant_nonrespondent_val("Dalit_Pct")),
    as.character(present_ind_migrant_nonrespondent_val("Muslim_Pct")),
    "",
    "",
    as.character(present_ind_migrant_nonrespondent_val("Agri_Pct")),
    as.character(present_ind_migrant_nonrespondent_val("HighSkill_Pct")),
    as.character(present_ind_migrant_nonrespondent_val("Service_Pct")),
    as.character(present_ind_migrant_nonrespondent_val("Craft_Pct")),
    as.character(present_ind_migrant_nonrespondent_val("Elementary_Pct")),
    as.character(present_ind_migrant_nonrespondent_val("Armed_Pct"))
  ),
  
  stringsAsFactors = FALSE
)

# --- Export LaTeX ---
latex_migrant <- kable(table_migrant_formatted,
                       format    = "latex",
                       booktabs  = TRUE,
                       caption   = "Covariate Summary by Respondent Migrant Status",
                       label     = "tab:migrant_summary",
                       col.names = c("Variable", "Respondent Migrant", "Respondent Non-Migrant"),
                       escape    = FALSE) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
  footnote(general = "Standard deviations in parentheses for continuous variables.",
           footnote_as_chunk = TRUE)

writeLines(as.character(latex_migrant), file.path(output_path, "5.Covariate_Summary_Respondent_Migrant.tex"))

# --- Export Markdown ---
md_migrant <- kable(table_migrant_formatted,
                    format    = "markdown",
                    col.names = c("Variable", "Respondent Migrant", "Respondent Non-Migrant"),
                    caption   = "Covariate Summary by Respondent Migrant Status")

md_migrant_full <- c(
  md_migrant,
  "",
  "*Notes:*",
  "- Standard deviations in parentheses for continuous variables.",
  "- Respondent Migrant includes individuals who travelled abroad previously for at least 3 months."
)

writeLines(md_migrant_full, file.path(output_path, "5.Covariate_Summary_Respondent_Migrant.md"))

#===============================================================================
# TABLE 6: SUMMARY BY RESPONDENT INTERNAL/NON-INTERNAL MIGRANT  STATUS  --------
#===============================================================================

table_internal_migrant_data <- nlss_conflict_data %>%
  filter(!is.na(national_migrant_label)) %>%  # prevent NA group
  group_by(national_migrant_label) %>%
  summarise(
    N = n(),
    Age_Mean = round(mean(age, na.rm = TRUE), 2),
    Age_SD   = round(sd(age, na.rm = TRUE), 2),
    Age_Conflict_Mean = round(mean(age_at_conflict_start, na.rm = TRUE), 2),
    Age_Conflict_SD   = round(sd(age_at_conflict_start, na.rm = TRUE), 2),
    Male_Pct = round(mean(sex == 1, na.rm = TRUE) * 100, 2),
    No_Edu_Pct    = round(mean(education_category == "No Education",     na.rm = TRUE) * 100, 2),
    Primary_Pct   = round(mean(education_category == "Primary (1-5)",    na.rm = TRUE) * 100, 2),
    Secondary_Pct = round(mean(education_category == "Secondary (6-12)", na.rm = TRUE) * 100, 2),
    Tertiary_Pct  = round(mean(education_category == "Tertiary",         na.rm = TRUE) * 100, 2),
    High_Caste_Pct = round(mean(Ethnicity == "Hill High Caste", na.rm = TRUE) * 100, 2),
    Janajati_Pct   = round(mean(Ethnicity == "Hill Janajati",   na.rm = TRUE) * 100, 2),
    Terai_Pct      = round(mean(Ethnicity == "Terai/Madhesi",   na.rm = TRUE) * 100, 2),
    Dalit_Pct      = round(mean(Ethnicity == "Dalit",           na.rm = TRUE) * 100, 2),
    Muslim_Pct     = round(mean(Ethnicity == "Muslim",          na.rm = TRUE) * 100, 2),
    Agri_Pct       = round(mean(occupation_category == "Agriculture",            na.rm = TRUE) * 100, 2),
    HighSkill_Pct  = round(mean(occupation_category == "High Skilled",           na.rm = TRUE) * 100, 2),
    Service_Pct    = round(mean(occupation_category == "Service & Clerical",     na.rm = TRUE) * 100, 2),
    Craft_Pct      = round(mean(occupation_category == "Craft & Manufacturing",  na.rm = TRUE) * 100, 2),
    Elementary_Pct = round(mean(occupation_category == "Elementary/Low Skilled", na.rm = TRUE) * 100, 2),
    Armed_Pct      = round(mean(occupation_category == "Armed Forces",           na.rm = TRUE) * 100, 2),
    .groups = "drop"
  )

# Shortcut helpers
internal_migrant_val     <- function(col) table_internal_migrant_data[[col]][table_internal_migrant_data$national_migrant_label == "Internal Migrant"]
internal_non_migrant_val <- function(col) table_internal_migrant_data[[col]][table_internal_migrant_data$national_migrant_label == "Non-Internal Migrant"]

table_migrant_formatted <- data.frame(
  Variable = c(
    "Sample Size",
    "",
    "Age:",
    "  Age in 2017",
    "",
    "  Age at Conflict Start",
    "",
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
    "  Muslim",
    "",
    "Occupation Type (%):",
    "  Agriculture",
    "  High Skilled",
    "  Service & Clerical",
    "  Craft & Manufacturing",
    "  Elementary/Low Skilled",
    "  Armed Forces"
  ),
  
  Internal_Migrant = c(
    as.character(internal_migrant_val("N")),
    "",
    "",
    format_mean_sd(internal_migrant_val("Age_Mean"), internal_migrant_val("Age_SD")),
    "",
    format_mean_sd(internal_migrant_val("Age_Conflict_Mean"), internal_migrant_val("Age_Conflict_SD")),
    "",
    as.character(internal_migrant_val("Male_Pct")),
    "",
    "",
    as.character(internal_migrant_val("No_Edu_Pct")),
    as.character(internal_migrant_val("Primary_Pct")),
    as.character(internal_migrant_val("Secondary_Pct")),
    as.character(internal_migrant_val("Tertiary_Pct")),
    "",
    "",
    as.character(internal_migrant_val("High_Caste_Pct")),
    as.character(internal_migrant_val("Janajati_Pct")),
    as.character(internal_migrant_val("Terai_Pct")),
    as.character(internal_migrant_val("Dalit_Pct")),
    as.character(internal_migrant_val("Muslim_Pct")),
    "",
    "",
    as.character(internal_migrant_val("Agri_Pct")),
    as.character(internal_migrant_val("HighSkill_Pct")),
    as.character(internal_migrant_val("Service_Pct")),
    as.character(internal_migrant_val("Craft_Pct")),
    as.character(internal_migrant_val("Elementary_Pct")),
    as.character(internal_migrant_val("Armed_Pct"))
  ),
  
  Non_Internal_Migrant = c(
    as.character(internal_non_migrant_val("N")),
    "",
    "",
    format_mean_sd(internal_non_migrant_val("Age_Mean"), internal_non_migrant_val("Age_SD")),
    "",
    format_mean_sd(internal_non_migrant_val("Age_Conflict_Mean"), internal_non_migrant_val("Age_Conflict_SD")),
    "",
    as.character(internal_non_migrant_val("Male_Pct")),
    "",
    "",
    as.character(internal_non_migrant_val("No_Edu_Pct")),
    as.character(internal_non_migrant_val("Primary_Pct")),
    as.character(internal_non_migrant_val("Secondary_Pct")),
    as.character(internal_non_migrant_val("Tertiary_Pct")),
    "",
    "",
    as.character(internal_non_migrant_val("High_Caste_Pct")),
    as.character(internal_non_migrant_val("Janajati_Pct")),
    as.character(internal_non_migrant_val("Terai_Pct")),
    as.character(internal_non_migrant_val("Dalit_Pct")),
    as.character(internal_non_migrant_val("Muslim_Pct")),
    "",
    "",
    as.character(internal_non_migrant_val("Agri_Pct")),
    as.character(internal_non_migrant_val("HighSkill_Pct")),
    as.character(internal_non_migrant_val("Service_Pct")),
    as.character(internal_non_migrant_val("Craft_Pct")),
    as.character(internal_non_migrant_val("Elementary_Pct")),
    as.character(internal_non_migrant_val("Armed_Pct"))
  ),
  
  stringsAsFactors = FALSE
)

# --- Export LaTeX ---
latex_migrant <- kable(table_migrant_formatted,
                       format    = "latex",
                       booktabs  = TRUE,
                       caption   = "Covariate Summary by Internal Migrant Status",
                       label     = "tab:migrant_summary",
                       col.names = c("Variable", "Internal Migrant", "Non-Internal Migrant"),
                       escape    = FALSE) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
  footnote(general = "Standard deviations in parentheses for continuous variables.",
           footnote_as_chunk = TRUE)

writeLines(as.character(latex_migrant), file.path(output_path, "6.Covariate_Summary_Internal_Migrant.tex"))

# --- Export Markdown ---
md_migrant <- kable(table_migrant_formatted,
                    format    = "markdown",
                    col.names = c("Variable", "Internal Migrant", "Non-Internal Migrant"),
                    caption   = "Covariate Summary by Internal Migrant Status")

md_migrant_full <- c(
  md_migrant,
  "",
  "*Notes:*",
  "- Standard deviations in parentheses for continuous variables.",
  "- Internal Migrant includes individuals who migrated within the country."
)

writeLines(md_migrant_full, file.path(output_path, "6.Covariate_Summary_Internal_Migrant.md"))


#===============================================================================
# TABLE 7: SUMMARY BY ALL MIGRANT CATEGORIES  ----------------------------------
#===============================================================================

compute_group_stats <- function(data_subset) {
  data_subset %>%
    summarise(
      N = n(),
      
      # Continuous
      Age_Mean          = round(mean(age,                  na.rm = TRUE), 2),
      Age_SD            = round(sd(age,                    na.rm = TRUE), 2),
      Age_Conflict_Mean = round(mean(age_at_conflict_start, na.rm = TRUE), 2),
      Age_Conflict_SD   = round(sd(age_at_conflict_start,  na.rm = TRUE), 2),
      
      # Sex
      Male_Pct = round(mean(sex == 1, na.rm = TRUE) * 100, 2),
      
      # Education
      No_Edu_Pct    = round(mean(education_category == "No Education",     na.rm = TRUE) * 100, 2),
      Primary_Pct   = round(mean(education_category == "Primary (1-5)",    na.rm = TRUE) * 100, 2),
      Secondary_Pct = round(mean(education_category == "Secondary (6-12)", na.rm = TRUE) * 100, 2),
      Tertiary_Pct  = round(mean(education_category == "Tertiary",         na.rm = TRUE) * 100, 2),
      
      # Ethnicity
      High_Caste_Pct = round(mean(Ethnicity == "Hill High Caste", na.rm = TRUE) * 100, 2),
      Janajati_Pct   = round(mean(Ethnicity == "Hill Janajati",   na.rm = TRUE) * 100, 2),
      Terai_Pct      = round(mean(Ethnicity == "Terai/Madhesi",   na.rm = TRUE) * 100, 2),
      Dalit_Pct      = round(mean(Ethnicity == "Dalit",           na.rm = TRUE) * 100, 2),
      Muslim_Pct     = round(mean(Ethnicity == "Muslim",          na.rm = TRUE) * 100, 2),
      
      # Occupation
      Agri_Pct        = round(mean(occupation_category == "Agriculture",            na.rm = TRUE) * 100, 2),
      HighSkill_Pct   = round(mean(occupation_category == "High Skilled",           na.rm = TRUE) * 100, 2),
      Service_Pct     = round(mean(occupation_category == "Service & Clerical",     na.rm = TRUE) * 100, 2),
      Craft_Pct       = round(mean(occupation_category == "Craft & Manufacturing",  na.rm = TRUE) * 100, 2),
      Elementary_Pct  = round(mean(occupation_category == "Elementary/Low Skilled", na.rm = TRUE) * 100, 2),
      Armed_Pct       = round(mean(occupation_category == "Armed Forces",           na.rm = TRUE) * 100, 2),
      
      .groups = "drop"
    )
}


# =============================================================================
# STEP 1: COMPUTE STATS FOR EACH GROUP
# =============================================================================

stats_baseline  <- nlss_conflict_data %>% filter(baseline == 1)                      %>% compute_group_stats()
stats_absent    <- nlss_conflict_data %>% filter(absent == 1)                         %>% compute_group_stats()
stats_intl      <- nlss_conflict_data %>% filter(international_absentee_only == 1)    %>% compute_group_stats()
stats_national  <- nlss_conflict_data %>% filter(national == 1)                       %>% compute_group_stats()
stats_returnee  <- nlss_conflict_data %>% filter(present_ind_migrant == 1)            %>% compute_group_stats()


# =============================================================================
# STEP 2: HELPER — pull one stat from a group's results
# =============================================================================


g <- function(stats_df, col) stats_df[[col]]


# =============================================================================
# STEP 3: ASSEMBLE THE FORMATTED TABLE
# =============================================================================


table_multigroup <- data.frame(
  
  Variable = c(
    "Sample Size",
    "",
    "Age:",
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
  ),
  
  # ---- COLUMN 1: Baseline (Present Non-Migrants) ----
  Baseline = c(
    as.character(g(stats_baseline, "N")),
    "", "",
    format_mean_sd(g(stats_baseline, "Age_Mean"),          g(stats_baseline, "Age_SD")),
    "",
    format_mean_sd(g(stats_baseline, "Age_Conflict_Mean"), g(stats_baseline, "Age_Conflict_SD")),
    "",
    as.character(g(stats_baseline, "Male_Pct")),
    "", "",
    as.character(g(stats_baseline, "No_Edu_Pct")),
    as.character(g(stats_baseline, "Primary_Pct")),
    as.character(g(stats_baseline, "Secondary_Pct")),
    as.character(g(stats_baseline, "Tertiary_Pct")),
    "", "",
    as.character(g(stats_baseline, "High_Caste_Pct")),
    as.character(g(stats_baseline, "Janajati_Pct")),
    as.character(g(stats_baseline, "Terai_Pct")),
    as.character(g(stats_baseline, "Dalit_Pct")),
    as.character(g(stats_baseline, "Muslim_Pct")),
    "", "",
    as.character(g(stats_baseline, "Agri_Pct")),
    as.character(g(stats_baseline, "HighSkill_Pct")),
    as.character(g(stats_baseline, "Service_Pct")),
    as.character(g(stats_baseline, "Craft_Pct")),
    as.character(g(stats_baseline, "Elementary_Pct")),
    as.character(g(stats_baseline, "Armed_Pct"))
  ),
  
  # ---- COLUMN 2: Total Absent ----
  Total_Absent = c(
    as.character(g(stats_absent, "N")),
    "", "",
    format_mean_sd(g(stats_absent, "Age_Mean"),          g(stats_absent, "Age_SD")),
    "",
    format_mean_sd(g(stats_absent, "Age_Conflict_Mean"), g(stats_absent, "Age_Conflict_SD")),
    "",
    as.character(g(stats_absent, "Male_Pct")),
    "", "",
    as.character(g(stats_absent, "No_Edu_Pct")),
    as.character(g(stats_absent, "Primary_Pct")),
    as.character(g(stats_absent, "Secondary_Pct")),
    as.character(g(stats_absent, "Tertiary_Pct")),
    "", "",
    as.character(g(stats_absent, "High_Caste_Pct")),
    as.character(g(stats_absent, "Janajati_Pct")),
    as.character(g(stats_absent, "Terai_Pct")),
    as.character(g(stats_absent, "Dalit_Pct")),
    as.character(g(stats_absent, "Muslim_Pct")),
    "", "",
    as.character(g(stats_absent, "Agri_Pct")),
    as.character(g(stats_absent, "HighSkill_Pct")),
    as.character(g(stats_absent, "Service_Pct")),
    as.character(g(stats_absent, "Craft_Pct")),
    as.character(g(stats_absent, "Elementary_Pct")),
    as.character(g(stats_absent, "Armed_Pct"))
  ),
  
  # ---- COLUMN 3: International Absentee ----
  Intl_Absentee = c(
    as.character(g(stats_intl, "N")),
    "", "",
    format_mean_sd(g(stats_intl, "Age_Mean"),          g(stats_intl, "Age_SD")),
    "",
    format_mean_sd(g(stats_intl, "Age_Conflict_Mean"), g(stats_intl, "Age_Conflict_SD")),
    "",
    as.character(g(stats_intl, "Male_Pct")),
    "", "",
    as.character(g(stats_intl, "No_Edu_Pct")),
    as.character(g(stats_intl, "Primary_Pct")),
    as.character(g(stats_intl, "Secondary_Pct")),
    as.character(g(stats_intl, "Tertiary_Pct")),
    "", "",
    as.character(g(stats_intl, "High_Caste_Pct")),
    as.character(g(stats_intl, "Janajati_Pct")),
    as.character(g(stats_intl, "Terai_Pct")),
    as.character(g(stats_intl, "Dalit_Pct")),
    as.character(g(stats_intl, "Muslim_Pct")),
    "", "",
    as.character(g(stats_intl, "Agri_Pct")),
    as.character(g(stats_intl, "HighSkill_Pct")),
    as.character(g(stats_intl, "Service_Pct")),
    as.character(g(stats_intl, "Craft_Pct")),
    as.character(g(stats_intl, "Elementary_Pct")),
    as.character(g(stats_intl, "Armed_Pct"))
  ),
  
  # ---- COLUMN 4: National Absent (Internal) ----
  National_Absent = c(
    as.character(g(stats_national, "N")),
    "", "",
    format_mean_sd(g(stats_national, "Age_Mean"),          g(stats_national, "Age_SD")),
    "",
    format_mean_sd(g(stats_national, "Age_Conflict_Mean"), g(stats_national, "Age_Conflict_SD")),
    "",
    as.character(g(stats_national, "Male_Pct")),
    "", "",
    as.character(g(stats_national, "No_Edu_Pct")),
    as.character(g(stats_national, "Primary_Pct")),
    as.character(g(stats_national, "Secondary_Pct")),
    as.character(g(stats_national, "Tertiary_Pct")),
    "", "",
    as.character(g(stats_national, "High_Caste_Pct")),
    as.character(g(stats_national, "Janajati_Pct")),
    as.character(g(stats_national, "Terai_Pct")),
    as.character(g(stats_national, "Dalit_Pct")),
    as.character(g(stats_national, "Muslim_Pct")),
    "", "",
    as.character(g(stats_national, "Agri_Pct")),
    as.character(g(stats_national, "HighSkill_Pct")),
    as.character(g(stats_national, "Service_Pct")),
    as.character(g(stats_national, "Craft_Pct")),
    as.character(g(stats_national, "Elementary_Pct")),
    as.character(g(stats_national, "Armed_Pct"))
  ),
  
  # ---- COLUMN 5: Returnees (Present Individual Migrants) ----
  Returnees = c(
    as.character(g(stats_returnee, "N")),
    "", "",
    format_mean_sd(g(stats_returnee, "Age_Mean"),          g(stats_returnee, "Age_SD")),
    "",
    format_mean_sd(g(stats_returnee, "Age_Conflict_Mean"), g(stats_returnee, "Age_Conflict_SD")),
    "",
    as.character(g(stats_returnee, "Male_Pct")),
    "", "",
    as.character(g(stats_returnee, "No_Edu_Pct")),
    as.character(g(stats_returnee, "Primary_Pct")),
    as.character(g(stats_returnee, "Secondary_Pct")),
    as.character(g(stats_returnee, "Tertiary_Pct")),
    "", "",
    as.character(g(stats_returnee, "High_Caste_Pct")),
    as.character(g(stats_returnee, "Janajati_Pct")),
    as.character(g(stats_returnee, "Terai_Pct")),
    as.character(g(stats_returnee, "Dalit_Pct")),
    as.character(g(stats_returnee, "Muslim_Pct")),
    "", "",
    as.character(g(stats_returnee, "Agri_Pct")),
    as.character(g(stats_returnee, "HighSkill_Pct")),
    as.character(g(stats_returnee, "Service_Pct")),
    as.character(g(stats_returnee, "Craft_Pct")),
    as.character(g(stats_returnee, "Elementary_Pct")),
    as.character(g(stats_returnee, "Armed_Pct"))
  ),
  
  stringsAsFactors = FALSE
)


# =============================================================================
# STEP 4: EXPORT — LaTeX
# =============================================================================

latex_multigroup <- kable(
  table_multigroup,
  format    = "latex",
  booktabs  = TRUE,
  caption   = "Covariate Summary by Migration Group",
  label     = "tab:multigroup_summary",
  col.names = c("Variable", "Baseline", "Total Absent",
                "Intl. Absentee", "National Absent", "Returnees"),
  escape    = FALSE,
  align     = c("l", "r", "r", "r", "r", "r")
) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size = 9
  ) %>%
  footnote(
    general = paste(
      "Standard deviations in parentheses for continuous variables.",
      "Baseline = present at survey and never migrated internationally.",
      "Total Absent = all absent individuals (abroad + internal).",
      "Intl. Absentee = absent and currently abroad.",
      "National Absent = absent and inside Nepal.",
      "Returnees = present at survey but was abroad for work ≥3 months."
    ),
    footnote_as_chunk = TRUE
  )

writeLines(
  as.character(latex_multigroup),
  file.path(output_path, "7.Multigroup_Summary.tex")
)

# --- Export Markdown ---
md_multigroup <- kable(
  table_multigroup,
  format    = "markdown",
  col.names = c("Variable", "Baseline", "Total Absent",
                "Intl. Absentee", "National Absent", "Returnees"),
  align     = c("l", "r", "r", "r", "r", "r")
)

md_multigroup_full <- c(
  md_multigroup,
  "",
  "*Notes:*",
  "- Standard deviations in parentheses for continuous variables.",
  "- Baseline: present at survey and never migrated internationally.",
  "- Total Absent: all absent individuals (abroad + internal).",
  "- Intl. Absentee: absent and currently abroad.",
  "- National Absent: absent and inside Nepal.",
  "- Returnees: present at survey but was abroad for work ≥3 months."
)

writeLines(
  md_multigroup_full,
  file.path(output_path, "7.Multigroup_Summary.md")
)

# =============================================================================
# TABLE 8: SUMMARY BY BASELINE, TOTAL ABSENT, TREATMENT & CONTROL -------------
# =============================================================================

# STEP 1: COMPUTE STATS FOR EACH GROUP
stats_baseline  <- nlss_conflict_data %>% filter(baseline == 1)                        %>% compute_group_stats()
stats_absent    <- nlss_conflict_data %>% filter(absent == 1)                           %>% compute_group_stats()
stats_treatment <- nlss_conflict_data %>% filter(treatment_label == "Treatment")        %>% compute_group_stats()
stats_control   <- nlss_conflict_data %>% filter(treatment_label == "Control")          %>% compute_group_stats()

# Quick check
cat("=== Group Size Check ===\n")
cat("Baseline:   ", g(stats_baseline,  "N"), "\n")
cat("Absent:     ", g(stats_absent,    "N"), "\n")
cat("Treatment:  ", g(stats_treatment, "N"), "\n")
cat("Control:    ", g(stats_control,   "N"), "\n")


# STEP 2: ASSEMBLE TABLE
table_baseline_absent_tc <- data.frame(
  
  Variable = c(
    "Sample Size",
    "",
    "Age:",
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
  ),
  
  # ---- COLUMN 1: Baseline ----
  Baseline = c(
    as.character(g(stats_baseline, "N")),
    "", "",
    format_mean_sd(g(stats_baseline, "Age_Mean"),          g(stats_baseline, "Age_SD")),
    "",
    format_mean_sd(g(stats_baseline, "Age_Conflict_Mean"), g(stats_baseline, "Age_Conflict_SD")),
    "",
    as.character(g(stats_baseline, "Male_Pct")),
    "", "",
    as.character(g(stats_baseline, "No_Edu_Pct")),
    as.character(g(stats_baseline, "Primary_Pct")),
    as.character(g(stats_baseline, "Secondary_Pct")),
    as.character(g(stats_baseline, "Tertiary_Pct")),
    "", "",
    as.character(g(stats_baseline, "High_Caste_Pct")),
    as.character(g(stats_baseline, "Janajati_Pct")),
    as.character(g(stats_baseline, "Terai_Pct")),
    as.character(g(stats_baseline, "Dalit_Pct")),
    as.character(g(stats_baseline, "Muslim_Pct")),
    "", "",
    as.character(g(stats_baseline, "Agri_Pct")),
    as.character(g(stats_baseline, "HighSkill_Pct")),
    as.character(g(stats_baseline, "Service_Pct")),
    as.character(g(stats_baseline, "Craft_Pct")),
    as.character(g(stats_baseline, "Elementary_Pct")),
    as.character(g(stats_baseline, "Armed_Pct"))
  ),
  
  # ---- COLUMN 2: Total Absent ----
  Total_Absent = c(
    as.character(g(stats_absent, "N")),
    "", "",
    format_mean_sd(g(stats_absent, "Age_Mean"),          g(stats_absent, "Age_SD")),
    "",
    format_mean_sd(g(stats_absent, "Age_Conflict_Mean"), g(stats_absent, "Age_Conflict_SD")),
    "",
    as.character(g(stats_absent, "Male_Pct")),
    "", "",
    as.character(g(stats_absent, "No_Edu_Pct")),
    as.character(g(stats_absent, "Primary_Pct")),
    as.character(g(stats_absent, "Secondary_Pct")),
    as.character(g(stats_absent, "Tertiary_Pct")),
    "", "",
    as.character(g(stats_absent, "High_Caste_Pct")),
    as.character(g(stats_absent, "Janajati_Pct")),
    as.character(g(stats_absent, "Terai_Pct")),
    as.character(g(stats_absent, "Dalit_Pct")),
    as.character(g(stats_absent, "Muslim_Pct")),
    "", "",
    as.character(g(stats_absent, "Agri_Pct")),
    as.character(g(stats_absent, "HighSkill_Pct")),
    as.character(g(stats_absent, "Service_Pct")),
    as.character(g(stats_absent, "Craft_Pct")),
    as.character(g(stats_absent, "Elementary_Pct")),
    as.character(g(stats_absent, "Armed_Pct"))
  ),
  
  # ---- COLUMN 3: Treatment (Exposed) ----
  Treatment = c(
    as.character(g(stats_treatment, "N")),
    "", "",
    format_mean_sd(g(stats_treatment, "Age_Mean"),          g(stats_treatment, "Age_SD")),
    "",
    format_mean_sd(g(stats_treatment, "Age_Conflict_Mean"), g(stats_treatment, "Age_Conflict_SD")),
    "",
    as.character(g(stats_treatment, "Male_Pct")),
    "", "",
    as.character(g(stats_treatment, "No_Edu_Pct")),
    as.character(g(stats_treatment, "Primary_Pct")),
    as.character(g(stats_treatment, "Secondary_Pct")),
    as.character(g(stats_treatment, "Tertiary_Pct")),
    "", "",
    as.character(g(stats_treatment, "High_Caste_Pct")),
    as.character(g(stats_treatment, "Janajati_Pct")),
    as.character(g(stats_treatment, "Terai_Pct")),
    as.character(g(stats_treatment, "Dalit_Pct")),
    as.character(g(stats_treatment, "Muslim_Pct")),
    "", "",
    as.character(g(stats_treatment, "Agri_Pct")),
    as.character(g(stats_treatment, "HighSkill_Pct")),
    as.character(g(stats_treatment, "Service_Pct")),
    as.character(g(stats_treatment, "Craft_Pct")),
    as.character(g(stats_treatment, "Elementary_Pct")),
    as.character(g(stats_treatment, "Armed_Pct"))
  ),
  
  # ---- COLUMN 4: Control (Non-Exposed) ----
  Control = c(
    as.character(g(stats_control, "N")),
    "", "",
    format_mean_sd(g(stats_control, "Age_Mean"),          g(stats_control, "Age_SD")),
    "",
    format_mean_sd(g(stats_control, "Age_Conflict_Mean"), g(stats_control, "Age_Conflict_SD")),
    "",
    as.character(g(stats_control, "Male_Pct")),
    "", "",
    as.character(g(stats_control, "No_Edu_Pct")),
    as.character(g(stats_control, "Primary_Pct")),
    as.character(g(stats_control, "Secondary_Pct")),
    as.character(g(stats_control, "Tertiary_Pct")),
    "", "",
    as.character(g(stats_control, "High_Caste_Pct")),
    as.character(g(stats_control, "Janajati_Pct")),
    as.character(g(stats_control, "Terai_Pct")),
    as.character(g(stats_control, "Dalit_Pct")),
    as.character(g(stats_control, "Muslim_Pct")),
    "", "",
    as.character(g(stats_control, "Agri_Pct")),
    as.character(g(stats_control, "HighSkill_Pct")),
    as.character(g(stats_control, "Service_Pct")),
    as.character(g(stats_control, "Craft_Pct")),
    as.character(g(stats_control, "Elementary_Pct")),
    as.character(g(stats_control, "Armed_Pct"))
  ),
  
  stringsAsFactors = FALSE
)


# STEP 3: EXPORT LaTeX
latex_baseline_absent_tc <- kable(
  table_baseline_absent_tc,
  format    = "latex",
  booktabs  = TRUE,
  caption   = "Covariate Summary: Baseline, Total Absent, Treatment and Control",
  label     = "tab:baseline_absent_tc",
  col.names = c("Variable", "Baseline", "Total Absent", "Treatment", "Control"),
  escape    = FALSE,
  align     = c("l", "r", "r", "r", "r")
) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size = 9
  ) %>%
  footnote(
    general = paste(
      "Standard deviations in parentheses for continuous variables.",
      "Baseline = present at survey and never migrated internationally.",
      "Total Absent = all absent individuals (abroad + internal).",
      "Treatment = individuals aged 0-17 at conflict start (1996).",
      "Control = individuals aged 18-40 at conflict start (1996)."
    ),
    footnote_as_chunk = TRUE
  )

writeLines(
  as.character(latex_baseline_absent_tc),
  file.path(output_path, "8.Baseline_Absent_Treatment_Control.tex")
)

# STEP 4: EXPORT Markdown
md_baseline_absent_tc <- kable(
  table_baseline_absent_tc,
  format    = "markdown",
  col.names = c("Variable", "Baseline", "Total Absent", "Treatment", "Control"),
  align     = c("l", "r", "r", "r", "r")
)

md_baseline_absent_tc_full <- c(
  md_baseline_absent_tc,
  "",
  "*Notes:*",
  "- Standard deviations in parentheses for continuous variables.",
  "- Baseline: present at survey and never migrated internationally.",
  "- Total Absent: all absent individuals (abroad + internal).",
  "- Treatment: individuals aged 0-17 at conflict start (1996).",
  "- Control: individuals aged 18-40 at conflict start (1996)."
)

writeLines(
  md_baseline_absent_tc_full,
  file.path(output_path, "8.Baseline_Absent_Treatment_Control.md")
)

# =============================================================================
# TABLE 9: SUMMARY BY BASELINE, NATIONAL ABSENT, TREATMENT & CONTROL -------------
# =============================================================================

# STEP 1: ASSEMBLE TABLE
table_baseline_intl_tc <- data.frame(
  
  Variable = c(
    "Sample Size",
    "",
    "Age:",
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
  ),
  
  # ---- COLUMN 1: Baseline ----
  Baseline = c(
    as.character(g(stats_baseline, "N")),
    "", "",
    format_mean_sd(g(stats_baseline, "Age_Mean"),          g(stats_baseline, "Age_SD")),
    "",
    format_mean_sd(g(stats_baseline, "Age_Conflict_Mean"), g(stats_baseline, "Age_Conflict_SD")),
    "",
    as.character(g(stats_baseline, "Male_Pct")),
    "", "",
    as.character(g(stats_baseline, "No_Edu_Pct")),
    as.character(g(stats_baseline, "Primary_Pct")),
    as.character(g(stats_baseline, "Secondary_Pct")),
    as.character(g(stats_baseline, "Tertiary_Pct")),
    "", "",
    as.character(g(stats_baseline, "High_Caste_Pct")),
    as.character(g(stats_baseline, "Janajati_Pct")),
    as.character(g(stats_baseline, "Terai_Pct")),
    as.character(g(stats_baseline, "Dalit_Pct")),
    as.character(g(stats_baseline, "Muslim_Pct")),
    "", "",
    as.character(g(stats_baseline, "Agri_Pct")),
    as.character(g(stats_baseline, "HighSkill_Pct")),
    as.character(g(stats_baseline, "Service_Pct")),
    as.character(g(stats_baseline, "Craft_Pct")),
    as.character(g(stats_baseline, "Elementary_Pct")),
    as.character(g(stats_baseline, "Armed_Pct"))
  ),
  
  # ---- COLUMN 2: International Absentee ----
  Intl_Absentee = c(
    as.character(g(stats_intl, "N")),
    "", "",
    format_mean_sd(g(stats_intl, "Age_Mean"),          g(stats_intl, "Age_SD")),
    "",
    format_mean_sd(g(stats_intl, "Age_Conflict_Mean"), g(stats_intl, "Age_Conflict_SD")),
    "",
    as.character(g(stats_intl, "Male_Pct")),
    "", "",
    as.character(g(stats_intl, "No_Edu_Pct")),
    as.character(g(stats_intl, "Primary_Pct")),
    as.character(g(stats_intl, "Secondary_Pct")),
    as.character(g(stats_intl, "Tertiary_Pct")),
    "", "",
    as.character(g(stats_intl, "High_Caste_Pct")),
    as.character(g(stats_intl, "Janajati_Pct")),
    as.character(g(stats_intl, "Terai_Pct")),
    as.character(g(stats_intl, "Dalit_Pct")),
    as.character(g(stats_intl, "Muslim_Pct")),
    "", "",
    as.character(g(stats_intl, "Agri_Pct")),
    as.character(g(stats_intl, "HighSkill_Pct")),
    as.character(g(stats_intl, "Service_Pct")),
    as.character(g(stats_intl, "Craft_Pct")),
    as.character(g(stats_intl, "Elementary_Pct")),
    as.character(g(stats_intl, "Armed_Pct"))
  ),
  
  # ---- COLUMN 3: Treatment (Exposed) ----
  Treatment = c(
    as.character(g(stats_treatment, "N")),
    "", "",
    format_mean_sd(g(stats_treatment, "Age_Mean"),          g(stats_treatment, "Age_SD")),
    "",
    format_mean_sd(g(stats_treatment, "Age_Conflict_Mean"), g(stats_treatment, "Age_Conflict_SD")),
    "",
    as.character(g(stats_treatment, "Male_Pct")),
    "", "",
    as.character(g(stats_treatment, "No_Edu_Pct")),
    as.character(g(stats_treatment, "Primary_Pct")),
    as.character(g(stats_treatment, "Secondary_Pct")),
    as.character(g(stats_treatment, "Tertiary_Pct")),
    "", "",
    as.character(g(stats_treatment, "High_Caste_Pct")),
    as.character(g(stats_treatment, "Janajati_Pct")),
    as.character(g(stats_treatment, "Terai_Pct")),
    as.character(g(stats_treatment, "Dalit_Pct")),
    as.character(g(stats_treatment, "Muslim_Pct")),
    "", "",
    as.character(g(stats_treatment, "Agri_Pct")),
    as.character(g(stats_treatment, "HighSkill_Pct")),
    as.character(g(stats_treatment, "Service_Pct")),
    as.character(g(stats_treatment, "Craft_Pct")),
    as.character(g(stats_treatment, "Elementary_Pct")),
    as.character(g(stats_treatment, "Armed_Pct"))
  ),
  
  # ---- COLUMN 4: Control (Non-Exposed) ----
  Control = c(
    as.character(g(stats_control, "N")),
    "", "",
    format_mean_sd(g(stats_control, "Age_Mean"),          g(stats_control, "Age_SD")),
    "",
    format_mean_sd(g(stats_control, "Age_Conflict_Mean"), g(stats_control, "Age_Conflict_SD")),
    "",
    as.character(g(stats_control, "Male_Pct")),
    "", "",
    as.character(g(stats_control, "No_Edu_Pct")),
    as.character(g(stats_control, "Primary_Pct")),
    as.character(g(stats_control, "Secondary_Pct")),
    as.character(g(stats_control, "Tertiary_Pct")),
    "", "",
    as.character(g(stats_control, "High_Caste_Pct")),
    as.character(g(stats_control, "Janajati_Pct")),
    as.character(g(stats_control, "Terai_Pct")),
    as.character(g(stats_control, "Dalit_Pct")),
    as.character(g(stats_control, "Muslim_Pct")),
    "", "",
    as.character(g(stats_control, "Agri_Pct")),
    as.character(g(stats_control, "HighSkill_Pct")),
    as.character(g(stats_control, "Service_Pct")),
    as.character(g(stats_control, "Craft_Pct")),
    as.character(g(stats_control, "Elementary_Pct")),
    as.character(g(stats_control, "Armed_Pct"))
  ),
  
  stringsAsFactors = FALSE
)


# STEP 2: EXPORT LaTeX
latex_baseline_intl_tc <- kable(
  table_baseline_intl_tc,
  format    = "latex",
  booktabs  = TRUE,
  caption   = "Covariate Summary: Baseline, International Absentee, Treatment and Control",
  label     = "tab:baseline_intl_tc",
  col.names = c("Variable", "Baseline", "Intl. Absentee", "Treatment", "Control"),
  escape    = FALSE,
  align     = c("l", "r", "r", "r", "r")
) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size = 9
  ) %>%
  footnote(
    general = paste(
      "Standard deviations in parentheses for continuous variables.",
      "Baseline = present at survey and never migrated internationally.",
      "Intl. Absentee = absent and currently abroad.",
      "Treatment = individuals aged 0-17 at conflict start (1996).",
      "Control = individuals aged 18-40 at conflict start (1996)."
    ),
    footnote_as_chunk = TRUE
  )

writeLines(
  as.character(latex_baseline_intl_tc),
  file.path(output_path, "9.Baseline_Intl_Absentee_Treatment_Control.tex")
)


# STEP 3: EXPORT Markdown
md_baseline_intl_tc <- kable(
  table_baseline_intl_tc,
  format    = "markdown",
  col.names = c("Variable", "Baseline", "Intl. Absentee", "Treatment", "Control"),
  align     = c("l", "r", "r", "r", "r")
)

md_baseline_intl_tc_full <- c(
  md_baseline_intl_tc,
  "",
  "*Notes:*",
  "- Standard deviations in parentheses for continuous variables.",
  "- Baseline: present at survey and never migrated internationally.",
  "- Intl. Absentee: absent and currently abroad.",
  "- Treatment: individuals aged 0-17 at conflict start (1996).",
  "- Control: individuals aged 18-40 at conflict start (1996)."
)

writeLines(
  md_baseline_intl_tc_full,
  file.path(output_path, "9.Baseline_Intl_Absentee_Treatment_Control.md")
)



# =============================================================================
# TABLE 10: SUMMARY BY BASELINE, NATIONAL ABSENT, TREATMENT & CONTROL -------------
# =============================================================================

# STEP 1: ASSEMBLE TABLE
table_baseline_national_tc <- data.frame(
  
  Variable = c(
    "Sample Size",
    "",
    "Age:",
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
  ),
  
  # ---- COLUMN 1: Baseline ----
  Baseline = c(
    as.character(g(stats_baseline, "N")),
    "", "",
    format_mean_sd(g(stats_baseline, "Age_Mean"),          g(stats_baseline, "Age_SD")),
    "",
    format_mean_sd(g(stats_baseline, "Age_Conflict_Mean"), g(stats_baseline, "Age_Conflict_SD")),
    "",
    as.character(g(stats_baseline, "Male_Pct")),
    "", "",
    as.character(g(stats_baseline, "No_Edu_Pct")),
    as.character(g(stats_baseline, "Primary_Pct")),
    as.character(g(stats_baseline, "Secondary_Pct")),
    as.character(g(stats_baseline, "Tertiary_Pct")),
    "", "",
    as.character(g(stats_baseline, "High_Caste_Pct")),
    as.character(g(stats_baseline, "Janajati_Pct")),
    as.character(g(stats_baseline, "Terai_Pct")),
    as.character(g(stats_baseline, "Dalit_Pct")),
    as.character(g(stats_baseline, "Muslim_Pct")),
    "", "",
    as.character(g(stats_baseline, "Agri_Pct")),
    as.character(g(stats_baseline, "HighSkill_Pct")),
    as.character(g(stats_baseline, "Service_Pct")),
    as.character(g(stats_baseline, "Craft_Pct")),
    as.character(g(stats_baseline, "Elementary_Pct")),
    as.character(g(stats_baseline, "Armed_Pct"))
  ),
  
  # ---- COLUMN 2: National Absent (Internal) ----
  National_Absent = c(
    as.character(g(stats_national, "N")),
    "", "",
    format_mean_sd(g(stats_national, "Age_Mean"),          g(stats_national, "Age_SD")),
    "",
    format_mean_sd(g(stats_national, "Age_Conflict_Mean"), g(stats_national, "Age_Conflict_SD")),
    "",
    as.character(g(stats_national, "Male_Pct")),
    "", "",
    as.character(g(stats_national, "No_Edu_Pct")),
    as.character(g(stats_national, "Primary_Pct")),
    as.character(g(stats_national, "Secondary_Pct")),
    as.character(g(stats_national, "Tertiary_Pct")),
    "", "",
    as.character(g(stats_national, "High_Caste_Pct")),
    as.character(g(stats_national, "Janajati_Pct")),
    as.character(g(stats_national, "Terai_Pct")),
    as.character(g(stats_national, "Dalit_Pct")),
    as.character(g(stats_national, "Muslim_Pct")),
    "", "",
    as.character(g(stats_national, "Agri_Pct")),
    as.character(g(stats_national, "HighSkill_Pct")),
    as.character(g(stats_national, "Service_Pct")),
    as.character(g(stats_national, "Craft_Pct")),
    as.character(g(stats_national, "Elementary_Pct")),
    as.character(g(stats_national, "Armed_Pct"))
  ),
  
  # ---- COLUMN 3: Treatment (Exposed) ----
  Treatment = c(
    as.character(g(stats_treatment, "N")),
    "", "",
    format_mean_sd(g(stats_treatment, "Age_Mean"),          g(stats_treatment, "Age_SD")),
    "",
    format_mean_sd(g(stats_treatment, "Age_Conflict_Mean"), g(stats_treatment, "Age_Conflict_SD")),
    "",
    as.character(g(stats_treatment, "Male_Pct")),
    "", "",
    as.character(g(stats_treatment, "No_Edu_Pct")),
    as.character(g(stats_treatment, "Primary_Pct")),
    as.character(g(stats_treatment, "Secondary_Pct")),
    as.character(g(stats_treatment, "Tertiary_Pct")),
    "", "",
    as.character(g(stats_treatment, "High_Caste_Pct")),
    as.character(g(stats_treatment, "Janajati_Pct")),
    as.character(g(stats_treatment, "Terai_Pct")),
    as.character(g(stats_treatment, "Dalit_Pct")),
    as.character(g(stats_treatment, "Muslim_Pct")),
    "", "",
    as.character(g(stats_treatment, "Agri_Pct")),
    as.character(g(stats_treatment, "HighSkill_Pct")),
    as.character(g(stats_treatment, "Service_Pct")),
    as.character(g(stats_treatment, "Craft_Pct")),
    as.character(g(stats_treatment, "Elementary_Pct")),
    as.character(g(stats_treatment, "Armed_Pct"))
  ),
  
  # ---- COLUMN 4: Control (Non-Exposed) ----
  Control = c(
    as.character(g(stats_control, "N")),
    "", "",
    format_mean_sd(g(stats_control, "Age_Mean"),          g(stats_control, "Age_SD")),
    "",
    format_mean_sd(g(stats_control, "Age_Conflict_Mean"), g(stats_control, "Age_Conflict_SD")),
    "",
    as.character(g(stats_control, "Male_Pct")),
    "", "",
    as.character(g(stats_control, "No_Edu_Pct")),
    as.character(g(stats_control, "Primary_Pct")),
    as.character(g(stats_control, "Secondary_Pct")),
    as.character(g(stats_control, "Tertiary_Pct")),
    "", "",
    as.character(g(stats_control, "High_Caste_Pct")),
    as.character(g(stats_control, "Janajati_Pct")),
    as.character(g(stats_control, "Terai_Pct")),
    as.character(g(stats_control, "Dalit_Pct")),
    as.character(g(stats_control, "Muslim_Pct")),
    "", "",
    as.character(g(stats_control, "Agri_Pct")),
    as.character(g(stats_control, "HighSkill_Pct")),
    as.character(g(stats_control, "Service_Pct")),
    as.character(g(stats_control, "Craft_Pct")),
    as.character(g(stats_control, "Elementary_Pct")),
    as.character(g(stats_control, "Armed_Pct"))
  ),
  
  stringsAsFactors = FALSE
)


# STEP 2: EXPORT LaTeX
latex_baseline_national_tc <- kable(
  table_baseline_national_tc,
  format    = "latex",
  booktabs  = TRUE,
  caption   = "Covariate Summary: Baseline, National Absent, Treatment and Control",
  label     = "tab:baseline_national_tc",
  col.names = c("Variable", "Baseline", "National Absent", "Treatment", "Control"),
  escape    = FALSE,
  align     = c("l", "r", "r", "r", "r")
) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size = 9
  ) %>%
  footnote(
    general = paste(
      "Standard deviations in parentheses for continuous variables.",
      "Baseline = present at survey and never migrated internationally.",
      "National Absent = absent and inside Nepal.",
      "Treatment = individuals aged 0-17 at conflict start (1996).",
      "Control = individuals aged 18-40 at conflict start (1996)."
    ),
    footnote_as_chunk = TRUE
  )

writeLines(
  as.character(latex_baseline_national_tc),
  file.path(output_path, "10.Baseline_National_Absent_Treatment_Control.tex")
)


# STEP 3: EXPORT Markdown
md_baseline_national_tc <- kable(
  table_baseline_national_tc,
  format    = "markdown",
  col.names = c("Variable", "Baseline", "National Absent", "Treatment", "Control"),
  align     = c("l", "r", "r", "r", "r")
)

md_baseline_national_tc_full <- c(
  md_baseline_national_tc,
  "",
  "*Notes:*"
)

writeLines(
  md_baseline_national_tc_full,
  file.path(output_path, "10.Baseline_National_Absent_Treatment_Control.md")
)


stop()
# =============================================================================
# TABLE 11: SUMMARY BY TREATMENT/CONTROL----------------------------------------
# =============================================================================

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

writeLines(as.character(latex_table2), file.path(output_path, "2.Summary_Treatment_Control.tex"))

html_table2 <- kable(table2_formatted, format = "html",
                     col.names = c("Variable", "Control", "Treatment"),
                     caption = "Summary Statistics by Treatment Status") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
html_table2 %>% save_kable(file.path(output_path, "2.Summary_Treatment_Control.png"))


# PART 3: SUMMARY BY ALL COHORTS =================================

# TABLE 3: SUMMARY BY COHORT ==============================================

table3_data <- nlss_conflict_data %>%
  filter(!is.na(cohort_group)) %>%
  group_by(cohort_group) %>%
  summarise(
    N = n(),
    
    # Outcomes
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
    Age_2017_Mean = round(mean(age, na.rm = TRUE), 2),
    Age_2017_SD = round(sd(age, na.rm = TRUE), 2),
    
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
  ) %>%
  arrange(desc(cohort_group))  # This will order from Treatment to Control

# Filter to only Treatment and Control cohorts (exclude "Excluded" groups)
table3_data <- table3_data %>%
  filter(grepl("Treatment|Control", cohort_group))

# Check the order and cohorts
print(table3_data$cohort_group)

# Function to format mean (SD)
format_mean_sd <- function(mean_val, sd_val) {
  paste0(mean_val, "\n(", sd_val, ")")
}

# Create formatted table with proper cohort ordering
# Assuming your cohorts are named like: "Treatment: Age 0-5 in 1996", etc.

# Get data for each cohort (adjust indices based on your actual data)
get_cohort_data <- function(cohort_name) {
  idx <- which(table3_data$cohort_group == cohort_name)
  if (length(idx) == 0) return(rep("", 28))  # Return empty if cohort not found
  
  c(
    as.character(table3_data$N[idx]),
    "",
    "",
    format_mean_sd(table3_data$Int_Migrant_Mean[idx], table3_data$Int_Migrant_SD[idx]),
    format_mean_sd(table3_data$Absentee_Mean[idx], table3_data$Absentee_SD[idx]),
    format_mean_sd(table3_data$Return_Mean[idx], table3_data$Return_SD[idx]),
    "",
    "",
    format_mean_sd(table3_data$Months_War_Mean[idx], table3_data$Months_War_SD[idx]),
    format_mean_sd(table3_data$Casualties_Mean[idx], table3_data$Casualties_SD[idx]),
    "",
    "",
    format_mean_sd(table3_data$Age_2017_Mean[idx], table3_data$Age_2017_SD[idx]),
    format_mean_sd(table3_data$Age_Conflict_Mean[idx], table3_data$Age_Conflict_SD[idx]),
    as.character(table3_data$Male_Pct[idx]),
    "",
    "",
    as.character(table3_data$No_Edu_Pct[idx]),
    as.character(table3_data$Primary_Pct[idx]),
    as.character(table3_data$Secondary_Pct[idx]),
    as.character(table3_data$Tertiary_Pct[idx]),
    "",
    "",
    as.character(table3_data$High_Caste_Pct[idx]),
    as.character(table3_data$Janajati_Pct[idx]),
    as.character(table3_data$Terai_Pct[idx]),
    as.character(table3_data$Dalit_Pct[idx]),
    as.character(table3_data$Muslim_Pct[idx])
  )
}

# Create the formatted table
table3_formatted <- data.frame(
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
  stringsAsFactors = FALSE
)

# Add columns for each cohort
# You need to match the exact cohort names from your data
table3_formatted$T1_Age0_5 <- get_cohort_data("Treatment: Age 0-5 in 1996")
table3_formatted$T2_Age6_12 <- get_cohort_data("Treatment: Age 6-12 in 1996")
table3_formatted$T3_Age13_17 <- get_cohort_data("Treatment: Age 13-17 in 1996")
#table3_formatted$C1_Age18_25 <- get_cohort_data("Control: Age 18-25 in 1996")
table3_formatted$C2_Age26_35 <- get_cohort_data("Control: Age 26-35 in 1996")
table3_formatted$C3_Age36_40 <- get_cohort_data("Control: Age 36-40 in 1996")


# Create LaTeX version
latex_table3 <- kable(table3_formatted,
                      format = "latex",
                      booktabs = TRUE,
                      caption = "Summary Statistics by Cohort",
                      label = "tab:cohort_summary",
                      col.names = c("Variable", 
                                    "T1\n(0-5)", 
                                    "T2\n(6-12)", 
                                    "T3\n(13-17)", 
                                    "C1\n(18-25)", 
                                    "C2\n(26-35)"),
                      escape = FALSE,
                      align = c("l", "r", "r", "r", "r", "r")) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"),
                font_size = 8) %>%
  footnote(general = "Standard deviations in parentheses. T = Treatment cohort (children during conflict), C = Control cohort (adults during conflict).",
           footnote_as_chunk = TRUE)

writeLines(as.character(latex_table3),file.path(output_path, "3.Summary_by_Cohort.tex"))

# HTML/PNG version
html_table3 <- kable(table3_formatted,
                     format = "html",
                     caption = "Summary Statistics by Cohort",
                     col.names = c("Variable", 
                                   "T1\n(0-5)", 
                                   "T2\n(6-12)", 
                                   "T3\n(13-17)", 
                                   "C1\n(18-25)", 
                                   "C2\n(26-35)"),
                     escape = FALSE,
                     align = c("l", "r", "r", "r", "r", "r")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE) %>%
  footnote(general = "Standard deviations in parentheses. T = Treatment cohort (children during conflict), C = Control cohort (adults during conflict).",
           footnote_as_chunk = TRUE)

# Save as PNG
save_kable(html_table3, file.path(output_path, "3.Summary_by_Cohort.png"))

# PART 4: SUMMARY BY CONFLICT INTENSITY (EXPANDED) ============================


# TABLE 4: SUMMARY BY CONFLICT INTENSITY (WAR AND CASUALTY) 

# Prepare data for both war-based and casualty-based conflict measures
table4_data_war <- nlss_conflict_data %>%
  filter(!is.na(high_conflict_q3_label)) %>%
  group_by(high_conflict_q3_label) %>%
  summarise(
    N = n(),
    
    # Outcomes
    Int_Migrant_Mean = round(mean(international_migrant == 1, na.rm = TRUE) * 100, 2),
    Int_Migrant_SD = round(sd(international_migrant, na.rm = TRUE) * 100, 2),
    
    Absentee_Mean = round(mean(international_absentee_only == 1, na.rm = TRUE) * 100, 2),
    Absentee_SD = round(sd(international_absentee_only, na.rm = TRUE) * 100, 2),
    
    Return_Mean = round(mean(present_ind_migrant == 1, na.rm = TRUE) * 100, 2),
    Return_SD = round(sd(present_ind_migrant, na.rm = TRUE) * 100, 2),
    
    # Conflict measures
    Months_War_Mean = round(mean(mwar_own_any, na.rm = TRUE), 2),
    Months_War_SD = round(sd(mwar_own_any, na.rm = TRUE), 2),
    
    Casualties_Mean = round(mean(cas_own_any, na.rm = TRUE), 2),
    Casualties_SD = round(sd(cas_own_any, na.rm = TRUE), 2),
    
    # Treatment distribution
    Pct_Treatment = round(mean(treatment == 1, na.rm = TRUE) * 100, 2),
    
    # Demographics
    Age_Mean = round(mean(age, na.rm = TRUE), 2),
    Age_SD = round(sd(age, na.rm = TRUE), 2),
    
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

# Same for casualty-based
table4_data_casualty <- nlss_conflict_data %>%
  filter(!is.na(high_conflict_casualty_label)) %>%
  group_by(high_conflict_casualty_label) %>%
  summarise(
    N = n(),
    
    # Outcomes
    Int_Migrant_Mean = round(mean(international_migrant == 1, na.rm = TRUE) * 100, 2),
    Int_Migrant_SD = round(sd(international_migrant, na.rm = TRUE) * 100, 2),
    
    Absentee_Mean = round(mean(international_absentee_only == 1, na.rm = TRUE) * 100, 2),
    Absentee_SD = round(sd(international_absentee_only, na.rm = TRUE) * 100, 2),
    
    Return_Mean = round(mean(present_ind_migrant == 1, na.rm = TRUE) * 100, 2),
    Return_SD = round(sd(present_ind_migrant, na.rm = TRUE) * 100, 2),
    
    # Conflict measures
    Months_War_Mean = round(mean(mwar_own_any, na.rm = TRUE), 2),
    Months_War_SD = round(sd(mwar_own_any, na.rm = TRUE), 2),
    
    Casualties_Mean = round(mean(cas_own_any, na.rm = TRUE), 2),
    Casualties_SD = round(sd(cas_own_any, na.rm = TRUE), 2),
    
    # Treatment distribution
    Pct_Treatment = round(mean(treatment == 1, na.rm = TRUE) * 100, 2),
    
    # Demographics
    Age_Mean = round(mean(age, na.rm = TRUE), 2),
    Age_SD = round(sd(age, na.rm = TRUE), 2),
    
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

# Function to format mean (SD)
format_mean_sd <- function(mean_val, sd_val) {
  paste0(mean_val, "\n(", sd_val, ")")
}

# Helper function to get data safely
get_conflict_data <- function(data, label, type = "mean_sd") {
  idx <- which(data[[1]] == label)
  if (length(idx) == 0) return("")
  
  if (type == "mean_sd") {
    return(format_mean_sd(data$Int_Migrant_Mean[idx], data$Int_Migrant_SD[idx]))
  } else {
    return(as.character(data[[type]][idx]))
  }
}

# Create formatted table with 4 columns
table4_formatted <- data.frame(
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
    "Treatment Distribution:",
    "  Treatment Cohort (%)",
    "",
    "Demographics:",
    "  Age in 2017",
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
  
  War_Low = c(
    as.character(table4_data_war$N[table4_data_war$high_conflict_q3_label == "Low Conflict"]),
    "",
    "",
    format_mean_sd(table4_data_war$Int_Migrant_Mean[table4_data_war$high_conflict_q3_label == "Low Conflict"],
                   table4_data_war$Int_Migrant_SD[table4_data_war$high_conflict_q3_label == "Low Conflict"]),
    format_mean_sd(table4_data_war$Absentee_Mean[table4_data_war$high_conflict_q3_label == "Low Conflict"],
                   table4_data_war$Absentee_SD[table4_data_war$high_conflict_q3_label == "Low Conflict"]),
    format_mean_sd(table4_data_war$Return_Mean[table4_data_war$high_conflict_q3_label == "Low Conflict"],
                   table4_data_war$Return_SD[table4_data_war$high_conflict_q3_label == "Low Conflict"]),
    "",
    "",
    format_mean_sd(table4_data_war$Months_War_Mean[table4_data_war$high_conflict_q3_label == "Low Conflict"],
                   table4_data_war$Months_War_SD[table4_data_war$high_conflict_q3_label == "Low Conflict"]),
    format_mean_sd(table4_data_war$Casualties_Mean[table4_data_war$high_conflict_q3_label == "Low Conflict"],
                   table4_data_war$Casualties_SD[table4_data_war$high_conflict_q3_label == "Low Conflict"]),
    "",
    "",
    as.character(table4_data_war$Pct_Treatment[table4_data_war$high_conflict_q3_label == "Low Conflict"]),
    "",
    "",
    format_mean_sd(table4_data_war$Age_Mean[table4_data_war$high_conflict_q3_label == "Low Conflict"],
                   table4_data_war$Age_SD[table4_data_war$high_conflict_q3_label == "Low Conflict"]),
    as.character(table4_data_war$Male_Pct[table4_data_war$high_conflict_q3_label == "Low Conflict"]),
    "",
    "",
    as.character(table4_data_war$No_Edu_Pct[table4_data_war$high_conflict_q3_label == "Low Conflict"]),
    as.character(table4_data_war$Primary_Pct[table4_data_war$high_conflict_q3_label == "Low Conflict"]),
    as.character(table4_data_war$Secondary_Pct[table4_data_war$high_conflict_q3_label == "Low Conflict"]),
    as.character(table4_data_war$Tertiary_Pct[table4_data_war$high_conflict_q3_label == "Low Conflict"]),
    "",
    "",
    as.character(table4_data_war$High_Caste_Pct[table4_data_war$high_conflict_q3_label == "Low Conflict"]),
    as.character(table4_data_war$Janajati_Pct[table4_data_war$high_conflict_q3_label == "Low Conflict"]),
    as.character(table4_data_war$Terai_Pct[table4_data_war$high_conflict_q3_label == "Low Conflict"]),
    as.character(table4_data_war$Dalit_Pct[table4_data_war$high_conflict_q3_label == "Low Conflict"]),
    as.character(table4_data_war$Muslim_Pct[table4_data_war$high_conflict_q3_label == "Low Conflict"])
  ),
  
  War_High = c(
    as.character(table4_data_war$N[table4_data_war$high_conflict_q3_label == "High Conflict"]),
    "",
    "",
    format_mean_sd(table4_data_war$Int_Migrant_Mean[table4_data_war$high_conflict_q3_label == "High Conflict"],
                   table4_data_war$Int_Migrant_SD[table4_data_war$high_conflict_q3_label == "High Conflict"]),
    format_mean_sd(table4_data_war$Absentee_Mean[table4_data_war$high_conflict_q3_label == "High Conflict"],
                   table4_data_war$Absentee_SD[table4_data_war$high_conflict_q3_label == "High Conflict"]),
    format_mean_sd(table4_data_war$Return_Mean[table4_data_war$high_conflict_q3_label == "High Conflict"],
                   table4_data_war$Return_SD[table4_data_war$high_conflict_q3_label == "High Conflict"]),
    "",
    "",
    format_mean_sd(table4_data_war$Months_War_Mean[table4_data_war$high_conflict_q3_label == "High Conflict"],
                   table4_data_war$Months_War_SD[table4_data_war$high_conflict_q3_label == "High Conflict"]),
    format_mean_sd(table4_data_war$Casualties_Mean[table4_data_war$high_conflict_q3_label == "High Conflict"],
                   table4_data_war$Casualties_SD[table4_data_war$high_conflict_q3_label == "High Conflict"]),
    "",
    "",
    as.character(table4_data_war$Pct_Treatment[table4_data_war$high_conflict_q3_label == "High Conflict"]),
    "",
    "",
    format_mean_sd(table4_data_war$Age_Mean[table4_data_war$high_conflict_q3_label == "High Conflict"],
                   table4_data_war$Age_SD[table4_data_war$high_conflict_q3_label == "High Conflict"]),
    as.character(table4_data_war$Male_Pct[table4_data_war$high_conflict_q3_label == "High Conflict"]),
    "",
    "",
    as.character(table4_data_war$No_Edu_Pct[table4_data_war$high_conflict_q3_label == "High Conflict"]),
    as.character(table4_data_war$Primary_Pct[table4_data_war$high_conflict_q3_label == "High Conflict"]),
    as.character(table4_data_war$Secondary_Pct[table4_data_war$high_conflict_q3_label == "High Conflict"]),
    as.character(table4_data_war$Tertiary_Pct[table4_data_war$high_conflict_q3_label == "High Conflict"]),
    "",
    "",
    as.character(table4_data_war$High_Caste_Pct[table4_data_war$high_conflict_q3_label == "High Conflict"]),
    as.character(table4_data_war$Janajati_Pct[table4_data_war$high_conflict_q3_label == "High Conflict"]),
    as.character(table4_data_war$Terai_Pct[table4_data_war$high_conflict_q3_label == "High Conflict"]),
    as.character(table4_data_war$Dalit_Pct[table4_data_war$high_conflict_q3_label == "High Conflict"]),
    as.character(table4_data_war$Muslim_Pct[table4_data_war$high_conflict_q3_label == "High Conflict"])
  ),
  
  Casualty_Low = c(
    as.character(table4_data_casualty$N[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"]),
    "",
    "",
    format_mean_sd(table4_data_casualty$Int_Migrant_Mean[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"],
                   table4_data_casualty$Int_Migrant_SD[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"]),
    format_mean_sd(table4_data_casualty$Absentee_Mean[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"],
                   table4_data_casualty$Absentee_SD[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"]),
    format_mean_sd(table4_data_casualty$Return_Mean[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"],
                   table4_data_casualty$Return_SD[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"]),
    "",
    "",
    format_mean_sd(table4_data_casualty$Months_War_Mean[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"],
                   table4_data_casualty$Months_War_SD[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"]),
    format_mean_sd(table4_data_casualty$Casualties_Mean[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"],
                   table4_data_casualty$Casualties_SD[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"]),
    "",
    "",
    as.character(table4_data_casualty$Pct_Treatment[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"]),
    "",
    "",
    format_mean_sd(table4_data_casualty$Age_Mean[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"],
                   table4_data_casualty$Age_SD[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"]),
    as.character(table4_data_casualty$Male_Pct[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"]),
    "",
    "",
    as.character(table4_data_casualty$No_Edu_Pct[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"]),
    as.character(table4_data_casualty$Primary_Pct[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"]),
    as.character(table4_data_casualty$Secondary_Pct[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"]),
    as.character(table4_data_casualty$Tertiary_Pct[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"]),
    "",
    "",
    as.character(table4_data_casualty$High_Caste_Pct[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"]),
    as.character(table4_data_casualty$Janajati_Pct[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"]),
    as.character(table4_data_casualty$Terai_Pct[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"]),
    as.character(table4_data_casualty$Dalit_Pct[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"]),
    as.character(table4_data_casualty$Muslim_Pct[table4_data_casualty$high_conflict_casualty_label == "Low Conflict"])
  ),
  
  Casualty_High = c(
    as.character(table4_data_casualty$N[table4_data_casualty$high_conflict_casualty_label == "High Conflict"]),
    "",
    "",
    format_mean_sd(table4_data_casualty$Int_Migrant_Mean[table4_data_casualty$high_conflict_casualty_label == "High Conflict"],
                   table4_data_casualty$Int_Migrant_SD[table4_data_casualty$high_conflict_casualty_label == "High Conflict"]),
    format_mean_sd(table4_data_casualty$Absentee_Mean[table4_data_casualty$high_conflict_casualty_label == "High Conflict"],
                   table4_data_casualty$Absentee_SD[table4_data_casualty$high_conflict_casualty_label == "High Conflict"]),
    format_mean_sd(table4_data_casualty$Return_Mean[table4_data_casualty$high_conflict_casualty_label == "High Conflict"],
                   table4_data_casualty$Return_SD[table4_data_casualty$high_conflict_casualty_label == "High Conflict"]),
    "",
    "",
    format_mean_sd(table4_data_casualty$Months_War_Mean[table4_data_casualty$high_conflict_casualty_label == "High Conflict"],
                   table4_data_casualty$Months_War_SD[table4_data_casualty$high_conflict_casualty_label == "High Conflict"]),
    format_mean_sd(table4_data_casualty$Casualties_Mean[table4_data_casualty$high_conflict_casualty_label == "High Conflict"],
                   table4_data_casualty$Casualties_SD[table4_data_casualty$high_conflict_casualty_label == "High Conflict"]),
    "",
    "",
    as.character(table4_data_casualty$Pct_Treatment[table4_data_casualty$high_conflict_casualty_label == "High Conflict"]),
    "",
    "",
    format_mean_sd(table4_data_casualty$Age_Mean[table4_data_casualty$high_conflict_casualty_label == "High Conflict"],
                   table4_data_casualty$Age_SD[table4_data_casualty$high_conflict_casualty_label == "High Conflict"]),
    as.character(table4_data_casualty$Male_Pct[table4_data_casualty$high_conflict_casualty_label == "High Conflict"]),
    "",
    "",
    as.character(table4_data_casualty$No_Edu_Pct[table4_data_casualty$high_conflict_casualty_label == "High Conflict"]),
    as.character(table4_data_casualty$Primary_Pct[table4_data_casualty$high_conflict_casualty_label == "High Conflict"]),
    as.character(table4_data_casualty$Secondary_Pct[table4_data_casualty$high_conflict_casualty_label == "High Conflict"]),
    as.character(table4_data_casualty$Tertiary_Pct[table4_data_casualty$high_conflict_casualty_label == "High Conflict"]),
    "",
    "",
    as.character(table4_data_casualty$High_Caste_Pct[table4_data_casualty$high_conflict_casualty_label == "High Conflict"]),
    as.character(table4_data_casualty$Janajati_Pct[table4_data_casualty$high_conflict_casualty_label == "High Conflict"]),
    as.character(table4_data_casualty$Terai_Pct[table4_data_casualty$high_conflict_casualty_label == "High Conflict"]),
    as.character(table4_data_casualty$Dalit_Pct[table4_data_casualty$high_conflict_casualty_label == "High Conflict"]),
    as.character(table4_data_casualty$Muslim_Pct[table4_data_casualty$high_conflict_casualty_label == "High Conflict"])
  ),
  
  stringsAsFactors = FALSE
)


# Create LaTeX version with grouped headers
latex_table4 <- kable(table4_formatted,
                      format = "latex",
                      booktabs = TRUE,
                      caption = "Summary Statistics by Conflict Intensity",
                      label = "tab:conflict_intensity",
                      col.names = c("Variable", "Low", "High", "Low", "High"),
                      escape = FALSE,
                      align = c("l", "r", "r", "r", "r")) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"),
                font_size = 7) %>%
  add_header_above(c(" " = 1, "War-Based Split" = 2, "Casualty-Based Split" = 2)) %>%
  footnote(general = "Standard deviations in parentheses. War-based split uses 75th percentile of months of war. Casualty-based split uses 75th percentile of casualties. Both splits define Low Conflict (at or below Q3) vs High Conflict (above Q3).",
           footnote_as_chunk = TRUE)

writeLines(as.character(latex_table4), file.path(output_path, "4.Summary_by_Conflict_Intensity.tex"))

# HTML/PNG version
html_table4 <- kable(table4_formatted,
                     format = "html",
                     caption = "Summary Statistics by Conflict Intensity",
                     col.names = c("Variable", "Low", "High", "Low", "High"),
                     escape = FALSE,
                     align = c("l", "r", "r", "r", "r")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE) %>%
  add_header_above(c(" " = 1, "War-Based Split" = 2, "Casualty-Based Split" = 2)) %>%
  footnote(general = "Standard deviations in parentheses. War-based split uses 75th percentile of months of war. Casualty-based split uses 75th percentile of casualties. Both splits define Low Conflict (at or below Q3) vs High Conflict (above Q3).",
           footnote_as_chunk = TRUE)

# Save as PNG
save_kable(html_table4, file.path(output_path, "4.Summary_by_Conflict_Intensity.png"))

# =============================================================================
# TABLE 5: BALANCE CHECK
# =============================================================================

cat("  Creating Table 5: Balance Check...\n")

# TABLE 5: BALANCE CHECK (TREATMENT VS CONTROL) ===========================

# Filter to Treatment and Control only
balance_sample <- nlss_conflict_data %>%
  filter(treatment_label %in% c("Treatment", "Control"))

# Calculate means and standard errors by treatment status
balance_stats <- balance_sample %>%
  group_by(treatment_label) %>%
  summarise(
    # Outcome (for reference - not a pre-treatment variable)
    Int_Migrant_Mean = round(mean(international_migrant == 1, na.rm = TRUE) * 100, 2),
    Int_Migrant_SE = round(sd(international_migrant, na.rm = TRUE) / sqrt(n()) * 100, 2),
    
    Absentee_Mean = round(mean(international_absentee_only == 1, na.rm = TRUE) * 100, 2),
    Absentee_SE = round(sd(international_absentee_only, na.rm = TRUE) / sqrt(n()) * 100, 2),
    
    # Conflict Exposure
    Months_War_Mean = round(mean(mwar_own_any, na.rm = TRUE), 2),
    Months_War_SE = round(sd(mwar_own_any, na.rm = TRUE) / sqrt(n()), 2),
    
    Casualties_Mean = round(mean(cas_own_any, na.rm = TRUE), 2),
    Casualties_SE = round(sd(cas_own_any, na.rm = TRUE) / sqrt(n()), 2),
    
    # Demographics
    Age_2017_Mean = round(mean(age, na.rm = TRUE), 2),
    Age_2017_SE = round(sd(age, na.rm = TRUE) / sqrt(n()), 2),
    
    Age_Conflict_Mean = round(mean(age_at_conflict_start, na.rm = TRUE), 2),
    Age_Conflict_SE = round(sd(age_at_conflict_start, na.rm = TRUE) / sqrt(n()), 2),
    
    Male_Mean = round(mean(sex == 1, na.rm = TRUE) * 100, 2),
    Male_SE = round(sd(sex, na.rm = TRUE) / sqrt(n()) * 100, 2),
    
    # Education
    No_Edu_Mean = round(mean(education_category == "No Education", na.rm = TRUE) * 100, 2),
    No_Edu_SE = round(sd(as.numeric(education_category == "No Education"), na.rm = TRUE) / sqrt(n()) * 100, 2),
    
    Primary_Mean = round(mean(education_category == "Primary (1-5)", na.rm = TRUE) * 100, 2),
    Primary_SE = round(sd(as.numeric(education_category == "Primary (1-5)"), na.rm = TRUE) / sqrt(n()) * 100, 2),
    
    Secondary_Mean = round(mean(education_category == "Secondary (6-12)", na.rm = TRUE) * 100, 2),
    Secondary_SE = round(sd(as.numeric(education_category == "Secondary (6-12)"), na.rm = TRUE) / sqrt(n()) * 100, 2),
    
    Tertiary_Mean = round(mean(education_category == "Tertiary", na.rm = TRUE) * 100, 2),
    Tertiary_SE = round(sd(as.numeric(education_category == "Tertiary"), na.rm = TRUE) / sqrt(n()) * 100, 2),
    
    # Ethnicity
    High_Caste_Mean = round(mean(Ethnicity == "Hill High Caste", na.rm = TRUE) * 100, 2),
    High_Caste_SE = round(sd(as.numeric(Ethnicity == "Hill High Caste"), na.rm = TRUE) / sqrt(n()) * 100, 2),
    
    Janajati_Mean = round(mean(Ethnicity == "Hill Janajati", na.rm = TRUE) * 100, 2),
    Janajati_SE = round(sd(as.numeric(Ethnicity == "Hill Janajati"), na.rm = TRUE) / sqrt(n()) * 100, 2),
    
    Terai_Mean = round(mean(Ethnicity == "Terai/Madhesi", na.rm = TRUE) * 100, 2),
    Terai_SE = round(sd(as.numeric(Ethnicity == "Terai/Madhesi"), na.rm = TRUE) / sqrt(n()) * 100, 2),
    
    Dalit_Mean = round(mean(Ethnicity == "Dalit", na.rm = TRUE) * 100, 2),
    Dalit_SE = round(sd(as.numeric(Ethnicity == "Dalit"), na.rm = TRUE) / sqrt(n()) * 100, 2),
    
    Muslim_Mean = round(mean(Ethnicity == "Muslim", na.rm = TRUE) * 100, 2),
    Muslim_SE = round(sd(as.numeric(Ethnicity == "Muslim"), na.rm = TRUE) / sqrt(n()) * 100, 2),
    
    .groups = 'drop'
  )

# Function to perform t-test
t_test_func <- function(var_name, is_binary = FALSE) {
  formula_str <- paste(var_name, "~ treatment")
  t_result <- t.test(as.formula(formula_str), data = balance_sample)
  
  difference <- diff(t_result$estimate)
  
  # For binary variables (proportions), multiply by 100 to get percentage points
  if (is_binary) {
    difference <- difference * 100
  }
  
  data.frame(
    Variable = var_name,
    Difference = round(difference, 3),
    T_Statistic = round(t_result$statistic, 3),
    P_Value = round(t_result$p.value, 4),
    Significant = case_when(
      t_result$p.value < 0.01 ~ "***",
      t_result$p.value < 0.05 ~ "**",
      t_result$p.value < 0.10 ~ "*",
      TRUE ~ ""
    )
  )
}

# Create binary variables for t-tests
balance_sample <- balance_sample %>%
  mutate(
    male_binary = as.numeric(sex == 1),
    no_edu_binary = as.numeric(education_category == "No Education"),
    primary_binary = as.numeric(education_category == "Primary (1-5)"),
    secondary_binary = as.numeric(education_category == "Secondary (6-12)"),
    tertiary_binary = as.numeric(education_category == "Tertiary"),
    high_caste_binary = as.numeric(Ethnicity == "Hill High Caste"),
    janajati_binary = as.numeric(Ethnicity == "Hill Janajati"),
    terai_binary = as.numeric(Ethnicity == "Terai/Madhesi"),
    dalit_binary = as.numeric(Ethnicity == "Dalit"),
    muslim_binary = as.numeric(Ethnicity == "Muslim")
  )

# Run t-tests for ALL variables
balance_tests <- bind_rows(
  t_test_func("international_migrant", is_binary = TRUE),
  t_test_func("international_absentee_only", is_binary = TRUE),
  t_test_func("mwar_own_any", is_binary = FALSE),
  t_test_func("cas_own_any", is_binary = FALSE),
  t_test_func("age", is_binary = FALSE),
  t_test_func("age_at_conflict_start", is_binary = FALSE),
  t_test_func("male_binary", is_binary = TRUE),
  t_test_func("no_edu_binary", is_binary = TRUE),
  t_test_func("primary_binary", is_binary = TRUE),
  t_test_func("secondary_binary", is_binary = TRUE),
  t_test_func("tertiary_binary", is_binary = TRUE),
  t_test_func("high_caste_binary", is_binary = TRUE),
  t_test_func("janajati_binary", is_binary = TRUE),
  t_test_func("terai_binary", is_binary = TRUE),
  t_test_func("dalit_binary", is_binary = TRUE),
  t_test_func("muslim_binary", is_binary = TRUE)
)

# Rename variables for clarity
balance_tests$Variable <- c(
  "International Migrant",
  "Currently Abroad",
  "Months of War",
  "Casualties",
  "Age in 2017",
  "Age at Conflict Start",
  "Male",
  "No Education",
  "Primary",
  "Secondary",
  "Tertiary",
  "Hill High Caste",
  "Hill Janajati",
  "Terai/Madhesi",
  "Dalit",
  "Muslim"
)

# Format mean (SE)
format_mean_se <- function(mean_val, se_val) {
  paste0(mean_val, "\n(", se_val, ")")
}

# Create formatted balance table WITH STARS ON DIFFERENCE
table5_formatted <- data.frame(
  Variable = c(
    "Outcome Variables:",
    "  International Migrant (%)",
    "  Currently Abroad (%)",
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
    "",
    format_mean_se(balance_stats$Int_Migrant_Mean[balance_stats$treatment_label == "Control"],
                   balance_stats$Int_Migrant_SE[balance_stats$treatment_label == "Control"]),
    format_mean_se(balance_stats$Absentee_Mean[balance_stats$treatment_label == "Control"],
                   balance_stats$Absentee_SE[balance_stats$treatment_label == "Control"]),
    "",
    "",
    format_mean_se(balance_stats$Months_War_Mean[balance_stats$treatment_label == "Control"],
                   balance_stats$Months_War_SE[balance_stats$treatment_label == "Control"]),
    format_mean_se(balance_stats$Casualties_Mean[balance_stats$treatment_label == "Control"],
                   balance_stats$Casualties_SE[balance_stats$treatment_label == "Control"]),
    "",
    "",
    format_mean_se(balance_stats$Age_2017_Mean[balance_stats$treatment_label == "Control"],
                   balance_stats$Age_2017_SE[balance_stats$treatment_label == "Control"]),
    format_mean_se(balance_stats$Age_Conflict_Mean[balance_stats$treatment_label == "Control"],
                   balance_stats$Age_Conflict_SE[balance_stats$treatment_label == "Control"]),
    format_mean_se(balance_stats$Male_Mean[balance_stats$treatment_label == "Control"],
                   balance_stats$Male_SE[balance_stats$treatment_label == "Control"]),
    "",
    "",
    format_mean_se(balance_stats$No_Edu_Mean[balance_stats$treatment_label == "Control"],
                   balance_stats$No_Edu_SE[balance_stats$treatment_label == "Control"]),
    format_mean_se(balance_stats$Primary_Mean[balance_stats$treatment_label == "Control"],
                   balance_stats$Primary_SE[balance_stats$treatment_label == "Control"]),
    format_mean_se(balance_stats$Secondary_Mean[balance_stats$treatment_label == "Control"],
                   balance_stats$Secondary_SE[balance_stats$treatment_label == "Control"]),
    format_mean_se(balance_stats$Tertiary_Mean[balance_stats$treatment_label == "Control"],
                   balance_stats$Tertiary_SE[balance_stats$treatment_label == "Control"]),
    "",
    "",
    format_mean_se(balance_stats$High_Caste_Mean[balance_stats$treatment_label == "Control"],
                   balance_stats$High_Caste_SE[balance_stats$treatment_label == "Control"]),
    format_mean_se(balance_stats$Janajati_Mean[balance_stats$treatment_label == "Control"],
                   balance_stats$Janajati_SE[balance_stats$treatment_label == "Control"]),
    format_mean_se(balance_stats$Terai_Mean[balance_stats$treatment_label == "Control"],
                   balance_stats$Terai_SE[balance_stats$treatment_label == "Control"]),
    format_mean_se(balance_stats$Dalit_Mean[balance_stats$treatment_label == "Control"],
                   balance_stats$Dalit_SE[balance_stats$treatment_label == "Control"]),
    format_mean_se(balance_stats$Muslim_Mean[balance_stats$treatment_label == "Control"],
                   balance_stats$Muslim_SE[balance_stats$treatment_label == "Control"])
  ),
  
  Treatment = c(
    "",
    format_mean_se(balance_stats$Int_Migrant_Mean[balance_stats$treatment_label == "Treatment"],
                   balance_stats$Int_Migrant_SE[balance_stats$treatment_label == "Treatment"]),
    format_mean_se(balance_stats$Absentee_Mean[balance_stats$treatment_label == "Treatment"],
                   balance_stats$Absentee_SE[balance_stats$treatment_label == "Treatment"]),
    "",
    "",
    format_mean_se(balance_stats$Months_War_Mean[balance_stats$treatment_label == "Treatment"],
                   balance_stats$Months_War_SE[balance_stats$treatment_label == "Treatment"]),
    format_mean_se(balance_stats$Casualties_Mean[balance_stats$treatment_label == "Treatment"],
                   balance_stats$Casualties_SE[balance_stats$treatment_label == "Treatment"]),
    "",
    "",
    format_mean_se(balance_stats$Age_2017_Mean[balance_stats$treatment_label == "Treatment"],
                   balance_stats$Age_2017_SE[balance_stats$treatment_label == "Treatment"]),
    format_mean_se(balance_stats$Age_Conflict_Mean[balance_stats$treatment_label == "Treatment"],
                   balance_stats$Age_Conflict_SE[balance_stats$treatment_label == "Treatment"]),
    format_mean_se(balance_stats$Male_Mean[balance_stats$treatment_label == "Treatment"],
                   balance_stats$Male_SE[balance_stats$treatment_label == "Treatment"]),
    "",
    "",
    format_mean_se(balance_stats$No_Edu_Mean[balance_stats$treatment_label == "Treatment"],
                   balance_stats$No_Edu_SE[balance_stats$treatment_label == "Treatment"]),
    format_mean_se(balance_stats$Primary_Mean[balance_stats$treatment_label == "Treatment"],
                   balance_stats$Primary_SE[balance_stats$treatment_label == "Treatment"]),
    format_mean_se(balance_stats$Secondary_Mean[balance_stats$treatment_label == "Treatment"],
                   balance_stats$Secondary_SE[balance_stats$treatment_label == "Treatment"]),
    format_mean_se(balance_stats$Tertiary_Mean[balance_stats$treatment_label == "Treatment"],
                   balance_stats$Tertiary_SE[balance_stats$treatment_label == "Treatment"]),
    "",
    "",
    format_mean_se(balance_stats$High_Caste_Mean[balance_stats$treatment_label == "Treatment"],
                   balance_stats$High_Caste_SE[balance_stats$treatment_label == "Treatment"]),
    format_mean_se(balance_stats$Janajati_Mean[balance_stats$treatment_label == "Treatment"],
                   balance_stats$Janajati_SE[balance_stats$treatment_label == "Treatment"]),
    format_mean_se(balance_stats$Terai_Mean[balance_stats$treatment_label == "Treatment"],
                   balance_stats$Terai_SE[balance_stats$treatment_label == "Treatment"]),
    format_mean_se(balance_stats$Dalit_Mean[balance_stats$treatment_label == "Treatment"],
                   balance_stats$Dalit_SE[balance_stats$treatment_label == "Treatment"]),
    format_mean_se(balance_stats$Muslim_Mean[balance_stats$treatment_label == "Treatment"],
                   balance_stats$Muslim_SE[balance_stats$treatment_label == "Treatment"])
  ),
  
  # DIFFERENCE WITH STARS ATTACHED
  Difference = c(
    "",
    paste0(balance_tests$Difference[balance_tests$Variable == "International Migrant"],
           balance_tests$Significant[balance_tests$Variable == "International Migrant"]),
    paste0(balance_tests$Difference[balance_tests$Variable == "Currently Abroad"],
           balance_tests$Significant[balance_tests$Variable == "Currently Abroad"]),
    "",
    "",
    paste0(balance_tests$Difference[balance_tests$Variable == "Months of War"],
           balance_tests$Significant[balance_tests$Variable == "Months of War"]),
    paste0(balance_tests$Difference[balance_tests$Variable == "Casualties"],
           balance_tests$Significant[balance_tests$Variable == "Casualties"]),
    "",
    "",
    paste0(balance_tests$Difference[balance_tests$Variable == "Age in 2017"],
           balance_tests$Significant[balance_tests$Variable == "Age in 2017"]),
    paste0(balance_tests$Difference[balance_tests$Variable == "Age at Conflict Start"],
           balance_tests$Significant[balance_tests$Variable == "Age at Conflict Start"]),
    paste0(balance_tests$Difference[balance_tests$Variable == "Male"],
           balance_tests$Significant[balance_tests$Variable == "Male"]),
    "",
    "",
    paste0(balance_tests$Difference[balance_tests$Variable == "No Education"],
           balance_tests$Significant[balance_tests$Variable == "No Education"]),
    paste0(balance_tests$Difference[balance_tests$Variable == "Primary"],
           balance_tests$Significant[balance_tests$Variable == "Primary"]),
    paste0(balance_tests$Difference[balance_tests$Variable == "Secondary"],
           balance_tests$Significant[balance_tests$Variable == "Secondary"]),
    paste0(balance_tests$Difference[balance_tests$Variable == "Tertiary"],
           balance_tests$Significant[balance_tests$Variable == "Tertiary"]),
    "",
    "",
    paste0(balance_tests$Difference[balance_tests$Variable == "Hill High Caste"],
           balance_tests$Significant[balance_tests$Variable == "Hill High Caste"]),
    paste0(balance_tests$Difference[balance_tests$Variable == "Hill Janajati"],
           balance_tests$Significant[balance_tests$Variable == "Hill Janajati"]),
    paste0(balance_tests$Difference[balance_tests$Variable == "Terai/Madhesi"],
           balance_tests$Significant[balance_tests$Variable == "Terai/Madhesi"]),
    paste0(balance_tests$Difference[balance_tests$Variable == "Dalit"],
           balance_tests$Significant[balance_tests$Variable == "Dalit"]),
    paste0(balance_tests$Difference[balance_tests$Variable == "Muslim"],
           balance_tests$Significant[balance_tests$Variable == "Muslim"])
  ),
  
  stringsAsFactors = FALSE
)


# Create LaTeX version (3 columns now)
latex_table5 <- kable(table5_formatted,
                      format = "latex",
                      booktabs = TRUE,
                      caption = "Balance Check: Treatment vs Control Groups",
                      label = "tab:balance_check",
                      col.names = c("Variable", "Control", "Treatment", "Difference"),
                      escape = FALSE,
                      align = c("l", "r", "r", "r")) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"),
                font_size = 9) %>%
  footnote(general = "Standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.10. Difference = Treatment - Control. T-tests for difference in means between treatment and control groups.",
           footnote_as_chunk = TRUE)

writeLines(as.character(latex_table5), file.path(output_path, "5.Balance_Check.tex"))

html_table5 <- kable(table5_formatted,
                     format = "html",
                     caption = "Balance Check: Treatment vs Control Groups",
                     col.names = c("Variable", "Control", "Treatment", "Difference"),
                     escape = FALSE,
                     align = c("l", "r", "r", "r")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE,
                font_size = 12) %>%
  footnote(general = "Standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.10. Difference = Treatment - Control. T-tests for difference in means between treatment and control groups.",
           footnote_as_chunk = TRUE)

# Save as PNG
save_kable(html_table5, file.path(output_path, "5.Balance_Check.png"))
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

writeLines(as.character(latex_table6), file.path(output_path, "6.DID_Framework.tex"))

html_table6 <- kable(table6_formatted, format = "html",
                     col.names = c("", "Low Conflict", "High Conflict", "Diff (H-L)"),
                     caption = "DID Framework: International Migration") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
html_table6 %>% save_kable(file.path(output_path, "6.DID_Framework.png"))


cat("\n✓ Summary statistics tables complete!\n")