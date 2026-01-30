# Preliminary============================================
# Project: Nepal Civil Conflict Analysis
# Script: Data Analysis
# Author: Ramesh Dulal
# Date: October 27, 2025
# Last Modified: October 27, 2025


# 1. SETUP ====================================

# Clear workspace (like 'clear all' in Stata)
rm(list = ls())
cat("\014")
# Set working directory
# For working with collaborator using shared Dropbox folder

#Ramesh
dropbox_path <- "/Users/rameshdulal/Library/CloudStorage/Dropbox/Nepal Civil Conflict"
output_path <- "/Users/rameshdulal/Documents/Web Portfolio/Nepal-Civil-War-and-Int-Migration/tables"

#Second User need to specify the shared dropbox path here and comment 1st users path
#dropbox_path <- "Specify the dropbox path here and uncomment this line"
#output_path <- "Specify the GitHub path for saving the output tables"

setwd(dropbox_path)


# Check if working directory is set correctly
getwd()

#install.packages(c("haven", "dplyr", "ggplot2", "labelled", 
                 #  "knitr", "kableExtra", "writexl", "stringr", 
                #   "tidyr", "fixest", "stargazer", "modelsummary"))

#install.packages("tinytex")
#tinytex::install_tinytex()
#tinytex::tlmgr_install(c("booktabs", "float", "colortbl", "xcolor"))
install.packages("webshot2")
# Load Packages
library(tinytex)
library(tinytable)
library(webshot2)
library(haven) # This package helps importing data from stata dataset
library(dplyr) # This package helps to clean the data
library(ggplot2) # This helps to create graphs and visualization options
library(labelled)
library(knitr) # For exporting to Latex
library(kableExtra) # For exporting to Latex
library(writexl) # For exporting to excel format
# Capitalize the initial letter of the name of the district
library(stringr)
library(tidyr) # For cleaning dataset
library(fixest)  # For fixed effects regression (fast and good for DID)
library(stargazer)  # For nice regression tables
library(modelsummary)

# 2. IMPORT MAIN DATASET ========================

# Import dataset with NLSS III and Conflict Data

nlss_conflict_data <- read_dta("Results/1_conflict_present_absentee_data.dta")

# Ordering the variables
nlss_conflict_data <- nlss_conflict_data %>%
  select(
    # 1. Identifiers
    psu, hhld, personid, id, sn, season,
    
    # 2. Geographic variables
    dist, district_name_std, vdcmun, ward,
    
    # 3. Conflict variables (your key variables!)
    incident_district_num,
    mwar_own_any, mwar_own_fatal, cas_own_any, cas_own_fatal,
    mwar_nbr_any, mwar_nbr_fatal, cas_nbr_any, cas_nbr_fatal,
    
    # 4. Demographics
    sex, age, rel_hhh, caste, marital, 
    
    # 5. Absentee status (important for your analysis!)
    international_migrant, international_absentee_only,  present_ind_migrant, occupation_types, absent, travelled5, rsn_travel,  abs_rsn, abs_nummonth, abs_living, abs_id,
    
    # 6. Education
    grade_comp, can_read, can_write, current_school, ever_school,
    
    # 7. Everything else
    everything()
  )



# Creating the summary table
nlss_conflict_data %>%
  count(dist) %>%
  mutate(
    percent = n / sum(n) * 100,
    cumulative = cumsum(percent) 
  ) %>%
  print(n = Inf)

# Browsing only selected observations
#nlss_conflict_data %>%   filter(is.na(dist)) %>%   View()


# Dropping the missing ovservations in variable "dist"
nlss_conflict_data <- nlss_conflict_data %>%
  drop_na(dist) # This can be achieved with the code: nlss_conflict_data <- nlss_conflict_data %>% filter(!is.na(dist))

# Fill the missing conflict values with district level values
nlss_conflict_data <- nlss_conflict_data %>%
  group_by(dist) %>%
  mutate(
    mwar_own_any = first(na.omit(mwar_own_any)),
    mwar_own_fatal = first(na.omit(mwar_own_fatal)),
    cas_own_any = first(na.omit(cas_own_any)),
    cas_own_fatal = first(na.omit(cas_own_fatal)),
    mwar_nbr_any = first(na.omit(mwar_nbr_any)),
    mwar_nbr_fatal = first(na.omit(mwar_nbr_fatal)),
    cas_nbr_any = first(na.omit(cas_nbr_any)),
    cas_nbr_fatal = first(na.omit(cas_nbr_fatal))
  ) %>%
  ungroup()

# Check if the absentee got the values on conflict intensity or not as in the previous districts
nlss_conflict_data %>%
  filter(dist == 1 & absent == 1) %>%
  select(dist, mwar_own_any, mwar_own_fatal, cas_own_any, cas_own_fatal) 

# Assigning absentees the same Caste as the HH has
nlss_conflict_data <- nlss_conflict_data %>%   
  group_by(psu, hhld) %>%   
  mutate(caste = first(na.omit(caste)) ) %>%   
  ungroup()

# GROUPING THE CASTE =====================================================

nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    Ethnicity = case_when(
      # Hill High Caste (Tagadhari)
      caste %in% c(1, 2, 14, 20, 27, 48, 49) ~ "Hill High Caste",
      # Brahmin/Chhetri: 1=Chhetri, 2=Brahmin-Hill, 14=Thakuri, 20=Sanyasi, 
      # 27=Brahmin-Tarai, 48=Rajput, 49=Kayastha
      
      # Hill Janajati (Indigenous)
      caste %in% c(3, 5, 6, 10, 11, 13, 24, 29, 32, 36, 45, 46, 60, 61, 62, 
                   66, 67, 69, 74, 77, 78, 79, 80, 81, 89, 90, 91, 92, 94, 
                   97, 98, 100, 110, 119, 120, 121, 124, 125, 126, 127, 130,
                   131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 992) ~ "Hill Janajati",
      # Major groups: 3=Magar, 5=Tamang, 6=Newar, 10=Rai, 11=Gurung, 13=Limbu, etc.
      
      # Terai/Madhesi Caste
      caste %in% c(4, 9, 16, 18, 19, 21, 26, 28, 30, 31, 33, 34, 35, 37, 
                   42, 43, 44, 47, 51, 52, 53, 54, 55, 56, 57, 58, 59, 
                   63, 64, 65, 68, 71, 72, 73, 84, 85, 86, 88, 96, 99,
                   115, 116, 117, 118, 122, 123, 128, 129, 993) ~ "Terai/Madhesi",
      # Major groups: 4=Tharu, 9=Yadav, 18=Koiri, 19=Kurmi, etc.
      
      # Dalit (Occupational Castes)
      caste %in% c(8, 12, 15, 17, 22, 23, 25, 38, 39, 40, 41, 50, 70, 
                   75, 76, 83, 93, 991) ~ "Dalit",
      # Major groups: 8=Kami, 12=Damai/Dholi, 15=Sarki, 17=Chamar, 22=Musahar, etc.
      
      # Muslim
      caste == 7 ~ "Muslim",
      
      TRUE ~ NA_character_
    )
  )

# Checking the categories
table(nlss_conflict_data$Ethnicity, useNA = "ifany")

# CREATE EDUCATION CATEGORIES==============================================
nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    education_category = case_when(
      grade_comp %in% c(16, 17) ~ "No Education",
      grade_comp >= 0 & grade_comp <= 5 ~ "Primary (1-5)",
      grade_comp >= 6 & grade_comp <= 12 ~ "Secondary (6-12)",
      grade_comp >= 13  ~ "Tertiary",
      TRUE ~ NA_character_
    )
  )

# Checking the Categories
table(nlss_conflict_data$education_category, useNA = "ifany")


# DEFINING TREATMENT AND CONTROL COHORT ==================================

# Define conflict period
conflict_start_year <- 1996
conflict_end_year <- 2006
survey_year <- 2017

# Find the birth year for NLFS dataset sample
nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    # Calculate ages
    birth_year = survey_year - age,
    age_at_conflict_start = 1996 - birth_year,
    age_at_conflict_end = 2006 - birth_year,
    
    
    # MAIN COHORT LABELS =============================================
   
    
    cohort_group = case_when(
      # TREATMENT: Childhood during conflict
      age_at_conflict_start >= 0 & age_at_conflict_start <= 5 & 
        age >= 18 & age <= 45 ~ 
        "Treatment: Age 0-5 in 1996",
      
      age_at_conflict_start >= 6 & age_at_conflict_start <= 12 & 
        age >= 18 & age <= 45 ~ 
        "Treatment: Age 6-12 in 1996",
      
      age_at_conflict_start >= 13 & age_at_conflict_start <= 17 & 
        age >= 18 & age <= 45 ~ 
        "Treatment: Age 13-17 in 1996",
      
      # CONTROL: Adult during conflict
      age_at_conflict_start >= 18 & age_at_conflict_start <= 25 & 
        age >= 47 & age <= 65 ~ 
        "Control: Age 18-25 in 1996",
      
      age_at_conflict_start >= 26 & age_at_conflict_start <= 35 & 
        age >= 47 & age <= 65 ~ 
        "Control: Age 26-35 in 1996",
      
      age_at_conflict_start >= 36 & age_at_conflict_start <= 40 & 
        age >= 47 & age <= 65 ~ 
        "Control: Age 36-40 in 1996",
      
      # EXCLUDED
      age < 18 ~ "Excluded: Too Young in 2017",
      age > 65 ~ "Excluded: Too Old in 2017",
      age_at_conflict_end < 6 ~ "Excluded: Too Young During Conflict",
      age_at_conflict_start >= 41 ~ "Excluded: Age 41+ in 1996",
      age_at_conflict_start >= 18 & age >= 18 & age <= 46 ~ "Excluded: Overlap Age",
      TRUE ~ "Excluded: Other"
    ),
    
    # Numeric treatment indicator
    childhood_exposed = case_when(
      grepl("^Treatment", cohort_group) ~ 1,
      grepl("^Control", cohort_group) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Simple label
    treatment_label = case_when(
      grepl("^Treatment", cohort_group) ~ "Treatment",
      grepl("^Control", cohort_group) ~ "Control",
      TRUE ~ "Excluded"
    ),
    
    # Short label for graphs
    cohort_short = case_when(
      cohort_group == "Treatment: Age 0-5 in 1996" ~ "T: 0-5",
      cohort_group == "Treatment: Age 6-12 in 1996" ~ "T: 6-12",
      cohort_group == "Treatment: Age 13-17 in 1996" ~ "T: 13-17",
      cohort_group == "Control: Age 18-25 in 1996" ~ "C: 18-25",
      cohort_group == "Control: Age 26-35 in 1996" ~ "C: 26-35",
      cohort_group == "Control: Age 36-40 in 1996" ~ "C: 36-40",
      TRUE ~ "Excluded"
    )
  )

nlss_conflict_data <- nlss_conflict_data %>%
  mutate(treatment = childhood_exposed)

# Conflict Intensity Categories =====================================

# Calculate q3 values based on month of war

q3_value <- quantile(nlss_conflict_data$mwar_own_any[nlss_conflict_data$mwar_own_any>0],
                     probs = 0.75,
                     na.rm = TRUE)

nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    high_conflict_q3_binary = case_when(
      mwar_own_any > q3_value ~ 1,
      mwar_own_any <= q3_value ~ 0,
      TRUE ~ NA_real_
    ),
  high_conflict_q3_label = case_when(
    mwar_own_any > q3_value ~ "High Conflict",
    mwar_own_any <= q3_value ~ "Low Conflict",
    TRUE ~ NA_character_
  )
)

# Calculate q3 values based on the number of casualties

q3_casualty <- quantile(nlss_conflict_data$cas_own_any[nlss_conflict_data$cas_own_any>0],
                         probs = 0.75,
                         na.rm = TRUE)

nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    high_conflict_casualty_binary = case_when(
      cas_own_any > q3_casualty ~ 1,
      cas_own_any <= q3_casualty ~ 0,
      TRUE ~ NA_real_
      ),
    high_conflict_casualty_label = case_when(
      cas_own_any > q3_casualty ~ "High Conflict",
      cas_own_any <= q3_casualty ~ "Low Conflict",
      TRUE ~ NA_character_
    )
  )

nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    high_conflict_q3_label = factor(high_conflict_q3_label, levels = c("Low Conflict", "High Conflict"))
  )




# PART 1: OVERALL SAMPLE CHARACTERISTICS =====================================


# Function to create summary statistics
create_summary_stats <- function(data, group_var = NULL, var_list) {
  if (is.null(group_var)) {
    # Overall summary
    summary_data <- data %>%
      summarise(across(all_of(var_list), 
                       list(
                         mean = ~mean(., na.rm = TRUE),
                         sd = ~sd(., na.rm = TRUE),
                         min = ~min(., na.rm = TRUE),
                         max = ~max(., na.rm = TRUE),
                         n = ~sum(!is.na(.))
                       ),
                       .names = "{.col}_{.fn}"))
  } else {
    # Grouped summary
    summary_data <- data %>%
      group_by(across(all_of(group_var))) %>%
      summarise(across(all_of(var_list), 
                       list(
                         mean = ~mean(., na.rm = TRUE),
                         sd = ~sd(., na.rm = TRUE),
                         min = ~min(., na.rm = TRUE),
                         max = ~max(., na.rm = TRUE),
                         n = ~sum(!is.na(.))
                       ),
                       .names = "{.col}_{.fn}"),
                .groups = 'drop')
  }
  return(summary_data)
}

# TABLE 1: OVERALL DESCRIPTIVE STATISTICS ==================================

# Define variable groups
continuous_vars <- c("mwar_own_any", "mwar_own_fatal", "cas_own_any", "cas_own_fatal", 
                     "age", "age_at_conflict_start", "grade_comp")

binary_vars <- c("international_migrant", "international_absentee_only", 
                 "present_ind_migrant", "treatment", "absent")

# Calculate statistics for continuous variables
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
               names_pattern = "(.+)_(N|Mean|SD|Min|Max)")

# Calculate statistics for binary variables (as percentages)
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
  mutate(Mean = Percent, SD = NA, Min = NA, Max = NA) %>%
  select(Variable, N, Mean, SD, Min, Max)

# Get categorical distributions
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

# Combine all into Table 1
table1_overall <- bind_rows(
  data.frame(Variable = "=== CONTINUOUS VARIABLES ===", N = "", Mean = "", SD = "", Min = "", Max = ""),
  table1_continuous,
  data.frame(Variable = "", N = "", Mean = "", SD = "", Min = "", Max = ""),
  data.frame(Variable = "=== BINARY VARIABLES (%) ===", N = "", Mean = "", SD = "", Min = "", Max = ""),
  table1_binary,
  data.frame(Variable = "", N = "", Mean = "", SD = "", Min = "", Max = ""),
  data.frame(Variable = "=== EDUCATION DISTRIBUTION (%) ===", N = "", Mean = "", SD = "", Min = "", Max = ""),
  table1_education,
  data.frame(Variable = "", N = "", Mean = "", SD = "", Min = "", Max = ""),
  data.frame(Variable = "=== ETHNICITY DISTRIBUTION (%) ===", N = "", Mean = "", SD = "", Min = "", Max = ""),
  table1_ethnicity,
  data.frame(Variable = "", N = "", Mean = "", SD = "", Min = "", Max = ""),
  data.frame(Variable = "=== COHORT DISTRIBUTION (%) ===", N = "", Mean = "", SD = "", Min = "", Max = ""),
  table1_cohort
)

# Replace "NA" strings with empty strings for cleaner display
table1_overall <- table1_overall %>%
  mutate(across(everything(), ~replace(., . == "NA", "")))


# Create LaTeX version
latex_table1 <- kable(table1_overall,
                      format = "latex",
                      booktabs = TRUE,
                      caption = "Descriptive Statistics: Overall Sample",
                      label = "tab:overall_summary",
                      col.names = c("Variable", "N", "Mean/%", "SD", "Min", "Max"),
                      escape = FALSE,
                      align = c("l", "r", "r", "r", "r", "r")) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"),
                font_size = 9)

writeLines(as.character(latex_table1), file.path(output_path, "Table1_Overall_Summary.tex"))

# Create HTML table and save as PNG (for GitHub display)
html_table1 <- kable(table1_overall,
                     format = "html",
                     col.names = c("Variable", "N", "Mean/%", "SD", "Min", "Max"),
                     align = c("l", "r", "r", "r", "r", "r"),
                     caption = "Descriptive Statistics: Overall Sample") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE)
html_table1 %>%
  save_kable(file.path(output_path, "Table1_Overall_Summary.png"))

# Creating MarkDown version for Github display
md_table1 <- kable(table1_overall,
                   format = "markdown",
                   col.names = c("Variable", "N", "Mean/%", "SD", "Min", "Max"),
                   align = c("l", "r", "r", "r", "r", "r"))
writeLines(md_table1, file.path(output_path, "Table1_Overall_Summary.md"))




# PART 2: TREATMENT VS CONTROL COMPARISON =============================


# TABLE 2: SUMMARY BY TREATMENT STATUS ====================================

table2_data <- nlss_conflict_data %>%
  filter(treatment_label %in% c("Treatment", "Control")) %>%
  group_by(treatment_label) %>%
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



# LaTeX version
latex_table2 <- kable(table2_formatted,
                      format = "latex",
                      booktabs = TRUE,
                      caption = "Summary Statistics by Treatment Status",
                      label = "tab:treatment_control",
                      col.names = c("Variable", "Control Cohort", "Treatment Cohort"),
                      escape = FALSE,
                      align = c("l", "r", "r")) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"),
                font_size = 9) %>%
  footnote(general = "Standard deviations in parentheses.",
           footnote_as_chunk = TRUE)

writeLines(as.character(latex_table2), file.path("Table2_Treatment_vs_Control.tex"))

# HTML table saved as PNG (for GitHub display)
html_table2 <- kable(table2_formatted,
                     format = "html",
                     col.names = c("Variable", "Control Cohort", "Treatment Cohort"),
                     align = c("l", "r", "r"),
                     caption = "Summary Statistics by Treatment Status") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  footnote(general = "Standard deviations in parentheses.",
           footnote_as_chunk = TRUE)
html_table2 %>%
  save_kable(file.path(output_path, "Table2_Treatment_vs_Control.png"))

stop()

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

# View the table
View(table3_formatted)

# Export to Excel
write_xlsx(table3_formatted, "Results/Table3_Summary_by_Cohort.xlsx")

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

writeLines(as.character(latex_table3), "Results/Table3_Summary_by_Cohort.tex")

cat("\n========== TABLE 3: Summary by Cohort LaTeX ==========\n")
cat(latex_table3)
cat("\n======================================================\n\n")




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

# View the table
View(table4_formatted)

# Export to Excel
write_xlsx(table4_formatted, "Results/Table4_Summary_by_Conflict_Intensity.xlsx")

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

writeLines(as.character(latex_table4), "Results/Table4_Summary_by_Conflict_Intensity.tex")

cat("\n========== TABLE 4: Summary by Conflict Intensity (Expanded) LaTeX ==========\n")
cat(latex_table4)
cat("\n=============================================================================\n\n")



# PART 5: BALANCE CHECK WITH T-TESTS ======================================


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

# View the table
View(table5_formatted)

# Export to Excel
write_xlsx(table5_formatted, "Results/Table5_Balance_Check.xlsx")

# Also export the detailed t-test results (with P-values)
write_xlsx(balance_tests, "Results/Table5_Balance_Tests_Detailed.xlsx")

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

writeLines(as.character(latex_table5), "Results/Table5_Balance_Check.tex")

cat("\n========== TABLE 5: Balance Check LaTeX ==========\n")
cat(latex_table5)
cat("\n==================================================\n\n")


# PART 6: DID FRAMEWORK TABLE (2x2) ==========================


# TABLE 6: DID FRAMEWORK (TREATMENT × CONFLICT INTENSITY) =================

# Create DID table for WAR-BASED conflict measure
did_war <- nlss_conflict_data %>%
  filter(!is.na(high_conflict_q3_label) & 
           treatment_label %in% c("Treatment", "Control")) %>%
  group_by(treatment_label, high_conflict_q3_label) %>%
  summarise(
    N = n(),
    Int_Migrant_Pct = round(mean(international_migrant == 1, na.rm = TRUE) * 100, 2),
    Absentee_Pct = round(mean(international_absentee_only == 1, na.rm = TRUE) * 100, 2),
    Return_Pct = round(mean(present_ind_migrant == 1, na.rm = TRUE) * 100, 2),
    .groups = 'drop'
  )

# Create DID table for CASUALTY-BASED conflict measure
did_casualty <- nlss_conflict_data %>%
  filter(!is.na(high_conflict_casualty_label) & 
           treatment_label %in% c("Treatment", "Control")) %>%
  group_by(treatment_label, high_conflict_casualty_label) %>%
  summarise(
    N = n(),
    Int_Migrant_Pct = round(mean(international_migrant == 1, na.rm = TRUE) * 100, 2),
    Absentee_Pct = round(mean(international_absentee_only == 1, na.rm = TRUE) * 100, 2),
    Return_Pct = round(mean(present_ind_migrant == 1, na.rm = TRUE) * 100, 2),
    .groups = 'drop'
  )

# Calculate DID estimates for WAR-BASED
control_low_war <- did_war$Int_Migrant_Pct[did_war$treatment_label == "Control" & 
                                             did_war$high_conflict_q3_label == "Low Conflict"]
control_high_war <- did_war$Int_Migrant_Pct[did_war$treatment_label == "Control" & 
                                              did_war$high_conflict_q3_label == "High Conflict"]
treatment_low_war <- did_war$Int_Migrant_Pct[did_war$treatment_label == "Treatment" & 
                                               did_war$high_conflict_q3_label == "Low Conflict"]
treatment_high_war <- did_war$Int_Migrant_Pct[did_war$treatment_label == "Treatment" & 
                                                did_war$high_conflict_q3_label == "High Conflict"]

did_estimate_war <- round((treatment_high_war - treatment_low_war) - (control_high_war - control_low_war), 2)

# Calculate DID estimates for CASUALTY-BASED
control_low_cas <- did_casualty$Int_Migrant_Pct[did_casualty$treatment_label == "Control" & 
                                                  did_casualty$high_conflict_casualty_label == "Low Conflict"]
control_high_cas <- did_casualty$Int_Migrant_Pct[did_casualty$treatment_label == "Control" & 
                                                   did_casualty$high_conflict_casualty_label == "High Conflict"]
treatment_low_cas <- did_casualty$Int_Migrant_Pct[did_casualty$treatment_label == "Treatment" & 
                                                    did_casualty$high_conflict_casualty_label == "Low Conflict"]
treatment_high_cas <- did_casualty$Int_Migrant_Pct[did_casualty$treatment_label == "Treatment" & 
                                                     did_casualty$high_conflict_casualty_label == "High Conflict"]

did_estimate_cas <- round((treatment_high_cas - treatment_low_cas) - (control_high_cas - control_low_cas), 2)

cat("\n========== DID ESTIMATES ==========\n")
cat("War-Based DID Estimate:", did_estimate_war, "percentage points\n")
cat("Casualty-Based DID Estimate:", did_estimate_cas, "percentage points\n")
cat("===================================\n\n")

# Create formatted table - WAR-BASED
table6a_formatted <- data.frame(
  Group = c("Control Cohort", "", "Treatment Cohort", "", 
            "Difference (T - C)", ""),
  
  Low_Conflict = c(
    paste0(control_low_war, "%"),
    paste0("(n=", did_war$N[did_war$treatment_label == "Control" & 
                              did_war$high_conflict_q3_label == "Low Conflict"], ")"),
    paste0(treatment_low_war, "%"),
    paste0("(n=", did_war$N[did_war$treatment_label == "Treatment" & 
                              did_war$high_conflict_q3_label == "Low Conflict"], ")"),
    paste0(round(treatment_low_war - control_low_war, 2), " pp"),
    ""
  ),
  
  High_Conflict = c(
    paste0(control_high_war, "%"),
    paste0("(n=", did_war$N[did_war$treatment_label == "Control" & 
                              did_war$high_conflict_q3_label == "High Conflict"], ")"),
    paste0(treatment_high_war, "%"),
    paste0("(n=", did_war$N[did_war$treatment_label == "Treatment" & 
                              did_war$high_conflict_q3_label == "High Conflict"], ")"),
    paste0(round(treatment_high_war - control_high_war, 2), " pp"),
    ""
  ),
  
  Difference = c(
    paste0(round(control_high_war - control_low_war, 2), " pp"),
    "",
    paste0(round(treatment_high_war - treatment_low_war, 2), " pp"),
    "",
    paste0("DID: ", did_estimate_war, " pp"),
    ""
  ),
  
  stringsAsFactors = FALSE
)

# Create formatted table - CASUALTY-BASED
table6b_formatted <- data.frame(
  Group = c("Control Cohort", "", "Treatment Cohort", "", 
            "Difference (T - C)", ""),
  
  Low_Conflict = c(
    paste0(control_low_cas, "%"),
    paste0("(n=", did_casualty$N[did_casualty$treatment_label == "Control" & 
                                   did_casualty$high_conflict_casualty_label == "Low Conflict"], ")"),
    paste0(treatment_low_cas, "%"),
    paste0("(n=", did_casualty$N[did_casualty$treatment_label == "Treatment" & 
                                   did_casualty$high_conflict_casualty_label == "Low Conflict"], ")"),
    paste0(round(treatment_low_cas - control_low_cas, 2), " pp"),
    ""
  ),
  
  High_Conflict = c(
    paste0(control_high_cas, "%"),
    paste0("(n=", did_casualty$N[did_casualty$treatment_label == "Control" & 
                                   did_casualty$high_conflict_casualty_label == "High Conflict"], ")"),
    paste0(treatment_high_cas, "%"),
    paste0("(n=", did_casualty$N[did_casualty$treatment_label == "Treatment" & 
                                   did_casualty$high_conflict_casualty_label == "High Conflict"], ")"),
    paste0(round(treatment_high_cas - control_high_cas, 2), " pp"),
    ""
  ),
  
  Difference = c(
    paste0(round(control_high_cas - control_low_cas, 2), " pp"),
    "",
    paste0(round(treatment_high_cas - treatment_low_cas, 2), " pp"),
    "",
    paste0("DID: ", did_estimate_cas, " pp"),
    ""
  ),
  
  stringsAsFactors = FALSE
)

# View the tables
cat("\n========== WAR-BASED DID TABLE ==========\n")
View(table6a_formatted)

cat("\n========== CASUALTY-BASED DID TABLE ==========\n")
View(table6b_formatted)

# Export to Excel
write_xlsx(list(
  "War_Based" = table6a_formatted,
  "Casualty_Based" = table6b_formatted
), "Results/Table6_DID_Framework.xlsx")

# Create LaTeX version - WAR-BASED
latex_table6a <- kable(table6a_formatted,
                       format = "latex",
                       booktabs = TRUE,
                       caption = "DID Framework: International Migration by Treatment Status and Conflict Intensity (War-Based)",
                       label = "tab:did_war",
                       col.names = c("", "Low Conflict", "High Conflict", "Difference (H - L)"),
                       escape = FALSE,
                       align = c("l", "r", "r", "r")) %>%
  kable_styling(latex_options = c("hold_position"),
                font_size = 10) %>%
  footnote(general = "International migrant rates shown as percentages. Sample sizes in parentheses. pp = percentage points. Low Conflict = districts at or below 75th percentile (Q3) of months of war. High Conflict = districts above Q3. DID estimate shows the interaction effect: (Treatment High - Treatment Low) - (Control High - Control Low).",
           footnote_as_chunk = TRUE)

writeLines(as.character(latex_table6a), "Results/Table6a_DID_Framework_War.tex")

# Create LaTeX version - CASUALTY-BASED
latex_table6b <- kable(table6b_formatted,
                       format = "latex",
                       booktabs = TRUE,
                       caption = "DID Framework: International Migration by Treatment Status and Conflict Intensity (Casualty-Based)",
                       label = "tab:did_casualty",
                       col.names = c("", "Low Conflict", "High Conflict", "Difference (H - L)"),
                       escape = FALSE,
                       align = c("l", "r", "r", "r")) %>%
  kable_styling(latex_options = c("hold_position"),
                font_size = 10) %>%
  footnote(general = "International migrant rates shown as percentages. Sample sizes in parentheses. pp = percentage points. Low Conflict = districts at or below 75th percentile (Q3) of casualties. High Conflict = districts above Q3. DID estimate shows the interaction effect: (Treatment High - Treatment Low) - (Control High - Control Low).",
           footnote_as_chunk = TRUE)

writeLines(as.character(latex_table6b), "Results/Table6b_DID_Framework_Casualty.tex")

cat("\n========== TABLE 6a: DID Framework (War-Based) LaTeX ==========\n")
cat(latex_table6a)
cat("\n===============================================================\n\n")

cat("\n========== TABLE 6b: DID Framework (Casualty-Based) LaTeX ==========\n")
cat(latex_table6b)
cat("\n====================================================================\n\n")



# !!! PROGRESSIVE SPECIFICATIONS: ALL COMBINATIONS ============================
# Order: 1A-1E (War Continuous) → 1F-1J (War Binary) → 1K-1O (Casualty Continuous) → 1P-1T (Casualty Binary)



# OUTCOME 1: INTERNATIONAL_MIGRANT ===========================
# International Migrant from Absentee Dataset and Present Dataset =============
# ---------- MODELS 1A-1E: MONTHS OF WAR (CONTINUOUS) ==================

# Model 1A: Baseline (no controls, no FE)
model_1a <- feols(international_migrant ~ treatment * mwar_own_any, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

# Model 1B: + Demographics
model_1b <- feols(international_migrant ~ treatment * mwar_own_any + 
                    sex + age + I(age^2), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

# Model 1C: + Education
model_1c <- feols(international_migrant ~ treatment * mwar_own_any + 
                    sex + age + I(age^2) + factor(education_category), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

# Model 1D: + Ethnicity
model_1d <- feols(international_migrant ~ treatment * mwar_own_any + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

# Model 1E: + District FE
model_1e <- feols(international_migrant ~ treatment * mwar_own_any + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity) | dist, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

# View all specifications side by side
etable(model_1a, model_1b, model_1c, model_1d, model_1e,
       title = "International Migrant ~ Treatment × Months of War",
       headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
       cluster = ~dist)

# Display LaTeX code in console
cat("\n========== TABLE 1A-1E: LaTeX Code ==========\n")
latex_1a <- etable(model_1a, model_1b, model_1c, model_1d, model_1e,
                   title = "International Migrant ~ Treatment × Months of War",
                   headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
                   tex = TRUE,
                   file = "Results/Table_1A_IntMigrant_War.tex",
                   replace = TRUE)

cat(latex_1a)
cat("\n=============================================\n\n")


# ---------- MODELS 1F-1J: HIGH CONFLICT BINARY (WAR) ----------

# Model 1F: Baseline (no controls, no FE)
model_1f <- feols(international_migrant ~ treatment * high_conflict_q3_binary, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

# Model 1G: + Demographics
model_1g <- feols(international_migrant ~ treatment * high_conflict_q3_binary + 
                    sex + age + I(age^2), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

# Model 1H: + Education
model_1h <- feols(international_migrant ~ treatment * high_conflict_q3_binary + 
                    sex + age + I(age^2) + factor(education_category), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

# Model 1I: + Ethnicity
model_1i <- feols(international_migrant ~ treatment * high_conflict_q3_binary + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

# Model 1J: + District FE
model_1j <- feols(international_migrant ~ treatment * high_conflict_q3_binary + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity) | dist, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

# View all specifications side by side
etable(model_1f, model_1g, model_1h, model_1i, model_1j,
       title = "International Migrant ~ Treatment × High Conflict Binary (War)",
       headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
       cluster = ~dist)

# Display LaTeX code in console
cat("\n========== TABLE 1F-1J: LaTeX Code ==========\n")
latex_1f <- etable(model_1f, model_1g, model_1h, model_1i, model_1j,
                   title = "International Migrant ~ Treatment × High Conflict Binary (War)",
                   headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
                   tex = TRUE,
                   file = "Results/Table_1C_IntMigrant_HighConflict_War.tex",
                   replace = TRUE)
cat(latex_1f)
cat("\n=============================================\n\n")


# ---------- MODELS 1K-1O: CASUALTIES (CONTINUOUS) ----------

# Model 1K: Baseline
model_1k <- feols(international_migrant ~ treatment * cas_own_any, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

# Model 1L: + Demographics
model_1l <- feols(international_migrant ~ treatment * cas_own_any + 
                    sex + age + I(age^2), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

# Model 1M: + Education
model_1m <- feols(international_migrant ~ treatment * cas_own_any + 
                    sex + age + I(age^2) + factor(education_category), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

# Model 1N: + Ethnicity
model_1n <- feols(international_migrant ~ treatment * cas_own_any + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

# Model 1O: + District FE
model_1o <- feols(international_migrant ~ treatment * cas_own_any + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity) | dist, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

# View all specifications
etable(model_1k, model_1l, model_1m, model_1n, model_1o,
       title = "International Migrant ~ Treatment × Casualties",
       headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"))

# Display LaTeX code in console
cat("\n========== TABLE 1K-1O: LaTeX Code ==========\n")
latex_1k <- etable(model_1k, model_1l, model_1m, model_1n, model_1o,
                   title = "International Migrant ~ Treatment × Casualties",
                   headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
                   tex = TRUE,
                   file = "Results/Table_1B_IntMigrant_Casualty.tex",
                   replace = TRUE)
cat(latex_1k)
cat("\n=============================================\n\n")


# ---------- MODELS 1P-1T: HIGH CONFLICT BINARY (CASUALTY) =============

# Model 1P: Baseline
model_1p <- feols(international_migrant ~ treatment * high_conflict_casualty_binary, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

# Model 1Q: + Demographics
model_1q <- feols(international_migrant ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

# Model 1R: + Education
model_1r <- feols(international_migrant ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2) + factor(education_category), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

# Model 1S: + Ethnicity
model_1s <- feols(international_migrant ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

# Model 1T: + District FE
model_1t <- feols(international_migrant ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity) | dist, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

# View all specifications
etable(model_1p, model_1q, model_1r, model_1s, model_1t,
       title = "International Migrant ~ Treatment × High Conflict Binary (Casualty)",
       headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"))

# Display LaTeX code in console
cat("\n========== TABLE 1P-1T: LaTeX Code ==========\n")
latex_1p <- etable(model_1p, model_1q, model_1r, model_1s, model_1t,
                   title = "International Migrant ~ Treatment × High Conflict Binary (Casualty)",
                   headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
                   tex = TRUE,
                   file = "Results/Table_1D_IntMigrant_HighConflict_Casualty.tex",
                   replace = TRUE)
cat(latex_1p)
cat("\n=============================================\n\n")



# OUTCOME 2: INTERNATIONAL_ABSENTEE_ONLY ===========================
# International Absentee only includes data from the Absentee Dataset ==========

# ---------- MODELS 2A-2E: MONTHS OF WAR (CONTINUOUS) ----------

model_2a <- feols(international_absentee_only ~ treatment * mwar_own_any, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_2b <- feols(international_absentee_only ~ treatment * mwar_own_any + 
                    sex + age + I(age^2), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_2c <- feols(international_absentee_only ~ treatment * mwar_own_any + 
                    sex + age + I(age^2) + factor(education_category), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_2d <- feols(international_absentee_only ~ treatment * mwar_own_any + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_2e <- feols(international_absentee_only ~ treatment * mwar_own_any + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity) | dist, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

etable(model_2a, model_2b, model_2c, model_2d, model_2e,
       title = "Absentee Only ~ Treatment × Months of War",
       headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"))

# Display LaTeX code in console
cat("\n========== TABLE 2A-2E: LaTeX Code ==========\n")
latex_2a <- etable(model_2a, model_2b, model_2c, model_2d, model_2e,
                   title = "Absentee Only ~ Treatment × Months of War",
                   headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
                   tex = TRUE,
                   file = "Results/Table_2A_Absentee_War.tex",
                   replace = TRUE)
cat(latex_2a)
cat("\n=============================================\n\n")


# ---------- MODELS 2F-2J: HIGH CONFLICT BINARY (WAR) ----------

model_2f <- feols(international_absentee_only ~ treatment * high_conflict_q3_binary, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_2g <- feols(international_absentee_only ~ treatment * high_conflict_q3_binary + 
                    sex + age + I(age^2), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_2h <- feols(international_absentee_only ~ treatment * high_conflict_q3_binary + 
                    sex + age + I(age^2) + factor(education_category), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_2i <- feols(international_absentee_only ~ treatment * high_conflict_q3_binary + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_2j <- feols(international_absentee_only ~ treatment * high_conflict_q3_binary + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity) | dist, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

etable(model_2f, model_2g, model_2h, model_2i, model_2j,
       title = "Absentee Only ~ Treatment × High Conflict Binary (War)",
       headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"))

# Display LaTeX code in console
cat("\n========== TABLE 2F-2J: LaTeX Code ==========\n")
latex_2f <- etable(model_2f, model_2g, model_2h, model_2i, model_2j,
                   title = "Absentee Only ~ Treatment × High Conflict Binary (War)",
                   headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
                   tex = TRUE,
                   file = "Results/Table_2C_Absentee_HighConflict_War.tex",
                   replace = TRUE)
cat(latex_2f)
cat("\n=============================================\n\n")


# ---------- MODELS 2K-2O: CASUALTIES (CONTINUOUS) ----------

model_2k <- feols(international_absentee_only ~ treatment * cas_own_any, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_2l <- feols(international_absentee_only ~ treatment * cas_own_any + 
                    sex + age + I(age^2), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_2m <- feols(international_absentee_only ~ treatment * cas_own_any + 
                    sex + age + I(age^2) + factor(education_category), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_2n <- feols(international_absentee_only ~ treatment * cas_own_any + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_2o <- feols(international_absentee_only ~ treatment * cas_own_any + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity) | dist, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

etable(model_2k, model_2l, model_2m, model_2n, model_2o,
       title = "Absentee Only ~ Treatment × Casualties",
       headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"))

# Display LaTeX code in console
cat("\n========== TABLE 2K-2O: LaTeX Code ==========\n")
latex_2k <- etable(model_2k, model_2l, model_2m, model_2n, model_2o,
                   title = "Absentee Only ~ Treatment × Casualties",
                   headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
                   tex = TRUE,
                   file = "Results/Table_2B_Absentee_Casualty.tex",
                   replace = TRUE)
cat(latex_2k)
cat("\n=============================================\n\n")


# ---------- MODELS 2P-2T: HIGH CONFLICT BINARY (CASUALTY) ----------

model_2p <- feols(international_absentee_only ~ treatment * high_conflict_casualty_binary, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_2q <- feols(international_absentee_only ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_2r <- feols(international_absentee_only ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2) + factor(education_category), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_2s <- feols(international_absentee_only ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_2t <- feols(international_absentee_only ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity) | dist, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

etable(model_2p, model_2q, model_2r, model_2s, model_2t,
       title = "Absentee Only ~ Treatment × High Conflict Binary (Casualty)",
       headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"))

# Display LaTeX code in console
cat("\n========== TABLE 2P-2T: LaTeX Code ==========\n")
latex_2p <- etable(model_2p, model_2q, model_2r, model_2s, model_2t,
                   title = "Absentee Only ~ Treatment × High Conflict Binary (Casualty)",
                   headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
                   tex = TRUE,
                   file = "Results/Table_2D_Absentee_HighConflict_Casualty.tex",
                   replace = TRUE)
cat(latex_2p)
cat("\n=============================================\n\n")



# OUTCOME 3: PRESENT_IND_MIGRANT ============================
# Present ind migrant includes International Migrant in Non-Absent Dataset only ==

# ---------- MODELS 3A-3E: MONTHS OF WAR (CONTINUOUS) ----------

model_3a <- feols(present_ind_migrant ~ treatment * mwar_own_any, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_3b <- feols(present_ind_migrant ~ treatment * mwar_own_any + 
                    sex + age + I(age^2), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_3c <- feols(present_ind_migrant ~ treatment * mwar_own_any + 
                    sex + age + I(age^2) + factor(education_category), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_3d <- feols(present_ind_migrant ~ treatment * mwar_own_any + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_3e <- feols(present_ind_migrant ~ treatment * mwar_own_any + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity) | dist, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

etable(model_3a, model_3b, model_3c, model_3d, model_3e,
       title = "Present Migrant ~ Treatment × Months of War",
       headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"))

# Display LaTeX code in console
cat("\n========== TABLE 3A-3E: LaTeX Code ==========\n")
latex_3a <- etable(model_3a, model_3b, model_3c, model_3d, model_3e,
                   title = "Present Migrant ~ Treatment × Months of War",
                   headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
                   tex = TRUE,
                   file = "Results/Table_3A_Present_War.tex",
                   replace = TRUE)
cat(latex_3a)
cat("\n=============================================\n\n")


# ---------- MODELS 3F-3J: HIGH CONFLICT BINARY (WAR) ----------

model_3f <- feols(present_ind_migrant ~ treatment * high_conflict_q3_binary, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_3g <- feols(present_ind_migrant ~ treatment * high_conflict_q3_binary + 
                    sex + age + I(age^2), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_3h <- feols(present_ind_migrant ~ treatment * high_conflict_q3_binary + 
                    sex + age + I(age^2) + factor(education_category), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_3i <- feols(present_ind_migrant ~ treatment * high_conflict_q3_binary + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_3j <- feols(present_ind_migrant ~ treatment * high_conflict_q3_binary + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity) | dist, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

etable(model_3f, model_3g, model_3h, model_3i, model_3j,
       title = "Present Migrant ~ Treatment × High Conflict Binary (War)",
       headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"))

# Display LaTeX code in console
cat("\n========== TABLE 3F-3J: LaTeX Code ==========\n")
latex_3f <- etable(model_3f, model_3g, model_3h, model_3i, model_3j,
                   title = "Present Migrant ~ Treatment × High Conflict Binary (War)",
                   headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
                   tex = TRUE,
                   file = "Results/Table_3C_Present_HighConflict_War.tex",
                   replace = TRUE)
cat(latex_3f)
cat("\n=============================================\n\n")


# ---------- MODELS 3K-3O: CASUALTIES (CONTINUOUS) ----------

model_3k <- feols(present_ind_migrant ~ treatment * cas_own_any, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_3l <- feols(present_ind_migrant ~ treatment * cas_own_any + 
                    sex + age + I(age^2), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_3m <- feols(present_ind_migrant ~ treatment * cas_own_any + 
                    sex + age + I(age^2) + factor(education_category), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_3n <- feols(present_ind_migrant ~ treatment * cas_own_any + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_3o <- feols(present_ind_migrant ~ treatment * cas_own_any + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity) | dist, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

etable(model_3k, model_3l, model_3m, model_3n, model_3o,
       title = "Present Migrant ~ Treatment × Casualties",
       headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"))

# Display LaTeX code in console
cat("\n========== TABLE 3K-3O: LaTeX Code ==========\n")
latex_3k <- etable(model_3k, model_3l, model_3m, model_3n, model_3o,
                   title = "Present Migrant ~ Treatment × Casualties",
                   headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
                   tex = TRUE,
                   file = "Results/Table_3B_Present_Casualty.tex",
                   replace = TRUE)
cat(latex_3k)
cat("\n=============================================\n\n")


# ---------- MODELS 3P-3T: HIGH CONFLICT BINARY (CASUALTY) ----------

model_3p <- feols(present_ind_migrant ~ treatment * high_conflict_casualty_binary, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_3q <- feols(present_ind_migrant ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_3r <- feols(present_ind_migrant ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2) + factor(education_category), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_3s <- feols(present_ind_migrant ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity), 
                  data = nlss_conflict_data,
                  cluster = ~dist)

model_3t <- feols(present_ind_migrant ~ treatment * high_conflict_casualty_binary + 
                    sex + age + I(age^2) + factor(education_category) +
                    factor(Ethnicity) | dist, 
                  data = nlss_conflict_data,
                  cluster = ~dist)

etable(model_3p, model_3q, model_3r, model_3s, model_3t,
       title = "Present Migrant ~ Treatment × High Conflict Binary (Casualty)",
       headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"))

# Display LaTeX code in console
cat("\n========== TABLE 3P-3T: LaTeX Code ==========\n")
latex_3p <- etable(model_3p, model_3q, model_3r, model_3s, model_3t,
                   title = "Present Migrant ~ Treatment × High Conflict Binary (Casualty)",
                   headers = c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnic", "(5) +FE"),
                   tex = TRUE,
                   file = "Results/Table_3D_Present_HighConflict_Casualty.tex",
                   replace = TRUE)
cat(latex_3p)
cat("\n=============================================\n\n")


# HETEROGENEITY ANALYSIS =====================================


# Analysis for Gender ========================================

# Filter data for males only (sex == 1)
male_data <- nlss_conflict_data %>% 
  filter(sex == 1, !is.na(treatment))

# Filter data for females only (sex == 2)
female_data <- nlss_conflict_data %>% 
  filter(sex == 2, !is.na(treatment))

# Run regression for MALES
model_male <- feols(
  international_absentee_only ~ treatment * high_conflict_casualty_binary + 
    age + I(age^2) + factor(education_category) + factor(Ethnicity) | dist,
  data = male_data,
  cluster = ~dist
)

# Run regression for FEMALES
model_female <- feols(
  international_absentee_only ~ treatment * high_conflict_casualty_binary + 
    age + I(age^2) + factor(education_category) + factor(Ethnicity) | dist,
  data = female_data,
  cluster = ~dist
)


# Display results side by side
etable(model_male, model_female,
       title = "Heterogeneity Analysis by Gender",
       headers = c("Males", "Females"),
       cluster = ~dist)

# Save as LaTeX
latex_gender <- etable(model_male, model_female,
       title = "Heterogeneity Analysis by Gender",
       headers = c("Males", "Females"),
       tex = TRUE,
       file = "Results/Heterogeneity_Gender.tex",
       replace = TRUE)   

cat(latex_gender)


# Analysis for Ethnic Groups=======================================

# 1. Hill High Caste
data_high_caste <- nlss_conflict_data %>% 
  filter(Ethnicity == "Hill High Caste", !is.na(treatment))

model_high_caste <- feols(
  international_absentee_only ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(education_category) | dist,
  data = data_high_caste,
  cluster = ~dist
)

# 2. Hill Janajati
data_janajati <- nlss_conflict_data %>% 
  filter(Ethnicity == "Hill Janajati", !is.na(treatment))

model_janajati <- feols(
  international_absentee_only ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(education_category) | dist,
  data = data_janajati,
  cluster = ~dist
)

# 3. Terai/Madhesi
data_terai <- nlss_conflict_data %>% 
  filter(Ethnicity == "Terai/Madhesi", !is.na(treatment))

model_terai <- feols(
  international_absentee_only ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(education_category) | dist,
  data = data_terai,
  cluster = ~dist
)

# 4. Dalit
data_dalit <- nlss_conflict_data %>% 
  filter(Ethnicity == "Dalit", !is.na(treatment))

model_dalit <- feols(
  international_absentee_only ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(education_category) | dist,
  data = data_dalit,
  cluster = ~dist
)

# 5. Muslim
data_muslim <- nlss_conflict_data %>% 
  filter(Ethnicity == "Muslim", !is.na(treatment))

model_muslim <- feols(
  international_absentee_only ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(education_category) | dist,
  data = data_muslim,
  cluster = ~dist
)

etable(model_high_caste, model_janajati, model_terai, model_dalit, model_muslim,
       title = "Heterogeneity Analysis by Ethnicity: International Absenteeism",
       headers = c("Hill High\nCaste", "Hill\nJanajati", "Terai/\nMadhesi", "Dalit", "Muslim"),
       order = c("treatment:high_conflict_casualty_binary", "treatment", "high_conflict_casualty_binary"),
       cluster = ~dist)

# Save as LaTeX
latex_ethnic <- etable(model_high_caste, model_janajati, model_terai, model_dalit, model_muslim,
       title = "Heterogeneity Analysis by Ethnicity",
       headers = c("Hill High Caste", "Hill Janajati", "Terai/Madhesi", "Dalit", "Muslim"),
       order = c("treatment:high_conflict_casualty_binary", "treatment", "high_conflict_casualty_binary"),
       tex = TRUE,
       file = "Results/Heterogeneity_Ethnicity.tex",
       replace = TRUE)

cat(latex_ethnic)

# Analysis for Education =============================================

# 1. No Education
data_no_edu <- nlss_conflict_data %>% 
  filter(education_category == "No Education", !is.na(treatment))

model_no_edu <- feols(
  international_absentee_only ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(Ethnicity) | dist,
  data = data_no_edu,
  cluster = ~dist
)

# 2. Primary (1-5)
data_primary <- nlss_conflict_data %>% 
  filter(education_category == "Primary (1-5)", !is.na(treatment))

model_primary <- feols(
  international_absentee_only ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(Ethnicity) | dist,
  data = data_primary,
  cluster = ~dist
)

# 3. Secondary (6-12)
data_secondary <- nlss_conflict_data %>% 
  filter(education_category == "Secondary (6-12)", !is.na(treatment))

model_secondary <- feols(
  international_absentee_only ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(Ethnicity) | dist,
  data = data_secondary,
  cluster = ~dist
)

# 4. Tertiary
data_tertiary <- nlss_conflict_data %>% 
  filter(education_category == "Tertiary", !is.na(treatment))

model_tertiary <- feols(
  international_absentee_only ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(Ethnicity) | dist,
  data = data_tertiary,
  cluster = ~dist
)

# Display results
etable(model_no_edu, model_primary, model_secondary, model_tertiary,
       title = "Heterogeneity Analysis by Education: International Absenteeism",
       headers = c("No\nEducation", "Primary\n(1-5)", "Secondary\n(6-12)", "Tertiary"),
       order = c("Treatment $\\times$ High Conflict",
                 "Treatment",
                 "High Conflict",
                 "Sex",
                 "Age",
                 "Age$^2$",
                 "High Hill Caste",
                 "Hill Janajati",
                 "Muslim",
                 "Terai/Madhesi"),
       dict = c("treatment:high_conflict_casualty_binary" = "Treatment × High Conflict",
                "treatment" = "Treatment",
                "high_conflict_casualty_binary" = "High Conflict",
                "sex" = "Sex",
                "age" = "Age",
                "I(I(age^2))" = "Age^2",
                "factor(Ethnicity)HillHighCaste" = "High Hill Caste",
                "factor(Ethnicity)HillJanajati" = "Hill Janajati",
                "factor(Ethnicity)Muslim" = "Muslim",
                "factor(Ethnicity)Terai/Madhesi" = "Terai/Madhesi"),
       cluster = ~dist)

# Save as LaTeX
latex_education <- etable(model_no_edu, model_primary, model_secondary, model_tertiary,
                          title = "Heterogeneity Analysis by Education",
                          headers = c("No Education", "Primary (1-5)", "Secondary (6-12)", "Tertiary"),
                          order = c("Treatment $\\times$ High Conflict",
                                    "Treatment",
                                    "High Conflict",
                                    "Sex",
                                    "Age",
                                    "Age$^2$",
                                    "High Hill Caste",
                                    "Hill Janajati",
                                    "Muslim",
                                    "Terai/Madhesi"),
                          dict = c("treatment:high_conflict_casualty_binary" = "Treatment $\\times$ High Conflict",
                                   "treatment" = "Treatment",
                                   "high_conflict_casualty_binary" = "High Conflict",
                                   "sex" = "Sex",
                                   "age" = "Age",
                                   "I(I(age^2))" = "Age$^2$",
                                   "factor(Ethnicity)HillHighCaste" = "High Hill Caste",
                                   "factor(Ethnicity)HillJanajati" = "Hill Janajati",
                                   "factor(Ethnicity)Muslim" = "Muslim",
                                   "factor(Ethnicity)Terai/Madhesi" = "Terai/Madhesi"),
                          tex = TRUE,
                          file = "Results/Heterogeneity_Education.tex",
                          replace = TRUE)

cat(latex_education)


# Analysis with different Cohort ==================================

# 1. Treatment Cohort 0-5 years in 1996

data_0_5years <- nlss_conflict_data %>% 
  filter(
    cohort_group %in% c(
      "Treatment: Age 0-5 in 1996",
      "Control: Age 26-35 in 1996",
      "Control: Age 36-40 in 1996"
      ),
    !is.na(treatment),
    !is.na(high_conflict_casualty_binary)
    )

model_0_5years <- feols(
  international_absentee_only ~ treatment * high_conflict_casualty_binary +
    sex + age + I(age^2) + factor(Ethnicity) + factor(education_category) | dist,
  data = data_0_5years,
  cluster = ~dist
)

# For Cohort 6-12 years
data_6_12years <- nlss_conflict_data %>%
  filter(
    cohort_group %in% c(
      "Treatment: Age 6-12 in 1996",
      "Control: Age 26-35 in 1996",
      "Control: Age 36-40 in 1996"
    ),
    !is.na(treatment),
    !is.na(high_conflict_casualty_binary)
  )

model_6_12years <- feols(
  international_absentee_only ~ treatment * high_conflict_casualty_binary +
    sex + age + I(age^2) + factor(Ethnicity) + factor(education_category) | dist,
  data = data_6_12years,
  cluster = ~dist
)


# For Cohort 13-17 years
data_13_17years <- nlss_conflict_data %>%
  filter(
    cohort_group %in% c(
      "Treatment: Age 13-17 in 1996",
      "Control: Age 26-35 in 1996",
      "Control: Age 36-40 in 1996" 
    ),
    !is.na(treatment),
    !is.na(high_conflict_casualty_binary) 
  )

model_13_17years <- feols(
  international_absentee_only ~ treatment * high_conflict_casualty_binary +
    sex + age + I(age^2) + factor(Ethnicity) + factor(education_category) | dist,
  data = data_13_17years,
  cluster = ~dist
)

# Tabulate the three models side by side
etable(model_0_5years, model_6_12years, model_13_17years,
       title = "Heterogeneity Analysis by Cohort Groups: International Absenteeism",
       headers = c("0-5 years in 1996", "6-12 years in 1996", "13-17 years in 1996"),
       order = c("Treatment $\\times$ High Conflict",
                 "Treatment",
                 "High Conflict",
                 "Sex",
                 "Age",
                 "Age$^2$",
                 "High Hill Caste",
                 "Hill Janajati",
                 "Muslim",
                 "Terai/Madhesi"),
       dict = c("treatment:high_conflict_casualty_binary" = "Treatment × High Conflict",
                "treatment" = "Treatment",
                "high_conflict_casualty_binary" = "High Conflict",
                "sex" = "Sex",
                "age" = "Age",
                "I(I(age^2))" = "Age^2",
                "factor(Ethnicity)HillHighCaste" = "High Hill Caste",
                "factor(Ethnicity)HillJanajati" = "Hill Janajati",
                "factor(Ethnicity)Muslim" = "Muslim",
                "factor(Ethnicity)Terai/Madhesi" = "Terai/Madhesi"),
       cluster = ~dist)

# Save as LaTeX
latex_cohort <- etable(model_0_5years, model_6_12years, model_13_17years,
                          title = "Heterogeneity Analysis by Cohort Groups: International Absenteeism",
                          headers = c("0-5 years in 1996", "6-12 years in 1996", "13-17 years in 1996"),
                          order = c("Treatment $\\times$ High Conflict",
                                    "Treatment",
                                    "High Conflict",
                                    "Sex",
                                    "Age",
                                    "Age$^2$",
                                    "High Hill Caste",
                                    "Hill Janajati",
                                    "Muslim",
                                    "Terai/Madhesi",
                                    "Primary(1-5)",
                                    "Secondary(6-12)",
                                    "Tertiary"),
                          dict = c("treatment:high_conflict_casualty_binary" = "Treatment $\\times$ High Conflict",
                                   "treatment" = "Treatment",
                                   "high_conflict_casualty_binary" = "High Conflict",
                                   "sex" = "Sex",
                                   "age" = "Age",
                                   "I(I(age^2))" = "Age$^2$",
                                   "factor(Ethnicity)HillHighCaste" = "High Hill Caste",
                                   "factor(Ethnicity)HillJanajati" = "Hill Janajati",
                                   "factor(Ethnicity)Muslim" = "Muslim",
                                   "factor(Ethnicity)Terai/Madhesi" = "Terai/Madhesi",
                                   "factor(education_category)Primary(1-5)" = "Primary(1-5)",
                                   "factor(education_category)Secondary(6-12)" = "Secondary(6-12)",
                                   "factor(education_category)Tertiary" = "Tertiary" ),
                          tex = TRUE,
                          file = "Results/Heterogeneity_Cohort.tex",
                          replace = TRUE)

cat(latex_cohort) 
# ROBUSTNESS CHECK  =====================================

  
# Testing with different percentile cutoffs for casualties ================

p75_casualty <- quantile(nlss_conflict_data$cas_own_any[nlss_conflict_data$cas_own_any > 0],
                         probs = 0.75, na.rm = TRUE)

p80_casualty <- quantile(nlss_conflict_data$cas_own_any[nlss_conflict_data$cas_own_any > 0],
                         probs = 0.80, na.rm = TRUE)

p90_casualty <- quantile(nlss_conflict_data$cas_own_any[nlss_conflict_data$cas_own_any > 0],
                         probs = 0.90, na.rm = TRUE)

p95_casualty <- quantile(nlss_conflict_data$cas_own_any[nlss_conflict_data$cas_own_any > 0],
                         probs = 0.95, na.rm = TRUE)

# Create binary variables for each cutoff
nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    high_conflict_p75 = ifelse(cas_own_any > p75_casualty, 1, 0),
    high_conflict_p80 = ifelse(cas_own_any > p80_casualty, 1, 0),
    high_conflict_p90 = ifelse(cas_own_any > p90_casualty, 1, 0),
    high_conflict_p95 = ifelse(cas_own_any > p95_casualty, 1, 0)
  )

# Run models with each cutoff
model_cutoff_p75 <- feols(
  international_absentee_only ~ treatment * high_conflict_p75 + 
    sex + age + I(age^2) + factor(education_category) + factor(Ethnicity) | dist,
  data = nlss_conflict_data %>% filter(!is.na(treatment)),
  cluster = ~dist
)

model_cutoff_p80 <- feols(
  international_absentee_only ~ treatment * high_conflict_p80 + 
    sex + age + I(age^2) + factor(education_category) + factor(Ethnicity) | dist,
  data = nlss_conflict_data %>% filter(!is.na(treatment)),
  cluster = ~dist
)

model_cutoff_p90 <- feols(
  international_absentee_only ~ treatment * high_conflict_p90 + 
    sex + age + I(age^2) + factor(education_category) + factor(Ethnicity) | dist,
  data = nlss_conflict_data %>% filter(!is.na(treatment)),
  cluster = ~dist
)

model_cutoff_p95 <- feols(
  international_absentee_only ~ treatment * high_conflict_p95 + 
    sex + age + I(age^2) + factor(education_category) + factor(Ethnicity) | dist,
  data = nlss_conflict_data %>% filter(!is.na(treatment)),
  cluster = ~dist
)

# Display results
etable(model_cutoff_p75, model_cutoff_p80, model_cutoff_p90, model_cutoff_p95,
       title = "Robustness: Alternative Conflict Intensity Cutoffs",
       headers = c("75th pctile", "80th pctile", "90th pctile", "95th pctile"),
       order = c("Treatment.*High Conflict P75" ,
                                 "treatment:high_conflict_p80" ,
                                 "treatment:high_conflict_p90" ,
                                 "treatment:high_conflict_p95" ,
                                 "treatment" = "Treatment"),
       dict = c("treatment:high_conflict_p75" = "1.Treatment $\\times$ High Conflict P75",
                "treatment:high_conflict_p80" = "2.Treatment $\\times$ High Conflict P80",
                "treatment:high_conflict_p90" = "3.Treatment $\\times$ High Conflict P90",
                "treatment:high_conflict_p95" = "4.Treatment $\\times$ High Conflict P95",
                "treatment" = "5.Treatment",
                "sex" = "Sex",
                "age" = "Age",
                "I(age^2)" = "Age$^2$",
                "factor(education_category)Primary(1-5)" = "Primary(1-5)",
                "factor(education_category)Secondary(6-12)" = "Secondary(6-12)",
                "factor(education_category)Tertiary" = "Tertiary",
                "factor(Ethnicity)HillHighCaste" = "High Hill Caste",
                "factor(Ethnicity)HillJanajati" = "Hill Janajati",
                "factor(Ethnicity)Muslim" = "Muslim",
                "factor(Ethnicity)Terai/Madhesi" = "Terai/Madhesi"),
       cluster = ~dist)

# Save as LaTeX
latex_cutoffs <- etable(model_cutoff_p75, model_cutoff_p80, model_cutoff_p90, model_cutoff_p95,
                        title = "Robustness: Alternative Conflict Intensity Cutoffs",
                        headers = c("P75(Baseline)", "P80", "P90", "P95"),
                        dict = c(
                          "treatment:high_conflict_p75" = "Treatment $\\times$ High Conflict P75",
                          "treatment:high_conflict_p80" = "Treatment $\\times$ High Conflict P80",
                          "treatment:high_conflict_p90" = "Treatment $\\times$ High Conflict P90",
                          "treatment:high_conflict_p95" = "Treatment $\\times$ High Conflict P95",
                          "treatment"                   = "Treatment",
                          "sex"                         = "Sex",
                          "age"                         = "Age",
                          "I(I(age^2))"                 = "Age$^2$",   
                          "factor(education_category)Primary(1-5)"    = "Primary(1-5)",
                          "factor(education_category)Secondary(6-12)" = "Secondary(6-12)",
                          "factor(education_category)Tertiary"        = "Tertiary",
                          "factor(Ethnicity)HillHighCaste"            = "High Hill Caste",
                          "factor(Ethnicity)HillJanajati"             = "Hill Janajati",
                          "factor(Ethnicity)Muslim"                   = "Muslim",
                          "factor(Ethnicity)Terai/Madhesi"            = "Terai/Madhesi"
                          ),
                        order = c(
                          "%treatment:high_conflict_p75",
                          "%treatment:high_conflict_p80",
                          "%treatment:high_conflict_p90",
                          "%treatment:high_conflict_p95",
                          "%treatment",
                          "%sex",
                          "%age",
                          "%I(I(age^2))",
                          "%factor(education_category)Primary(1-5)",
                          "%factor(education_category)Secondary(6-12)",
                          "%factor(education_category)Tertiary",
                          "%factor(Ethnicity)HillHighCaste",
                          "%factor(Ethnicity)HillJanajati",
                          "%factor(Ethnicity)Muslim",
                          "%factor(Ethnicity)Terai/Madhesi"
                          ),
                        tex = TRUE,
                        file = "Results/Robustness_Cutoffs.tex",
                        replace = TRUE)

cat(latex_cutoffs)

# Occupation Channel ===============================================
# Categorization of the Occupation =================================
nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    nsco_major = floor(occupation_types /1000),
    occupation_skill_3cat = case_when(
      nsco_major %in% c(1, 2) ~ "High Skilled",
      nsco_major %in% c(3, 4, 5) ~ "Medium Skilled", 
      nsco_major %in% c(6, 7, 8, 9) ~ "Low Skilled",
      nsco_major == 0 ~ "Armed Forces",
      TRUE ~ NA_character_
    ),
  )

# Create binary outcome variable for each skill level to use in regression =====
# Create binary outcome variables for each skill level
nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    high_skilled = as.numeric(occupation_skill_3cat == "High Skilled"),
    medium_skilled = as.numeric(occupation_skill_3cat == "Medium Skilled"),
    low_skilled = as.numeric(occupation_skill_3cat == "Low Skilled")
  )

# Create occupation sample_dataset
occupation_sample <- nlss_conflict_data %>%
  filter(!is.na(treatment) & !is.na(occupation_skill_3cat))

# Run regressions for each skill level WITH interaction =====================

# Model 1: High Skilled
model_high_int <- feols(
  high_skilled ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(education_category) + factor(Ethnicity) | dist,
  data = occupation_sample,
  cluster = ~dist
)

# Model 2: Medium Skilled
model_medium_int <- feols(
  medium_skilled ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(education_category) + factor(Ethnicity) | dist,
  data = occupation_sample,
  cluster = ~dist
)

# Model 3: Low Skilled
model_low_int <- feols(
  low_skilled ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(education_category) + factor(Ethnicity) | dist,
  data = occupation_sample,
  cluster = ~dist
)

etable(model_high_int, model_medium_int, model_low_int,
       title = "Treatment × Conflict Effect on Occupation Skill Levels",
       headers = c("High Skilled", "Medium Skilled", "Low Skilled"),
       order = c(":", "treatment", "high_conflict"),
       cluster = ~dist)


# Save as LaTeX
latex_occupation_interaction <- etable(
  model_high_int, model_medium_int, model_low_int,
  title = "Treatment $\\times$ Conflict Effect on Occupation Skill Levels",
  headers = c("High Skilled", "Medium Skilled", "Low Skilled"),
  dict = c(
    "treatment:high_conflict_casualty_binary" = "Treatment $\\times$ High Conflict",
    "treatment"                   = "Treatment",
    "sex"                         = "Sex",
    "age"                         = "Age",
    "I(I(age^2))"                 = "Age$^2$",   
    "factor(education_category)Primary(1-5)"    = "Primary(1-5)",
    "factor(education_category)Secondary(6-12)" = "Secondary(6-12)",
    "factor(education_category)Tertiary"        = "Tertiary",
    "factor(Ethnicity)HillHighCaste"            = "High Hill Caste",
    "factor(Ethnicity)HillJanajati"             = "Hill Janajati",
    "factor(Ethnicity)Muslim"                   = "Muslim",
    "factor(Ethnicity)Terai/Madhesi"            = "Terai/Madhesi"
  ),
  order = c(
    "%treatment:high_conflict_casualty_binary",
    "%treatment",
    "%sex",
    "%age",
    "%I(I(age^2))",
    "%factor(education_category)Primary(1-5)",
    "%factor(education_category)Secondary(6-12)",
    "%factor(education_category)Tertiary",
    "%factor(Ethnicity)HillHighCaste",
    "%factor(Ethnicity)HillJanajati",
    "%factor(Ethnicity)Muslim",
    "%factor(Ethnicity)Terai/Madhesi"
  ),
  tex = TRUE,
  file = "Results/Table_Occupation_Interaction.tex",
  replace = TRUE
)

cat(latex_occupation_interaction)

# Education Channel ===========================================
# Create exclusive education categories 
nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    no_education_binary = as.numeric(education_category == "No Education"),
    primary_education = as.numeric(education_category == "Primary (1-5)"),
    secondary_education = as.numeric(education_category == "Secondary (6-12)"),
    tertiary_education = as.numeric(education_category == "Tertiary")
  )

# Regression Models
model_no_edu <- feols(
  no_education_binary ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(Ethnicity) | dist,
  data = nlss_conflict_data %>% filter(!is.na(treatment)),
  cluster = ~dist
)

model_primary <- feols(
  primary_education ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(Ethnicity) | dist,
  data = nlss_conflict_data %>% filter(!is.na(treatment)),
  cluster = ~dist
)

model_secondary <- feols(
  secondary_education ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(Ethnicity) | dist,
  data = nlss_conflict_data %>% filter(!is.na(treatment)),
  cluster = ~dist
)

model_tertiary <- feols(
  tertiary_education ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(Ethnicity) | dist,
  data = nlss_conflict_data %>% filter(!is.na(treatment)),
  cluster = ~dist
)

# Display all 4 side-by-side (MATCHING your occupation table)
etable(model_no_edu, model_primary, model_secondary, model_tertiary,
       title = "Treatment × Conflict Effect on Educational Attainment",
       headers = c("No Education", "Primary", "Secondary", "Tertiary"),
       order = c(":", "treatment", "high_conflict_casualty_binary"),
       cluster = ~dist)

#Save as Latex
latex_education <- etable(
  model_no_edu, model_primary, model_secondary, model_tertiary,
  title = "Treatment $\\times$ Conflict Effect on Education Levels",
  headers = c("No Education", "Primary", "Secondary", "Tertiary"),
  dict = c(
    "treatment:high_conflict_casualty_binary" = "Treatment $\\times$ High Conflict",
    "treatment"                   = "Treatment",
    "sex"                         = "Sex",
    "age"                         = "Age",
    "I(I(age^2))"                 = "Age$^2$",   
    "factor(Ethnicity)HillHighCaste"            = "High Hill Caste",
    "factor(Ethnicity)HillJanajati"             = "Hill Janajati",
    "factor(Ethnicity)Muslim"                   = "Muslim",
    "factor(Ethnicity)Terai/Madhesi"            = "Terai/Madhesi"
  ),
  order = c(
    "%treatment:high_conflict_casualty_binary",
    "%treatment",
    "%sex",
    "%age",
    "%I(I(age^2))",
    "%factor(Ethnicity)HillHighCaste",
    "%factor(Ethnicity)HillJanajati",
    "%factor(Ethnicity)Muslim",
    "%factor(Ethnicity)Terai/Madhesi"
  ),
  tex = TRUE,
  file = "Results/Table_Education.tex",
  replace = TRUE
)

cat(latex_education)

 # TEST ==============================

