# =============================================================================
# 06_mechanism_analysis.R - Occupation & Education Channel Analysis
# =============================================================================
# Project: Nepal Civil Conflict and International Migration
# Author: Ramesh Dulal
#
# REQUIRES: 02_data_cleaning.R must be run first
#
# ANALYSES:
# 1. Occupation channel (skill levels)
# 2. Education channel (educational attainment)
#
# OUTPUTS:
# - Table_Occupation_Interaction.tex
# - Table_Education.tex
# =============================================================================

# Load helper functions
source(file.path(code_path, "functions/helper_functions.R"))

cat("Running mechanism analysis...\n")

# Standard coefficient dictionary
coef_dict <- get_coef_dict()
coef_order <- get_coef_order()

# =============================================================================
# 1. OCCUPATION CHANNEL
# =============================================================================

cat("  1. Occupation Channel Analysis...\n")

# Categorize occupations by skill level using NSCO major groups
nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    nsco_major = floor(occupation_types / 1000),
    occupation_skill_3cat = case_when(
      nsco_major %in% c(1, 2) ~ "High Skilled",
      nsco_major %in% c(3, 4, 5) ~ "Medium Skilled", 
      nsco_major %in% c(6, 7, 8, 9) ~ "Low Skilled",
      nsco_major == 0 ~ "Armed Forces",
      TRUE ~ NA_character_
    )
  )

# Check distribution
cat("    Occupation skill distribution:\n")
print(table(nlss_conflict_data$occupation_skill_3cat, useNA = "ifany"))

# Create binary outcome variables
nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    high_skilled = as.numeric(occupation_skill_3cat == "High Skilled"),
    medium_skilled = as.numeric(occupation_skill_3cat == "Medium Skilled"),
    low_skilled = as.numeric(occupation_skill_3cat == "Low Skilled")
  )

# Create occupation sample (non-missing treatment and occupation)
occupation_sample <- nlss_conflict_data %>%
  filter(!is.na(treatment) & !is.na(occupation_skill_3cat))

cat("    Occupation sample size:", nrow(occupation_sample), "\n")

# Run regressions by skill level
model_high_skill <- feols(
  high_skilled ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(education_category) + factor(Ethnicity) | dist,
  data = occupation_sample,
  cluster = ~dist
)

model_medium_skill <- feols(
  medium_skilled ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(education_category) + factor(Ethnicity) | dist,
  data = occupation_sample,
  cluster = ~dist
)

model_low_skill <- feols(
  low_skilled ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(education_category) + factor(Ethnicity) | dist,
  data = occupation_sample,
  cluster = ~dist
)

# Display results
etable(model_high_skill, model_medium_skill, model_low_skill,
       title = "Treatment × Conflict Effect on Occupation Skill Levels",
       headers = c("High Skilled", "Medium Skilled", "Low Skilled"),
       order = c(":", "treatment", "high_conflict"),
       cluster = ~dist)

# Export to LaTeX
etable(
  model_high_skill, model_medium_skill, model_low_skill,
  title = "Treatment $\\times$ Conflict Effect on Occupation Skill Levels",
  headers = c("High Skilled", "Medium Skilled", "Low Skilled"),
  dict = c(
    "treatment:high_conflict_casualty_binary" = "Treatment $\\times$ High Conflict",
    "treatment"                   = "Treatment",
    "sex"                         = "Male",
    "age"                         = "Age",
    "I(I(age^2))"                 = "Age$^2$",   
    "factor(education_category)Primary(1-5)"    = "Primary (1-5)",
    "factor(education_category)Secondary(6-12)" = "Secondary (6-12)",
    "factor(education_category)Tertiary"        = "Tertiary",
    "factor(Ethnicity)Hill High Caste"          = "Hill High Caste",
    "factor(Ethnicity)Hill Janajati"            = "Hill Janajati",
    "factor(Ethnicity)Muslim"                   = "Muslim",
    "factor(Ethnicity)Terai/Madhesi"            = "Terai/Madhesi"
  ),
  order = c(
    "%treatment:high_conflict_casualty_binary",
    "%treatment",
    "%sex",
    "%age"
  ),
  tex = TRUE,
  file = file.path(output_path, "Table_Occupation_Interaction.tex"),
  replace = TRUE
)

cat("    High skilled interaction:", coef(model_high_skill)["treatment:high_conflict_casualty_binary"], "\n")
cat("    Medium skilled interaction:", coef(model_medium_skill)["treatment:high_conflict_casualty_binary"], "\n")
cat("    Low skilled interaction:", coef(model_low_skill)["treatment:high_conflict_casualty_binary"], "\n")


# =============================================================================
# 2. EDUCATION CHANNEL
# =============================================================================

cat("\n  2. Education Channel Analysis...\n")

# Create binary outcome variables for education levels
nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    no_education_binary = as.numeric(education_category == "No Education"),
    primary_education = as.numeric(education_category == "Primary (1-5)"),
    secondary_education = as.numeric(education_category == "Secondary (6-12)"),
    tertiary_education = as.numeric(education_category == "Tertiary")
  )

# Education sample
education_sample <- nlss_conflict_data %>%
  filter(!is.na(treatment) & !is.na(education_category))

cat("    Education sample size:", nrow(education_sample), "\n")

# Run regressions for each education level
# Note: Don't control for education when education is the outcome!

model_no_edu <- feols(
  no_education_binary ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(Ethnicity) | dist,
  data = education_sample,
  cluster = ~dist
)

model_primary_edu <- feols(
  primary_education ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(Ethnicity) | dist,
  data = education_sample,
  cluster = ~dist
)

model_secondary_edu <- feols(
  secondary_education ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(Ethnicity) | dist,
  data = education_sample,
  cluster = ~dist
)

model_tertiary_edu <- feols(
  tertiary_education ~ treatment * high_conflict_casualty_binary + 
    sex + age + I(age^2) + factor(Ethnicity) | dist,
  data = education_sample,
  cluster = ~dist
)

# Display results
etable(model_no_edu, model_primary_edu, model_secondary_edu, model_tertiary_edu,
       title = "Treatment × Conflict Effect on Educational Attainment",
       headers = c("No Education", "Primary", "Secondary", "Tertiary"),
       order = c(":", "treatment", "high_conflict_casualty_binary"),
       cluster = ~dist)

# Export to LaTeX
etable(
  model_no_edu, model_primary_edu, model_secondary_edu, model_tertiary_edu,
  title = "Treatment $\\times$ Conflict Effect on Education Levels",
  headers = c("No Education", "Primary", "Secondary", "Tertiary"),
  dict = c(
    "treatment:high_conflict_casualty_binary" = "Treatment $\\times$ High Conflict",
    "treatment"                   = "Treatment",
    "sex"                         = "Male",
    "age"                         = "Age",
    "I(I(age^2))"                 = "Age$^2$",   
    "factor(Ethnicity)Hill High Caste"          = "Hill High Caste",
    "factor(Ethnicity)Hill Janajati"            = "Hill Janajati",
    "factor(Ethnicity)Muslim"                   = "Muslim",
    "factor(Ethnicity)Terai/Madhesi"            = "Terai/Madhesi"
  ),
  order = c(
    "%treatment:high_conflict_casualty_binary",
    "%treatment",
    "%sex",
    "%age"
  ),
  tex = TRUE,
  file = file.path(output_path, "Table_Education.tex"),
  replace = TRUE
)

cat("    No education interaction:", coef(model_no_edu)["treatment:high_conflict_casualty_binary"], "\n")
cat("    Primary education interaction:", coef(model_primary_edu)["treatment:high_conflict_casualty_binary"], "\n")
cat("    Secondary education interaction:", coef(model_secondary_edu)["treatment:high_conflict_casualty_binary"], "\n")
cat("    Tertiary education interaction:", coef(model_tertiary_edu)["treatment:high_conflict_casualty_binary"], "\n")


