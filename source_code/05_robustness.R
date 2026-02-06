# =============================================================================
# 05_robustness.R - Robustness Checks & Sensitivity Analysis
# =============================================================================
# Project: Nepal Civil Conflict and International Migration
# Author: Ramesh Dulal
#
# REQUIRES: 02_data_cleaning.R must be run first
#
# ANALYSES:
# 1. Alternative conflict intensity cutoffs (P75, P80, P90, P95)
# 2. Heterogeneity by gender
# 3. Heterogeneity by ethnicity
# 4. Placebo tests
#
# OUTPUTS:
# - Robustness_Cutoffs.tex
# - Robustness_Gender.tex
# - Robustness_Ethnicity.tex
# =============================================================================

# Load helper functions
source(file.path(code_path, "functions/Helper_functions.R"))

cat("Running robustness checks...\n")

# Standard coefficient dictionary
coef_dict <- get_coef_dict()
coef_order <- get_coef_order()

# =============================================================================
# 1. ALTERNATIVE CONFLICT INTENSITY CUTOFFS
# =============================================================================

cat("  1. Testing alternative conflict intensity cutoffs...\n")

# Calculate cutoffs at different percentiles
cutoffs <- c(0.75, 0.80, 0.90, 0.95)
cutoff_values <- sapply(cutoffs, function(p) {
  quantile(nlss_conflict_data$cas_own_any[nlss_conflict_data$cas_own_any > 0],
           probs = p, na.rm = TRUE)
})
names(cutoff_values) <- paste0("p", cutoffs * 100)

cat("    Cutoff values:\n")
print(cutoff_values)

# Create binary variables for each cutoff
nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    high_conflict_p75 = as.numeric(cas_own_any > cutoff_values["p75"]),
    high_conflict_p80 = as.numeric(cas_own_any > cutoff_values["p80"]),
    high_conflict_p90 = as.numeric(cas_own_any > cutoff_values["p90"]),
    high_conflict_p95 = as.numeric(cas_own_any > cutoff_values["p95"])
  )

# Run models with each cutoff
model_p75 <- feols(international_migrant ~ treatment * high_conflict_p75 + 
                     sex + age + I(age^2) + factor(education_category) +
                     factor(Ethnicity) | dist, 
                   data = nlss_conflict_data, cluster = ~dist)

model_p80 <- feols(international_migrant ~ treatment * high_conflict_p80 + 
                     sex + age + I(age^2) + factor(education_category) +
                     factor(Ethnicity) | dist, 
                   data = nlss_conflict_data, cluster = ~dist)

model_p90 <- feols(international_migrant ~ treatment * high_conflict_p90 + 
                     sex + age + I(age^2) + factor(education_category) +
                     factor(Ethnicity) | dist, 
                   data = nlss_conflict_data, cluster = ~dist)

model_p95 <- feols(international_migrant ~ treatment * high_conflict_p95 + 
                     sex + age + I(age^2) + factor(education_category) +
                     factor(Ethnicity) | dist, 
                   data = nlss_conflict_data, cluster = ~dist)

# Display and export
etable(model_p75, model_p80, model_p90, model_p95,
       title = "Robustness: Alternative Conflict Intensity Cutoffs",
       headers = c("P75", "P80", "P90", "P95"),
       keep = c("treatment", "high_conflict"),
       cluster = ~dist)

etable(model_p75, model_p80, model_p90, model_p95,
       title = "Robustness: Alternative Conflict Intensity Cutoffs",
       headers = c("P75", "P80", "P90", "P95"),
       dict = c(
         "treatment:high_conflict_p75" = "Treatment $\\times$ High Conflict",
         "treatment:high_conflict_p80" = "Treatment $\\times$ High Conflict",
         "treatment:high_conflict_p90" = "Treatment $\\times$ High Conflict",
         "treatment:high_conflict_p95" = "Treatment $\\times$ High Conflict",
         "treatment" = "Treatment",
         "sex" = "Male",
         "age" = "Age",
         "I(I(age^2))" = "Age$^2$"
       ),
       order = c("%treatment:high_conflict", "%treatment"),
       tex = TRUE,
       file = file.path(output_path, "Robustness_Cutoffs.tex"),
       replace = TRUE)


# =============================================================================
# 2. HETEROGENEITY BY GENDER
# =============================================================================

cat("  2. Heterogeneity by gender...\n")

# Male sample
model_male <- feols(international_migrant ~ treatment * high_conflict_casualty_binary + 
                      age + I(age^2) + factor(education_category) +
                      factor(Ethnicity) | dist, 
                    data = nlss_conflict_data %>% filter(sex == 1), 
                    cluster = ~dist)

# Female sample
model_female <- feols(international_migrant ~ treatment * high_conflict_casualty_binary + 
                        age + I(age^2) + factor(education_category) +
                        factor(Ethnicity) | dist, 
                      data = nlss_conflict_data %>% filter(sex == 2), 
                      cluster = ~dist)

# Display and export
etable(model_male, model_female,
       title = "Heterogeneity by Gender",
       headers = c("Male", "Female"),
       dict = coef_dict, order = coef_order,
       tex = TRUE,
       file = file.path(output_path, "Robustness_Gender.tex"),
       replace = TRUE)

cat("    Male interaction coefficient:", coef(model_male)["treatment:high_conflict_casualty_binary"], "\n")
cat("    Female interaction coefficient:", coef(model_female)["treatment:high_conflict_casualty_binary"], "\n")


# =============================================================================
# 3. HETEROGENEITY BY ETHNICITY
# =============================================================================

cat("  3. Heterogeneity by ethnicity...\n")

# Run models by ethnicity group
model_highcaste <- feols(international_migrant ~ treatment * high_conflict_casualty_binary + 
                           sex + age + I(age^2) + factor(education_category) | dist, 
                         data = nlss_conflict_data %>% filter(Ethnicity == "Hill High Caste"), 
                         cluster = ~dist)

model_janajati <- feols(international_migrant ~ treatment * high_conflict_casualty_binary + 
                          sex + age + I(age^2) + factor(education_category) | dist, 
                        data = nlss_conflict_data %>% filter(Ethnicity == "Hill Janajati"), 
                        cluster = ~dist)

model_terai <- feols(international_migrant ~ treatment * high_conflict_casualty_binary + 
                       sex + age + I(age^2) + factor(education_category) | dist, 
                     data = nlss_conflict_data %>% filter(Ethnicity == "Terai/Madhesi"), 
                     cluster = ~dist)

model_dalit <- feols(international_migrant ~ treatment * high_conflict_casualty_binary + 
                       sex + age + I(age^2) + factor(education_category) | dist, 
                     data = nlss_conflict_data %>% filter(Ethnicity == "Dalit"), 
                     cluster = ~dist)

# Display and export
etable(model_highcaste, model_janajati, model_terai, model_dalit,
       title = "Heterogeneity by Ethnicity",
       headers = c("High Caste", "Janajati", "Terai/Madhesi", "Dalit"),
       dict = coef_dict, order = coef_order,
       tex = TRUE,
       file = file.path(output_path, "Robustness_Ethnicity.tex"),
       replace = TRUE)


# =============================================================================
# 4. HETEROGENEITY BY EDUCATION
# =============================================================================

cat("  4. Heterogeneity by education level...\n")

model_no_edu <- feols(international_migrant ~ treatment * high_conflict_casualty_binary + 
                        sex + age + I(age^2) + factor(Ethnicity) | dist, 
                      data = nlss_conflict_data %>% filter(education_category == "No Education"), 
                      cluster = ~dist)

model_primary <- feols(international_migrant ~ treatment * high_conflict_casualty_binary + 
                         sex + age + I(age^2) + factor(Ethnicity) | dist, 
                       data = nlss_conflict_data %>% filter(education_category == "Primary (1-5)"), 
                       cluster = ~dist)

model_secondary <- feols(international_migrant ~ treatment * high_conflict_casualty_binary + 
                           sex + age + I(age^2) + factor(Ethnicity) | dist, 
                         data = nlss_conflict_data %>% filter(education_category == "Secondary (6-12)"), 
                         cluster = ~dist)

model_tertiary <- feols(international_migrant ~ treatment * high_conflict_casualty_binary + 
                          sex + age + I(age^2) + factor(Ethnicity) | dist, 
                        data = nlss_conflict_data %>% filter(education_category == "Tertiary"), 
                        cluster = ~dist)

etable(model_no_edu, model_primary, model_secondary, model_tertiary,
       title = "Heterogeneity by Education Level",
       headers = c("No Education", "Primary", "Secondary", "Tertiary"),
       dict = coef_dict, order = coef_order,
       tex = TRUE,
       file = file.path(output_path, "Robustness_Education.tex"),
       replace = TRUE)


# =============================================================================
# 5. TRIPLE DIFFERENCE: TREATMENT × CONFLICT × GENDER
# =============================================================================

cat("  5. Triple difference (Treatment × Conflict × Gender)...\n")

model_triple_diff <- feols(
  international_migrant ~ treatment * high_conflict_casualty_binary * sex + 
    age + I(age^2) + factor(education_category) + factor(Ethnicity) | dist, 
  data = nlss_conflict_data %>% filter(!is.na(treatment)), 
  cluster = ~dist
)

etable(model_triple_diff,
       title = "Triple Difference: Treatment × Conflict × Gender",
       dict = c(coef_dict,
                "treatment:high_conflict_casualty_binary:sex" = "Treatment $\\times$ High Conflict $\\times$ Male"),
       tex = TRUE,
       file = file.path(output_path, "Robustness_TripleDiff.tex"),
       replace = TRUE)


cat("\n✓ Robustness checks complete!\n")