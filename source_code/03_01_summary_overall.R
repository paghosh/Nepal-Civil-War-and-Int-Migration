# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Summary Statistics - Table 1: Overall Descriptive Statistics
# Last Updated  : April 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions, 02_data_cleaning.R dataset
# OUTPUTS : tables_summary/1.Overall_Summary.tex / .png


# ==============================================================================
# SECTION 1: DEFINE VARIABLE GROUPS
# ==============================================================================

continuous_vars <- c("mwar_own_any", "mwar_own_fatal", "cas_own_any", "cas_own_fatal",
                     "age", "age_at_conflict_start", "grade_comp")

binary_vars <- c("international_migrant", "international_absentee_only", "national",
                 "present_ind_migrant", "treatment", "absent", "baseline", "male")


# ==============================================================================
# SECTION 2: CLEAN VARIABLE NAME FUNCTION
# ==============================================================================

clean_var_names <- function(var) {
  case_when(
    var == "mwar_own_any"                ~ "Months of War (any)",
    var == "mwar_own_fatal"              ~ "Months of War (fatal)",
    var == "cas_own_any"                 ~ "Casualties (any)",
    var == "cas_own_fatal"               ~ "Casualties (fatal)",
    var == "age"                         ~ "Age in 2017",
    var == "age_at_conflict_start"       ~ "Age at Conflict Start",
    var == "grade_comp"                  ~ "Years of Education",
    var == "international_migrant"       ~ "International Migrant (%)",
    var == "international_absentee_only" ~ "Currently Abroad (%)",
    var == "present_ind_migrant"         ~ "Return Migrant (%)",
    var == "national"                    ~ "Internal Migrant (%)",
    var == "treatment"                   ~ "Treatment Cohort (%)",
    var == "absent"                      ~ "Absent from Household (%)",
    var == "baseline"                    ~ "Non-Migrant",
    var == "male"                        ~ "Male (%)",
    var == "marital"                     ~ "Marital Status",
    var == "nsco_major"                  ~ "Occupation Type",
    TRUE ~ var
  )
}


# ==============================================================================
# SECTION 3: COMPUTE STATISTICS
# ==============================================================================

# Continuous variables
table1_continuous <- nlss_conflict_data %>%
  summarise(
    across(all_of(continuous_vars),
           list(
             N    = ~sum(!is.na(.)),
             Mean = ~round(mean(., na.rm = TRUE), 2),
             SD   = ~round(sd(.,   na.rm = TRUE), 2),
             Min  = ~round(min(.,  na.rm = TRUE), 2),
             Max  = ~round(max(.,  na.rm = TRUE), 2)
           ),
           .names = "{.col}_{.fn}")
  ) %>%
  pivot_longer(everything(),
               names_to  = c("Variable", ".value"),
               names_pattern = "(.+)_(N|Mean|SD|Min|Max)") %>%
  mutate(Variable = clean_var_names(Variable))

# Binary variables (as percentages)
table1_binary <- nlss_conflict_data %>%
  summarise(
    across(all_of(binary_vars),
           list(
             N       = ~sum(!is.na(.)),
             Percent = ~round(mean(. == 1, na.rm = TRUE) * 100, 2)
           ),
           .names = "{.col}_{.fn}")
  ) %>%
  pivot_longer(everything(),
               names_to  = c("Variable", ".value"),
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
    N        = n,
    Mean     = round(n / sum(n) * 100, 2),
    SD = NA, Min = NA, Max = NA
  ) %>%
  select(Variable, N, Mean, SD, Min, Max)

# Ethnicity distribution
table1_ethnicity <- nlss_conflict_data %>%
  filter(!is.na(Ethnicity)) %>%
  count(Ethnicity) %>%
  mutate(
    Variable = paste("  ", Ethnicity),
    N        = n,
    Mean     = round(n / sum(n) * 100, 2),
    SD = NA, Min = NA, Max = NA
  ) %>%
  select(Variable, N, Mean, SD, Min, Max)

# Marital status distribution
table1_marital <- nlss_conflict_data %>%
  filter(!is.na(marital)) %>%
  count(marital_label) %>%
  mutate(
    Variable = paste("  ", marital_label),
    N        = n,
    Mean     = round(n / sum(n) * 100, 2),
    SD = NA, Min = NA, Max = NA
  ) %>%
  select(Variable, N, Mean, SD, Min, Max)

# Occupation type distribution
table1_occupation <- nlss_conflict_data %>%
  filter(!is.na(occupation_category)) %>%
  count(occupation_category) %>%
  mutate(
    Variable = paste("  ", occupation_category),
    N        = n,
    Mean     = round(n / sum(n) * 100, 2),
    SD = NA, Min = NA, Max = NA
  ) %>%
  select(Variable, N, Mean, SD, Min, Max)

# Cohort distribution
table1_cohort <- nlss_conflict_data %>%
  filter(!is.na(cohort_group)) %>%
  count(cohort_group) %>%
  mutate(
    Variable = paste("  ", cohort_group),
    N        = n,
    Mean     = round(n / sum(n) * 100, 2),
    SD = NA, Min = NA, Max = NA
  ) %>%
  select(Variable, N, Mean, SD, Min, Max)


# ==============================================================================
# SECTION 4: COMBINE INTO SINGLE TABLE
# ==============================================================================

# Convert all to character before binding
to_char <- function(df) df %>% mutate(across(c(N, Mean, SD, Min, Max), as.character))

table1_overall <- bind_rows(
  data.frame(Variable = "Continuous Variables",     N = NA, Mean = NA, SD = NA, Min = NA, Max = NA),
  to_char(table1_continuous),
  data.frame(Variable = "",                          N = NA, Mean = NA, SD = NA, Min = NA, Max = NA),
  data.frame(Variable = "Binary Variables (%)",     N = NA, Mean = NA, SD = NA, Min = NA, Max = NA),
  to_char(table1_binary),
  data.frame(Variable = "",                          N = NA, Mean = NA, SD = NA, Min = NA, Max = NA),
  data.frame(Variable = "Education Distribution (%)", N = NA, Mean = NA, SD = NA, Min = NA, Max = NA),
  to_char(table1_education),
  data.frame(Variable = "",                          N = NA, Mean = NA, SD = NA, Min = NA, Max = NA),
  data.frame(Variable = "Ethnicity Distribution (%)", N = NA, Mean = NA, SD = NA, Min = NA, Max = NA),
  to_char(table1_ethnicity),
  data.frame(Variable = "Marital Status (%)",       N = NA, Mean = NA, SD = NA, Min = NA, Max = NA),
  to_char(table1_marital),
  data.frame(Variable = "Occupation Type (%)",      N = NA, Mean = NA, SD = NA, Min = NA, Max = NA),
  to_char(table1_occupation),
  data.frame(Variable = "Cohort Distribution (%)",  N = "",  Mean = "",  SD = "",  Min = "",  Max = ""),
  to_char(table1_cohort)
) %>%
  mutate(across(everything(), ~ifelse(is.na(.), "", as.character(.))))


# ==============================================================================
# SECTION 5: EXPORT OUTPUTS
# ==============================================================================

# --- LaTeX ---

table1_latex <- table1_overall %>%
  mutate(Variable = sanitize_latex(Variable))

latex_table1 <- kable(table1_latex,
                      format    = "latex",
                      booktabs  = TRUE,
                      caption   = "Descriptive Statistics: Overall Sample",
                      label     = "tab:overall_summary",
                      col.names = c("Variable", "N", "Mean/\\%", "SD", "Min", "Max"),
                      escape    = FALSE,
                      align     = c("l", "r", "r", "r", "r", "r")) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"),
                font_size = 10)

writeLines(as.character(latex_table1),
           file.path(tables_summary, "1.Overall_Summary.tex"))


# --- PNG ---
html_table1 <- kable(table1_overall,
                     format    = "html",
                     col.names = c("Variable", "N", "Mean/%", "SD", "Min", "Max"),
                     caption   = "Descriptive Statistics: Overall Sample") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  footnote(
    general = c(
      "International Migrant includes individuals abroad at time of survey and those who had been abroad for at least 3 months in the past.",
      "Currently Abroad includes individuals abroad at the time of survey.",
      "Internal Migrant includes individuals migrating inside the country at the time of survey.",
      "Return Migrant includes only individuals who travelled abroad for at least 3 months.",
      "Absent from Household includes all absent individuals at the time of survey."
    ),
    general_title    = "Notes:",
    footnote_as_chunk = FALSE
  )

html_table1 %>% save_kable(file.path(tables_summary, "1.Overall_Summary.png"),
                           zoom    = 2,
                           vwidth  = 900,
                           vheight = 600)