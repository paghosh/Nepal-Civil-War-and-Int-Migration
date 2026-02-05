# =============================================================================
# 02_data_cleaning.R - Data Import, Cleaning, and Variable Creation
# =============================================================================
# Project: Nepal Civil Conflict and International Migration
# Author: Ramesh Dulal
#
# CREATES:
# - nlss_conflict_data: Main analysis dataset with all variables
# =============================================================================


# -----------------------------------------------------------------------------
# 1. IMPORT DATA
# -----------------------------------------------------------------------------

nlss_conflict_data <- read_dta("Results/1_conflict_present_absentee_data.dta")

# -----------------------------------------------------------------------------
# 2. REORDER VARIABLES FOR CLARITY
# -----------------------------------------------------------------------------

nlss_conflict_data <- nlss_conflict_data %>%
  select(
    # Identifiers
    psu, hhld, personid, id, sn, season,
    
    # Geographic
    dist, district_name_std, vdcmun, ward,
    
    # Conflict measures
    incident_district_num,
    mwar_own_any, mwar_own_fatal, cas_own_any, cas_own_fatal,
    mwar_nbr_any, mwar_nbr_fatal, cas_nbr_any, cas_nbr_fatal,
    
    # Demographics
    sex, age, rel_hhh, caste, marital,
    
    # Migration outcomes
    international_migrant, international_absentee_only, present_ind_migrant,
    occupation_types, absent, travelled5, rsn_travel, abs_rsn, 
    abs_nummonth, abs_living, abs_id,
    
    # Education
    grade_comp, can_read, can_write, current_school, ever_school,
    
    # Everything else
    everything()
  )

# -----------------------------------------------------------------------------
# 3. HANDLE MISSING VALUES
# -----------------------------------------------------------------------------

# Drop observations with missing district
nlss_conflict_data <- nlss_conflict_data %>%
  drop_na(dist)

# Fill missing conflict values with district-level values
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

# Assign absentees the same caste as household
nlss_conflict_data <- nlss_conflict_data %>%
  group_by(psu, hhld) %>%
  mutate(caste = first(na.omit(caste))) %>%
  ungroup()

# -----------------------------------------------------------------------------
# 4. CREATE ETHNICITY CATEGORIES
# -----------------------------------------------------------------------------

nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    Ethnicity = case_when(
      # Hill High Caste (Tagadhari)
      caste %in% c(1, 2, 14, 20, 27, 48, 49) ~ "Hill High Caste",
      
      # Hill Janajati (Indigenous)
      caste %in% c(3, 5, 6, 10, 11, 13, 24, 29, 32, 36, 45, 46, 60, 61, 62, 
                   66, 67, 69, 74, 77, 78, 79, 80, 81, 89, 90, 91, 92, 94, 
                   97, 98, 100, 110, 119, 120, 121, 124, 125, 126, 127, 130,
                   131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 992) ~ "Hill Janajati",
      
      # Terai/Madhesi Caste
      caste %in% c(4, 9, 16, 18, 19, 21, 26, 28, 30, 31, 33, 34, 35, 37, 
                   42, 43, 44, 47, 51, 52, 53, 54, 55, 56, 57, 58, 59, 
                   63, 64, 65, 68, 71, 72, 73, 84, 85, 86, 88, 96, 99,
                   115, 116, 117, 118, 122, 123, 128, 129, 993) ~ "Terai/Madhesi",
      
      # Dalit
      caste %in% c(8, 12, 15, 17, 22, 23, 25, 38, 39, 40, 41, 50, 70, 
                   75, 76, 83, 93, 991) ~ "Dalit",
      
      # Muslim
      caste == 7 ~ "Muslim",
      
      TRUE ~ NA_character_
    )
  )


# -----------------------------------------------------------------------------
# 5. CREATE EDUCATION CATEGORIES
# -----------------------------------------------------------------------------

nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    education_category = case_when(
      grade_comp %in% c(16, 17) ~ "No Education",
      grade_comp >= 0 & grade_comp <= 5 ~ "Primary (1-5)",
      grade_comp >= 6 & grade_comp <= 12 ~ "Secondary (6-12)",
      grade_comp >= 13 ~ "Tertiary",
      TRUE ~ NA_character_
    )
  )



# -----------------------------------------------------------------------------
# 6. CREATE TREATMENT/CONTROL COHORTS
# -----------------------------------------------------------------------------

nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    # Calculate ages
    birth_year = SURVEY_YEAR - age,
    age_at_conflict_start = CONFLICT_START - birth_year,
    age_at_conflict_end = CONFLICT_END - birth_year,
    
# Detailed cohort labels
    cohort_group = case_when(
      # TREATMENT: Childhood during conflict
      age_at_conflict_start >= 0 & age_at_conflict_start <= 5 & 
        age >= 18 & age <= 45 ~ "Treatment: Age 0-5 in 1996",
      
      age_at_conflict_start >= 6 & age_at_conflict_start <= 12 & 
        age >= 18 & age <= 45 ~ "Treatment: Age 6-12 in 1996",
      
      age_at_conflict_start >= 13 & age_at_conflict_start <= 17 & 
        age >= 18 & age <= 45 ~ "Treatment: Age 13-17 in 1996",
      
      # CONTROL: Adult during conflict
      age_at_conflict_start >= 18 & age_at_conflict_start <= 25 & 
        age >= 47 & age <= 65 ~ "Control: Age 18-25 in 1996",
      
      age_at_conflict_start >= 26 & age_at_conflict_start <= 35 & 
        age >= 47 & age <= 65 ~ "Control: Age 26-35 in 1996",
      
      age_at_conflict_start >= 36 & age_at_conflict_start <= 40 & 
        age >= 47 & age <= 65 ~ "Control: Age 36-40 in 1996",
      
      # EXCLUDED
      age < 18 ~ "Excluded: Too Young in 2017",
      age > 65 ~ "Excluded: Too Old in 2017",
      age_at_conflict_end < 6 ~ "Excluded: Too Young During Conflict",
      age_at_conflict_start >= 41 ~ "Excluded: Age 41+ in 1996",
      age_at_conflict_start >= 18 & age >= 18 & age <= 46 ~ "Excluded: Overlap Age",
      TRUE ~ "Excluded: Other"
    ),
    
    # Binary treatment indicator
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
    ),
    
    # Treatment variable for regressions
    treatment = childhood_exposed
  )


# -----------------------------------------------------------------------------
# 7. CREATE CONFLICT INTENSITY MEASURES
# -----------------------------------------------------------------------------

# Q3 cutoff for months of war
q3_war <- quantile(nlss_conflict_data$mwar_own_any[nlss_conflict_data$mwar_own_any > 0],
                   probs = 0.75, na.rm = TRUE)

# Q3 cutoff for casualties
q3_casualty <- quantile(nlss_conflict_data$cas_own_any[nlss_conflict_data$cas_own_any > 0],
                        probs = 0.75, na.rm = TRUE)

nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    # War-based binary
    high_conflict_q3_binary = case_when(
      mwar_own_any > q3_war ~ 1,
      mwar_own_any <= q3_war ~ 0,
      TRUE ~ NA_real_
    ),
    high_conflict_q3_label = factor(
      case_when(
        mwar_own_any > q3_war ~ "High Conflict",
        mwar_own_any <= q3_war ~ "Low Conflict",
        TRUE ~ NA_character_
      ),
      levels = c("Low Conflict", "High Conflict")
    ),
    
    # Casualty-based binary
    high_conflict_casualty_binary = case_when(
      cas_own_any > q3_casualty ~ 1,
      cas_own_any <= q3_casualty ~ 0,
      TRUE ~ NA_real_
    ),
    high_conflict_casualty_label = factor(
      case_when(
        cas_own_any > q3_casualty ~ "High Conflict",
        cas_own_any <= q3_casualty ~ "Low Conflict",
        TRUE ~ NA_character_
      ),
      levels = c("Low Conflict", "High Conflict")
    )
  )

