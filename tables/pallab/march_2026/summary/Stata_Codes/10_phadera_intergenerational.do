/*
================================================================================
10_phadera_intergenerational.do

Replicates Phadera (2021) Equation 8 - Intergenerational specification.
Child outcome = f(mother's conflict exposure × mother's cohort) + child controls

Adaptation for NLFS data with migration as outcome:
- Link mothers (female head/spouse/parent) to children (son/daughter/grandchild)
  within households
- Mother's conflict exposure = months of war / casualties in district
- Mother's cohort = age at conflict start (same as Eq 7)
- Child outcomes = international migration, domestic migration
- Child controls = child birth year FE, child sex (girl indicator), birth order FE

Outputs:
  1. Mother-child pairs summary
  2. Table 6 (Eq 8 with Phadera cohorts)
  3. Table 7 (Eq 8 with user-defined age groups)
  4. CSV, LaTeX output
================================================================================
*/

clear all
set more off
set matsize 11000

// ══════════════════════════════════════════════════════════════════════════════
// CONFIGURATION & PATHS
// ══════════════════════════════════════════════════════════════════════════════

global BASE_PATH "/Users/pallab.ghosh/Documents/GitHub/Nepal-Civil-War-and-Int-Migration"
global DATA_PATH "$BASE_PATH/data/Modified_Data/1_conflict_present_absentee_data.dta"
global OUT_DIR "$BASE_PATH/tables/pallab/march_2026/summary/results"
global TEMP_DIR "$BASE_PATH/temp"

capture mkdir "$OUT_DIR"
capture mkdir "$TEMP_DIR"

global SURVEY_YEAR = 2017
global CONFLICT_START = 1996
global CONFLICT_END = 2006

// ══════════════════════════════════════════════════════════════════════════════
// LOAD DATA
// ══════════════════════════════════════════════════════════════════════════════

di "Loading data from: $DATA_PATH"
use "$DATA_PATH", clear

// ══════════════════════════════════════════════════════════════════════════════
// DATA PREPARATION
// ══════════════════════════════════════════════════════════════════════════════

// Numeric versions of key variables
destring sex, replace
destring age, replace
gen sex_num = sex
gen age_num = age
gen male = (sex_num == 1)
gen female = (sex_num == 2)
gen girl = female
gen birth_year = $SURVEY_YEAR - age_num
gen age_at_conflict_start = $CONFLICT_START - birth_year
gen age_at_conflict_end = $CONFLICT_END - birth_year

// Casualty variables
foreach var in cas_own_any cas_own_fatal mwar_own_any mwar_own_fatal ///
              cas_nbr_any cas_nbr_fatal mwar_nbr_any mwar_nbr_fatal {
  destring `var', replace
}

// Fill missing casualty data using district mapping
foreach var in cas_own_any cas_own_fatal mwar_own_any mwar_own_fatal ///
              cas_nbr_any cas_nbr_fatal mwar_nbr_any mwar_nbr_fatal {
  bysort dist: egen temp_`var' = first(`var')
  replace `var' = temp_`var' if missing(`var')
  drop temp_`var'
}

// Ethnicity
destring caste, replace
gen caste_num = caste
sort psu hhld
by psu hhld: egen temp_caste = mode(caste_num)
replace caste_num = temp_caste if missing(caste_num)
drop temp_caste

// High caste indicator
gen high_caste = inlist(caste_num, 1, 2, 14, 20, 27, 48, 49)
replace high_caste = . if missing(caste_num)

// Migration outcomes
gen intl_mig = international_absentee_only
gen dom_mig = national
destring intl_mig, replace
destring dom_mig, replace

// District and PSU numeric
destring dist, replace
destring psu, replace
gen dist_num = dist
gen psu_num = psu
destring rel_hhh, replace

// Save full dataset
save "$TEMP_DIR/phadera_full_data.dta", replace

di "Data loaded: " _N " rows"

// ══════════════════════════════════════════════════════════════════════════════
// LINK MOTHERS TO CHILDREN WITHIN HOUSEHOLDS
// ══════════════════════════════════════════════════════════════════════════════

di ""
di strrep("=", 70)
di "  LINKING MOTHERS TO CHILDREN WITHIN HOUSEHOLDS"
di strrep("=", 70)

// Extract mothers (female, rel_hhh in {1, 2, 6})
keep if sex_num == 2 & inlist(rel_hhh, 1, 2, 6)

rename age_num mother_age
rename birth_year mother_birth_year
rename age_at_conflict_start mother_age_at_conflict_start
rename high_caste mother_high_caste
gen mother_idx = _n

keep psu hhld mother_idx mother_age mother_birth_year mother_age_at_conflict_start ///
     mother_high_caste dist_num psu_num mwar_own_any cas_own_any mwar_own_fatal cas_own_fatal

save "$TEMP_DIR/mothers_data.dta", replace

// Extract children (rel_hhh in {3, 4, 5})
use "$TEMP_DIR/phadera_full_data.dta", clear
keep if inlist(rel_hhh, 3, 4, 5)

rename age_num child_age
rename birth_year child_birth_year
rename sex_num child_sex
gen child_idx = _n

keep psu hhld child_idx child_age child_birth_year child_sex girl intl_mig dom_mig

save "$TEMP_DIR/children_data.dta", replace

// Merge mothers to children
merge m:m psu hhld using "$TEMP_DIR/mothers_data.dta", keep(match)

// Apply age gap constraints (15-50 years)
gen age_gap = mother_age - child_age
keep if age_gap >= 15 & age_gap <= 50

di "Mother-child pairs: " _N

// If multiple mothers, keep the one closest to typical motherhood age (25)
gen age_gap_abs = abs(age_gap - 25)
sort child_idx age_gap_abs
by child_idx: keep if _n == 1

di "After de-duplication: " _N " pairs"

// Compute birth order within household by mother
sort psu hhld mother_idx child_age
by psu hhld mother_idx: gen birth_order = _N - _n + 1
replace birth_order = 6 if birth_order > 6

// ══════════════════════════════════════════════════════════════════════════════
// CREATE MOTHER'S COHORT VARIABLES
// ══════════════════════════════════════════════════════════════════════════════

// Phadera cohorts
gen m_cohort_0_3 = (mother_age_at_conflict_start >= 0 & mother_age_at_conflict_start <= 3)
gen m_cohort_4_8 = (mother_age_at_conflict_start >= 4 & mother_age_at_conflict_start <= 8)
gen m_cohort_9_15 = (mother_age_at_conflict_start >= 9 & mother_age_at_conflict_start <= 15)
gen m_cohort_16_21 = (mother_age_at_conflict_start >= 16 & mother_age_at_conflict_start <= 21)  // control
gen m_cohort_22_29 = (mother_age_at_conflict_start >= 22 & mother_age_at_conflict_start <= 29)  // placebo

// User-defined age groups
gen m_cohort_0_5 = (mother_age_at_conflict_start >= 0 & mother_age_at_conflict_start <= 5)
gen m_cohort_6_10 = (mother_age_at_conflict_start >= 6 & mother_age_at_conflict_start <= 10)
gen m_cohort_11_18 = (mother_age_at_conflict_start >= 11 & mother_age_at_conflict_start <= 18)
gen m_cohort_19_25 = (mother_age_at_conflict_start >= 19 & mother_age_at_conflict_start <= 25)  // control
gen m_cohort_26_40 = (mother_age_at_conflict_start >= 26 & mother_age_at_conflict_start <= 40)  // placebo

// Create interactions: mother's conflict × mother's cohort
foreach cv in mwar_own_any cas_own_any {
  foreach cn in 0_3 4_8 9_15 22_29 0_5 6_10 11_18 26_40 {
    gen `cv'_x_m`cn' = `cv' * m_cohort_`cn'
  }
}

// Fill high_caste NAs with 0
replace mother_high_caste = 0 if missing(mother_high_caste)

// Keep sample: mothers aged 0-40 at conflict start
keep if mother_age_at_conflict_start >= 0 & mother_age_at_conflict_start <= 40
keep if !missing(mwar_own_any) & !missing(child_age)

di "Regression sample (mothers age 0-40 at CS): " _N
di "  Mother age at conflict start:"
summarize mother_age_at_conflict_start, detail
di "  Child age:"
summarize child_age, detail

// ══════════════════════════════════════════════════════════════════════════════
// RUN EQUATION 8 REGRESSIONS
// ══════════════════════════════════════════════════════════════════════════════

di ""
di strrep("=", 70)
di "  RUNNING EQUATION 8: INTERGENERATIONAL REGRESSIONS"
di strrep("=", 70)

eststo clear

// Phadera cohorts (0-3, 4-8, 9-15, control 16-21, placebo 22-29)
foreach outcome in intl_mig dom_mig {
  foreach conflict_var in mwar_own_any cas_own_any {

    reghdfe `outcome' c.`conflict_var'#i.m_cohort_0_3 ///
                      c.`conflict_var'#i.m_cohort_4_8 ///
                      c.`conflict_var'#i.m_cohort_9_15 ///
                      c.`conflict_var'#i.m_cohort_22_29 ///
                      mother_high_caste girl i.birth_order, ///
            absorb(dist_num) vce(cluster psu_num) nostd

    local key "`outcome'_`conflict_var'_phadera_eq8"
    eststo `key'

    di "  `outcome' ~ `conflict_var' (Phadera Eq8): N=" e(N) ", R2=" string(e(r2_a), "%9.4f")
  }
}

// User-defined age groups (0-5, 6-10, 11-18, control 19-25, placebo 26-40)
foreach outcome in intl_mig dom_mig {
  foreach conflict_var in mwar_own_any cas_own_any {

    reghdfe `outcome' c.`conflict_var'#i.m_cohort_0_5 ///
                      c.`conflict_var'#i.m_cohort_6_10 ///
                      c.`conflict_var'#i.m_cohort_11_18 ///
                      c.`conflict_var'#i.m_cohort_26_40 ///
                      mother_high_caste girl i.birth_order, ///
            absorb(dist_num) vce(cluster psu_num) nostd

    local key "`outcome'_`conflict_var'_agegroups_eq8"
    eststo `key'

    di "  `outcome' ~ `conflict_var` (Age Groups Eq8): N=" e(N) ", R2=" string(e(r2_a), "%9.4f")
  }
}

// ══════════════════════════════════════════════════════════════════════════════
// WRITE RESULTS
// ══════════════════════════════════════════════════════════════════════════════

di ""
di strrep("=", 70)
di "  WRITING EQUATION 8 RESULTS"
di strrep("=", 70)

// CSV output - Phadera cohorts
esttab intl_mig_mwar_phadera_eq8 intl_mig_cas_phadera_eq8 ///
       dom_mig_mwar_phadera_eq8 dom_mig_cas_phadera_eq8 ///
       using "$OUT_DIR/phadera_replication_eq8_phadera_cohorts.csv", ///
       se r2 ar2 scalar(N) replace plain

// CSV output - Age groups
esttab intl_mig_mwar_agegroups_eq8 intl_mig_cas_agegroups_eq8 ///
       dom_mig_mwar_agegroups_eq8 dom_mig_cas_agegroups_eq8 ///
       using "$OUT_DIR/phadera_replication_eq8_age_groups.csv", ///
       se r2 ar2 scalar(N) replace plain

di "  Saved CSV: $OUT_DIR/phadera_replication_eq8_*.csv"

// LaTeX output - Phadera cohorts
esttab intl_mig_mwar_phadera_eq8 intl_mig_cas_phadera_eq8 ///
       dom_mig_mwar_phadera_eq8 dom_mig_cas_phadera_eq8 ///
       using "$OUT_DIR/phadera_replication_eq8_phadera_cohorts.tex", ///
       se r2 ar2 scalar(N) replace booktabs ///
       title("Table 6: Intergenerational Impact on Children's Migration (Equation 8, Phadera Cohorts)") ///
       label nonotes alignment(D{.}{.}{-1}) ///
       mtitle("Intl (Months)" "Intl (Cas)" "Dom (Months)" "Dom (Cas)")

// LaTeX output - Age groups
esttab intl_mig_mwar_agegroups_eq8 intl_mig_cas_agegroups_eq8 ///
       dom_mig_mwar_agegroups_eq8 dom_mig_cas_agegroups_eq8 ///
       using "$OUT_DIR/phadera_replication_eq8_age_groups.tex", ///
       se r2 ar2 scalar(N) replace booktabs ///
       title("Table 7: Intergenerational Impact on Children's Migration (Equation 8, Age Groups)") ///
       label nonotes alignment(D{.}{.}{-1}) ///
       mtitle("Intl (Months)" "Intl (Cas)" "Dom (Months)" "Dom (Cas)")

di "  Saved LaTeX: $OUT_DIR/phadera_replication_eq8_*.tex"

// ══════════════════════════════════════════════════════════════════════════════
// CLEANUP
// ══════════════════════════════════════════════════════════════════════════════

capture erase "$TEMP_DIR/phadera_full_data.dta"
capture erase "$TEMP_DIR/mothers_data.dta"
capture erase "$TEMP_DIR/children_data.dta"

// ══════════════════════════════════════════════════════════════════════════════
// SUMMARY
// ══════════════════════════════════════════════════════════════════════════════

di ""
di strrep("=", 70)
di "  EQUATION 8 COMPLETE!"
di strrep("=", 70)
di "  Results saved to: $OUT_DIR"
