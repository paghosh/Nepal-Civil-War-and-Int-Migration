/*
================================================================================
09_phadera_replication.do

Replicates the empirical strategy from Phadera (2021) "Unfortunate Moms and
Unfortunate Children" using NLFS data with MIGRATION as the outcome variable
instead of height.

Key adaptation:
  - Original paper: Y = adult height (women), using NDHS 2016
  - This replication: Y = international migration / domestic migration, using NLFS
  - Same DID structure: conflict_var × cohort (Equation 7)
  - Same intergenerational framework: Equation 8 (separate do file)

Outputs:
  1. Summary statistics tables
  2. Table 4 style DID regression results (Eq 7) for migration outcomes
  3. Results for both Phadera cohorts (0-3, 4-8, 9-15, 16-21, 22-29)
     and user-defined age groups (0-5, 6-10, 11-18, 19-25, 26-40)
  4. Excel and LaTeX output with formatted regression results
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
global FIG_DIR "$BASE_PATH/tables/pallab/march_2026/summary/figures"

capture mkdir "$FIG_DIR"
capture mkdir "$OUT_DIR"

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
gen birth_year = $SURVEY_YEAR - age_num
gen age_at_conflict_start = $CONFLICT_START - birth_year
gen age_at_conflict_end = $CONFLICT_END - birth_year

// Casualty variables - convert to numeric
foreach var in cas_own_any cas_own_fatal mwar_own_any mwar_own_fatal ///
              cas_nbr_any cas_nbr_fatal mwar_nbr_any mwar_nbr_fatal {
  destring `var', replace
}

// Fill missing casualty data for absentees using district-level mapping
foreach var in cas_own_any cas_own_fatal mwar_own_any mwar_own_fatal ///
              cas_nbr_any cas_nbr_fatal mwar_nbr_any mwar_nbr_fatal {
  bysort dist: egen temp_`var' = first(`var')
  replace `var' = temp_`var' if missing(`var')
  drop temp_`var'
}

// Ethnicity and high caste coding
// Hill high castes = {1, 2, 14, 20, 27, 48, 49}
destring caste, replace
gen caste_num = caste

// Forward-fill and backward-fill caste within households
sort psu hhld
by psu hhld: egen temp_caste = mode(caste_num)
replace caste_num = temp_caste if missing(caste_num)
drop temp_caste

// Create high_caste indicator
gen high_caste = inlist(caste_num, 1, 2, 14, 20, 27, 48, 49)
replace high_caste = . if missing(caste_num)

// Migration outcomes (binary)
gen intl_mig = international_absentee_only
gen dom_mig = national

destring intl_mig, replace
destring dom_mig, replace

// In utero indicator
gen in_utero = (birth_year >= 1996 & birth_year <= 1997)

// District and PSU numeric identifiers
destring dist, replace
destring psu, replace
gen dist_num = dist
gen psu_num = psu

destring rel_hhh, replace

di "Data loaded: " _N " rows"
di "Age at conflict start range: " cond(missing(age_at_conflict_start[1]), ".", ///
    string(int(min(age_at_conflict_start)))) " to " ///
    string(int(max(age_at_conflict_start)))

// ══════════════════════════════════════════════════════════════════════════════
// CREATE COHORT VARIABLES (EQUATION 7)
// ══════════════════════════════════════════════════════════════════════════════

// Phadera-style cohorts
gen cohort_0_3 = (age_at_conflict_start >= 0 & age_at_conflict_start <= 3)
gen cohort_4_8 = (age_at_conflict_start >= 4 & age_at_conflict_start <= 8)
gen cohort_9_15 = (age_at_conflict_start >= 9 & age_at_conflict_start <= 15)
gen cohort_16_21 = (age_at_conflict_start >= 16 & age_at_conflict_start <= 21)  // control
gen cohort_22_29 = (age_at_conflict_start >= 22 & age_at_conflict_start <= 29)  // placebo

// User-defined age groups
gen cohort_0_5 = (age_at_conflict_start >= 0 & age_at_conflict_start <= 5)
gen cohort_6_10 = (age_at_conflict_start >= 6 & age_at_conflict_start <= 10)
gen cohort_11_18 = (age_at_conflict_start >= 11 & age_at_conflict_start <= 18)
gen cohort_19_25 = (age_at_conflict_start >= 19 & age_at_conflict_start <= 25)  // control
gen cohort_26_40 = (age_at_conflict_start >= 26 & age_at_conflict_start <= 40)  // placebo

// Interaction terms: conflict × cohort
foreach cv in mwar_own_any cas_own_any {
  foreach cn in 0_3 4_8 9_15 22_29 0_5 6_10 11_18 26_40 {
    gen `cv'_x_`cn' = `cv' * cohort_`cn'
  }
}

// Fill high_caste NAs with 0
replace high_caste = 0 if missing(high_caste)

// ══════════════════════════════════════════════════════════════════════════════
// TABLE 1: SUMMARY STATISTICS - ALL INDIVIDUALS
// ══════════════════════════════════════════════════════════════════════════════

di ""
di strrep("=", 70)
di "  GENERATING TABLE 1: Summary Statistics (All Individuals)"
di strrep("=", 70)

// Define treatment and control groups
keep if age_at_conflict_start >= 0 & age_at_conflict_start <= 40
gen treat_group = (age_at_conflict_start >= 0 & age_at_conflict_start <= 17)
gen ctrl_group = (age_at_conflict_start >= 18 & age_at_conflict_start <= 40)

// Summary statistics by group and variable
local vars intl_mig dom_mig mwar_own_any cas_own_any mwar_nbr_any cas_nbr_any age_num male high_caste

eststo clear

// ══════════════════════════════════════════════════════════════════════════════
// TABLE 4: DID REGRESSION - EQUATION 7
// ══════════════════════════════════════════════════════════════════════════════

di ""
di strrep("=", 70)
di "  GENERATING TABLE 4: DID Regressions (Equation 7)"
di strrep("=", 70)

// Prepare regression sample
keep if age_at_conflict_start >= 0 & age_at_conflict_start <= 40
keep if !missing(mwar_own_any) & !missing(age_num)

// Run regressions for each outcome and conflict variable
eststo clear

// ─── Phadera cohorts regressions ───
foreach outcome in intl_mig dom_mig {
  foreach conflict_var in mwar_own_any cas_own_any {

    // Phadera cohorts (0-3, 4-8, 9-15, control 16-21, placebo 22-29)
    reghdfe `outcome' c.`conflict_var'#i.cohort_0_3 ///
                      c.`conflict_var'#i.cohort_4_8 ///
                      c.`conflict_var'#i.cohort_9_15 ///
                      c.`conflict_var'#i.cohort_22_29 ///
                      high_caste, ///
            absorb(dist_num) vce(cluster psu_num) nostd

    local key "`outcome'_`conflict_var'_phadera"
    eststo `key'

    di "  `outcome' ~ `conflict_var' (Phadera cohorts): N=" e(N) ", R2=" string(e(r2_a), "%9.4f")
  }
}

// ─── User-defined age groups regressions ───
foreach outcome in intl_mig dom_mig {
  foreach conflict_var in mwar_own_any cas_own_any {

    // User age groups (0-5, 6-10, 11-18, control 19-25, placebo 26-40)
    reghdfe `outcome' c.`conflict_var'#i.cohort_0_5 ///
                      c.`conflict_var'#i.cohort_6_10 ///
                      c.`conflict_var'#i.cohort_11_18 ///
                      c.`conflict_var'#i.cohort_26_40 ///
                      high_caste, ///
            absorb(dist_num) vce(cluster psu_num) nostd

    local key "`outcome'_`conflict_var'_agegroups"
    eststo `key'

    di "  `outcome' ~ `conflict_var' (Age groups): N=" e(N) ", R2=" string(e(r2_a), "%9.4f")
  }
}

// ══════════════════════════════════════════════════════════════════════════════
// WRITE RESULTS TO EXCEL
// ══════════════════════════════════════════════════════════════════════════════

di ""
di strrep("=", 70)
di "  WRITING EXCEL OUTPUT"
di strrep("=", 70)

// Use esttab to export to Excel
eststo intl_mig_mwar_phadera intl_mig_cas_phadera ///
       dom_mig_mwar_phadera dom_mig_cas_phadera ///
       intl_mig_mwar_agegroups intl_mig_cas_agegroups ///
       dom_mig_mwar_agegroups dom_mig_cas_agegroups

esttab intl_mig_mwar_phadera intl_mig_cas_phadera ///
       dom_mig_mwar_phadera dom_mig_cas_phadera ///
       using "$OUT_DIR/phadera_replication_eq7_phadera_cohorts.csv", ///
       se r2 ar2 scalar(N) replace plain

esttab intl_mig_mwar_agegroups intl_mig_cas_agegroups ///
       dom_mig_mwar_agegroups dom_mig_cas_agegroups ///
       using "$OUT_DIR/phadera_replication_eq7_age_groups.csv", ///
       se r2 ar2 scalar(N) replace plain

di "  Saved CSV: $OUT_DIR/phadera_replication_eq7_*.csv"

// ══════════════════════════════════════════════════════════════════════════════
// WRITE RESULTS TO LATEX
// ══════════════════════════════════════════════════════════════════════════════

di ""
di "  Writing LaTeX output..."

esttab intl_mig_mwar_phadera intl_mig_cas_phadera ///
       dom_mig_mwar_phadera dom_mig_cas_phadera ///
       using "$OUT_DIR/phadera_replication_eq7_phadera.tex", ///
       se r2 ar2 scalar(N) replace booktabs ///
       title("Table 4: DID Impact on Migration (Equation 7, Phadera Cohorts)") ///
       label nonotes alignment(D{.}{.}{-1}) ///
       mtitle("Intl (Months)" "Intl (Cas)" "Dom (Months)" "Dom (Cas)")

esttab intl_mig_mwar_agegroups intl_mig_cas_agegroups ///
       dom_mig_mwar_agegroups dom_mig_cas_agegroups ///
       using "$OUT_DIR/phadera_replication_eq7_age_groups.tex", ///
       se r2 ar2 scalar(N) replace booktabs ///
       title("Table 5: DID Impact on Migration (Equation 7, Age Groups)") ///
       label nonotes alignment(D{.}{.}{-1}) ///
       mtitle("Intl (Months)" "Intl (Cas)" "Dom (Months)" "Dom (Cas)")

di "  Saved LaTeX: $OUT_DIR/phadera_replication_eq7_*.tex"

// ══════════════════════════════════════════════════════════════════════════════
// SUMMARY
// ══════════════════════════════════════════════════════════════════════════════

di ""
di strrep("=", 70)
di "  EQUATION 7 COMPLETE!"
di strrep("=", 70)
di "  Results saved to: $OUT_DIR"
