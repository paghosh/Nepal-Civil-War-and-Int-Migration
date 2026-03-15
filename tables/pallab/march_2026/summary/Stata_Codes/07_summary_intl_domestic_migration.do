* =============================================================================
* 07_summary_intl_domestic_migration.do
* Summary Statistics: International vs. Domestic Migration Channels
* With Treatment-Control Difference and Welch's t-test p-values
* =============================================================================
* Project: Nepal Civil Conflict and International Migration
* Author:  Pallab Ghosh
* Date:    March 2026
*
* REQUIRES: nlfs_conflict_data.dta (from 06_Final_Data.do)
*           02_data_cleaning.R variables must be constructed
*
* OUTPUTS:
*   Table_A_International_Migration_Summary.tex  (with Diff T-C column)
*   Table_B_Domestic_Migration_Summary.tex       (with Diff T-C column)
*
* KEY FINDING:
*   Two distinct migration channels emerge from the Nepal civil conflict:
*   (1) International: older, male-dominated, low-skilled economic migration
*   (2) Domestic: younger, more educated, conflict-displacement driven
* =============================================================================

clear all
set more off

* ── Paths ────────────────────────────────────────────────────────────────────

global dropbox "/Users/pallab.ghosh/Library/CloudStorage/Dropbox/Papers_with_Coauthors/PhD_Students/Ramesh/Nepal Civil Conflict"
global results "/Users/pallab.ghosh/Documents/GitHub/Nepal-Civil-War-and-Int-Migration/tables/pallab/march_2026/summary_stats"

cd "$dropbox"

* ── Load data ────────────────────────────────────────────────────────────────

use "Modified_Data/nlfs_conflict_data.dta", clear

* ── Constants ────────────────────────────────────────────────────────────────

local SURVEY_YEAR = 2017
local CONFLICT_START = 1996

* ── Reconstruct derived variables ────────────────────────────────────────────

* Age at conflict start
gen birth_year = `SURVEY_YEAR' - age
gen age_at_conflict_start = `CONFLICT_START' - birth_year

* Treatment/Control assignment
gen treatment = .
replace treatment = 1 if age_at_conflict_start >= 0 & age_at_conflict_start <= 17 ///
    & age >= 18 & age <= 45
replace treatment = 0 if age_at_conflict_start >= 18 & age_at_conflict_start <= 40 ///
    & age >= 47 & age <= 65

* Male indicator
gen male = (sex == 1) if !missing(sex)

* Education dummies (grade_comp is string in this dta, convert)
destring grade_comp, gen(grade_num) force
gen edu_no_education = (grade_num == 16 | grade_num == 17) if !missing(grade_num)
gen edu_primary = (grade_num >= 0 & grade_num <= 5) if !missing(grade_num)
gen edu_secondary = (grade_num >= 6 & grade_num <= 12) if !missing(grade_num)
gen edu_tertiary = (grade_num >= 13) if !missing(grade_num)

* Ethnicity dummies (caste is numeric)
destring caste, gen(caste_num) force
gen eth_hill_high = inlist(caste_num, 1, 2, 14, 20, 27, 48, 49)
gen eth_janajati = inlist(caste_num, 3, 5, 6, 10, 11, 13, 24, 29, 32, 36, 45, 46)
replace eth_janajati = 1 if inlist(caste_num, 60, 61, 62, 66, 67, 69, 74, 77, 78, 79)
replace eth_janajati = 1 if inlist(caste_num, 80, 81, 89, 90, 91, 92, 94, 97, 98, 100)
replace eth_janajati = 1 if inlist(caste_num, 110, 119, 120, 121, 124, 125, 126, 127, 130)
replace eth_janajati = 1 if inlist(caste_num, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 992)
gen eth_dalit = inlist(caste_num, 8, 12, 15, 17, 22, 23, 25, 38, 39, 40, 41)
replace eth_dalit = 1 if inlist(caste_num, 50, 70, 75, 76, 83, 93, 991)
gen eth_muslim = (caste_num == 7)
gen eth_terai = inlist(caste_num, 4, 9, 16, 18, 19, 21, 26, 28, 30, 31)
replace eth_terai = 1 if inlist(caste_num, 33, 34, 35, 37, 42, 43, 44, 47, 51, 52)
replace eth_terai = 1 if inlist(caste_num, 53, 54, 55, 56, 57, 58, 59, 63, 64, 65)
replace eth_terai = 1 if inlist(caste_num, 68, 71, 72, 73, 84, 85, 86, 88, 96, 99)
replace eth_terai = 1 if inlist(caste_num, 115, 116, 117, 118, 122, 123, 128, 129, 993)

* Occupation dummies
destring occupation_types, gen(occ_num) force
gen nsco_major = floor(occ_num / 100) if occ_num < 1000
replace nsco_major = floor(occ_num / 1000) if occ_num >= 1000

gen occ_armed = inlist(occ_num, 110, 210, 310)
gen occ_high_skilled = inlist(nsco_major, 1, 2, 3) & occ_armed == 0
gen occ_service = inlist(nsco_major, 4, 5)
gen occ_agriculture = (nsco_major == 6)
gen occ_craft = inlist(nsco_major, 7, 8)
gen occ_elementary = (nsco_major == 9)

* Baseline indicator
gen baseline = (absent == 0 & international_migrant == 0)

* =============================================================================
* T-TEST: Treatment vs Control — All variables
* =============================================================================

local all_vars "age age_at_conflict_start male" ///
    " edu_no_education edu_primary edu_secondary edu_tertiary" ///
    " eth_hill_high eth_janajati eth_terai eth_dalit eth_muslim" ///
    " occ_agriculture occ_high_skilled occ_service occ_craft occ_elementary occ_armed"

display _newline(2)
display "═══════════════════════════════════════════════════════════════════"
display "  WELCH'S T-TEST: Treatment (aged 0-17 in 1996) vs Control"
display "═══════════════════════════════════════════════════════════════════"
display _newline
display "{hline 80}"
display %30s "Variable" %15s "Diff (T-C)" %15s "p-value" %10s "Sig"
display "{hline 80}"

foreach var of local all_vars {
    quietly ttest `var', by(treatment) unequal welch
    local diff = r(mu_1) - r(mu_2)
    local pval = r(p)
    local sig = ""
    if `pval' < 0.01 local sig = "***"
    else if `pval' < 0.05 local sig = "**"
    else if `pval' < 0.10 local sig = "*"
    display %30s "`var'" %15.3f `diff' %15.4f `pval' %10s "`sig'"
}

display "{hline 80}"

* =============================================================================
* TABLE A: INTERNATIONAL MIGRATION — with Diff (T-C) column
* =============================================================================

* Store group means
estpost summarize `all_vars' if baseline == 1, detail
estimates store baseline

estpost summarize `all_vars' if international_absentee_only == 1, detail
estimates store intl_absentee

estpost summarize `all_vars' if treatment == 1, detail
estimates store treat

estpost summarize `all_vars' if treatment == 0, detail
estimates store ctrl

* T-test for Diff column
estpost ttest `all_vars', by(treatment) unequal welch
estimates store ttest_diff

esttab baseline intl_absentee treat ctrl ttest_diff ///
    using "$results/Table_A_International_Migration_Summary.tex", ///
    cells("mean(fmt(2)) sd(fmt(2) par)") ///
    label replace booktabs ///
    title("International Migration: Summary Statistics with T-C Difference") ///
    mtitles("Baseline" "Intl. Absentee" "Treatment" "Control" "Diff (T-C)") ///
    addnotes("Standard deviations in parentheses." ///
             "Diff column: Treatment minus Control with Welch's t-test p-value." ///
             "Baseline: present at survey and never migrated internationally." ///
             "Intl. Absentee: absent and currently abroad." ///
             "Treatment: aged 0-17 at conflict start (1996)." ///
             "Control: aged 18-40 at conflict start (1996)." ///
             "*** p<0.01, ** p<0.05, * p<0.10")

* =============================================================================
* TABLE B: DOMESTIC (NATIONAL) MIGRATION — with Diff (T-C) column
* =============================================================================

estpost summarize `all_vars' if national == 1, detail
estimates store national

esttab baseline national treat ctrl ttest_diff ///
    using "$results/Table_B_Domestic_Migration_Summary.tex", ///
    cells("mean(fmt(2)) sd(fmt(2) par)") ///
    label replace booktabs ///
    title("Domestic Migration: Summary Statistics with T-C Difference") ///
    mtitles("Baseline" "National Absent" "Treatment" "Control" "Diff (T-C)") ///
    addnotes("Standard deviations in parentheses." ///
             "Diff column: Treatment minus Control with Welch's t-test p-value." ///
             "Baseline: present at survey and never migrated internationally." ///
             "National Absent: absent and migrating inside Nepal." ///
             "Treatment: aged 0-17 at conflict start (1996)." ///
             "Control: aged 18-40 at conflict start (1996)." ///
             "*** p<0.01, ** p<0.05, * p<0.10")


display _newline(2)
display "────────────────────────────────────────────────────────────────"
display "Tables saved to: $results"
display "Both tables include Diff (T-C) with Welch's t-test p-values"
display "────────────────────────────────────────────────────────────────"
