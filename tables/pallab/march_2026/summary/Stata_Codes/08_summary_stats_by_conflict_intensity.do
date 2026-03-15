********************************************************************************
* 08_summary_stats_by_conflict_intensity.do
*
* Summary Statistics Tables by Conflict Intensity (Casualty-Median T/C)
*
* 3 Panels:
*   Panel A: Outcomes (International Migration, Domestic Migration rates)
*   Panel B: Education (No Education, Primary, Secondary, Tertiary)
*   Panel C: Occupation (Agriculture, High Skilled, Service & Clerical,
*            Craft & Manufacturing, Elementary/Low Skilled, Armed Forces)
*
* Treatment = districts with casualty measure ABOVE median
* Control   = districts with casualty measure AT OR BELOW median
*
* 3 column panels: All | International Migration | Domestic Migration
* 7 age-group tabs: 0-17, 18-40, 0-5, 6-10, 11-18, 19-25, 26-40
* 4 casualty variables: cas_own_any, cas_own_fatal, mwar_own_any, mwar_own_fatal
*
* Output: .tex files saved to tables/pallab/march_2026/summary/results/
********************************************************************************

clear all
set more off

* ── Paths ────────────────────────────────────────────────────────────────────
global base_dir "/Users/pallab.ghosh/Documents/GitHub/Nepal-Civil-War-and-Int-Migration"
global data_path "${base_dir}/data/Modified_Data/1_conflict_present_absentee_data.dta"
global out_dir   "${base_dir}/tables/pallab/march_2026/summary/results"

* ── Load data ────────────────────────────────────────────────────────────────
use "${data_path}", clear

local SURVEY_YEAR = 2017
local CONFLICT_START = 1996

* ── Derived variables ────────────────────────────────────────────────────────
gen male = (sex == 1) if !missing(sex)
gen age_num = real(string(age)) if !missing(age)
gen birth_year = `SURVEY_YEAR' - age_num
gen age_at_conflict_start = `CONFLICT_START' - birth_year

* Ethnicity (from caste codes)
destring caste, gen(caste_num) force
bysort psu hhld: replace caste_num = caste_num[_n-1] if missing(caste_num)
bysort psu hhld: replace caste_num = caste_num[_n+1] if missing(caste_num)

gen ethnicity = ""
replace ethnicity = "Hill High Caste" if inlist(caste_num, 1, 2, 14, 20, 27, 48, 49)
replace ethnicity = "Hill Janajati" if inlist(caste_num, 3, 5, 6, 10, 11, 13, 24, 29) | ///
    inlist(caste_num, 32, 36, 45, 46, 60, 61, 62, 66, 67) | ///
    inlist(caste_num, 69, 74, 77, 78, 79, 80, 81, 89, 90) | ///
    inlist(caste_num, 91, 92, 94, 97, 98, 100, 110, 119, 120) | ///
    inlist(caste_num, 121, 124, 125, 126, 127, 130, 131, 132, 133) | ///
    inlist(caste_num, 134, 135, 136, 137, 138, 139, 140, 992)
replace ethnicity = "Dalit" if inlist(caste_num, 8, 12, 15, 17, 22, 23, 25, 38, 39) | ///
    inlist(caste_num, 40, 41, 50, 70, 75, 76, 83, 93, 991)
replace ethnicity = "Muslim" if caste_num == 7

* Education
destring grade_comp, gen(grade_num) force
gen edu_cat = ""
replace edu_cat = "No Education" if inlist(grade_num, 16, 17)
replace edu_cat = "Primary (1-5)" if grade_num >= 0 & grade_num <= 5
replace edu_cat = "Secondary (6-12)" if grade_num >= 6 & grade_num <= 12
replace edu_cat = "Tertiary" if grade_num >= 13 & !missing(grade_num)

* Education dummies
gen edu_no_education = (edu_cat == "No Education") if edu_cat != ""
gen edu_primary = (edu_cat == "Primary (1-5)") if edu_cat != ""
gen edu_secondary = (edu_cat == "Secondary (6-12)") if edu_cat != ""
gen edu_tertiary = (edu_cat == "Tertiary") if edu_cat != ""

* Occupation
destring occupation_types, gen(occ_num) force
gen occ_cat = ""
replace occ_cat = "Armed Forces" if inlist(occ_num, 110, 210, 310)
gen nsco_major = floor(occ_num / 100) if occ_num < 1000
replace nsco_major = floor(occ_num / 1000) if occ_num >= 1000 & !missing(occ_num)
replace occ_cat = "High Skilled" if inlist(nsco_major, 1, 2, 3) & occ_cat == ""
replace occ_cat = "Service & Clerical" if inlist(nsco_major, 4, 5) & occ_cat == ""
replace occ_cat = "Agriculture" if nsco_major == 6 & occ_cat == ""
replace occ_cat = "Craft & Manufacturing" if inlist(nsco_major, 7, 8) & occ_cat == ""
replace occ_cat = "Elementary/Low Skilled" if nsco_major == 9 & occ_cat == ""

* Occupation dummies
gen occ_agriculture = (occ_cat == "Agriculture") if occ_cat != ""
gen occ_high_skilled = (occ_cat == "High Skilled") if occ_cat != ""
gen occ_service_clerical = (occ_cat == "Service & Clerical") if occ_cat != ""
gen occ_craft_mfg = (occ_cat == "Craft & Manufacturing") if occ_cat != ""
gen occ_elementary = (occ_cat == "Elementary/Low Skilled") if occ_cat != ""
gen occ_armed_forces = (occ_cat == "Armed Forces") if occ_cat != ""

* Panel A outcome dummies
gen outcome_intl_mig = international_absentee_only
gen outcome_dom_mig = national

* Casualty variables as numeric
foreach cv in cas_own_any cas_own_fatal mwar_own_any mwar_own_fatal {
    capture destring `cv', replace force
}

* Fill missing casualty data for absentees using district-level mapping
foreach cv in cas_own_any cas_own_fatal mwar_own_any mwar_own_fatal {
    bysort dist: egen temp_`cv' = mean(`cv')
    replace `cv' = temp_`cv' if missing(`cv')
    drop temp_`cv'
}

* ── Program to compute one row ───────────────────────────────────────────────
capture program drop compute_row
program define compute_row, rclass
    args var treat_var age_lo age_hi sub_filter

    * Create temp masks
    tempvar age_ok treat ctrl sample_ok

    gen `age_ok' = (age_at_conflict_start >= `age_lo' & age_at_conflict_start <= `age_hi')
    gen `treat' = (`treat_var' == 1) if !missing(`treat_var')
    gen `ctrl' = (`treat_var' == 0) if !missing(`treat_var')

    if "`sub_filter'" == "all" {
        gen `sample_ok' = `age_ok'
    }
    else if "`sub_filter'" == "intl" {
        gen `sample_ok' = `age_ok' & (international_absentee_only == 1)
    }
    else if "`sub_filter'" == "dom" {
        gen `sample_ok' = `age_ok' & (national == 1)
    }

    * Treatment mean
    quietly summarize `var' if `sample_ok' & `treat', meanonly
    local t_mean = r(mean) * 100
    local t_n = r(N)

    * Control mean
    quietly summarize `var' if `sample_ok' & `ctrl', meanonly
    local c_mean = r(mean) * 100
    local c_n = r(N)

    * Diff and t-test
    local diff = `t_mean' - `c_mean'
    local pval = .
    if `t_n' >= 2 & `c_n' >= 2 {
        quietly ttest `var' if `sample_ok' & (`treat' | `ctrl'), by(`treat') unequal
        local pval = r(p)
    }

    return local t_mean = `t_mean'
    return local c_mean = `c_mean'
    return local diff = `diff'
    return local pval = `pval'
    return local t_n = `t_n'
    return local c_n = `c_n'
end

* ── Program to format one diff cell ──────────────────────────────────────────
capture program drop format_diff
program define format_diff, rclass
    args diff pval

    local stars ""
    if `pval' < 0.01 local stars "***"
    else if `pval' < 0.05 local stars "**"
    else if `pval' < 0.10 local stars "*"

    local fmt_diff: display %6.2f `diff'
    local fmt_pval: display %5.3f `pval'

    return local result = "`fmt_diff' (p=`fmt_pval')`stars'"
end

* ── Generate LaTeX tables ────────────────────────────────────────────────────

* Casualty variable loop
local cas_vars "cas_own_any cas_own_fatal mwar_own_any mwar_own_fatal"
local cas_labels `" "Number of Casualties (Any)" "Number of Fatal Casualties" "Months of War (Any)" "Months of War (Fatal)" "'
local cas_shorts "cas_own_any cas_own_fatal mwar_own_any mwar_own_fatal"

* Age group definitions
local ag_labels  `" "0-17" "18-40" "0-5" "6-10" "11-18" "19-25" "26-40" "'
local ag_los     "0 18 0 6 11 19 26"
local ag_his     "17 40 5 10 18 25 40"

* Row variable names (Stata column names)
local row_vars "outcome_intl_mig outcome_dom_mig edu_no_education edu_primary edu_secondary edu_tertiary occ_agriculture occ_high_skilled occ_service_clerical occ_craft_mfg occ_elementary occ_armed_forces"
local row_labels `" "International Migration" "Domestic Migration" "No Education" "Primary (1-5)" "Secondary (6-12)" "Tertiary" "Agriculture" "High Skilled" "Service \& Clerical" "Craft \& Manufacturing" "Elementary/Low Skilled" "Armed Forces" "'

local n_cas : word count `cas_vars'
forvalues ci = 1/`n_cas' {
    local cv : word `ci' of `cas_vars'
    local cl : word `ci' of `cas_labels'
    local cs : word `ci' of `cas_shorts'

    * Compute median
    quietly summarize `cv', detail
    local median_val = r(p50)

    * Create treatment indicator for this casualty variable
    capture drop treat_`cv'
    gen treat_`cv' = (`cv' > `median_val') if !missing(`cv')

    display ""
    display "============================================================"
    display "  `cl' (median = `median_val')"
    display "============================================================"

    * Open LaTeX file
    local outfile "${out_dir}/summary_stats_`cs'.tex"
    capture file close texfile

    file open texfile using "`outfile'", write replace

    local n_ag : word count `ag_los'
    forvalues ai = 1/`n_ag' {
        local ag_label : word `ai' of `ag_labels'
        local ag_lo : word `ai' of `ag_los'
        local ag_hi : word `ai' of `ag_his'

        if `ai' > 1 {
            file write texfile _n _n "\clearpage" _n _n
        }

        file write texfile "\begin{table}[htbp]" _n
        file write texfile "\centering" _n
        file write texfile "\small" _n
        file write texfile "\caption{Summary Statistics by Conflict Intensity (`cl'): Aged `ag_label' at Conflict Start}" _n
        file write texfile "\label{tab:`cs'_age`ag_label'}" _n
        file write texfile "\begin{tabular}{l rrr c rrr c rrr}" _n
        file write texfile "\toprule" _n
        file write texfile " & \multicolumn{3}{c}{All} & & \multicolumn{3}{c}{International Migration} & & \multicolumn{3}{c}{Domestic Migration} \\" _n
        file write texfile "\cmidrule(lr){2-4} \cmidrule(lr){6-8} \cmidrule(lr){10-12}" _n
        file write texfile " & Treat & Control & Diff & & Treat & Control & Diff & & Treat & Control & Diff \\" _n
        file write texfile "\midrule" _n

        * Panel A header
        file write texfile "\textit{Panel A: Outcomes} & & & & & & & & & & & \\" _n

        * Panel B header row number
        local panel_b_after = 2
        * Panel C header row number
        local panel_c_after = 6

        local row_num = 0
        local n_rows : word count `row_vars'
        forvalues ri = 1/`n_rows' {
            local rv : word `ri' of `row_vars'
            local rl : word `ri' of `row_labels'
            local row_num = `row_num' + 1

            * Insert panel headers
            if `row_num' == 3 {
                file write texfile "\textit{Panel B: Education} & & & & & & & & & & & \\" _n
            }
            if `row_num' == 7 {
                file write texfile "\textit{Panel C: Occupation} & & & & & & & & & & & \\" _n
            }

            * Compute for each column panel
            local line "\quad `rl'"
            foreach sub in all intl dom {
                * Treatment mean
                if "`sub'" == "all" {
                    quietly summarize `rv' if age_at_conflict_start >= `ag_lo' & ///
                        age_at_conflict_start <= `ag_hi' & treat_`cv' == 1, meanonly
                }
                else if "`sub'" == "intl" {
                    quietly summarize `rv' if age_at_conflict_start >= `ag_lo' & ///
                        age_at_conflict_start <= `ag_hi' & treat_`cv' == 1 & ///
                        international_absentee_only == 1, meanonly
                }
                else {
                    quietly summarize `rv' if age_at_conflict_start >= `ag_lo' & ///
                        age_at_conflict_start <= `ag_hi' & treat_`cv' == 1 & ///
                        national == 1, meanonly
                }
                local t_mean = cond(r(N) > 0, r(mean) * 100, .)
                local t_n = r(N)

                * Control mean
                if "`sub'" == "all" {
                    quietly summarize `rv' if age_at_conflict_start >= `ag_lo' & ///
                        age_at_conflict_start <= `ag_hi' & treat_`cv' == 0, meanonly
                }
                else if "`sub'" == "intl" {
                    quietly summarize `rv' if age_at_conflict_start >= `ag_lo' & ///
                        age_at_conflict_start <= `ag_hi' & treat_`cv' == 0 & ///
                        international_absentee_only == 1, meanonly
                }
                else {
                    quietly summarize `rv' if age_at_conflict_start >= `ag_lo' & ///
                        age_at_conflict_start <= `ag_hi' & treat_`cv' == 0 & ///
                        national == 1, meanonly
                }
                local c_mean = cond(r(N) > 0, r(mean) * 100, .)
                local c_n = r(N)

                * Diff and p-value
                local diff = `t_mean' - `c_mean'
                local pval = .
                if `t_n' >= 2 & `c_n' >= 2 {
                    if "`sub'" == "all" {
                        capture ttest `rv' if age_at_conflict_start >= `ag_lo' & ///
                            age_at_conflict_start <= `ag_hi' & !missing(treat_`cv'), ///
                            by(treat_`cv') unequal
                    }
                    else if "`sub'" == "intl" {
                        capture ttest `rv' if age_at_conflict_start >= `ag_lo' & ///
                            age_at_conflict_start <= `ag_hi' & !missing(treat_`cv') & ///
                            international_absentee_only == 1, by(treat_`cv') unequal
                    }
                    else {
                        capture ttest `rv' if age_at_conflict_start >= `ag_lo' & ///
                            age_at_conflict_start <= `ag_hi' & !missing(treat_`cv') & ///
                            national == 1, by(treat_`cv') unequal
                    }
                    if _rc == 0 local pval = r(p)
                }

                local stars ""
                if `pval' < 0.01 local stars "***"
                else if `pval' < 0.05 local stars "**"
                else if `pval' < 0.10 local stars "*"

                local fmt_t: display %6.2f `t_mean'
                local fmt_c: display %6.2f `c_mean'
                local fmt_d: display %6.2f `diff'
                local fmt_p: display %5.3f `pval'

                local diff_str "`fmt_d' (p=`fmt_p')`stars'"

                if "`sub'" != "all" {
                    local line "`line' & "
                }
                local line "`line' & `fmt_t' & `fmt_c' & `diff_str'"
            }

            file write texfile "`line' \\" _n
        }

        file write texfile "\bottomrule" _n
        file write texfile "\end{tabular}" _n
        file write texfile "\begin{tablenotes}" _n
        file write texfile "\small" _n
        file write texfile "\item Treatment = districts with `cl' above median (>`median_val'). Control = at or below median." _n
        file write texfile "\item Diff: Treatment $-$ Control with Welch's t-test p-value. $^{***}$ p$<$0.01, $^{**}$ p$<$0.05, $^{*}$ p$<$0.10" _n
        file write texfile "\end{tablenotes}" _n
        file write texfile "\end{table}" _n
    }

    file close texfile
    display "  TEX: `outfile'"
}

display ""
display "All files saved to: ${out_dir}"
display "Done!"
