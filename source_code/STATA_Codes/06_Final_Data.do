

* 1. IMPORT DATA =====================================

use "$nlfs/S00_rc.dta", clear


merge 1:1 psu hhld using "$nlfs/S01_rc.dta"
drop _merge


merge 1:m psu hhld using "$nlfs/S02_rc.dta"
drop _merge

merge 1:1 personid using "$nlfs/S03_rc.dta"
drop _merge

merge 1:1 personid using "$nlfs/S04_rc.dta"
drop _merge
// Create whether the individual is absent or not

gen absent = 0

rename mwrk_nsco4 occupation_types

save "$results/personal_data.dta", replace


// Use Absentee dataset

use "$nlfs/S00_rc.dta", clear

merge 1:1 psu hhld using "$nlfs/S01_rc.dta"

drop _merge

merge 1:m psu hhld using "$nlfs/S12_rc"
drop  if _merge == 1
drop _merge

gen absent = 1

rename abs_sex sex
rename abs_age age
rename abs_relhh rel_hhh
rename abs_edulvl grade_comp

gen long id = _n
gen long personid = id + 20000000
format personid %15.0f
drop id

rename l10_nsco occupation_types

append using "$results/personal_data.dta"

sort psu hhld



merge 1:1 personid using "$nlfs/S05_rc.dta"
drop _merge

merge 1:1 personid using "$nlfs/S06_rc.dta"
drop _merge

merge 1:1 personid using "$nlfs/S07_rc.dta"
drop _merge


merge 1:1 personid using "$nlfs/S08_rc.dta"
drop _merge


merge 1:1 personid using "$nlfs/S09_rc.dta"
drop _merge


merge 1:1 personid using "$nlfs/S10_rc.dta"
drop _merge

merge 1:1 personid using "$nlfs/S11_rc.dta"
drop _merge

merge 1:1 personid using "$nlfs/S13_rc.dta"
drop _merge


save "$results/absentee_personal_nlfs.dta", replace


use "$results/nlfs_conflict_data.dta", clear

keep district_name_std incident_district_num mwar_own_any mwar_own_fatal cas_own_any cas_own_fatal mwar_nbr_any mwar_nbr_fatal cas_nbr_any cas_nbr_fatal personid id psu hhld season dist vdcmun ward

merge 1:1 personid using "$results/absentee_personal_nlfs.dta"
drop _merge

// This will create the variable from the absentee dataset only
gen international_absentee_only = 0
replace international_absentee_only = 1 if inrange(abs_living, 101, 233)
replace international_absentee_only = 1 if abs_living == 990
replace international_absentee_only = . if abs_living == 0 | abs_living == 999
label var international_absentee_only "Only from Absentee Dataset"

// This will create the variable from absentee and present individuals
gen national = 0
replace national = 1 if inrange(abs_living, 1, 75)
replace national = . if abs_living == 0 | abs_living == 990 | abs_living == 999


gen international_migrant = 0
replace international_migrant = 1 if travelled5 == 1 & inrange(rsn_travel, 1, 6)
replace international_migrant =1 if absent == 1 & national == 0
label var international_migrant "International Migrant from Absentee Dataset and Present Dataset"
// This will create the variable from present individuals who have been abroad at any time

gen present_ind_migrant = 0
replace present_ind_migrant = 1 if travelled5 == 1 & inrange(rsn_travel, 1, 6)
replace present_ind_migrant = . if absent == 1
label var present_ind_migrant "International Migrant in Non-Absent Dataset only"
save "$results/1_conflict_present_absentee_data.dta", replace
