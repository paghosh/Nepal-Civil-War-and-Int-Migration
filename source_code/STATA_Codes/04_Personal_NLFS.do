
// Use the raw dataset 

use "$nlfs/S00_rc.dta", clear

merge 1:1 psu hhld using "$nlfs/S01_rc.dta"
drop _merge

merge 1:m psu hhld using "$nlfs/S02_rc.dta"
drop _merge

merge 1:1 personid using "$nlfs/S03_rc.dta"
drop if _merge == 2
drop _merge


merge 1:1 psu hhld personid using "$nlfs/S04_rc.dta"

drop _merge

merge 1:1 psu hhld personid using "$nlfs/S05_rc.dta"

drop _merge

merge 1:1 psu hhld personid using "$nlfs/S06_rc.dta"

drop _merge

merge 1:1 psu hhld personid using "$nlfs/S07_rc.dta"

drop _merge

merge 1:1 psu hhld personid using "$nlfs/S08_rc.dta"

drop _merge


merge 1:1 psu hhld personid using "$nlfs/S09_rc.dta"

drop _merge

merge 1:1 psu hhld personid using "$nlfs/S10_rc.dta"

drop _merge

merge 1:1 psu hhld personid using "$nlfs/S11_rc.dta"

drop _merge

merge 1:1 psu hhld personid using "$nlfs/S13_rc.dta"

drop _merge

save "$results/personal_nlfs_data.dta", replace
