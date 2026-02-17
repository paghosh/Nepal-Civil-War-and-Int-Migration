/*==============================================================================
Project: Intergenerational Impact of Conflict
Author: Ramesh Dulal
Date: October 8, 2025
Purpose: This do-file creates the conflict intensity variable based on the paper

https://www-sciencedirect-com.ezproxy.lib.ou.edu/science/article/pii/S0167629620310560#bfn0085
Phadera, L. (2021). Unfortunate moms and unfortunate children: Impact of the nepali civil war on women's stature and İntergenerational health. Journal of Health Economics, 76, 102410.
==============================================================================*/


/*------------------------------------------------------------------------------
1. Load and inspect data
------------------------------------------------------------------------------*/

// Load dataset (use your actual file)
use "$results/conflict_data.dta", clear



/*------------------------------------------------------------------------------
2. Keep incidents in the civil war window: Feb 1996 - Nov 2006
------------------------------------------------------------------------------*/

// build monthly date
gen date_incident = ym(year, month)
format date_incident %tm

keep if inrange(date_incident, tm(1996m2), tm(2006m11))

// Using all kind of incidents as the effect of war
gen any_casualty = 1

*For robustness check, using only "Killed" as the effect of war

gen fatal_casualty = (incident_outcome == 1) 


// Collapse to district × month 

collapse (sum) casualties_any = any_casualty casualties_fatal = fatal_casualty, by (incident_district_num date_incident)

tempfile dmon
save `dmon'


// Build a complete panel: all districts × all 131 months

preserve
keep incident_district_num
duplicates drop
tempfile alldist
save `alldist', replace
restore

** Creating all 131 months

clear
set obs 131
gen date_incident = tm(1996m2) + _n - 1
format date_incident %tm
tempfile allmonths
save `allmonths', replace



** Combine district "alldist" and all months "allmonths"

use `alldist', clear
cross using `allmonths'


// Bring in actual counts and fill missing with 0

merge 1:1 incident_district_num date_incident using `dmon'

replace casualties_any   = 0 if missing(casualties_any)
replace casualties_fatal = 0 if missing(casualties_fatal)
drop _merge

// Flagging the casualties

gen war_month_any = casualties_any > 0
gen war_month_fatal = casualties_fatal > 0


tempfile district_month_panel
save `district_month_panel'


* Creating District level measures

*use `district_month_panel', clear


*collapse (sum) months_of_war_any = war_month_any (sum) months_of_war_fatal = war_month_fatal (sum) total_casualties_any = casualties_any (sum) total_casualties_fatal = casualties_fatal, by (incident_district_num)

*tempfile own_conflict
*save `own_conflict'

/*------------------------------------------------------------------------------
3. Calculate Neighbor conflict intensity
------------------------------------------------------------------------------*/

use `district_month_panel', clear


tempfile neighbor_conflict_data
save `neighbor_conflict_data'

use "$results/neighbor_districts.dta", clear

rename contiguous_distr incident_district_num

cross using `allmonths'

merge m:1 incident_district_num date_incident using `neighbor_conflict_data'
keep if _merge == 3
drop _merge

rename casualties_any neighbor_casualties_any
rename casualties_fatal neighbor_casualties_fatal
rename war_month_any neighbor_war_month_any
rename war_month_fatal neighbor_war_month_fatal

rename incident_district_num contiguous_distr
rename district1 incident_district_num

collapse (sum) neighbor_casualties_any neighbor_casualties_fatal ///
			(max) neighbor_war_month_any neighbor_war_month_fatal, by (incident_district_num date_incident)

tempfile neighbor_monthly
save `neighbor_monthly'


/*------------------------------------------------------------------------------
4. Combine own and neighbor at district-month level
------------------------------------------------------------------------------*/

use `district_month_panel', clear

merge 1:1 incident_district_num date_incident using `neighbor_monthly'
drop _merge

replace neighbor_casualties_any = 0 if missing(neighbor_casualties_any)

replace neighbor_casualties_fatal = 0 if missing(neighbor_casualties_fatal)

replace neighbor_war_month_any = 0 if missing(neighbor_war_month_any)

replace neighbor_war_month_fatal = 0 if missing(neighbor_war_month_fatal)



// Create combined measures (own + neighbors)

gen war_month_with_neighbors_any = (war_month_any == 1 | neighbor_war_month_any == 1)
gen war_month_with_neighbors_fatal = (war_month_fatal == 1 | neighbor_war_month_fatal == 1)
gen casualties_with_neighbors_any = casualties_any + neighbor_casualties_any
gen casualties_with_neighbors_fatal = casualties_fatal + neighbor_casualties_fatal

*/
/*------------------------------------------------------------------------------
5. Collapse to District Level
------------------------------------------------------------------------------*/


collapse (sum) mwar_own_any = war_month_any ///
         (sum) mwar_own_fatal = war_month_fatal ///
         (sum) cas_own_any = casualties_any ///
         (sum) cas_own_fatal = casualties_fatal ///
         (sum) mwar_nbr_any = war_month_with_neighbors_any ///
         (sum) mwar_nbr_fatal = war_month_with_neighbors_fatal ///
         (sum) cas_nbr_any = casualties_with_neighbors_any ///
         (sum) cas_nbr_fatal = casualties_with_neighbors_fatal, ///
         by(incident_district_num)

label var mwar_own_any "Months of war (own) - any"
label var mwar_own_fatal "Months of war (own) - fatal"
label var cas_own_any "Casualties (own) - any"
label var cas_own_fatal "Casualties (own) - fatal"
label var mwar_nbr_any "Months of war (with neighbors) - any"
label var mwar_nbr_fatal "Months of war (with neighbors) - fatal"
label var cas_nbr_any "Casualties (with neighbors) - any"
label var cas_nbr_fatal "Casualties (with neighbors) - fatal"


save "$results/conflict_intensity.dta", replace
