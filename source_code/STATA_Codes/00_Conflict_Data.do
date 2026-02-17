clear all
version 19
capture log close
*set maxvar 32767, permanently
********************************************************************************
*Project Title:  Importing the Conflict Dataset								   *
*Description:    															   *
*Dataset: INSEC Nepal Maoist Conflict Dataset		  						   *
*Author: Ramesh Dulal 														   *
********************************************************************************


*==============================================================================*
*          		        DEFINE THE PATH HERE        		                   *
*------------------------------------------------------------------------------*

if "`c(username)'" == "rameshdulal" {
    global workdir "/Users/rameshdulal/Library/CloudStorage/Dropbox/Nepal Civil Conflict/Data"
    global data    "/Users/rameshdulal/Library/CloudStorage/Dropbox/Nepal Civil Conflict/Data/Raw_Data"
    global results "/Users/rameshdulal/Library/CloudStorage/Dropbox/Nepal Civil Conflict/Data/Modified_Data"
}

*Please put the stata's username in place of yourname. To finde stata's username type
* display c(username)
*in stata. And chnage the directory for workdir, data, results
if "`c(username)'" == "yourname" {
    global workdir ""
    global data    ""
    global results ""
}
* ============================================================

*Importing the .xlsx data from the Raw_Data folder


import excel using "$data/conflict_data.xlsx", sheet("Sheet1") firstrow clear


label define district 66 "achham" 45 "arghakhanchi" 47 "baglung" 72 "baitadi" 69 "bajhang" 68 "bajura" 55 "banke" ///
24 "bara" 56 "bardiya" 29 "bhaktapur" 9 "bhojpur" 26 "chitawan" 71 "dadeldhura" 59 "dailekh" 50 "dang" 73 "darchula" ///
32 "dhading" 7 "dhankuta" 17 "dhanusha" 22 "dolakha" 60 "dolpa" 67 "doti" 37 "gorakha" 46 "gulmi" 64 "humla" 2 "ilam" ///
58 "jajarkot" 1 "jhapa" 61 "jumla" 65 "kailali" 62 "kalikot" 70 "kanchanpur" 44 "kapilvastu" 40 "kaski" 30 "kathmandu" ///
31 "kavre" 14 "khotang" 28 "lalitpur" 38 "lamjung" 18 "mahottari" 27 "makawanpur" 5 "morang" 63 "mugu" 74 "mustang" ///
49 "myagdi" 41 "nawalparasi" 34 "nuwakot" 15 "okhaldhunga" 43 "palpa" 3 "panchathar" 25 "parsa" 48 "parvat" 51 "pyuthan" ///
21 "ramechhap" 35 "rasuwa" 23 "rautahat" 52 "rolpa" 54 "rukum" 42 "rupandehi" 53 "salyan" 10 "sankhuwasabha" 11 "saptari" ///
19 "sarlahi" 20 "sindhuli" 33 "sindhupalchowk" 12 "siraha" 16 "solukhumbu" 6 "sunsari" 57 "surkhet" 39 "syangja" 36 "tanahu" ///
4 "taplejung" 8 "terhathum" 13 "udaypur" 0 "manang"


encode incident_district, gen(Incident_District) label(district)

label values incident_district_num district

label values resident_district district


label define eco_region 1 "Tarai" 2 "Hill" 3 "Mountain"
label values ecological_region eco_region

label define dev_region 1 "Eastern Development Region" 2 "Central Development Region" 3 "Western Development Region" 4 "Mid-Western Development Region" ///
5 "Far-Western Development Region"
label values development_region dev_region


destring month, replace

destring date, replace

destring year, replace

drop nepali_year age_2 Year event_date 

rename Age age

label define mobility_defn 0 "Incident neither in permanent or incident dist" 1 "Incident in contiguous district" 2 "Incident in permanent district"
label values mobility mobility_defn

label define outcome 1 "Killed" 2 "Disappeared" 3 "Sustained Injury"
label values incident_outcome outcome

label define lbl_killed_type 1 "Combat fighting" ///
 2 "Extra-judicial killing" ///
 3 "Serious nature" ///
 4 "Electrocution" ///
 5 "Self-bomb explosion" ///
 6 "Bomb explosion" ///
 0 "Other" ///
 9 "Not killed" 
label values killed_type lbl_killed_type


label define lbl_perp 0 "No" 1 "Yes", replace

label values by_state lbl_perp
label values by_maoist lbl_perp
label values other_perpetrators lbl_perp

label define lbl_gender 0 "Male" 1 "Female"
label values gender lbl_gender

label define lbl_marital ///
 1 "Married" ///
 2 "Unmarried" ///
 3 "Divorced" ///
 4 "Widow" ///
 5 "Widower" ///
 6 "Not clear" 

label values marital_status lbl_marital


label define lbl_language ///
 1 "Nepali" ///
 2 "Newari" ///
 3 "Tamang" ///
 4 "Gurung" ///
 5 "Tharu" ///
 6 "Maithili" ///
 7 "Hindi" ///
 8 "Awadhi" ///
 9 "Bhojpuri" ///
 10 "Magar" ///
 11 "Chepang" ///
 12 "Rai" ///
 13 "Thakali" ///
 14 "Dhimal" ///
 15 "Dotali" ///
 16 "Muslim" ///
 17 "Thami" ///
 18 "Jirel" ///
 19 "Other" ///
 20 "Not clear" 
 
label values language lbl_language


label define lbl_caste ///
 1 "Bramin/Chettrey" ///
 2 "Janajati" ///
 3 "Aadibashi" ///
 4 "Madeshi" ///
 5 "Dalit" ///
 6 "Muslim" ///
 7 "Not clear / not mentioned" ///
 9 "Other" 
 
label values ethnicity lbl_caste


label define lbl_party ///
 1  "Nepali Congress (Democratic)" ///
 2  "Nepali Congress (incl. sister orgs & student wing)" ///
 3  "CPN-UML (incl. sister orgs & student wing)" ///
 4  "CPN-ML" ///
 5  "United Maoist (rebel party incl. fronts)" ///
 6  "People's Front - Nepal" ///
 7  "Rastriya Prajatantrik Party (RPP)" ///
 8  "Nepal Sadbhawana Party" ///
 9  "United People's Front" ///
 10 "Nepal Workers and Peasants Party" ///
 11 "Nepal Communist Party - United" ///
 12 "National People's Liberation Party" ///
 13 "Limbuwan National Liberation Front" ///
 99 "Not identified / not affiliated" 
 
label values party_affiliation lbl_party


label define lbl_educ ///
 1 "Master's degree" ///
 2 "Bachelor's degree" ///
 3 "Intermediate" ///
 4 "High school" ///
 5 "Lower high school" ///
 6 "Primary school" ///
 7 "Literate" ///
 8 "Illiterate" ///
 9 "Not clear" 
 
label values education lbl_educ

label define lbl_econ ///
 1 "Higher" ///
 2 "Middle" ///
 3 "Lower-Middle" ///
 4 "Lower" ///
 5 "Not clear" 
 
label values economic_status lbl_econ

label define lbl_occupation ///
 1  "Agriculture" ///
 2  "Wage laborer" ///
 3  "Employed" ///
 4  "Teacher" ///
 5  "Police" ///
 6  "Army" ///
 7  "Lawyer" ///
 8  "Doctor" ///
 9  "Politician" ///
 10 "Social worker" ///
 11 "Rights activists" ///
 12 "Musician" ///
 13 "Player" ///
 14 "Driver" ///
 15 "Student" ///
 16 "Journalist" ///
 17 "Businessman" ///
 18 "Ex-security personnel" ///
 19 "Other / not clear" 
 
label values occupation lbl_occupation


label define lbl_informer 0 "No" 1 "Yes"
label values Informer lbl_informer

label define lbl_livelihood ///
 1 "Agriculture" ///
 2 "Other" ///
 3 "Unknown" 
 
label values livelihood_dependence lbl_livelihood


label define lbl_comp 0 "No" 1 "Yes"
label values compensation lbl_comp

label define lbl_displacement 0 "No" 1 "Yes"
label values displacement lbl_displacement

label define lbl_resettlement 0 "Not resettled" 1 "Resettled"
label values resettlement lbl_resettlement


drop incident_district number_involved Incident_District

order serialno Computer_FileNO resident_district incident_district_num incident_district year month date age gender marital_status language ethnicity education occupation , before(development_region)

label var year "Year of Incident"
label var month "Month of Incident"
label var date "Day of Incident"

label var by_state "State as Perpretrator"
label var by_maoist "Maoist as Perpretrator"
label var other_perpetrators "Other as Perpretrator"

sort serialno


save "$results/conflict_data.dta", replace
