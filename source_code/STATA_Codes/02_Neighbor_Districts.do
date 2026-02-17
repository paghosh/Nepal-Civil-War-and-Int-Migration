/*==============================================================================
Project: Intergenerational Impact of Conflict
Author: Ramesh Dulal
Date: October 8, 2025
Purpose: This do-file creates the conflict intensity variable based on the paper

https://www-sciencedirect-com.ezproxy.lib.ou.edu/science/article/pii/S0167629620310560#bfn0085
Phadera, L. (2021). Unfortunate moms and unfortunate children: Impact of the nepali civil war on women's stature and İntergenerational health. Journal of Health Economics, 76, 102410.
==============================================================================*/


/*------------------------------------------------------------------------------
1. Import the excel file
------------------------------------------------------------------------------*/

import excel using "$data/contiguous district.xlsx", sheet(Sheet1) firstrow clear

label define district 66 "achham" 45 "arghakhanchi" 47 "baglung" 72 "baitadi" 69 "bajhang" 68 "bajura" 55 "banke" ///
24 "bara" 56 "bardiya" 29 "bhaktapur" 9 "bhojpur" 26 "chitawan" 71 "dadeldhura" 59 "dailekh" 50 "dang" 73 "darchula" ///
32 "dhading" 7 "dhankuta" 17 "dhanusha" 22 "dolakha" 60 "dolpa" 67 "doti" 37 "gorakha" 46 "gulmi" 64 "humla" 2 "ilam" ///
58 "jajarkot" 1 "jhapa" 61 "jumla" 65 "kailali" 62 "kalikot" 70 "kanchanpur" 44 "kapilvastu" 40 "kaski" 30 "kathmandu" ///
31 "kavre" 14 "khotang" 28 "lalitpur" 38 "lamjung" 18 "mahottari" 27 "makawanpur" 5 "morang" 63 "mugu" 74 "mustang" ///
49 "myagdi" 41 "nawalparasi" 34 "nuwakot" 15 "okhaldhunga" 43 "palpa" 3 "panchathar" 25 "parsa" 48 "parvat" 51 "pyuthan" ///
21 "ramechhap" 35 "rasuwa" 23 "rautahat" 52 "rolpa" 54 "rukum" 42 "rupandehi" 53 "salyan" 10 "sankhuwasabha" 11 "saptari" ///
19 "sarlahi" 20 "sindhuli" 33 "sindhupalchowk" 12 "siraha" 16 "solukhumbu" 6 "sunsari" 57 "surkhet" 39 "syangja" 36 "tanahu" ///
4 "taplejung" 8 "terhathum" 13 "udaypur" 0 "manang"

encode district, gen(district1) label(district)

encode contiguous_dist1, gen (contiguous_distr1) label (district)
encode contiguous_dist2, gen (contiguous_distr2) label (district)
encode contiguous_dist3, gen (contiguous_distr3) label (district)
encode contiguous_dist4, gen (contiguous_distr4) label (district)
encode contiguous_dist5, gen (contiguous_distr5) label (district)
encode contiguous_dist6, gen (contiguous_distr6) label (district)
encode contiguous_dist7, gen (contiguous_distr7) label (district)
encode contiguous_dist8, gen (contiguous_distr8) label (district)
encode contiguous_dist9, gen (contiguous_distr9) label (district)

drop contiguous_dist1 contiguous_dist2 contiguous_dist3 contiguous_dist4 contiguous_dist5 contiguous_dist6 contiguous_dist7 contiguous_dist8 contiguous_dist9 district

reshape long contiguous_distr, i(district_code district1) j(neighbor_num)

drop if missing(contiguous_distr) | contiguous_distr == .

save "$results/neighbor_districts.dta", replace

