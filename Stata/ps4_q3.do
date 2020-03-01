// Name: Diana Liang
// Date: 11/15/19
// Course: Stats 506 Fall 2019

// Create tables to compare logistic models of
// the effect of weekday on water drank

// PART A: Set up data----------------------------------------------------------

// Clean up day 1 data
// Set up data
import sasxport5 DR1TOT_D.XPT
keep seqn wtdrd1 dr1day dr1_320z dr1_330z dr1bwatz
rename seqn id
rename wtdrd1 weight
rename dr1day date
// Create binary weekday
generate weekday = date
replace weekday=0 if weekday == 1 | weekday == 7
replace weekday=1 if weekday > 1 & weekday < 7
// Create binary water
generate tot_water = dr1_320z + dr1_330z + dr1bwatz
generate water = tot_water
replace water = 1 if water > 0
// Make sure this is only for day 1
generate day = 1
// Save data
drop dr1_320z dr1_330z dr1bwatz
save "C:\Users\dliangia\Documents\ps4_q3_day1.dta", replace

// Clean up day 2 data
// Set up data
import sasxport5 DR2TOT_D.XPT, clear
keep seqn wtdrd1 dr2day dr2_320z dr2_330z dr2bwatz
rename seqn id
rename wtdrd1 weight
rename dr2day date
// Create binary weekday
generate weekday = date
replace weekday=0 if weekday == 1 | weekday == 7
replace weekday=1 if weekday > 1 & weekday < 7
// Create binary water
generate tot_water = dr2_320z + dr2_330z + dr2bwatz
generate water = tot_water
replace water = 1 if water > 0
// Make sure this is for day 2
generate day = 2
// Save data
drop dr2_320z dr2_330z dr2bwatz
save "C:Users\dliangia\Documents\ps4_q3_day2.dta", replace

// Clean up demographics data
import sasxport5 DEMO_D.XPT, clear
keep seqn riagendr ridageyr indfmpir ridexmon
rename seqn id
rename riagendr gender
rename ridageyr age
rename indfmpir pir
rename ridexmon winter
replace winter = 0 if winter == 2
replace winter = 1 if winter == 1
save "C:Users\dliangia\Documents\ps4_q3_dem.dta", replace

// Merge day 1 data with demographics
use "C:Users\dliangia\Documents\ps4_q3_day1.dta", clear
merge 1:1 id using ps4_q3_dem.dta
drop _merge
save "C:Users\dliangia\Documents\ps4_q3_day1.dta", replace

// Merge day 2 data with demographics
use "C:Users\dliangia\Documents\ps4_q3_day2.dta", clear
merge 1:1 id using ps4_q3_dem.dta
drop _merge
save "C:Users\dliangia\Documents\ps4_q3_day2.dta", replace

// Merge day 1 and day 2 data
append using ps4_q3_day1.dta
sort id
save "C:\Users\dliangia\Documents\ps4_q3_data.dta", replace


//------------------------------------------------------------------------------
// PART B: Clean up data--------------------------------------------------------

// Load data
// use "C:\Users\dliangia\Documents\ps4_q3_data.dta", clear

// Only keep data that if none of the data is missing
generate missing = 0
replace missing = 1 if weight == . | date == . | gender == . | age == . | pir == . | tot_water == . | winter == .
keep if missing == 0

// Center age and pir
// Only have one age/pir for each remaining respondent
by id: generate age_n = age if _n == 1
by id: generate pir_n = pir if _n == 1
// Find the mean age/pir
egen age_mean = mean(age_n)
egen pir_mean = mean(pir_n)
// Calculate centered age
generate age_center = age - age_mean
replace age_center = age_center/10
// Calculate centered pir
generate pir_center = pir - pir_mean
generate age_center2 = age_center^2

// Save data
drop missing age_n pir_n age_mean pir_mean age pir
save "C:\Users\dliangia\Documents\ps4_q3_b.dta"


//------------------------------------------------------------------------------
// PART C: Logistic Model-------------------------------------------------------

// Set up data
// use "C:\Users\dliangia\Documents\ps4_q3_b.dta", clear
keep if day == 1

// Set up excel sheet
putexcel set ps4_q3_partc.xlsx, sheet("Part C") modify
putexcel A1 = "Variable"
putexcel B1 = "Odds_Ratio"
putexcel C1 = "OR_lwr"
putexcel D1 = "OR_upr"
putexcel E1 = "Log_Coeff"
putexcel F1 = "L_lwr"
putexcel G1 = "L_upr"
putexcel H1 = "Marginal"
putexcel I1 = "M_lwr"
putexcel J1 = "M_upr"
putexcel A2 = "weekday"
putexcel A3 = "winter"
putexcel A4 = "age"
putexcel A5 = "age^2"
putexcel A6 = "gender"
putexcel A7 = "pir"

// Do logistic model
logistic water i.weekday i.winter c.age_center##c.age_center2 i.gender pir_center
//
// Extract odd ratio and conf int
matrix o_ratio = r(table)
mata: O_RATIO = st_matrix("o_ratio")
mata: VARIABLES = O_RATIO[(1,5,6), (2,4,5,6,9,10)]
mata: FINAL = VARIABLES'
mata: st_matrix("final", FINAL)
putexcel B2 = matrix(final)
//
// Extract logistic coeff and conf int
matrix eb = e(b)
matrix ev = e(V)
mata: EB = st_matrix("eb")
mata: ECOEF = EB[1, (2,4,5,6,9,10)]
mata: ECOEF = ECOEF'
mata: st_matrix("ecoef", ECOEF)
mata: EV = st_matrix("ev")
mata: ESE = sqrt( diagonal(EV) )
mata: ESE = ESE[(2,4,5,6,9,10), 1]
mata: st_matrix("ese", ESE)
matrix lwr = ecoef - 1.96*ese
matrix upr = ecoef + 1.96*ese
putexcel E2 = matrix(ecoef)
putexcel F2 = matrix(lwr)
putexcel G2 = matrix(upr)

// Find marginal effect
margins, dydx(weekday winter age_center age_center2 gender pir_center)
//
// Extract margin coeff and conf int
matrix table = r(table)
mata: Table = st_matrix("table")
mata: Variables = Table[(1,5,6), (2,4,5,6,8,9)]
mata: Marg = Variables'
mata: st_matrix("marg", Marg)
putexcel H2 = matrix(marg)


//------------------------------------------------------------------------------
// PART D: Meglm Model----------------------------------------------------------
//
// Set up data
use "C:\Users\dliangia\Documents\ps4_q3_b.dta", clear

// Set up excel sheet
putexcel set ps4_q3_partd.xlsx, sheet("Part D") modify
putexcel A1 = "Variable"
putexcel B1 = "Coef"
putexcel C1 = "Lwr"
putexcel D1 = "Upr"
putexcel E1 = "Marginal"
putexcel F1 = "M_lwr"
putexcel G1 = "M_upr"
putexcel A2 = "weekday"
putexcel A3 = "day"
putexcel A4 = "winter"
putexcel A5 = "age"
putexcel A6 = "age^2"
putexcel A7 = "gender"
putexcel A8 = "pir"

// Do meglm with logit model
meglm water i.weekday i.day i.winter c.age_center##c.age_center2 i.gender pir_center || _all: R.id, family(binomial) link(logit)
//
// Extract coeff and conf int
matrix eb = e(b)
matrix ev = e(V)
mata: EB = st_matrix("eb")
mata: ECOEF = EB[1, (2,4,6,7,8,11,12)]
mata: ECOEF = ECOEF'
mata: st_matrix("ecoef", ECOEF)
mata: EV = st_matrix("ev")
mata: ESE = sqrt( diagonal(EV) )
mata: ESE = ESE[(2,4,6,7,8,11,12), 1]
mata: st_matrix("ese", ESE)
matrix lwr = ecoef - 1.96*ese
matrix upr = ecoef + 1.96*ese
putexcel B2 = matrix(ecoef)
putexcel C2 = matrix(lwr)
putexcel D2 = matrix(upr)

// Find marginal effect
margins, dydx(weekday day winter age_center age_center2 gender pir_center)
//
// Extract coeff and conf int
matrix table = r(table)
mata: Table = st_matrix("table")
mata: Variables = Table[(1,5,6), (2,4,6,7,8,10,11)]
mata: Marg = Variables'
mata: st_matrix("marg", Marg)
putexcel E2 = matrix(marg)
