// Name: Diana Liang
// Date: 11/15/19
// Course: Stats 506 Fall 2019

// Find proportions of homes with internet
// for urban and rural areas for each division

// Set up data
import delimited recs2015_public_v4.csv, clear
keep doeid nweight internet division uatyp10 brrwt1-brrwt96

// Make uatyp10 a binary of R and U
replace uatyp10="U" if uatyp10 == "C"
encode uatyp10, generate(uatype)
drop uatyp10

// Pivot longer to make replicates usable
reshape long brrwt, i(doeid division uatype nweight) j(rep)
rename brrwt rweight
drop doeid

// Calculate proportions for each division and replicate
//
// Calculate prop for nweight
egen top_n = total(nweight*internet), by(division uatype)
egen bot_n = total(nweight), by(division uatype)
generate prop_n = top_n/bot_n
// Calculate prop for replicate weight
egen top_r = total(rweight*internet), by(division uatype rep)
egen bot_r = total(rweight), by(division uatype rep)
generate prop_r = top_r/bot_r
// Collapse
drop top_n bot_n top_r bot_r
collapse (mean) prop_n prop_r, by(division uatype rep)

// Pivot wider to get proportions for urban vs rural
reshape wide prop_n prop_r, i(division rep) j(uatype)

// Calculate diff btwn urban and rural
rename prop_n2 uprop_n
rename prop_r2 uprop_r
rename prop_n1 rprop_n
rename prop_r1 rprop_r
generate diff_n = uprop_n-rprop_n
generate diff_r = uprop_r-rprop_r

// Calculate variances
generate var_diff = (4/96)*(diff_n-diff_r)^2
generate var_u = (4/96)*(uprop_n-uprop_r)^2
generate var_r = (4/96)*(rprop_n-rprop_r)^2
// Collapse for each division
collapse (sum) var_diff var_u var_r (mean) diff_n uprop_n rprop_n, by (division)

// Find conf int
// diff conf int
generate se_diff = sqrt(var_diff)
generate lwr_diff = diff_n-1.96*se_diff
generate upr_diff = diff_n+1.96*se_diff
// urban conf int
generate se_u = sqrt(var_u)
generate lwr_u = uprop_n-1.96*se_u
generate upr_u = uprop_n+1.96*se_u
// rural conf int
generate se_r = sqrt(var_r)
generate lwr_r = rprop_n-1.96*se_r
generate upr_r = rprop_n+1.96*se_r
drop var_diff var_u var_r se_diff se_u se_r