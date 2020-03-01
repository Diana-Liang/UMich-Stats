// Name: Diana Liang
// Date: 11/15/19
// Course: Stats 506 Fall 2019

// Create table of coeff and conf int for
// mixed models of log of each curvature measure

// Set up data
import delimited ps2_q2c.csv, clear
encode condition, generate(cond)
encode exemplar, generate(exemp)
drop v1 trial condition exemplar

// Set up excel
putexcel set ps4_q1.xlsx, sheet(ps4_q1) modify
putexcel A1 = "curve_measure"
putexcel B1 = "coeff"
putexcel C1 = "lwr"
putexcel D1 = "upr"
putexcel E1 = "st_err"

foreach i in total_dist max_dev avg_dev auc {
	// Run models for log of each curvature measure
	generate `i'_log = log(`i')
	drop `i'
	mixed `i'_log i.cond || _all: R.subject || _all: R.exemp
	
	// Extract coeff and se
	matrix b = e(b)
	matrix v = e(V)
	mata: B = st_matrix("b")
	mata: COEF = B[1,2]
	mata: st_matrix("coef", COEF)
	mata: V = st_matrix("v")
	mata: all_SE = sqrt( diagonal(V) )
	mata: SE = all_SE[2,1]
	mata: st_matrix("se", SE)
	
	// Store values for excel
	matrix pe_`i' = coef
	matrix lwr_`i' = coef - 1.96*se
	matrix upr_`i' = coef + 1.96*se
	matrix se_`i' = se
}


// Put into excel
putexcel A2 = "total_dist"
putexcel B2 = matrix(pe_total_dist)
putexcel C2 = matrix(lwr_total_dist)
putexcel D2 = matrix(upr_total_dist)
putexcel E2 = matrix(se_total_dist)

putexcel A3 = "max_dev"
putexcel B3 = matrix(pe_max_dev)
putexcel C3 = matrix(lwr_max_dev)
putexcel D3 = matrix(upr_max_dev)
putexcel E3 = matrix(se_max_dev)

putexcel A4 = "avg_dev"
putexcel B4 = matrix(pe_avg_dev)
putexcel C4 = matrix(lwr_avg_dev)
putexcel D4 = matrix(upr_avg_dev)
putexcel E4 = matrix(se_avg_dev)

putexcel A5 = "auc"
putexcel B5 = matrix(pe_auc)
putexcel C5 = matrix(lwr_auc)
putexcel D5 = matrix(upr_auc)
putexcel E5 = matrix(se_auc)

