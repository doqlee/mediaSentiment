cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\figure_2_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** Figure 2. Benchmark Results - Equity Returns ***
********************************************************************************

* Number of lags

global Nlags = 8

* Projection horizon

global horizon = 20
global US_horizon = 15

* Specify significance level

scalar sig1 = 0.05
scalar sig2 = 0.3

* Load data

use ///
	weo* iso DATE dow* ///
	sent1_best L*_sent1_best ///
	art1_best L*_art1_best ///
	F*_index L*_returns F*_d_returns_h F*_d_returns_l ///
	vlm_ma60_adj L*_vlm_ma60_adj ///
	vol_ma60 L*_vol_ma60 ///
	vix L*_vix ///
	/// commo L*_commo /// Returns to S\&P Goldman Sachs Commodity index (proprietary)
	/// R_DJ_global L*_R_DJ_global /// Returns to US Dow Jones Industrial Index (proprietary)
	using "..\data\regression_sample.dta", clear

* Shorthand for row vectors

ds ///
	sent1_best art1_best ///
	vlm_ma60_adj vol_ma60 vix ///
	// commo R_DJ_global 
	
foreach vv in `r(varlist)' {
	global L_`vv'
	forval i = 1(1)$Nlags {
		global L_`vv' ${L_`vv'} L`i'_`vv'
	}
}

* Shorthand for dummy controls

ds dow2-dow5 
global other_dummies = "`r(varlist)'"

********************************************************************************
*** Figure 2.A. US only

xtset weo DATE
foreach s in 1 {
foreach impulse in sent`s'_best { 

preserve
	
	keep weo iso DATE `impulse' ///
		F*_index ${L_`impulse'} ///
			${L_returns} F*_d_returns_h F*_d_returns_l ///
			${L_vlm_ma60_adj} ${L_vol_ma60} ///
			${L_art1_best} ${L_vix} ${L_commo} ${L_R_DJ_global} /// 
			${other_dummies}
	
	keep if iso=="US"
	
	* Variables to store impulse response and SE
	cap drop b_LP se_LP
	gen b_LP = 0
	gen se_LP = 0

	* One regression for each horizon of the response
	eststo clear
	cap drop infhat 
	cap drop res0-res$US_horizon
	forvalues i=0/$US_horizon {	
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			newey2 F`i'_index ${L_`impulse'} res`j' ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ///
				${L_art1_best} ${L_vix} ${L_commo} ${L_R_DJ_global} /// 
				${other_dummies}, lag(`i') force
			eststo ols_LP`i'
		}
		else if `i'==0 {
			newey2 F`i'_index ${L_`impulse'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ///
				${L_art1_best} ${L_vix} ${L_commo} ${L_R_DJ_global} /// 
				${other_dummies}, lag(`i') force
			eststo ols_LP`i'
		}
		predict infhat
			gen res`i' = F`i'_index - infhat
			drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP = _se[L1_`impulse'] if _n==`i'+2
	}
	esttab * using "..\results\figure_2a.csv" ///
		, se csv nocon ar2 nogaps replace ///
		b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) drop() order() /// 
		stats(N r2, fmt(%9.0f %9.3fc) labels("Observations" "R-squared")) 

	* Time horizon
	cap drop t
	gen t = _n-1 if _n <= $US_horizon +1

	* Zero line
	cap drop zero
	gen zero = 0 if _n <= $US_horizon +1

***** create confidence bands (in this case 90 and 95%) *****
	
	scalar sig1 = 0.05	 // specify significance level
	scalar sig2 = 0.3	 // specify significance level

	cap drop up*_LP dn*_LP
	
	gen up_LP = b_LP + invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn_LP = b_LP - invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)

	gen up2_LP = b_LP + invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn2_LP = b_LP - invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	
	twoway (rarea up_LP dn_LP t, ///
			fcolor(gs12) lcolor(white) lpattern(solid)) ///
		(rarea up2_LP dn2_LP t, ///
			fcolor(gs10) lcolor(white) lpattern(solid)) ///
		(line b_LP t, lcolor(blue) ///
			lpattern(solid) lwidth(thick)) /// 
		(line zero t, lcolor(black)) ///
		, title(, color(black) size(medium)) ///
		ytitle("Cumulative equity (fund) flows (%)", size(large)) ///
		ylabel(-0.1(0.05)0.15, labsize(large)) ///
		xtitle("Horizon (h Days)", size(large)) ///
		xlabel(, labsize(large)) ///
		graphregion(color(white)) plotregion(color(white)) ///
		legend(off) 
	graph export "..\results\figure_2a.pdf", as(pdf) replace 
	cap graph close
	cap drop res0-res$US_horizon

restore
} // end impulse loop
} // end sent type loop


* Figure 2.B - Panel Full Sample

xtset weo DATE
foreach s in 1 {
foreach impulse in sent`s'_best { 

preserve
	
	keep weo* iso DATE `impulse' ///
		F*_index ${L_`impulse'} ///
			${L_returns} F*_d_returns_h F*_d_returns_l ///
			${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
			${L_art1_best} ${L_commo} ${L_R_DJ_global} ///
			${other_dummies}
	
	* Variables to store impulse response and SE
	cap drop b_LP se_LP
	gen b_LP = 0
	gen se_LP = 0

	* One regression for each horizon of the response
	eststo clear
	cap drop infhat  
	cap drop res0-res$US_horizon
	forvalues i=0/$US_horizon {	
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			xtscc F`i'_index ${L_`impulse'} res`j' ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_art1_best} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		else if `i'==0 {
			xtscc F`i'_index ${L_`impulse'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_art1_best} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		predict infhat
			gen res`i' = F`i'_index - infhat
			drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP = _se[L1_`impulse'] if _n==`i'+2
	}
	esttab * using ///
		"..\results\figure_2b.csv" ///
		, se csv nocon ar2 nogaps replace ///
	b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) drop() order() /// 
	stats(N r2, fmt(%9.0f %9.3fc) labels("Observations" "R-squared")) 

	* Time horizon
	cap drop t
	gen t = _n-1 if _n <= $US_horizon +1

	* Zero line
	cap drop zero
	gen zero = 0 if _n <= $US_horizon +1

***** create confidence bands (in this case 90 and 95%) *****
	
	scalar sig1 = 0.05	 // specify significance level
	scalar sig2 = 0.3	 // specify significance level

	cap drop up*_LP dn*_LP
	
	gen up_LP = b_LP + invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn_LP = b_LP - invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)

	gen up2_LP = b_LP + invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn2_LP = b_LP - invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	
	twoway (rarea up_LP dn_LP t, ///
		fcolor(gs12) lcolor(white) lpattern(solid)) ///
		(rarea up2_LP dn2_LP t, ///
		fcolor(gs10) lcolor(white) lpattern(solid)) ///
		(line b_LP t, lcolor(blue) ///
		lpattern(solid) lwidth(thick)) /// 
		(line zero t, lcolor(black)), ///
		title(, color(black) size(medium)) ///
		ytitle("Cumulative equity (fund) flows (%)", size(large)) ///
		ylabel(-0.1(0.05)0.15, labsize(large)) ///
		xtitle("Horizon (h Days)", size(large)) ///
		xlabel(, labsize(large)) ///
		graphregion(color(white)) plotregion(color(white)) ///
		legend(off) ///
		title("")
	graph export "..\results\figure_2b.pdf", as(pdf) replace 
	cap graph close
	cap drop res0-res$US_horizon
	
restore
	
} // end impulse loop
} // end sent type loop

cap log close
