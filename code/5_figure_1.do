cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\figure_1_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** Event study around larve deviations in equity returns
********************************************************************************

use ///
	weo* iso DATE ///
	sent1_best ///
	returns ///
	using "..\data\regression_sample.dta", clear

xtset weo DATE
	
* Events defined by large movements in stock prices

foreach dd in 3 { // Large = dd Standard deviation 
	
	cap drop mean_returns sd_returns
	bys weo: egen mean_returns = mean(returns)
	bys weo: egen sd_returns = sd(returns)
	
	gen byte returns_chigh`dd'sd = ///
		(returns > mean_returns + `dd' * sd_returns) if !mi(returns)
	gen byte returns_clow`dd'sd = ///
		(returns < mean_returns - `dd' * sd_returns) if !mi(returns)
	gen byte returns_cabs`dd'sd = ///
		(returns > mean_returns + `dd' * sd_returns) | ///
		(returns < mean_returns - `dd' * sd_returns) if !mi(returns)

	tab returns_*high`dd'sd, m
	tab returns_*low`dd'sd, m
	
	preserve
	
	keep weo DATE returns_*`dd'sd
	cap drop time_*
	
	foreach ss in chigh clow { // high or low movements by country
		
		gen time_`ss'`dd'sd = .
		
		levelsof weo if returns_`ss'`dd'sd==1, local(clist)
		foreach cc in `clist' {
			
			levelsof DATE if returns_`ss'`dd'sd==1 & weo==`cc', local(tlist)
			foreach tt in `tlist' {
				
				qui replace time_`ss'`dd'sd = 0 if DATE==`tt' & weo==`cc'
				
				forval ii = 1(1)10 {
					
					qui replace time_`ss'`dd'sd = `ii' if ///
						mi(time_`ss'`dd'sd) & DATE==(`tt' + `ii') & weo==`cc'
					
					qui replace time_`ss'`dd'sd = -`ii' ///
						if mi(time_`ss'`dd'sd) & DATE==(`tt' - `ii') & weo==`cc'
				}
			}
		}
	}
	tempfile time_`dd'sd
	save `time_`dd'sd'
	
	restore
	
	merge 1:1 weo DATE using `time_`dd'sd', nogen
	
}

* Construct z-scores

ds returns sent1_best t_sent1_best_local factor_sent1_best_filter
foreach vv in `r(varlist)' {
	
	cap drop mean_`vv' 
	bys weo: egen mean_`vv' = mean(`vv')
	
	cap drop sd_`vv' 
	bys weo: egen sd_`vv' = sd(`vv')
	
	cap drop zscore_`vv'
	gen zscore_`vv' = (`vv' - mean_`vv') / sd_`vv'
}

* Major events

// Lehman brothers file chapter 11 bankruptcy

gen byte time_gfc = 0 if tin(15sep2008, 15sep2008) 

// Chinese stock market turbulence

gen byte time_china2015 = 0 if tin(13jun2015, 13jun2015) 

foreach ss in gfc china2015 {
	levelsof DATE if time_`ss'==0, local(tlist)
	foreach tt in `tlist' {
		forval ii = 1(1)10 {
			qui replace time_`ss' = `ii' if DATE==(`tt' + `ii')
			qui replace time_`ss' = -`ii' if DATE==(`tt' - `ii')
		}
	}
}
cap gen byte zero = 0

* Sentiment indices around selected major events

twoway (line zscore_sent1_best time_china2015 ///
		if !mi(time_china2015) & iso=="CN" ///
		, lcolor(black) lpattern(solid) lwidth(thick)) /// 
	(line zero time_china2015, lcolor(black) lw(medium)), ///
	title(, color(black) size(medium)) ///
	xline(0, lcolor(red) lwidth(medium) lpattern(dash)) ///
	ytitle("News sentiment index (z-score)", size(large)) ///
	xtitle("Event time (Days)", size(large)) ///
	ylabel(, labsize(large)) xlabel(, labsize(large)) ///
	graphregion(color(white)) plotregion(color(white)) ///
	legend(off) /// 
	title("") 
graph export "..\results\figure_1_china2015_CN.pdf", as(pdf) replace 
cap graph close

twoway (line zscore_sent1_best time_gfc ///
		if !mi(time_gfc) & iso=="US" ///
		, lcolor(black) lpattern(solid) lwidth(thick)) /// 
	(line zero time_gfc, lcolor(black) lw(medium)), ///
	title(, color(black) size(medium)) ///
	xline(0, lcolor(red) lwidth(medium) lpattern(dash)) ///
	ytitle("News sentiment index (z-score)", size(large)) ///
	xtitle("Event time (Days)", size(large)) ///
	ylabel(, labsize(large)) xlabel(, labsize(large)) ///
	graphregion(color(white)) plotregion(color(white)) ///
	legend(off) /// 
	title("")
graph export "..\results\figure_1_gfc_US.pdf", as(pdf) replace 
cap graph close

* Average over events defined by large movements in stock prices

foreach dd in 3 {
foreach ss in chigh clow {
	
	* Average over all countries
	
	preserve
	
		keep weo DATE returns_*`dd'sd time_* zscore_* ///
			returns sent1_best t_sent1_best_local factor_sent1_best_filter
			
		drop if mi(time_`ss'`dd'sd)
		
		collapse ///
			(mean) mean_returns = returns ///
				mean_sent1_best = sent1_best ///
				mean_t_sent1_best_local = t_sent1_best_local ///
				mean_glb_sent1_best = factor_sent1_best_filter ///
				mean_zscore_returns = zscore_returns ///
				mean_zscore_sent1_best = zscore_sent1_best ///
				mean_zscore_t_sent1_best_local = zscore_t_sent1_best_local ///
				mean_zscore_glb_sent1_best = zscore_factor_sent1_best_filter ///
			(sd) sd_returns = returns ///
				sd_sent1_best = sent1_best ///
				sd_t_sent1_best_local = t_sent1_best_local ///
				sd_glb_sent1_best = factor_sent1_best_filter ///
			, by(time_`ss'`dd'sd)
			
		tsset time_`ss'`dd'sd
		gen zero = 0
		
		twoway (line mean_zscore_sent1_best time_`ss'`dd'sd ///
					, lcolor(black) lpattern(solid) lwidth(thick)) /// 
				(line zero time_`ss'`dd'sd, lcolor(black) lw(medium)), ///
				title(, color(black) size(medium)) ///
				xline(0, lcolor(red) lwidth(medium) lpattern(dash)) ///
				ytitle("News sentiment index (z-score)", size(large)) ///
				xtitle("Event time (Days)", size(large)) ///
				ylabel(, labsize(large)) xlabel(, labsize(large)) ///
				graphregion(color(white)) plotregion(color(white)) ///
				legend(off) /// 
				title("")
		graph export "..\results\figure_1_`ss'`dd'sd.pdf", as(pdf) replace 
		cap graph close
		
	restore
}
}

cap log close
