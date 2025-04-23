cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using ".\log\4_merge_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** COLLECT SENTIMENT INDICATORS INTO A SINGLE FILE ***
********************************************************************************

use "..\data\sentiment_country.dta", clear

keep iso2 date sentiment_country

merge 1:1 iso2 date using ///
	"..\data\sentiment_local.dta" ///
	, nogen keep(1 2 3) keepusing(sentiment_local)
	
encode iso2, gen(num_iso2)
drop iso2

xtset num_iso2 date
tsfill

decode num_iso2, gen(iso2)
drop num_iso2

merge m:1 date using ///
	"..\data\sentiment_global.dta" ///
	, nogen keep(1 2 3) keepusing(sentiment_global)

* Convert to z-scores within each country

ds sentiment*

foreach vv in `r(varlist)' {
	
	bys iso2: egen _mean = mean(`vv')
	bys iso2: egen _sd = sd(`vv')
	
	replace `vv' = (`vv' - _mean) / _sd
	
	drop _mean _sd
}

* Standardized country names

kountry iso2, from(iso2c) to(iso3c) marker

drop MARKER _ISO3C_
ren NAMES_STD country
	
keep country date sentiment* // iso2 
order country date sentiment* // iso2 
sort country date // iso2 

compress
outsheet using "..\data\news_sentiment.csv", comma replace

clear
cd "..\data\"

!del sentiment_*

cd "..\code"

cap log close
