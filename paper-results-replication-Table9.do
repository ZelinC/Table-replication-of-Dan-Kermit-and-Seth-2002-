clear all
set mem 500m   
set matsize 10000
set more off   
capture log close 

use "bdsdata.dta",clear

// drop non-county-level data.
drop if (county=="USA")
drop if (county =="KENTUCKY")
* 39000-Ohio, 42000-Penn, 54000-WestVergi
drop if(fips ==39000)|(fips == 42000)|(fips ==54000) 

tsset fips year, yearly

******************************************************
*** Logarithmic difference in county earnings (Correct)
******************************************************
gen lctyInc = ln(EarnPOW/p7)
bys fips (year): gen dlctyInc = d.lctyInc

******************************************************
*** Change in Unemployment Insurance Expenditure
******************************************************
// generate difference of log Unemployment Insurance exp
gen ldUnemploy = ln(dUnemply)
bys fips (year): gen dldUnemploy = d.ldUnemploy

******************************************************
*** Regression 
******************************************************
// OLS
reg dldUnemploy dlctyInc state#year if year>=1970 & year<=1993, cluster(fips)

// 2SLS
* create instruments: region and time-period interactions 
* copied directly from table 6 calculation
gen lgcoal=0
gen mdcoal=0
gen oscoal=0
replace lgcoal=1 if coalres>=1000 & !missing(coalres)
replace mdcoal=1 if coalres>=100 & coalres<1000 
replace oscoal=1 if coalres<100

gen boom=0
replace boom=1 if year>=1970 & year<=1977
gen peak=0
replace peak=1 if year>=1978 & year<=1982
gen bust=0
replace bust=1 if year>=1983 & year<=1993

gen mdboom=1 if mdcoal==1 & boom==1
replace mdboom=0 if mdboom==.
gen mdpeak=1 if mdcoal==1 & peak==1
replace mdpeak=0 if mdpeak==.
gen mdbust=1 if mdcoal==1 & bust==1
replace mdbust=0 if mdbust==.

gen lgboom=1 if lgcoal==1 & boom==1
replace lgboom=0 if lgboom==.
gen lgpeak=1 if lgcoal==1 & peak==1
replace lgpeak=0 if lgpeak==.
gen lgbust=1 if lgcoal==1 & bust==1
replace lgbust=0 if lgbust==.

ivregress 2sls dldUnemploy state#year ///
(dlctyInc=mdboom mdpeak mdbust lgboom lgpeak lgbust) ///
if year>=1970 & year<=1993, cluster(fips)
