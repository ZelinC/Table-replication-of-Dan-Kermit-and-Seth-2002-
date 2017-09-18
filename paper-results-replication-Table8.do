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
*** Logarithmic difference in county earnings 
******************************************************
gen lctyInc = ln(EarnPOW/p7)
bys fips (year): gen dlctyInc = d.lctyInc

*****************************************************
*** Logarithmic difference in DI payments
*****************************************************
gen lpay = ln(pay/p7)
bys fips (year): gen dlpay = d.lpay

*****************************************************
*** generate region FE
*****************************************************
gen lgcoal= (coalres>=1000 & !missing(coalres))
gen mdcoal=  (coalres>=100 & coalres<1000)
gen oscoal=  (coalres<100)

*****************************************************
*** generate period FE
*****************************************************
gen boom=0
replace boom=1 if year>=1970 & year<=1977
gen peak=0
replace peak=1 if year>=1978 & year<=1982
gen bust=0
replace bust=1 if year>=1983 & year<=1993

******************************************************
*** Logarithmic difference in Coal Value Instrument (CLOSE in 0.001 dp)
******************************************************
gen pcoal = pcoal7/p7
gen lpcoal = ln(pcoal)
bys fips (year): gen dlpcoal = D.lpcoal
gen d_resval = dlpcoal*ln(coalres)
replace d_resval=0 if d_resval==.

******************************************************
*** seam and time interactions
******************************************************
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

gen osboom=1 if oscoal==1 & boom==1
replace osboom=0 if osboom==.
gen ospeak=1 if oscoal==1 & peak==1
replace ospeak=0 if ospeak==.
gen osbust=1 if oscoal==1 & bust==1
replace osbust=0 if osbust==.


*****************************************************
*** Regression - row-1
*****************************************************
ivregress 2sls dlpay state#year lgcoal mdcoal (dlctyInc=mdboom mdpeak mdbust lgboom lgpeak lgbust) if year>=1970, cluster(fips)

*****************************************************
*** Regression - Two-way comparison by seam:
ivregress 2sls dlpay state#year lgcoal mdcoal (dlctyInc=mdboom mdpeak mdbust lgboom lgpeak lgbust) if year>=1970 & lgcoal!=1, cluster(fips)
ivregress 2sls dlpay state#year lgcoal mdcoal (dlctyInc=mdboom mdpeak mdbust lgboom lgpeak lgbust) if year>=1970 & mdcoal!=1, cluster(fips)
ivregress 2sls dlpay state#year lgcoal mdcoal (dlctyInc=mdboom mdpeak mdbust lgboom lgpeak lgbust) if year>=1970 & oscoal!=1, cluster(fips)

*****************************************************
*** Regression - Two-way comparison by time period:
ivregress 2sls dlpay state#year lgcoal mdcoal (dlctyInc=mdboom mdpeak mdbust lgboom lgpeak lgbust) if year>=1970 & boom!=1, cluster(fips)
ivregress 2sls dlpay state#year lgcoal mdcoal (dlctyInc=mdboom mdpeak mdbust lgboom lgpeak lgbust) if year>=1970 & peak!=1, cluster(fips)
ivregress 2sls dlpay state#year lgcoal mdcoal (dlctyInc=mdboom mdpeak mdbust lgboom lgpeak lgbust) if year>=1970 & bust!=1, cluster(fips)

*****************************************************
*** Regression - Alternative 2SLS estimates:
ivregress 2sls dlpay mdcoal lgcoal state#year (dlctyInc=d_resval L.d_resval L2.d_resval) if year>=1970, vce(cluster fips)
ivregress 2sls dlpay i.fips state#year (dlctyInc=d_resval L.d_resval L2.d_resval) if year>=1970, cluster(fips)
ivregress 2sls dlpay i.fips state#year (dlctyInc=lgboom lgpeak lgbust mdboom mdpeak mdbust) if year>=1970, cluster(fips)
