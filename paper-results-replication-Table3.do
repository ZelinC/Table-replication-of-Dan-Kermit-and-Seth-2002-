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

*****************************************************
*** Logarithmic difference in SSI & DI payments (Correct)
*****************************************************
* generate logithmetic difference of SSI payment in real term (p7)
gen ldSSI = ln(dSSI/p7)
bys fips (year): gen dldSSI = d.ldSSI

* generate logithmetic difference of DI payment in real term (p7)
gen lpay = ln(pay/p7)
bys fips (year): gen dlpay = d.lpay

******************************************************
*** Logarithmic difference in county earnings (Correct)
******************************************************
gen lctyInc = ln(EarnPOW/p7)
bys fips (year): gen dlctyInc = d.lctyInc

******************************************************
*** Logarithmic difference in population (Correct)
*** Population 
******************************************************
* logarithm of county's population
gen lpop = ln(Pop)
* log difference in county's population
bys fips (year): gen dlpop = d.lpop

******************************************************
*** Logarithmic difference in real price of Coal (Correct)
*** Mean coal reserves (DIFF but CLOSE)
******************************************************
// measure of coal price: (July index values of each year are used)
// PPI for coal/Consumer Price Index of Urban wage earners
gen pcoal = pcoal7/p7
gen lpcoal = ln(pcoal)
bys fips (year): gen dlpcoal = D.lpcoal  

******************************************************
*** Logarithmic difference in Coal Value Instrument (CLOSE in 0.001 dp)
******************************************************
gen d_resval = dlpcoal*ln(coalres)
replace d_resval=0 if d_resval==.

******************************************************
*** Fraction of economy in manufacturing (CLOSE in 0.001 dp)
******************************************************
// proportion of manufacturing in economy 
// = manufacturing earning/ (farm earnings+nonFarm earnings)
gen propmanu = Manufact/(EarnPOW)

*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*
*************************** REGRESSION *****************************************
*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*

/* TABLE 3 Regression (1) - OLS using year FE
Controlled for: 
1. State-year dummies
2. MSA
3. Population
4. Changes in popn
5. fraction of earnings from manufacturing, 1969
*/
// generate constant term of propmanu for each county
gen manu69=propmanu if year==1969
by fips: replace manu69 = manu69[_n-1] if missing(manu69)
by fips: replace manu69 = manu69[_n+1] if missing(manu69)
replace manu69=0 if missing(manu69)

// Panel A - DI
reg dlpay dlctyInc state#year msa lpop dlpop manu69 if year>=1970, cluster(fips)

// Panel B - SSI
reg dldSSI dlctyInc state#year msa lpop dlpop manu69 if year>=1970, cluster(fips)

**********************************************************
/* TABLE 3 Regression (3) - OLS using year FE
Controlled for: 
1. State-year dummies
*/
// Panel A - DI
reg dlpay dlctyInc state#year if year>=1970, cluster(fips)

// Panel B - SSI
reg dldSSI dlctyInc state#year if year>=1970, cluster(fips)

**********************************************************
/* TABLE 3 Regression (2) - 2SLS using year FE
Controlled for: 
1. State-year dummies
2. MSA
3. Population
4. Changes in popn
5. fraction of earnings from manufacturing, 1969
*/
// Panel A - DI
ivregress 2sls dlpay state#year msa lpop dlpop manu69 (dlctyInc=d_resval L.d_resval L2.d_resval) if year>=1970, cluster(fips) first
estat firststage
// Panel B - SSI
ivregress 2sls dldSSI state#year msa lpop dlpop manu69 (dlctyInc=d_resval L.d_resval L2.d_resval) if year>=1970, cluster(fips) first
estat firststage

**********************************************************
/* TABLE 3 Regression (4) - 2SLS using year FE
Controlled for: 
1. State-year dummies
*/
// Panel A - DI
ivregress 2sls dlpay state#year (dlctyInc=d_resval L.d_resval L2.d_resval) if year>=1970, cluster(fips) first
estat firststage
// Panel B - SSI
ivregress 2sls dldSSI state#year (dlctyInc=d_resval L.d_resval L2.d_resval) if year>=1970, cluster(fips) first
estat firststage
