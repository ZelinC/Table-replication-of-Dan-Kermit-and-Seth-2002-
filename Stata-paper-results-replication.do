clear all
set mem 500m   
set matsize 10000
set more off   
capture log close 

use "bdsdata.dta",clear

/* replication of table 3
"TABLE 2—SUMMARY STATISTICS OF SAMPLE BY COAL PRICE PERIOD:
KENTUCKY, OHIO, PENNSYLVANIA, AND WEST VIRGINIA, 
1970–1977 AND 1982–1993"
*/

/* create logarithmic difference in the real SSI or DI
*payment for country i in state s between time t and t-1
(consider to exclude data for county=USA, as they are aggregate statistics)
*/
drop if county == "USA"


* re-code county name into numbers. / can just use "fips" area code
encode county, generate(county_id)

* declare data to be time series
tsset fips year, yearly

* generate logithmetic difference of DI payment
gen lpay = ln(pay)
bys fips (year): gen dlpay = d.lpay

* generate logithmetic difference of SSI payment
gen ldSSI = ln(dSSI)
bys fips (year): gen dldSSI = d.ldSSI


// generate control variables
* whether or not the county is in a Metropolitan statistical area
* --> msa: MSA indicator

* logarithm of county's population
gen lpop = ln(Pop)

* log difference in county's population
bys fips (year): gen dlpop = d.lpop

* fraction of earnings in 1969 from manufacturing industries
* % = manufacturing/(farm earnings+nonfarm earnings)
gen p_manu = Manufact/(FarmEarn+NFrmEarn)
* create a joint variable to identify 1969 manufacturing fraction
*......"needs revise"

// generate explanatory variable - real earnings
* difference in the logarithm of real earnings in county i

* real earning=Wages and Salaries + other Labour Income + 
* + Proprietors' Income
gen lrlearn = ln(Wages+OLI+Proprtrs)
bys fips (year): gen dlrlearn = d.lrlearn

/* 2SLS
calculate "change in value of reserves"
*/
* measure of coal price: (July index values of each year are used)
* PPI for coal/Consumer Price Index of Urban wage earners
gen pcoal = pcoal7/p7
gen lpcoal = ln(pcoal)
bys fips (year): gen dlpcoal = D.lpcoal
gen d_resval = dlpcoal*ln(coalres)
replace d_resval=0 if d_resval==.


drop if year==1968 | year==1969
**********************************************************
/* TABLE 3 Regression (1) - OLS using year FE
Controlled for: 
1. State-year dummies
2. MSA
3. Population
4. Changes in popn
5. fraction of earnings from manufacturing, 1969
*/
// Panel A - DI
reg dlpay dlrlearn state#year msa lpop dlpop p_manu
// Panel B - SSI
reg dldSSI dlrlearn state#year msa lpop dlpop p_manu

**********************************************************
/* TABLE 3 Regression (3) - OLS using year FE
Controlled for: 
1. State-year dummies
*/
// Panel A - DI
reg dlpay dlrlearn state#year

// Panel B - SSI
reg dldSSI dlrlearn state#year

**********************************************************



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
ivregress 2sls dlpay state#year msa lpop dlpop p_manu (dlrlearn=d_resval L.d_resval L2.d_resval), vce(robust)
// Panel B - SSI
ivregress 2sls dldSSI state#year msa lpop dlpop p_manu (dlrlearn=d_resval L.d_resval L2.d_resval), vce(robust)

**********************************************************
/* TABLE 3 Regression (4) - 2SLS using year FE
Controlled for: 
1. State-year dummies
*/
// Panel A - DI
ivregress 2sls dlpay state#year (dlrlearn=d_resval L.d_resval L2.d_resval), vce(robust)
// Panel B - SSI
ivregress 2sls dldSSI state#year (dlrlearn=d_resval L.d_resval L2.d_resval), vce(robust)
