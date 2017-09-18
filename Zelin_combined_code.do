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
*** Logarithmic difference in SSI & DI payments 
*****************************************************
* generate logithmetic difference of SSI payment in real term (p7)
gen ldSSI = ln(dSSI/p7)
bys fips (year): gen dldSSI = d.ldSSI

* generate logithmetic difference of DI payment in real term (p7)
gen lpay = ln(pay/p7)
bys fips (year): gen dlpay = d.lpay

******************************************************
*** Logarithmic difference in county earnings 
******************************************************
gen lctyInc = ln(EarnPOW/p7)
bys fips (year): gen dlctyInc = d.lctyInc

******************************************************
*** Logarithmic difference in population 
*** Population 
******************************************************
* logarithm of county's population
gen lpop = ln(Pop)
* log difference in county's population
bys fips (year): gen dlpop = d.lpop

******************************************************
*** Logarithmic difference in real price of Coal
*** Mean coal reserves
******************************************************
// measure of coal price: (July index values of each year are used)
// PPI for coal/Consumer Price Index of Urban wage earners
gen pcoal = pcoal7/p7
gen lpcoal = ln(pcoal)
bys fips (year): gen dlpcoal = D.lpcoal

******************************************************
*** Logarithmic difference in Coal Value Instrument
******************************************************
gen d_resval = dlpcoal*ln(coalres)
replace d_resval=0 if d_resval==.

******************************************************
*** Fraction of economy in manufacturing 
******************************************************
gen propmanu = Manufact/EarnPOW
replace propmanu=0 if missing(propmanu)

******************************************************
** scale of coal-reserve 
******************************************************
// >1000m, 100m<x<1000m, <100m
gen lgcoal= (coalres>=1000) & !missing(coalres)
gen mdcoal= (coalres>=100 & coalres<1000 )
gen oscoal= (coalres<100)

// easier for tabstat
gen coalsize=0
replace coalsize=2 if coalres>=1000 & !missing(coalres)
replace coalsize=1 if coalres>=100 & coalres<1000 

// boom, peak and bust periods
gen boom=0
replace boom=1 if year>=1970 & year<=1977
gen peak=0
replace peak=1 if year>=1978 & year<=1982
gen bust=0
replace bust=1 if year>=1983 & year<=1993

*******************************************************
*** Table 2 Output
*******************************************************
tabstat dldSSI dlpay dlctyInc dlpop dlpcoal d_resval coalres propmanu msa ///
if boom==1, s(mean sd) by(coalsize)
tabstat propmanu if year==1969, s(mean sd) by(coalsize)
tabstat dldSSI dlpay dlctyInc dlpop dlpcoal d_resval Pop if  bust==1, ///
s(mean sd) by(coalsize)

*******************************************************
*** Table 3 Output
*******************************************************
/* Regression (1) - OLS using year FE
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
reg dlpay dlctyInc state#year msa lpop dlpop manu69 if year>=1970, ///
cluster(fips)

// Panel B - SSI
reg dldSSI dlctyInc state#year msa lpop dlpop manu69 if year>=1970, ///
cluster(fips)

**********************************************************
/* Regression (3) - OLS using year FE
Controlled for: 
1. State-year dummies
*/
// Panel A - DI
reg dlpay dlctyInc state#year if year>=1970, cluster(fips)

// Panel B - SSI
reg dldSSI dlctyInc state#year if year>=1970, cluster(fips)

**********************************************************
/* Regression (2) - 2SLS using year FE
Controlled for: 
1. State-year dummies
2. MSA
3. Population
4. Changes in popn
5. fraction of earnings from manufacturing, 1969
*/
// Panel A - DI
ivregress 2sls dlpay state#year msa lpop dlpop manu69 (dlctyInc=d_resval ///
L.d_resval L2.d_resval) if year>=1970, cluster(fips) first
estat firststage
// Panel B - SSI
ivregress 2sls dldSSI state#year msa lpop dlpop manu69 (dlctyInc=d_resval ///
L.d_resval L2.d_resval) if year>=1970, cluster(fips) first
estat firststage

**********************************************************
/* Regression (4) - 2SLS using year FE
Controlled for: 
1. State-year dummies
*/
// Panel A - DI
ivregress 2sls dlpay state#year (dlctyInc=d_resval L.d_resval L2.d_resval) ///
if year>=1970, cluster(fips) first
estat firststage
// Panel B - SSI
ivregress 2sls dldSSI state#year (dlctyInc=d_resval L.d_resval L2.d_resval) ///
if year>=1970, cluster(fips) first
estat firststage


*******************************************************
*** Table 6 Output
*******************************************************

*************************************************
// Diff in log of real earnings
reg dlctyInc state#year mdcoal lgcoal if year>=1970 & year<=1977, cluster(fips)
reg dlctyInc state#year mdcoal lgcoal if year>=1978 & year<=1982, cluster(fips)
reg dlctyInc state#year mdcoal lgcoal if year>=1983, cluster(fips)

*************************************************
// Diff in log of real Disability Insurance payments
reg dlpay state#year mdcoal lgcoal if year>=1970 & year<=1977, cluster(fips)
reg dlpay state#year mdcoal lgcoal if year>=1978 & year<=1982, cluster(fips)
reg dlpay state#year mdcoal lgcoal if year>=1983, cluster(fips)

*************************************************
// Diff in log of real SSI payment
reg dldSSI state#year mdcoal lgcoal if year>=1970 & year<=1977, cluster(fips)
reg dldSSI state#year mdcoal lgcoal if year>=1978 & year<=1982, cluster(fips)
reg dldSSI state#year mdcoal lgcoal if year>=1983, cluster(fips)

*******************************************************
*** Table 7 Output
*******************************************************
******************************************************
// Panel A: DI payment
* Column 1: counties with little -->large reserves
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if mdcoal==0 & boom, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if mdcoal==0 & peak, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if mdcoal==0 & bust, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if mdcoal==0, cluster(fips)

* Column 2: counties with moderate -->large reserves
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if oscoal==0 & boom, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if oscoal==0 & peak, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if oscoal==0 & bust, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if oscoal==0, cluster(fips)

* Column 3: counties with little --> moderate
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if lgcoal==0 & boom, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if lgcoal==0 & peak, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if lgcoal==0 & bust, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if lgcoal==0, cluster(fips)

* Column 4: three region comparison
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if boom, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if peak, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if bust, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year, cluster(fips)

******************************************************
// Panel B: SSI payment
* Column 1: counties with little -->large reserves
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if mdcoal==0 & boom, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if mdcoal==0 & peak, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if mdcoal==0 & bust, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if mdcoal==0, cluster(fips)

* Column 2: counties with moderate -->large reserves
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if oscoal==0 & boom, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if oscoal==0 & peak, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if oscoal==0 & bust, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if oscoal==0, cluster(fips)

* Column 3: counties with little --> moderate
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if lgcoal==0 & boom, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if lgcoal==0 & peak, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if lgcoal==0 & bust, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if lgcoal==0, cluster(fips)

* Column 4: three region comparison
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if boom, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if peak, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year if bust, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust ///
 mdcoal#boom mdcoal#peak mdcoal#bust) state#year, cluster(fips)

******************************************************
*** Table-8 output
******************************************************
// seam and time interactions
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
// Regression - row-1
ivregress 2sls dlpay state#year lgcoal mdcoal (dlctyInc=mdboom mdpeak ///
 mdbust lgboom lgpeak lgbust) if year>=1970, cluster(fips)

*****************************************************
// Regression - Two-way comparison by seam:
ivregress 2sls dlpay state#year lgcoal mdcoal (dlctyInc=mdboom mdpeak ///
 mdbust lgboom lgpeak lgbust) if year>=1970 & lgcoal!=1, cluster(fips)
ivregress 2sls dlpay state#year lgcoal mdcoal (dlctyInc=mdboom mdpeak ///
 mdbust lgboom lgpeak lgbust) if year>=1970 & mdcoal!=1, cluster(fips)
ivregress 2sls dlpay state#year lgcoal mdcoal (dlctyInc=mdboom mdpeak ///
 mdbust lgboom lgpeak lgbust) if year>=1970 & oscoal!=1, cluster(fips)

*****************************************************
// Regression - Two-way comparison by time period:
ivregress 2sls dlpay state#year lgcoal mdcoal (dlctyInc=mdboom mdpeak ///
 mdbust lgboom lgpeak lgbust) if year>=1970 & boom!=1, cluster(fips)
ivregress 2sls dlpay state#year lgcoal mdcoal (dlctyInc=mdboom mdpeak ///
 mdbust lgboom lgpeak lgbust) if year>=1970 & peak!=1, cluster(fips)
ivregress 2sls dlpay state#year lgcoal mdcoal (dlctyInc=mdboom mdpeak ///
 mdbust lgboom lgpeak lgbust) if year>=1970 & bust!=1, cluster(fips)

*****************************************************
// Regression - Alternative 2SLS estimates:
ivregress 2sls dlpay mdcoal lgcoal state#year (dlctyInc=d_resval L.d_resval ///
 L2.d_resval) if year>=1970, vce(cluster fips)
ivregress 2sls dlpay i.fips state#year (dlctyInc=d_resval L.d_resval ///
 L2.d_resval) if year>=1970, cluster(fips)
ivregress 2sls dlpay i.fips state#year (dlctyInc=lgboom lgpeak lgbust ///
 mdboom mdpeak mdbust) if year>=1970, cluster(fips)

 
******************************************************
*** Table 9 output
******************************************************
// generate difference of log Unemployment Insurance exp
gen ldUnemploy = ln(dUnemply)
bys fips (year): gen dldUnemploy = d.ldUnemploy
// OLS
reg dldUnemploy dlctyInc state#year if year>=1970 & year<=1993, cluster(fips)
// 2SLS
ivregress 2sls dldUnemploy state#year ///
(dlctyInc=mdboom mdpeak mdbust lgboom lgpeak lgbust) ///
if year>=1970 & year<=1993, cluster(fips)

