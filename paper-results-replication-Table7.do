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
** scale of coal-reserve 
******************************************************
// >1000m, 100m<x<1000m, <100m
gen lgcoal= (coalres>=1000 & !missing(coalres))
gen mdcoal= (coalres>=100 & coalres<1000 )
gen oscoal= (coalres<100)

gen boom=0
replace boom=1 if year>=1970 & year<=1977
gen peak=0
replace peak=1 if year>=1978 & year<=1982
gen bust=0
replace bust=1 if year>=1983 & year<=1993

******************************************************
** Panel A: DI payment
******************************************************
* Column 1: counties with little -->large reserves
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if mdcoal==0 & boom, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if mdcoal==0 & peak, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if mdcoal==0 & bust, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if mdcoal==0, cluster(fips)

* Column 2: counties with moderate -->large reserves
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if oscoal==0 & boom, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if oscoal==0 & peak, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if oscoal==0 & bust, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if oscoal==0, cluster(fips)

* Column 3: counties with little --> moderate
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if lgcoal==0 & boom, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if lgcoal==0 & peak, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if lgcoal==0 & bust, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if lgcoal==0, cluster(fips)

* Column 4: three region comparison
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if boom, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if peak, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if bust, cluster(fips)
ivregress 2sls dlpay (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year, cluster(fips)

******************************************************
** Panel B: SSI payment
******************************************************
* Column 1: counties with little -->large reserves
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if mdcoal==0 & boom, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if mdcoal==0 & peak, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if mdcoal==0 & bust, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if mdcoal==0, cluster(fips)

* Column 2: counties with moderate -->large reserves
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if oscoal==0 & boom, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if oscoal==0 & peak, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if oscoal==0 & bust, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if oscoal==0, cluster(fips)

* Column 3: counties with little --> moderate
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if lgcoal==0 & boom, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if lgcoal==0 & peak, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if lgcoal==0 & bust, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if lgcoal==0, cluster(fips)

* Column 4: three region comparison
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if boom, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if peak, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year if bust, cluster(fips)
ivregress 2sls dldSSI (dlctyInc= lgcoal#boom lgcoal#peak lgcoal#bust mdcoal#boom mdcoal#peak mdcoal#bust) state#year, cluster(fips)
