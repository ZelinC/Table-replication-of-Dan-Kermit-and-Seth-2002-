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
gen lgcoal=0
gen mdcoal=0
gen oscoal=0
replace lgcoal=1 if coalres>=1000 & !missing(coalres)
replace mdcoal=1 if coalres>=100 & coalres<1000 
replace oscoal=1 if coalres<100

// easier for tabstat
gen coalsize=0
replace coalsize=2 if coalres>=1000 & !missing(coalres)
replace coalsize=1 if coalres>=100 & coalres<1000 

// boom and bust periods
gen boom=0
replace boom=1 if year>=1970 & year<=1977
gen bust=0
replace bust=1 if year>=1983 & year<=1993

*******************************************************
*** Tabstat Output
*******************************************************
tabstat dldSSI dlpay dlctyInc dlpop dlpcoal d_resval coalres propmanu msa ///
if boom==1, s(mean sd) by(coalsize)
tabstat propmanu if year==1969, s(mean sd) by(coalsize)
tabstat dldSSI dlpay dlctyInc dlpop dlpcoal d_resval Pop if  bust==1, ///
s(mean sd) by(coalsize)
