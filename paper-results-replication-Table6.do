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
*** Logarithmic difference in county earnings
******************************************************
gen lctyInc = ln(EarnPOW/p7)
bys fips (year): gen dlctyInc = d.lctyInc

******************************************************
** scale of coal-reserve 
******************************************************
// >1000m, 100m<x<1000m, <100m
gen lgcoal= (coalres>=1000) & !missing(coalres)
gen mdcoal= (coalres>=100 & coalres<1000 )
gen oscoal= (coalres<100)

*************************************************
*** Diff in log of real earnings
*************************************************
reg dlctyInc state#year mdcoal lgcoal if year>=1970 & year<=1977, cluster(fips)
reg dlctyInc state#year mdcoal lgcoal if year>=1978 & year<=1982, cluster(fips)
reg dlctyInc state#year mdcoal lgcoal if year>=1983, cluster(fips)

*************************************************
*** Diff in log of real Disability Insurance payments
*************************************************
reg dlpay state#year mdcoal lgcoal if year>=1970 & year<=1977, cluster(fips)
reg dlpay state#year mdcoal lgcoal if year>=1978 & year<=1982, cluster(fips)
reg dlpay state#year mdcoal lgcoal if year>=1983, cluster(fips)

*************************************************
*** Diff in log of real SSI payment
*************************************************
reg dldSSI state#year mdcoal lgcoal if year>=1970 & year<=1977, cluster(fips)
reg dldSSI state#year mdcoal lgcoal if year>=1978 & year<=1982, cluster(fips)
reg dldSSI state#year mdcoal lgcoal if year>=1983, cluster(fips)
