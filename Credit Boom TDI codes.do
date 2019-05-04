**************************************************************
**************************************************************
*******************Part 1: Merging Datasets*******************
**************************************************************
**************************************************************
clear
cd "C:\Users\HZhang2\Desktop\APD\creditBoom"

import excel "C:\Users\HZhang2\Desktop\APD\creditBoom\wb_data3.xlsx", sheet("wb_data3") firstrow
destring gov_csmp FDI edu trd_gdp RGDPPC_PPP, replace force
kountry CountryCode, from(iso3c) to(imfn)

rename CountryCode code_wb
rename _IMFN_ CountryCode
rename year Year
drop code_wb
drop if missing(CountryCode)
save "C:\Users\HZhang2\Desktop\APD\creditBoom\wbdata.dta",replace

//Merge with credit dataset
clear
use C:\Users\HZhang2\Desktop\APD\creditBoom\datastata.dta

merge 1:1 Year CountryCode using C:\Users\HZhang2\Desktop\APD\creditBoom\wbdata.dta
drop _merge
drop if missing(Country)
rename Unique unique
save "C:\Users\HZhang2\Desktop\APD\creditBoom\merge1.dta",replace

//Merge with crisis dataset
use C:\Users\HZhang2\Desktop\APD\creditBoom\crisis.dta
merge 1:1 unique using C:\Users\HZhang2\Desktop\APD\creditBoom\merge1.dta
drop year _merge
drop if missing(CountryCode) &  missing(Year)
save "C:\Users\HZhang2\Desktop\APD\creditBoom\merge2.dta",replace

//Merge with Financial Direct Investment(FDI) dataset
clear
import excel "\\DATA2\APD\Data\PIU\Export_Diversification\datacleaning\FDI_stata.xlsx", sheet("Panel") firstrow
rename code_id CountryCode
rename year Year
merge 1:1 CountryCode Year using C:\Users\HZhang2\Desktop\APD\creditBoom\merge2.dta
drop _merge

**************************************************************
**************************************************************
*******************Part 2: Data Cleaning**********************
**************************************************************
**************************************************************

//Convert variables to the right scale
**NGDP is in billion national currency, population is in units
sort Country Year
gen GDPPC = NGDP*1000000000/Pop //in national currency, unit
gen GDPPC_growth = 100*((GDPPC[_n]/GDPPC[_n-1])-1)

gen RGDPPC = RGDPPC_PPP/1000 //in thousands
gen RGDPPC1 = RGDP*1000000000/Pop //non-PPP RGDPPC
gen RGDPPC_growth = 100*((RGDPPC1[_n]/RGDPPC1[_n-1])-1)
gen RGDPG = 100*((RGDP[_n]/RGDP[_n-1])-1)
encode Region, gen(region)
encode Income_IMF, gen(income)

//clean up years
drop if Year < 1992
drop if Year ==2017
drop if missing(Year)

// Drop special country cases and small states
foreach num of numlist 612 624 632 684 716 512 636 732 967 614	912	419	218	516	628	634	248	642	646	429	433	916	443	672	694	449	453	922	456	733	537	369	925	466	299	474 {
drop if CountryCode == `num'
}

//clean up outliners in terms of extreme GDP growth or credit growth
quietly sum RGDPPC_growth, d
gen RGDPPC_g_trim = RGDPPC_growth
replace RGDPPC_g_trim = . if RGDPPC_g_trim <= r(p1)|RGDPPC_g_trim>= r(p99)
replace RGDPPC_growth = RGDPPC_g_trim
drop RGDPPC_g_trim

replace cred2gdp = . if cred2gdp > 180 | cred2gdp < 5
replace credgrowth = . if credgrowth > 90 | credgrowth < -40
gen APD = 0
replace APD = 1 if Region == "APD"

// Collapse data to generate 5 year average for Generalized Method of Moments(GMM) regressions 
sort CountryCode Year
xtile quant6 = Year, nq(5)
collapse (mean) RGDPPC_growth credgrowth cred2gdp gov_csmp Crisis edu trd_gdp FDI inflation FD (first) Income_IMF Country Region RGDPPC APD , by(CountryCode quant6)

//generate interation terms and controls
replace RGDPPC_growth = RGDPPC_growth/100
replace cred2gdp = cred2gdp/100
gen credgrowth2 = credgrowth^2
gen cred2gdp2 = cred2gdp^2

gen interaction = credgrowth * cred2gdp
gen interaction2 = interaction^2
gen control = RGDPPC * credgrowth
gen control2 = control^2

tabulate quant6, generate(t_)
replace Crisis = 1 if Crisis !=. & Crisis !=0

// Generate log form of each control variable
foreach var of varlist FDI edu gov_csmp trd_gdp RGDPPC {
	gen l`var' = log(`var')
}

foreach var of varlist FDI edu gov_csmp trd_gdp  {
	gen n`var' = `var'/100
	replace `var' = n`var'
	drop n`var'
}

// Classify countries based on their financial development index.
// Data source: http://data.imf.org/?sk=F8032E80-B36C-43B1-AC26-493C5B1CD33B

** Countries with high-level of financial development
gen FD_AD = 0
replace FD_AD = 1 if FD > .7224529  & !missing(FD)
gen FD_control = FD_AD * credgrowth
gen FD_control2 = FD_control ^2

** Countries with low-level of financial development
gen FD_LD = 0
replace FD_LD = 1 if FD <  .15   & !missing(FD)
gen LD_control = FD_LD * credgrowth
gen LD_control2 = LD_control ^2


**************************************************************
**************************************************************
**************Part 3: Panel Regression GMM *******************
**************************************************************
**************************************************************
xtset CountryCode quant6
****************************
local controls FDI Crisis edu gov_csmp trd_gdp  
local cred credgrowth credgrowth2 LD_control2
xtabond2 RGDPPC_growth lRGDPPC `cred' `controls' t_*, gmm(lRGDPPC `controls', lag(1 3) ) gmm(`cred', lag(1 2) collapse) iv(t_*) two robust
outreg2 using Results.xls, append ctitle(regression 0) label tstat coefastr addstat(AR2 ,e(ar2p),Hansen,e(hansenp),Instruments,e(j)) tdec(3) drop(t_*)
****************************
local controls FDI Crisis edu gov_csmp trd_gdp  
local cred credgrowth credgrowth2 LD_control LD_control2
xtabond2 RGDPPC_growth lRGDPPC `cred' `controls' t_*, gmm(lRGDPPC `controls', lag(1 3) ) gmm(`cred', lag(1 2) collapse) iv(t_*) two robust
outreg2 using Results.xls, append ctitle(regression 0) label tstat coefastr addstat(AR2 ,e(ar2p),Hansen,e(hansenp),Instruments,e(j)) tdec(3) drop(t_*)
****************************
local controls FDI Crisis edu gov_csmp trd_gdp
local cred credgrowth credgrowth2 FD_control2
xtabond2 RGDPPC_growth lRGDPPC `cred' `controls' t_*, gmm(lRGDPPC `controls', lag(1 3) ) gmm(`cred', lag(1 2) collapse) iv(t_*) two robust
outreg2 using Results.xls, append ctitle(regression 0) label tstat coefastr addstat(AR2 ,e(ar2p),Hansen,e(hansenp),Instruments,e(j)) tdec(3) drop(t_*)
****************************
local controls FDI Crisis edu gov_csmp trd_gdp
local cred credgrowth credgrowth2 FD_control FD_control2
xtabond2 RGDPPC_growth lRGDPPC `cred' `controls' t_*, gmm(lRGDPPC `controls', lag(1 3) ) gmm(`cred', lag(1 2) collapse) iv(t_*) two robust
outreg2 using Results.xls, append ctitle(regression 0) label tstat coefastr addstat(AR2 ,e(ar2p),Hansen,e(hansenp),Instruments,e(j)) tdec(3) drop(t_*)
****************************

**************************************************************
**************************************************************
**************Part 4: Logit Regression *** *******************
**************************************************************
**************************************************************

****************************
xtlogit Crisis CAD rev_imp inflation interaction credgrowth RGDPgrowth if cred2gdp > 90 , fe nolog // not significant
margins, at(interaction = (0(100)3000)) atmeans
marginsplot, ylabel(0(0.5)1.5) xtitle("Credit growth x Credit-to-GDP")
outreg2 using 4-9-2019.xls, append
****************************
xtlogit Crisis CAD rev_imp inflation interaction credgrowth RGDPgrowth if cred2gdp < 50 , fe nolog // not significant
margins, at(interaction = (0(100)10000)) atmeans
marginsplot, ylabel(0(0.5)1.5) xtitle("Credit growth x Credit-to-GDP")
****************************

