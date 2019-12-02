/* Master-file V-Dem civil peace */
/* Last changed 040718 by HF */

/*
capture cd "C:/Users/havnyg/Dropbox/"
capture cd "/Users/havard/Dropbox/"
capture cd "/Users/efx/Dropbox"
capture cd "/Users/havard/Dropbox"
capture cd "C:/Users/efx/Dropbox"
capture cd "C:/Users/havnyg/Dropbox"
capture cd "~/Dropbox"
capture cd "/Users/hannefjelde/Dropbox"

cd "Collaborations/V-DemCivilPeace/"
set scheme s1mono
set more off
*/


/* Base everything on a copy of the masterdata from the FVP book project */

use  "InputData/FVP_masterdata_copy.dta", clear

drop if gwno == 1000
drop if year > 2016 /* Remove projection-only part of dataset */
drop if year < 1900

/* Conflict onset variable. Censored if conflict is going on */
sort gwno year
gen c_onset = .
replace c_onset = 1 if (conflict==1 | conflict == 2) & conflict[_n-1]==0 & gwno==gwno[_n-1] 
replace c_onset = 0 if conflict == 0

sort gwno year
gen c2_onset = .
replace c2_onset = 1 if (conflict==1 | conflict == 2) & conflict[_n-1]==0 & conflict[_n-2]==0 & gwno==gwno[_n-1]
replace c2_onset = 0 if conflict== 0 
replace c2_onset =. if (conflict[_n-1]==1 | conflict[_n-1]==2) &  c2_onset[_n+1]==. & (conflict[_n+1]==1 | conflict[_n+1]==2)  

sort gwno year
gen c2war_onset = .
replace c2war_onset = 1 if (conflict == 2) & (conflict[_n-1]==0 |conflict[_n-1]==1) & conflict[_n-2]==0 & gwno==gwno[_n-1]
replace c2war_onset = 0 if conflict==0 
replace c2war_onset =. if (conflict[_n-1]==2) &  c2war_onset[_n+1]==. & (conflict[_n+1]==2)  

/* ======================================================== */
save "Tee/whole.dta", replace
/* Inspecting decay variable */
keep gwno year c_onset c2_onset c2war_onset conflict
save "Tee/onset.dta", replace
/* ========================= */
use "Tee/whole.dta"
/* ======================================================== */


* Decay function
btscs c_onset year gwno, g(no_c_onset) nspl(3)
replace no_c_onset=999 if gwno!= gwno[_n-1] & c_onset==0
by gwno: replace no_c_onset=999 if no_c_onset[_n-1]==999 & c_onset[_n-1]==0
drop _spline*
generate decay_c= 2^(-no_c_onset/2)

btscs c2_onset year gwno, g(no_c2_onset) nspl(3)
replace no_c2_onset=999 if gwno!= gwno[_n-1] & c2_onset==0
by gwno: replace no_c2_onset=999 if no_c2_onset[_n-1]==999 & c2_onset[_n-1]==0
drop _spline*
generate decay_c2= 2^(-no_c2_onset/2)

btscs c2war_onset year gwno, g(no_c2war_onset) nspl(3)
replace no_c2war_onset=999 if gwno!= gwno[_n-1] & c2_onset==0
by gwno: replace no_c2war_onset=999 if no_c2war_onset[_n-1]==999 & c2war_onset[_n-1]==0
drop _spline*
generate decay_c2war= 2^(-no_c2war_onset/2)

/* ======================================================== */
save "Tee/whole.dta", replace
/* Inspecting decay variable */
keep gwno year decay_c2war decay_c2 decay_c conflict
save "Tee/decayvars.dta", replace
/* ========================= */
use "Tee/whole.dta"
/* ======================================================== */
	
	
/* Dummy to account for UCDP/COW coding differences */	
gen cowyear = 0
replace cowyear = 1 if year <= 1945

/* ======================================================== */
save "Tee/whole.dta", replace
/* Inspecting decay variable */
keep gwno year cowyear 
save "Tee/cowyear.dta", replace
/* ========================= */
use "Tee/whole.dta"
/* ======================================================== */

/* Preparing other variables */
gen lnpop = lnpop200
gen lnGDPcap = lnGDPPerCap
summ lnpop
summ lnGDPcap
 
 
*Region dummies 
generate africa=0 
replace africa=1 if region_name=="Southern Africa"
replace africa=1 if region_name== "West Africa"
replace africa=1 if region_name== "East Africa"

generate asia=0
replace asia=1 if  region_name=="Central Asia"
replace asia=1 if  region_name=="East/South Asia"

generate europe=0
replace europe=1 if  region_name=="West Europe"
replace europe=1 if  region_name=="East Europe"

generate MENA=0
replace MENA=1 if region_name=="MENA"

generate  america=0
replace america=1 if region_name=="Central/South America"


*Decade dummies
generate  _40=0 
  replace _40=1 if year>=1944 & year<=1949 

generate _50=0 
  replace _50=1 if year>=1950 & year<=1959 

generate   _60=0 
  replace _60=1 if year>=1960 & year<=1969 

generate   _70=0 
  replace _70=1 if year>=1970 & year<=1979 

generate   _80=0 
  replace _80=1 if year>=1980 & year<=1989 

generate   _90=0 
  replace _90=1 if year>=1990 & year<=1999 

generate   _00=0 
  replace _00=1 if year>=2000 & year<=2009 

generate   _10=0 
  replace _10=1 if year>=2010 & year<=2019 


/* V-Dem variables */
 
/* Vertical accountability */
summarize v2x_suffr v2xel_frefair v2x_elecoff v2elembaut v2elembcap v2elrgstry v2elvotbuy v2elirreg v2elfrfair

sort gwno year
foreach elvar in v2elrgstry v2elvotbuy v2elirreg v2elfrfair {
	replace `elvar' = `elvar'[_n-1] if gwno == gwno[_n-1] & `elvar' ==. & v2x_elecreg == 1
	replace `elvar' = 0 if `elvar' ==. & v2x_elecreg == 0
}

gen FHKN_frefair_temp = (v2elembaut + v2elembcap + v2elrgstry + v2elvotbuy + v2elirreg + v2elfrfair)/6
gen FHKN_frefair = (FHKN_frefair_temp + 2.533413)/ 6.125325


gen free_fair_elections_narrow = (v2x_suffr * FHKN_frefair * v2x_elecoff)
gen free_fair_elections = (v2x_suffr * FHKN_frefair * v2x_elecoff * v2x_frassoc_thick * v2x_freexp_thick)

summarize v2x_suffr v2xel_frefair v2x_elecoff v2elembaut v2elembcap v2elrgstry v2elvotbuy v2elirreg ///
	v2elfrfair v2x_elecreg FHKN_frefair* free_fair_elections
	
gen combined_vertical = (2*free_fair_elections + v2x_cspart)/3

/* Other civil liberties */
summarize v2x_freexp_thick v2x_frassoc_thick
gen civlib = (v2x_freexp_thick + v2x_frassoc_thick)/ 2

/* Horizontal accountability */	
capture drop FHKN_RoL	
gen FHKN_RoL = (v2clrspct + v2cltrnslw +(v2clprptym + v2clprptyw)/2 )/3
summarize FHKN_RoL, detail
replace FHKN_RoL = (FHKN_RoL - r(min)) / (r(max) - r(min))
summarize FHKN_RoL, detail

summarize v2xlg_legcon v2x_jucon v2xcl_rol
gen horizontal_constraint =  (v2xlg_legcon  + v2x_jucon + FHKN_RoL)/3

gen horizontal_constraint_narrow =  (v2xlg_legcon + v2x_jucon)/2

/* ======================================================== */
save "Tee/whole.dta", replace
/* Inspecting decay variable */
keep gwno year v2elrgstry v2elvotbuy v2elirreg v2elfrfair 
save "Tee/elvars.dta", replace
/* ========================= */
use "Tee/whole.dta"
/* ======================================================== */

/* ======================================================== */
save "Tee/whole.dta", replace
/* Inspecting decay variable */
keep gwno year FHKN_frefair_temp FHKN_frefair free_fair_elections_narrow free_fair_elections combined_vertical civlib FHKN_RoL horizontal_constraint horizontal_constraint_narrow 
save "Tee/vdem.dta", replace
/* ========================= */
use "Tee/whole.dta"
/* ======================================================== */


/* Vreeland's X-Polity index */
replace xconst = . if xconst < -10
replace xropen = . if xropen < -10
replace xrcomp = . if xrcomp < -10
gen x_polity = (xconst + xropen + xrcomp) - 7
gen x_polity_sq = x_polity^2

/* ======================================================== */
save "Tee/whole.dta", replace
/* Inspecting decay variable */
keep gwno year x_polity x_polity_sq 
save "Tee/xpolity.dta", replace
/* ========================= */
use "Tee/whole.dta"
/* ======================================================== */


* Change in institutions

correlate horizontal_constraint_narrow free_fair_elections

by gwno: generate hc_fd= horizontal_constraint_narrow-horizontal_constraint_narrow[_n-1]

generate hc_fd_neg=0 if hc_fd!=.
by gwno: replace hc_fd_neg=hc_fd if hc_fd<0 & hc_fd!=.

generate hc_change_neg=0 if hc_fd_neg!=. 
replace hc_change_neg=1 if hc_fd_neg<=-0.1 & hc_fd_neg!=.

generate hc_fd_pos=0 if hc_fd!=.
by gwno: replace hc_fd_pos=hc_fd if hc_fd>0 & hc_fd!=.

generate hc_change_pos=0 if hc_fd_pos!=. 
replace hc_change_pos=1 if hc_fd_pos>=0.1 & hc_fd_pos!=.

btscs hc_change_neg year gwno, g(no_hc_change_neg) nspl(3)
replace no_hc_change_neg=999 if gwno!= gwno[_n-2] & hc_change_neg==0 
by gwno: replace no_hc_change_neg=999 if no_hc_change_neg[_n-1]==999 & hc_change_neg[_n-1]==0  
drop _spline*
generate hc_neg_decay= 2^(-no_hc_change_neg/2) 

btscs hc_change_pos year gwno, g(no_hc_change_pos) nspl(3)
replace no_hc_change_pos=999 if gwno!= gwno[_n-2] & hc_change_pos==0 
by gwno: replace no_hc_change_pos=999 if no_hc_change_pos[_n-1]==999 & hc_change_pos[_n-1]==0  
drop _spline*
generate hc_pos_decay= 2^(-no_hc_change_pos/2)
 
generate t_dem_hc=hc_pos_decay if hc_change_pos[_n-1]==1 & hc_change_neg!=1
replace t_dem_hc= hc_pos_decay if t_dem_hc[_n-1]!=. &  hc_change_neg[_n-1]!=1
replace t_dem_hc=0 if  t_dem_hc==. & hc_fd!=.

generate t_aut_hc=hc_neg_decay if hc_change_neg[_n-1]==1 & hc_change_pos!=1
replace t_aut_hc= hc_neg_decay if t_aut_hc[_n-1]!=. &  hc_change_pos[_n-1]!=1
replace t_aut_hc=0 if  t_aut_hc==. & hc_fd!=.

/* ======================================================== */
save "Tee/whole.dta", replace
/* Inspecting decay variable */
keep gwno year hc_fd_neg hc_change_neg hc_fd_pos hc_change_pos t_dem_hc t_aut_hc 
save "Tee/institutions.dta", replace
/* ========================= */
use "Tee/whole.dta"
/* ======================================================== */
 
 
* Elections change
su free_fair_elections free_fair_elections

by gwno: generate ff_fd= free_fair_elections-free_fair_elections[_n-1]

generate ff_fd_neg=0 if ff_fd!=.
by gwno: replace ff_fd_neg=ff_fd if ff_fd<0 & ff_fd!=.

generate ff_change_neg=0 if ff_fd_neg!=. 
replace ff_change_neg=1 if ff_fd_neg<=-0.1 & ff_fd_neg!=.

generate ff_fd_pos=0 if ff_fd!=.
by gwno: replace ff_fd_pos=ff_fd if ff_fd>0 & ff_fd!=.

generate ff_change_pos=0 if ff_fd_pos!=. 
replace ff_change_pos=1 if ff_fd_pos>=0.1 & ff_fd_pos!=.

/* ======================================================== */
save "Tee/whole.dta", replace
/* Inspecting decay variable */
keep gwno year ff_fd ff_change free_fair_elections 
save "Tee/elections_change.dta", replace
/* ========================= */
use "Tee/whole.dta"
/* ======================================================== */


btscs ff_change_neg year gwno, g(no_ff_change_neg) nspl(3)
replace no_ff_change_neg=999 if gwno!= gwno[_n-2] & ff_change_neg==0 
by gwno: replace no_ff_change_neg=999 if no_ff_change_neg[_n-1]==999 & ff_change_neg[_n-1]==0  
drop _spline*
generate ff_neg_decay= 2^(-no_ff_change_neg/2) 

btscs ff_change_pos year gwno, g(no_ff_change_pos) nspl(3)
replace no_ff_change_pos=999 if gwno!= gwno[_n-2] & ff_change_pos==0 
by gwno: replace no_ff_change_pos=999 if no_ff_change_pos[_n-1]==999 & ff_change_pos[_n-1]==0  
drop _spline*
generate ff_pos_decay= 2^(-no_ff_change_pos/2)
 
generate t_dem_ff=ff_pos_decay if ff_change_pos[_n-1]==1 & ff_change_neg!=1
replace t_dem_ff= ff_pos_decay if t_dem_ff[_n-1]!=. &  ff_change_neg[_n-1]!=1
replace t_dem_ff=0 if  t_dem_ff==. & ff_fd!=.

generate t_aut_ff=ff_neg_decay if ff_change_neg[_n-1]==1 & ff_change_pos!=1
replace t_aut_ff= ff_neg_decay if t_aut_ff[_n-1]!=. &  ff_change_pos[_n-1]!=1
replace t_aut_ff=0 if  t_aut_ff==. & ff_fd!=.

/* ======================================================== */
save "Tee/whole.dta", replace
/* Inspecting decay variable */
keep gwno year ff_fd ff_change free_fair_elections ff_pos_decay ff_neg_decay t_dem_ff t_aut_ff 
save "Tee/elections_change.dta", replace
/* ========================= */
use "Tee/whole.dta"
/* ======================================================== */
 
/* Descriptive statistics */
/*
foreach ind in v2x_polyarchy v2x_frassoc_thick v2x_suffr v2xel_frefair v2x_accex v2x_freexp_thick ///
	v2x_cspart  v2clrelig v2lgqumin v2xcl_dmove v2pepwrsoc v2xlg_legcon v2x_jucon v2xcl_rol minority_rep {
	histogram `ind' if year == 1950, bin(30)
	graph export "Figures/`ind'Hist1950.pdf", replace
	histogram `ind' if year == 2012, bin(30)
	graph export "Figures/`ind'Hist2012.pdf", replace
}

corr v2x_polyarchy v2x_frassoc_thick v2x_suffr v2xel_frefair v2x_elecoff v2x_freexp_thick ///
	v2x_cspart  v2clrelig  v2xcl_dmove v2pepwrsoc v2xlg_legcon v2x_jucon v2xcl_rol ///
	minority_rep free_fair_elections horizontal_constraint civlib*/
 
corr horizontal_constraint_narrow  free_fair_elections


* Generate lagged versions of variables to make it easier
tsset gwno year
by gwno: generate lhorizontal_constraint_narrow=l.horizontal_constraint_narrow
by gwno: generate lfree_fair_elections=l.free_fair_elections
by gwno: generate llnpop200=l.lnpop200
by gwno: generate llnGDPPerCapita200=l.lnGDPPerCapita200
by gwno: generate lgrGDPPercapita200=l.grGDPPercapita200
by gwno: generate llnGDPcap_oilrent=l.lnGDPcap_oilrent
by gwno: generate lt_dem_ff=l.t_dem_ff
by gwno: generate lt_aut_ff= l.t_aut_ff
by gwno: generate lt_dem_hc=l.t_dem_hc
by gwno: generate lt_aut_hc= l.t_aut_hc
by gwno: generate lx_polity=l.x_polity 
by gwno:  generate lx_polity_sq=l.x_polity_sq

generate free_fair_elections_sq=free_fair_elections*free_fair_elections
generate horizontal_constraint_narrow_sq=horizontal_constraint_narrow*horizontal_constraint_narrow

by gwno:  generate lhorizontal_constraint_narrow_sq=l.horizontal_constraint_narrow_sq
by gwno:  generate lfree_fair_elections_sq=l.free_fair_elections_sq
 
gen vert_hor_int = free_fair_elections*horizontal_constraint_narrow 
by gwno:  generate lvert_hor_int=l.vert_hor_int

/* ======================================================== */
save "Tee/whole.dta", replace
/* Inspecting decay variable */
keep gwno year  lhorizontal_constraint_narrow lfree_fair_elections llnpop200 llnGDPPerCapita200 lgrGDPPercapita200 llnGDPcap_oilrent lt_dem_ff lt_aut_ff lt_dem_hc lt_aut_hc lx_polity lx_polity_sq free_fair_elections_sq horizontal_constraint_narrow_sq lhorizontal_constraint_narrow_sq lfree_fair_elections_sq free_fair_elections horizontal_constraint_narrow lvert_hor_int
save "Tee/lagged.dta", replace
/* ========================= */
use "Tee/whole.dta"
/* ======================================================== */

************************************************************************************
* Models for paper May 2018
************************************************************************************
 

* Table 1: Main model: main variablels, random effects and longer time series 
 
eststo clear
eststo: xi: logit c2_onset lfree_fair_elections decay_c2 llnpop200 llnGDPPerCapita200  if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . , cl(gwno)
eststo: xi: logit c2_onset lhorizontal_constraint_narrow decay_c2 llnpop200 llnGDPPerCapita200  if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != .  , cl(gwno)
eststo: xi: xtlogit c2_onset lfree_fair_elections decay_c2 llnpop200 llnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow!= ., re
eststo: xi: xtlogit c2_onset lhorizontal_constraint_narrow decay_c2 llnpop200 llnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != ., re
eststo: xi: logit c2_onset lfree_fair_elections decay_c2 llnpop200 llnGDPPerCapita200 if year <= 2016 & year>=1900 & free_fair_elections != . & horizontal_constraint_narrow!= ., cl(gwno)
eststo: xi: logit c2_onset lhorizontal_constraint_narrow decay_c2 llnpop200 llnGDPPerCapita200 if year <= 2016 & year>=1900 & free_fair_elections != . & horizontal_constraint_narrow !=., cl(gwno)

esttab using "Models_Table1.tex", replace ///
	se nogaps stats(aic ll N)  ///
	coeflabels(decay_c2 "Time since conflict" llnpop200 "Population, log" llnGDPPerCapita200 "GDP per cap log" lfree_fair_elections "Vertical constraints" ///
	lhorizontal_constraint_narrow "Horizontal constraints"  ///
	cowyear "COW conflict/before 1946") /// 
	order (lfree_fair_elections lhorizontal_constraint_narrow llnpop200 llnGDPPerCapita200 decay_c2 ) ///
	mtitles("1" "2" "3" "4" "5" "6") nonumbers nodepvars
		

eststo: logit c2_onset lfree_fair_elections decay_c2 llnpop200 llnGDPPerCapita200  if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . , cl(gwno)		
su lfree_fair_elections if e(sample), detail

margins, at (lfree_fair_elections=(0 .64)) atmeans vsquish		
	

eststo: xi: logit c2_onset lhorizontal_constraint_narrow decay_c2 llnpop200 llnGDPPerCapita200  if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != .  , cl(gwno)
su lhorizontal_constraint_narrow if e(sample), detail

margins, at (lhorizontal_constraint_narrow=(.149 .932)) atmeans vsquish		
	

logit c2_onset lfree_fair_elections decay_c2 llnpop200 llnGDPPerCapita200  if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . , cl(gwno)
	
su c2_onset lfree_fair_elections lhorizontal_constraint_narrow llnpop200 llnGDPPerCapita200 llnGDPcap_oilrent lgrGDPPercapita200 ltimeindep decay_c2 lt_dem_ff lt_aut_ff lt_dem_hc lt_aut_hc lx_polity if e(sample)


saveold  "InputData/GAMData.dta", replace
	
*Table 2: additional confounders
 
eststo clear
eststo: xi: logit c2_onset lfree_fair_elections lgrGDPPercapita200 llnGDPcap_oilrent ltimeindep decay_c2 llnpop200 llnGDPPerCapita200  if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow!= ., cl(gwno)
eststo: xi: logit c2_onset lhorizontal_constraint_narrow lgrGDPPercapita200 llnGDPcap_oilrent ltimeindep decay_c2 llnpop200 llnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != ., cl(gwno)
eststo: xi: logit c2_onset lfree_fair_elections decay_c2 llnpop200 llnGDPPerCapita200 _40 _50 _60 _70 _80 _90 _00 _10 america MENA africa asia  if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . , cl(gwno) 
eststo: xi: logit c2_onset lhorizontal_constraint_narrow decay_c2 llnpop200 llnGDPPerCapita200 _40 _50 _60 _70 _80 _90 _00 _10 america MENA africa asia   if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != .  , cl(gwno)
eststo: xi: logit c2_onset lfree_fair_elections decay_c2 llnpop200 llnGDPPerCapita200  lt_dem_ff lt_aut_ff if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . , cl(gwno) 
eststo: xi: logit c2_onset lhorizontal_constraint_narrow decay_c2 llnpop200 llnGDPPerCapita200 lt_dem_hc lt_aut_hc if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != .  , cl(gwno)
eststo: xi: logit c2_onset lfree_fair_elections lhorizontal_constraint_narrow decay_c2 llnpop200 llnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . , cl(gwno)

esttab using "Models_Table2.tex", replace ///
	se nogaps stats(aic ll N)  ///
	coeflabels (decay_c2 "Time since conflict" llnpop200 "ln population" llnGDPPerCapita200 "ln GDP per cap" lfree_fair_elections "Vertical constraints" ///
	lhorizontal_constraint_narrow "Horizontal constraints" ///
	 grGDPPercapita200 "Economic growth" lt_dem_ff "Time since dem,VC"  ltimeindep "Time since independence" llnGDPcap_oilrent "ln oil rent GDP pc" lt_aut_ff "Time since aut,VC" lt_dem_hc "Time since dem,HC" lt_aut_hc "Time since aut,HC" cowyear "COW conflict/before 1946") ///
	order(lfree_fair_elections lhorizontal_constraint_narrow llnpop200 llnGDPPerCapita200 decay_c2) ///
	mtitles("1" "2" "3" "4" "5" "6" "7") nonumbers nodepvars
		


*Table 3: non-monotonic and interaction effects

eststo clear

eststo: xi:logit c2_onset  lx_polity lx_polity_sq  decay_c2 llnpop200 llnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . , cl(gwno)
eststo: xi:logit c2_onset  lfree_fair_elections lx_polity lx_polity_sq  decay_c2 llnpop200 llnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != ., cl(gwno)
eststo: xi: logit c2_onset lhorizontal_constraint_narrow  lx_polity lx_polity_sq decay_c2 llnpop200 llnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . , cl(gwno)
eststo: xi: logit c2_onset c.lfree_fair_elections##c.lfree_fair_elections decay_c2 llnpop200 llnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != ., cl(gwno)
eststo: xi: logit c2_onset c.lhorizontal_constraint_narrow##c.lhorizontal_constraint_narrow decay_c2 llnpop200 llnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . , cl(gwno)

* Interaction models - two and four-way
eststo: xi: logit c2_onset c.lfree_fair_elections##c.lhorizontal_constraint_narrow decay_c2 llnpop200 llnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . , cl(gwno)
eststo: xi: logit c2_onset c.lfree_fair_elections##c.lhorizontal_constraint_narrow##c.lhorizontal_constraint_narrow##c.lfree_fair_elections  decay_c2 llnpop200 llnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . , cl(gwno)

xi: logit c2_onset lfree_fair_elections lhorizontal_constraint_narrow ///
	 c.lfree_fair_elections#c.lhorizontal_constraint_narrow  ///
	 c.lhorizontal_constraint_narrow#c.lhorizontal_constraint_narrow  ///
	 c.lfree_fair_elections#c.lhorizontal_constraint_narrow#c.lhorizontal_constraint_narrow   ///
	 c.lfree_fair_elections#c.lfree_fair_elections ///
	 c.lfree_fair_elections#c.lhorizontal_constraint_narrow#c.lfree_fair_elections ///
	 c.lfree_fair_elections#c.lhorizontal_constraint_narrow#c.lhorizontal_constraint_narrow#c.lfree_fair_elections ///
	 decay_c2 llnpop200 llnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . , cl(gwno)

	 
esttab using "Models_Table3.tex", replace ///
	se nogaps stats(aic ll N)  ///
	coeflabels (decay_c2 "Time since conflict" llnpop200 "ln population" llnGDPPerCapita200 "ln GDP per cap" lfree_fair_elections "Vertical constraints" ///
	lhorizontal_constraint_narrow "Horizontal constraints" ///
	lx_polity "Xpolity" lx_polity_sq "Xpolity sq" lvert_hor_int "Vertical*Horizontal"cowyear "COW conflict/before 1946") ///
	order(lfree_fair_elections lhorizontal_constraint_narrow lvert_hor_int llnpop200 llnGDPPerCapita200 decay_c2) ///
	mtitles("1" "2" "3" "4" "5" "6" "7") nonumbers nodepvars
		

		
/* Graphs non-monotonic */
		
eststo: logit c2_onset c.lfree_fair_elections##c.lfree_fair_elections decay_c2 llnpop200 llnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != ., cl(gwno)
su lfree_fair_elections if e(sample) 

set more off
margins, at (lfree_fair_elections=(0 (0.01).9)) atmeans vsquish		
marginsplot,recast(line) plotopts(lwidth(medthick)) recastci(rline) ciopts(lwidth(vthin) lpattern(longdash)) level(95) xlabel(0(.1)1)



eststo: logit c2_onset c.lhorizontal_constraint_narrow##c.lhorizontal_constraint_narrow  decay_c2 llnpop200 llnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . , cl(gwno)
su lhorizontal_constraint_narrow if e(sample) 
set more off
margins, at (lhorizontal_constraint_narrow=(0 (0.01) 1)) atmeans  vsquish		
marginsplot,recast(line) plotopts(lwidth(medthick)) recastci(rline) ciopts(lwidth(vthin) lpattern(longdash)) level(95) xlabel(0(.1)1)


/* Graphs two-way interaction */
eststo: xi: logit c2_onset c.lfree_fair_elections_10##c.lhorizontal_constraint_narrow  decay_c2 llnpop200 llnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . , cl(gwno)
qui margins, dydx(lfree_fair_elections_10) at(lhorizontal_constraint_narrow = (0(0.1)1))   
marginsplot, recast(line) plotopts(lwidth(medthick)) recastci(rline) ciopts(lwidth(vthin) lpattern(longdash)) level(95) xlabel(0(.01)1)

				
/* Graphs four-way interaction */
eststo: xi: logit c2_onset c.lfree_fair_elections##c.lhorizontal_constraint_narrow##c.lhorizontal_constraint_narrow##c.lfree_fair_elections  decay_c2 llnpop200 llnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . , cl(gwno)
qui margins, dydx(lfree_fair_elections) at(lhorizontal_constraint_narrow = (0(0.1)1))   
marginsplot,recast(line) plotopts(lwidth(medthick)) recastci(rline) ciopts(lwidth(vthin) lpattern(longdash)) level(95) xlabel(0(.1)1)





 /* Graphs interaction - twoway */
eststo: xi: logit c2_onset c.lfree_fair_elections##c.lhorizontal_constraint_narrow  decay_c2 llnpop200 llnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . , cl(gwno)

su lfree_fair_elections lhorizontal_constraint_narrow if e(sample), detail

set more off
intgph logit c2_onset decay_c2 llnpop200 llnGDPPerCapita200 ///
	if year <= 2016 & year>1945 & lfree_fair_elections != . & lhorizontal_constraint_narrow!= ., ///
	ivars( lhorizontal_constraint_narrow lfree_fair_elections) cmdopts(r) difvals(.00066 .46464) ///
	ytitle(Change in pr(y)) ///
	gphopts(yline(0)) xinc(200)
*graph export "../../Apps/ShareLaTex/V-dem civil peace/Figures/vert_hor_int_dif.pdf", replace
graph export "/Users/hannefjelde/Dropbox/Collaborations/V-DemCivilPeace/Figures/vert_hor_int_dif.pdf", replace

intgph logit c2_onset decay_c2 llnpop200 llnGDPPerCapita200  ///
	if year <= 2016 & year>1945 & lfree_fair_elections != . & lhorizontal_constraint_narrow != . ,  ///
	ivars(lhorizontal_constraint_narrow lfree_fair_elections) cmdopts(r) gphdif difvals(.00066 .46464) ///
	ytitle(Pr(y)) ///
	gphopts(yline(0)) xinc(200)
*graph export "../../Apps/ShareLaTex/V-dem civil peace/Figures/vert_hor_int_sep.pdf", replace
graph export "/Users/hannefjelde/Dropbox/Collaborations/V-DemCivilPeace/Figures/vert_hor_int_sep.pdf", replace

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
****
* DV: coup - 46 and onwards (controlling for armed conflict) - what other control variables should we include? Military spending? 
****
* Model 1 - vertical constraints
logit coup_incl  lfree_fair_elections  decay_coupincl incidence_flag lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
outreg using tabcoup.doc, se var starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors clustered on country.) replace

*Model 2 - horizontal constraints 
logit coup_incl  lhorizontal_constraint_narrow decay_coupincl incidence_flag lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
outreg using tabcoup.doc, se var starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors clustered on country.) merge

*Model 3 - liberal protecting minority rights
logit coup_incl  l.minority_rep_narrow decay_coupincl incidence_flag lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
outreg using tabcoup.doc, se var starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors clustered on country.) merge

*Model 4: horse-race
logit coup_incl lfree_fair_elections lhorizontal_constraint_narrow l.minority_rep_narrow decay_coupincl lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
outreg using tabcoup.doc, se var starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors clustered on country.) merge


/* HF: do we want to construct an elite conflict variable - with coups + non-ethnic government conflicts for example*/ 
 
 
**** 
* DV: ethnic conflict (here we should perhaps do multinomial logit, but not sure what to do with DV with onsets during ongoing
****
* Model 1 - vertical constraints
logit onset_do_eth_flag  lfree_fair_elections decay_eth l.incidence_noneth_flag  lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
outreg using tabethnic.doc, se var starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors clustered on country.) replace

logit onset_do_noneth_flag  lfree_fair_elections decay_noeth l.incidence_eth_flag  lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
outreg using tabethnic.doc, se var starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors clustered on country.) merge

*Model 2 - horizontal constraints 
logit onset_do_eth_flag  lhorizontal_constraint_narrow decay_eth l.incidence_noneth_flag  lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
outreg using tabethnic.doc, se var starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors clustered on country.) merge

logit onset_do_noneth_flag  lhorizontal_constraint_narrow decay_noeth l.incidence_eth_flag  lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
outreg using tabethnic.doc, se var starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors clustered on country.) merge

*Model 3 - liberal protecting minority rights
logit onset_do_eth_flag  l.minority_rep_narrow decay_eth l.incidence_noneth_flag  lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
outreg using tabethnic.doc, se var starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors clustered on country.) merge

logit onset_do_noneth_flag  l.minority_rep_narrow decay_noeth l.incidence_eth_flag  lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
outreg using tabethnic.doc, se var starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors clustered on country.) merge

*Model 4: horse-race
logit onset_do_eth_flag lfree_fair_elections lhorizontal_constraint_narrow  l.minority_rep_narrow decay_eth l.incidence_noneth_flag lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
logit onset_do_noneth_flag lfree_fair_elections lhorizontal_constraint_narrow  l.minority_rep_narrow decay_noeth l.incidence_eth_flag lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)

***

* Model 1 - vertical constraints
mlogit ml_onset lfree_fair_elections decay_ml_onset  lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
outreg using tabethnic_2.doc, se var starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors clustered on country.) replace
 
*Model 2 - horizontal constraints 
mlogit ml_onset lhorizontal_constraint_narrow decay_ml_onset  lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
outreg using tabethnic_2.doc, se var starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors clustered on country.) merge
 
*Model 3 - liberal protecting minority rights
mlogit ml_onset  l.minority_rep_narrow  decay_ml_onset lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
outreg using tabethnic_2.doc, se var starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors clustered on country.) merge
 
*Model 4: horse-race
mlogit ml_onset lfree_fair_elections lhorizontal_constraint_narrow  l.minority_rep_narrow  decay_ml_onset  lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
 outreg using tabethnic_2.doc, se var starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors clustered on country.) merge

 
 
**** 
* DV: territorial versus government
****
* Model 1 - vertical constraints
logit onset_do_terr_flag  lfree_fair_elections decay_eth l.incidence_noneth_flag  lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
outreg using tabterr.doc, se var starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors clustered on country.) replace

logit onset_do_gov_flag  lfree_fair_elections decay_noeth l.incidence_eth_flag  lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
outreg using tabterr.doc, se var starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors clustered on country.) merge

*Model 2 - horizontal constraints 
logit onset_do_terr_flag  lhorizontal_constraint_narrow decay_eth l.incidence_noneth_flag  lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
outreg using tabterr.doc, se var starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors clustered on country.) merge

logit onset_do_gov_flag  lhorizontal_constraint_narrow decay_noeth l.incidence_eth_flag  lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
outreg using tabterr.doc, se var starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors clustered on country.) merge

*Model 3 - liberal protecting minority rights
logit onset_do_terr_flag  l.minority_rep_narrow decay_eth l.incidence_noneth_flag  lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
outreg using tabterr.doc, se var starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors clustered on country.) merge

logit onset_do_gov_flag  l.minority_rep_narrow decay_noeth l.incidence_eth_flag  lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
outreg using tabterr.doc, se var starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors clustered on country.) merge

*Model 4: horse-race
logit onset_do_terr_flag lfree_fair_elections lhorizontal_constraint_narrow  l.minority_rep_narrow decay_eth l.incidence_noneth_flag lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
logit onset_do_gov_flag lfree_fair_elections lhorizontal_constraint_narrow  l.minority_rep_narrow decay_noeth l.incidence_eth_flag lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)



* In multinomial logit 
* Model 1 - vertical constraints
mlogit ml_onset_inc  lfree_fair_elections decay_ml_onset_inc  lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
 
*Model 2 - horizontal constraints 
mlogit ml_onset_inc  lhorizontal_constraint_narrow decay_ml_onset_inc  lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
  
*Model 3 - liberal protecting minority rights
mlogit ml_onset_inc  l.minority_rep_narrow  decay_ml_onset_inc lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
 
*Model 4: horse-race
mlogit ml_onset_inc lfree_fair_elections lhorizontal_constraint_narrow  l.minority_rep_narrow  decay_ml_onset_inc  lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
  
 
 
***************************************************************  
* Models with interaction terms
***************************************************************

gen vert_hor_int = free_fair_elections*horizontal_constraint_narrow 
gen min_hor_int = minority_rep_narrow*horizontal_constraint_narrow 
 
* Vertical and horizontal
logit onset_do_flag l.vert_hor_int lfree_fair_elections lhorizontal_constraint_narrow  decay lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
outreg using tab2_int.doc, se var starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors clustered on country.) replace

logit onset_do_flag l.vert_hor_int lfree_fair_elections lhorizontal_constraint_narrow l.minority_rep_narrow decay lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)

* Minority protection and horizontal
logit onset_do_flag l.min_hor_int  lhorizontal_constraint_narrow l.minority_rep_narrow decay lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)
 
logit onset_do_flag l.min_hor_int lhorizontal_constraint_narrow l.minority_rep_narrow free_fair_elections decay lnpop200 lnGDPPerCapita200 if year <= 2016 & year>=1945 & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep_narrow != ., cl(gwno)


by gwno: generate lhorizontal_constraint_narrow=l.horizontal_constraint_narrow
by gwno: generate lminority_rep_narrow=l.minority_rep_narrow
by gwno: generate lfree_fair_elections=lfree_fair_elections
 

 /* Electoral rights - horizontal constraints interaction */

set more off
intgph logit onset_do_flag decay lnpop200 lnGDPPerCapita200 lminority_rep_narrow cowyear ///
	if year <= 2016 & lfree_fair_elections != . & lhorizontal_constraint_narrow!= . & lminority_rep_narrow !=. , ///
	ivars( horizontal_constraint_narrow free_fair_elections) cmdopts(r) difvals(0 .36) ///
	ytitle(Electoral rights, change in predicted probability) ///
	gphopts(yline(0)) xinc(200)
*graph export "../../Apps/ShareLaTex/V-dem civil peace/Figures/vert_hor_int_dif.pdf", replace
graph export "/Users/hannefjelde/Dropbox/Collaborations/V-DemCivilPeace/Figures/vert_hor_int_dif.pdf", replace

intgph logit onset_do_flag decay lnpop200 lnGDPPerCapita200 lminority_rep_narrow cowyear ///
	if year <= 2016 & lfree_fair_elections != . & lhorizontal_constraint_narrow != . & lminority_rep_narrow != . ,  ///
	ivars( lhorizontal_constraint_narrow lfree_fair_elections) cmdopts(r) gphdif difvals(0 .36) ///
	ytitle(Electoral rights, predicted probability)  ///
	gphopts(yline(0)) xinc(200)
*graph export "../../Apps/ShareLaTex/V-dem civil peace/Figures/vert_hor_int_sep.pdf", replace
graph export "/Users/hannefjelde/Dropbox/Collaborations/V-DemCivilPeace/Figures/vert_hor_int_sep.pdf", replace


/* Minority representation - horizontal constraints interaction */

intgph logit onset_do_flag decay lnpop200 lnGDPPerCapita200 lfree_fair_elections cowyear   ///
	if year <= 2016 & lfree_fair_elections != . & lhorizontal_constraint_narrow != . & lminority_rep_narrow != .,  ///
	ivars(lhorizontal_constraint minority_rep) cmdopts(r) difvals(-.14 1.14) ///
	ytitle(Minority representation, change in predicted probability)  ///
	gphopts(yline(0)) xinc(200)
*graph export "../../Apps/ShareLaTex/V-dem civil peace/Figures/minor_hor_int_dif.pdf", replace
graph export "/Users/hannefjelde/Dropbox/Collaborations/V-DemCivilPeace/Figures/minor_hor_int_dif.pdf", replace


intgph logit onset_do_flag decay lnpop200 lnGDPPerCapita200 lfree_fair_elections cowyear   ///
	if year <= 2016 & lfree_fair_elections != . & lhorizontal_constraint_narrow != . & lminority_rep_narrow != .,  ///
	ivars( lhorizontal_constraint_narrow lminority_rep_narrow) cmdopts(r) gphdif difvals(-.14 1.14) ///
	ytitle(Minority representation, predicted probability)  ///
	gphopts(yline(0)) xinc(200)
*graph export "../../Apps/ShareLaTex/V-dem civil peace/Figures/minor_hor_int_sep.pdf", replace
graph export "/Users/hannefjelde/Dropbox/Collaborations/V-DemCivilPeace/Figures/minor_hor_int_sep.pdf", replace

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
************************************************************************************
************************************************************************************

/* Logit and felogit models */

*foreach model in logit fe re {
set more off
foreach model in logit fe re logit46 {
	if "`model'" == "logit" {
		local ptag = "cl(gwno)"
		local strmod = "logit"
		local fy = 1900
	}
	/*if "`model'" == "fe" {
		local ptag = "fe i(gwno)"
		local strmod = "xtlogit"
		local fy = 1900
	}
	if "`model'" == "re" {
		local ptag = "re i(gwno)"
		local strmod = "xtlogit"
		local fy = 1900 
	}*/
	if "`model'" == "logit46" {
		local ptag = "cl(gwno)"
		local strmod = "logit"
		local fy = 1946
	}
	display "Structural model: `strmod', panel tag: `ptag'"


	
/* Hypothesis 1-3, Our own cleaned-up indices */	
eststo clear
	/* H1:  Law and property rights */


eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200  horizontal_constraint_narrow cowyear ///
	if year <= 2016 & year >= `fy' & free_fair_elections != . & horizontal_constraint_narrow != . & minority_rep != ., `ptag'

eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200 free_fair_elections_narrow cowyear ///
	if year <= 2016 & year >= `fy' & free_fair_elections_narrow != . & horizontal_constraint != . & minority_rep != ., `ptag'

eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200 minority_rep_narrow cowyear ///
	if year <= 2016 & year >= `fy' & free_fair_elections != . & horizontal_constraint != . & minority_rep != ., `ptag'
			
			
			}
/* All together */	
	
	
*eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200 FHKN_RoL free_fair_elections  horizontal_constraint minority_rep cowyear   ///
*	if year <= 2016 & year >= `fy' & free_fair_elections != . & horizontal_constraint != . & minority_rep != ., `ptag'
	
	
eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200 free_fair_elections  horizontal_constraint minority_rep cowyear   ///
	if year <= 2016 & year >= `fy' & free_fair_elections != . & horizontal_constraint != . & minority_rep != ., `ptag'

esttab using "../../Apps/ShareLaTex/V-dem civil peace/Results/`model'_Models_Table1.tex", replace ///
	se nogaps stats(aic ll N)  ///
	coeflabels(decay_2 "Time since conflict" lnpop200 "ln population" lnGDPPerCapita200 "ln GDP per cap" free_fair_elections "Electoral rights" ///
	horizontal_constraint "Horizontal constraints" minority_rep "Minority representation" v2x_partip "V-Dem participation component" v2x_liberal "V-Dem liberal component" ///
	v2x_polyarchy "V-Dem polyarchy" v2x_frassoc_thick "V-Dem freedom of association" v2x_suffr "Suffrage" v2xel_frefair "Free and fair elections" ///
	v2x_accex "V-Dem elected exec" v2x_cspart "Civil society participation" v2xlg_legcon "Legislative constraints" v2x_jucon "Judicial constraints" ///
	v2xcl_rol "V-Dem rule of law" SDI "Local elections"  ///
	cowyear "COW conflict/before 1946") ///
	order( horizontal_constraint  free_fair_elections minority_rep  lnpop200 lnGDPPerCapita200 decay_2 cowyear ) ///
	mtitles("H1" "H2" "H3"  "All"  ) nonumbers nodepvars
		
		
/* With the more narrow electoral rights index */	
eststo clear
	/* H1:  Law and property rights */

*eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200 FHKN_RoL cowyear ///
*	if year <= 2016 & year >= `fy' & free_fair_wider != . & horizontal_constraint != . & minority_rep != ., `ptag'

eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200  horizontal_constraint cowyear ///
	if year <= 2016 & year >= `fy' & free_fair_narrow != . & horizontal_constraint != . & minority_rep != ., `ptag'

eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200 free_fair_narrow cowyear ///
	if year <= 2016 & year >= `fy' & free_fair_narrow != . & horizontal_constraint != . & minority_rep != ., `ptag'
*/
eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200 minority_rep cowyear ///
	if year <= 2016 & year >= `fy' & free_fair_narrow != . & horizontal_constraint != . & minority_rep != ., `ptag'
	
	
*eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200 FHKN_RoL free_fair_narrow  horizontal_constraint minority_rep cowyear   ///
*	if year <= 2016 & year >= `fy' & free_fair_wider != . & horizontal_constraint != . & minority_rep != ., `ptag'
	
	
eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200 free_fair_narrow  horizontal_constraint minority_rep cowyear   ///
	if year <= 2016 & year >= `fy' & free_fair_narrow != . & horizontal_constraint != . & minority_rep != ., `ptag'

	esttab using "../../Apps/ShareLaTex/V-dem civil peace/Results/`model'_Models_Table1b.tex", replace ///
	se nogaps stats(aic ll N)  ///
	coeflabels(decay_2 "Time since conflict" lnpop200 "ln population" lnGDPPerCapita200 "ln GDP per cap" free_fair_narrow "Electoral rights, narrow" ///
	horizontal_constraint "Horizontal constraints" minority_rep "Minority representation" v2x_partip "V-Dem participation component" v2x_liberal "V-Dem liberal component" ///
	v2x_polyarchy "V-Dem polyarchy" v2x_frassoc_thick "V-Dem freedom of association" v2x_suffr "Suffrage" v2xel_frefair "Free and fair elections" ///
	v2x_accex "V-Dem elected exec" v2x_cspart "Civil society participation" v2xlg_legcon "Legislative constraints" v2x_jucon "Judicial constraints" ///
	v2xcl_rol "V-Dem rule of law" SDI "Local elections" FHKN_RoL "Economic rights" ///
	cowyear "COW conflict/before 1946") ///
	order(horizontal_constraint  free_fair_narrow minority_rep  lnpop200 lnGDPPerCapita200 decay_2 cowyear ) ///
	mtitles("H1" "H2" "H3" "All") nonumbers nodepvars
				
				
/* Hypothesis 1-3, adding square terms*/	
eststo clear
capture drop ffe_sq
capture drop hc_sq
capture drop mr_sq
gen ffe_sq = free_fair_elections^2
gen hc_sq = horizontal_constraint^2
gen mr_sq = minority_rep^2

eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200  horizontal_constraint hc_sq cowyear ///
	if year <= 2016 & year >= `fy' & free_fair_elections != . & horizontal_constraint != . & minority_rep != ., `ptag'

eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200 free_fair_elections ffe_sq cowyear ///
	if year <= 2016 & year >= `fy' & free_fair_elections != . & horizontal_constraint != . & minority_rep != ., `ptag'

eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200 minority_rep mr_sq cowyear ///
	if year <= 2016 & year >= `fy' & free_fair_elections != . & horizontal_constraint != . & minority_rep != ., `ptag'
			
esttab using "../../Apps/ShareLaTex/V-dem civil peace/Results/`model'_Models_squares.tex", replace ///
	se nogaps stats(aic ll N)  ///
	coeflabels(decay_2 "Time since conflict" lnpop200 "ln population" lnGDPPerCapita200 "ln GDP per cap" free_fair_elections "Electoral rights" ///
	horizontal_constraint "Horizontal constraints" minority_rep "Minority representation"  ///
	ffe_sq "Electoral rights squared" ///
	hc_sq "Horizontal constraints squared" mir_sq "Minority representation squared"  ///
	v2xcl_rol "V-Dem rule of law" SDI "Local elections"  ///
	cowyear "COW conflict/before 1946") ///
	order( horizontal_constraint hc_sq  free_fair_elections fee_sq minority_rep mr_sq lnpop200 lnGDPPerCapita200 decay_2 cowyear ) ///
	mtitles("H1" "H2" "H3"  "All"  ) nonumbers nodepvars 
		
			
		
		
eststo clear		
		
/* Interaction models */
capture drop vert_hor_int
capture drop rol_hor_int
capture drop minor_hor_int
capture drop vert_horsq_int 
capture drop vertsq_hor_int 
capture drop vertsq_horsq_int 
capture drop min_horsq_int 
capture drop minsq_hor_int 
capture drop minsq_horsq_int 

gen vert_hor_int = free_fair_elections*horizontal_constraint 
gen rol_hor_int = FHKN_RoL*horizontal_constraint 
gen minor_hor_int = minority_rep*horizontal_constraint 
*gen vert_part_int = free_fair_elections*v2x_cspart
gen vert_horsq_int = free_fair_elections*hc_sq
gen vertsq_hor_int = ffe_sq*horizontal_constraint 
gen vertsq_horsq_int = ffe_sq*hc_sq
gen min_horsq_int = minority_rep*hc_sq
gen minsq_hor_int = mr_sq*horizontal_constraint 
gen minsq_horsq_int = mr_sq*hc_sq

*eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200 FHKN_RoL free_fair_elections horizontal_constraint minority_rep rol_hor_int  cowyear  ///
*	if year <= 2016 & year >= `fy' & free_fair_elections != . & horizontal_constraint != . & minority_rep != . & x_polity !=., `ptag'
	
eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200  free_fair_elections horizontal_constraint minority_rep vert_hor_int  cowyear  ///
	if year <= 2016 & year >= `fy' & free_fair_elections != . & horizontal_constraint != . & minority_rep != . , `ptag'
	
eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200 free_fair_elections horizontal_constraint minority_rep minor_hor_int  cowyear  ///
	if year <= 2016 & year >= `fy' & free_fair_elections != . & horizontal_constraint != . & minority_rep != ., `ptag'
	
/* Categorical models, vertical vs horizontal dimensions */	
capture drop categorical
gen categorical = 0 if free_fair_elections != . & horizontal_constraint != .
replace categorical = 0 if free_fair_elections <= 0.25 & horizontal_constraint <= 0.3
replace categorical = 1 if (free_fair_elections > 0.25 & horizontal_constraint <= 0.3) | (free_fair_elections <= 0.25 & horizontal_constraint > 0.3)
replace categorical = 2 if free_fair_elections > 0.25 & horizontal_constraint > 0.3

eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200 i.categorical minority_rep  cowyear  ///
	if year <= 2016 & year >= `fy' & free_fair_elections != . & horizontal_constraint != . & minority_rep != ., `ptag'



matrix coef =e(b)	

esttab using "../../Apps/ShareLaTex/V-dem civil peace/Results/`model'_Models_Table2.tex", replace ///
	se nogaps stats(aic ll N)  ///
	coeflabels(decay_2 "Time since conflict" lnpop200 "ln population" lnGDPPerCapita200 "ln GDP per cap" free_fair_elections "Electoral rights" ///
	horizontal_constraint "Horizontal constraints" minority_rep "Minority representation" v2x_partip "V-Dem participation component" v2x_liberal "V-Dem liberal component" ///
	v2x_polyarchy "V-Dem polyarchy" v2x_frassoc_thick "V-Dem freedom of association" v2x_suffr "Suffrage" v2xel_frefair "Free and fair elections" ///
	v2x_accex "V-Dem elected exec" v2x_cspart "Civil society participation" v2xlg_legcon "Legislative constraints" v2x_jucon "Judicial constraints" ///
	v2xcl_rol "V-Dem rule of law" civlib "Other civil liberties" SDI "Local elections" FHKN_RoL "Economic rights" vert_hor_int "Vertical*horizontal" ///
	rol_hor_int "Economic rights * horizontal"	minor_hor_int "Minority repr. * horizontal"  cowyear "COW conflict/before 1946" ///
	x_polity "Vreeland's x-polity" x_polity_sq "x-polity squared" ///
	hc_sq "Horizontal constraints squared" mir_sq "Minority representation squared"  ///
	v2xcl_rol "V-Dem rule of law" SDI "Local elections")  ///
	order( horizontal_constraint free_fair_elections minority_rep vert_hor_int minor_hor_int lnpop200 lnGDPPerCapita200  decay_2 cowyear ) ///
	mtitles("H4" "H5" "Cat" "H4sq" "H4sq" "H5sq" "H5sq") nonumbers nodepvars	
	
/* Adding square terms for an appendix version of this table */
	
eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200  free_fair_elections horizontal_constraint minority_rep ///
	vert_hor_int vertsq_hor_int vert_horsq_int vertsq_horsq_int hc_sq ffe_sq mr_sq cowyear  ///
	if year <= 2016 & year >= `fy' & free_fair_elections != . & horizontal_constraint != . & minority_rep != . , `ptag'

eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200  free_fair_elections horizontal_constraint minority_rep ///
	vert_hor_int hc_sq ffe_sq mr_sq cowyear  ///
	if year <= 2016 & year >= `fy' & free_fair_elections != . & horizontal_constraint != . & minority_rep != . , `ptag'
	

eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200 free_fair_elections horizontal_constraint ///
	minority_rep minor_hor_int hc_sq minsq_hor_int min_horsq_int minsq_horsq_int ffe_sq mr_sq cowyear  ///
	if year <= 2016 & year >= `fy' & free_fair_elections != . & horizontal_constraint != . & minority_rep != ., `ptag'

eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200 free_fair_elections horizontal_constraint ///
	minority_rep minor_hor_int hc_sq ffe_sq mr_sq cowyear  ///
	if year <= 2016 & year >= `fy' & free_fair_elections != . & horizontal_constraint != . & minority_rep != ., `ptag'

*eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200 x_polity x_polity_sq   cowyear  ///
*	if year <= 2016 & year >= `fy' & free_fair_elections != . & horizontal_constraint != . & minority_rep != . & x_polity !=., `ptag'

*eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200 x_polity x_polity_sq FHKN_RoL free_fair_elections horizontal_constraint minority_rep rol_hor_int  cowyear  ///
*	if year <= 2016 & year >= `fy' & free_fair_elections != . & horizontal_constraint != . & minority_rep != . & x_polity !=., `ptag'
	
*eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200 x_polity x_polity_sq FHKN_RoL free_fair_elections horizontal_constraint minority_rep vert_hor_int  cowyear  ///
*	if year <= 2016 & year >= `fy' & free_fair_elections != . & horizontal_constraint != . & minority_rep != . & x_polity !=., `ptag'

*eststo: xi: `strmod' c_onset decay_2 lnpop200 lnGDPPerCapita200 x_polity x_polity_sq FHKN_RoL free_fair_elections horizontal_constraint minority_rep minor_hor_int  cowyear  ///
*	if year <= 2016 & year >= `fy' & free_fair_elections != . & horizontal_constraint != . & minority_rep != . & x_polity !=., `ptag'

}	
	

matrix coef =e(b)	

esttab using "../../Apps/ShareLaTex/V-dem civil peace/Results/`model'_Models_Table2_app.tex", replace ///
	se nogaps stats(aic ll N)  ///
	coeflabels(decay_2 "Time since conflict" lnpop200 "ln population" lnGDPPerCapita200 "ln GDP per cap" free_fair_elections "Electoral rights" ///
	horizontal_constraint "Horizontal constraints" minority_rep "Minority representation" v2x_partip "V-Dem participation component" v2x_liberal "V-Dem liberal component" ///
	v2x_polyarchy "V-Dem polyarchy" v2x_frassoc_thick "V-Dem freedom of association" v2x_suffr "Suffrage" v2xel_frefair "Free and fair elections" ///
	v2x_accex "V-Dem elected exec" v2x_cspart "Civil society participation" v2xlg_legcon "Legislative constraints" v2x_jucon "Judicial constraints" ///
	v2xcl_rol "V-Dem rule of law" civlib "Other civil liberties" SDI "Local elections" FHKN_RoL "Economic rights" vert_hor_int "Vertical*horizontal" ///
	rol_hor_int "Economic rights * horizontal"	minor_hor_int "Minority repr. * horizontal"  cowyear "COW conflict/before 1946" ///
	x_polity "Vreeland's x-polity" x_polity_sq "x-polity squared" ///
	hc_sq "Horizontal constraints squared" mir_sq "Minority representation squared"  ///
	v2xcl_rol "V-Dem rule of law" SDI "Local elections")  ///
	order( horizontal_constraint hc_sq free_fair_elections fee_sq minority_rep mr_sq vert_hor_int minor_hor_int  lnpop200 lnGDPPerCapita200  decay_2 cowyear ) ///
	mtitles("H4" "H5" "Cat" "H4sq" "H4sq" "H5sq" "H5sq") nonumbers nodepvars



*
/* x-polity graph */
twoway function y= -0.0566*x - 0.000885*x*x, range(-6 7)
/* horcon i-U graph */
twoway function y=5.427*x-6.911*x*x

/* Redo the interactive models to obtain predictions and plot against the V-Dem liberal index */
/*
logit c_onset decay_2 lnpop200 lnGDPPerCapita200 FHKN_RoL  free_fair_elections  horizontal_constraint v2x_cspart vert_hor_int vert_part_int minority_rep cowyear   ///
	if year <= 2016 & free_fair_elections != . & horizontal_constraint != . & minority_rep != ., cl(gwno)

matrix coef =e(b)	
	
capture drop intmodelpred
predict intmodelpred, pr

summarize lnGDPPerCapita200

twoway (scatter intmodelpred polity2 if lnGDPPerCapita200 < 12, mcolor(orange) ) ///
	(lowess intmodelpred polity2 if lnGDPPerCapita200 < 12, bwidth(.4) lcolor(black))
	*/
/* Some pure V-Dem models */ 

	
eststo clear
eststo: logit c_onset decay_2 lnpop200 lnGDPPerCapita200 v2x_polyarchy cowyear ///
	if year <= 2016 & free_fair_elections != . & horizontal_constraint != . & minority_rep != . , cl(gwno)
eststo: logit c_onset decay_2 lnpop200 lnGDPPerCapita200 v2x_liberal cowyear ///
	if year <= 2016 & free_fair_elections != . & horizontal_constraint != . & minority_rep != ., cl(gwno)
eststo: logit c_onset decay_2 lnpop200 lnGDPPerCapita200 v2x_partip cowyear ///
	if year <= 2016 & free_fair_elections != . & horizontal_constraint != . & minority_rep != . , cl(gwno)
eststo: logit c_onset decay_2 lnpop200 lnGDPPerCapita200 v2x_polyarchy v2x_liberal v2x_partip cowyear ///
	if year <= 2016 & free_fair_elections != . & horizontal_constraint != . & minority_rep != . , cl(gwno)

		
	
esttab using "../../Apps/ShareLaTex/V-dem civil peace/Results/VDem_models.tex", replace ///
	se nogaps stats(aic ll N)  ///
		order( v2x_polyarchy v2x_liberal v2x_partip lnpop200 lnGDPPerCapita200  decay_2 cowyear ) ///


summarize free_fair_elections horizontal_constraint minority_rep v2x_cspart FHKN_RoL v2x_polyarchy v2x_partip v2x_liberal, detail
	
	
/* Simple plot of effects */

matrix list coef
summ v2x_cspart, detail
local partval1 = r(p10)
local partval2 = r(p90)
summ horizontal_constraint, detail
local horval1 = r(p10)
local horval2 = r(p90)
twoway (function y= coef[1,4]*x + `partval1'*(coef[1,6] + coef[1,8]*x) + `horval1'*(coef[1,5] + coef[1,7]*x), color(black) lwidth(medthick) lpattern(shortdash_dot) ) ///
		(function y= coef[1,4]*x + `partval2'*(coef[1,6] + coef[1,8]*x) + `horval1'*(coef[1,5] + coef[1,7]*x), color(green) lwidth(medthick) lpattern(dash_dot) ) ///
		(function y= coef[1,4]*x + `partval1'*(coef[1,6] + coef[1,8]*x) + `horval2'*(coef[1,5] + coef[1,7]*x), color(blue) lwidth(medthick) lpattern(longdash) ) ///
		(function y= coef[1,4]*x + `partval2'*(coef[1,6] + coef[1,8]*x) + `horval2'*(coef[1,5] + coef[1,7]*x), color(red) lwidth(medthick) lpattern(solid) ///
		legend(lab(1 "Low participation, low horizontal accountability") lab(2 "High participation, low horizontal accountability") ///
		lab(3 "Low participation, high horizontal accountability") lab(4 "High participation, high horizontal accountability")) ///
		xtitle("Vertical accountability") ytitle("Log odds of conflict") legend(col(1)) )
graph export "Figures/Our_Interaction.pdf", replace
	
/* intgph versions, one interaction at the time */	
/* difvals are 25 and 75 percentiles */

label variable  free_fair_elections "Free and fair elections"


/* Economic rights - horizontal constraints interaction */
set more off
/*
intgph logit c_onset decay_2 lnpop200 lnGDPPerCapita200 free_fair_elections minority_rep cowyear   ///
	if year <= 2016 & free_fair_elections != . & horizontal_constraint != . & minority_rep != . & x_polity !=., ///
	ivars( horizontal_constraint FHKN_RoL) cmdopts(r) difvals(.254 .795) ///
	ytitle(Economic rights, change in predicted probability) 
graph export "../../Apps/ShareLaTex/V-dem civil peace/Figures/ec_hor_int_dif.pdf", replace

intgph logit c_onset decay_2 lnpop200 lnGDPPerCapita200 free_fair_elections minority_rep cowyear   ///
	if year <= 2016 & free_fair_elections != . & horizontal_constraint != . & minority_rep != . & x_polity !=., ///
	ivars( horizontal_constraint FHKN_RoL) cmdopts(r) gphdif difvals(.254 .795) ///
	ytitle(Economic rights, predicted probability) 
graph export "../../Apps/ShareLaTex/V-dem civil peace/Figures/ec_hor_int_sep.pdf", replace
*/
/* Electoral rights - horizontal constraints interaction */
set more off

intgph logit c_onset decay_2 lnpop200 lnGDPPerCapita200 minority_rep cowyear ///
	if year <= 2016 & free_fair_elections != . & horizontal_constraint != . & minority_rep != . & x_polity !=., ///
	ivars( horizontal_constraint free_fair_elections) cmdopts(r) difvals(0 .36) ///
	ytitle(Electoral rights, change in predicted probability) ///
	gphopts(yline(0)) xinc(200)
graph export "../../Apps/ShareLaTex/V-dem civil peace/Figures/vert_hor_int_dif.pdf", replace


intgph logit c_onset decay_2 lnpop200 lnGDPPerCapita200 minority_rep cowyear ///
	if year <= 2016 & free_fair_elections != . & horizontal_constraint != . & minority_rep != . & x_polity !=., ///
	ivars( horizontal_constraint free_fair_elections) cmdopts(r) gphdif difvals(0 .36) ///
	ytitle(Electoral rights, predicted probability)  ///
	gphopts(yline(0)) xinc(200)
graph export "../../Apps/ShareLaTex/V-dem civil peace/Figures/vert_hor_int_sep.pdf", replace


/* Minority representation - horizontal constraints interaction */

intgph logit c_onset decay_2 lnpop200 lnGDPPerCapita200 free_fair_elections cowyear   ///
	if year <= 2016 & free_fair_elections != . & horizontal_constraint != . & minority_rep != . & x_polity !=., ///
	ivars( horizontal_constraint minority_rep) cmdopts(r) difvals(-.14 1.14) ///
	ytitle(Minority representation, change in predicted probability)  ///
	gphopts(yline(0)) xinc(200)
graph export "../../Apps/ShareLaTex/V-dem civil peace/Figures/minor_hor_int_dif.pdf", replace


intgph logit c_onset decay_2 lnpop200 lnGDPPerCapita200 free_fair_elections cowyear   ///
	if year <= 2016 & free_fair_elections != . & horizontal_constraint != . & minority_rep != . & x_polity !=., ///
	ivars( horizontal_constraint minority_rep) cmdopts(r) gphdif difvals(-.14 1.14) ///
	ytitle(Minority representation, predicted probability)  ///
	gphopts(yline(0)) xinc(200)
graph export "../../Apps/ShareLaTex/V-dem civil peace/Figures/minor_hor_int_sep.pdf", replace


save "Tee/whole.dta", replace

outsheet using "InputData/FVP_V-Demcivpeace.csv", comma replace

