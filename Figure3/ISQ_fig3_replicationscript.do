clear
insheet using "/Users/hafje172/Dropbox/forskning/WhichInstitutions/interaction/prepped_data.csv"


destring llnpop200, force replace
destring llngdppercapita200, force replace
destring timesince, force replace
destring timesince_sq, force replace
destring timesince_cb, force replace
destring ethfrac, force replace
destring lmtnest, force replace
destring nbconflict, force replace
destring lfree_fair_elections, force replace
destring lhorizontal_constraint_narrow, force replace
destring c2_onset, force replace


eststo: xi: logit c2_onset c.lfree_fair_elections##c.lhorizontal_constraint_narrow llnpop200 llngdppercapita200 timesince timesince_sq timesince_cb ethfrac lmtnest nbconflict if year>1945 & lfree_fair_elections != . & lhorizontal_constraint_narrow != . , cl(gwno)


 /* Graphs interaction - twoway */
eststo: xi: logit c2_onset c.lfree_fair_elections##c.lhorizontal_constraint_narrow llnpop200 llngdppercapita200 timesince timesince_sq timesince_cb ethfrac lmtnest nbconflict if year>1945 & lfree_fair_elections != . & lhorizontal_constraint_narrow != . , cl(gwno)

su lfree_fair_elections lhorizontal_constraint_narrow if e(sample), detail

set more off
intgph logit c2_onset llnpop200 llngdppercapita200 timesince timesince_sq timesince_cb ethfrac lmtnest nbconflict if year>1945 & lfree_fair_elections != . & lhorizontal_constraint_narrow != . , ///
	ivars(lhorizontal_constraint_narrow lfree_fair_elections) cmdopts(r) difvals(.0006 .4695) ///
	ytitle(Change in pr(y)) ///
	gphopts(yline(0)) xinc(200)

	graph export "vert_hor_int_dif.pdf", replace


intgph logit c2_onset llnpop200 llngdppercapita200 timesince timesince_sq timesince_cb ethfrac lmtnest nbconflict if year>1945 & lfree_fair_elections != . & lhorizontal_constraint_narrow != . , ///
	ivars(lhorizontal_constraint_narrow lfree_fair_elections) cmdopts(r) gphdif difvals(.0006 .4695) ///
	ytitle(Pr(y)) ///
	gphopts(yline(0)) xinc(200)
graph export "vert_hor_int_sep.pdf", replace


******

set more off
intgph logit c2_onset llnpop200 llngdppercapita200 timesince timesince_sq timesince_cb ethfrac lmtnest nbconflict if year>1945 & lfree_fair_elections != . & lhorizontal_constraint_narrow != . , ///
	ivars(lfree_fair_elections lhorizontal_constraint_narrow) cmdopts(r) difvals(.2726  .8301) ///
	ytitle(Change in pr(y)) ///
	gphopts(yline(0)) xinc(200)
graph export "vert_hor_int_dif.pdf", replace



intgph logit c2_onset llnpop200 llngdppercapita200 timesince timesince_sq timesince_cb ethfrac lmtnest nbconflict if year>1945 & lfree_fair_elections != . & lhorizontal_constraint_narrow != . , ///
	ivars(lfree_fair_elections lhorizontal_constraint_narrow) cmdopts(r) gphdif difvals(.2726 .8301) ///
	ytitle(Pr(y)) ///
	gphopts(yline(0)) xinc(200)
graph export "vert_hor_int_sep.pdf", replace
