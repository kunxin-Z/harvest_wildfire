********************************************************************************
*******Clean the csv dataset constructed by python to a stata dta file**********
********************************************************************************
capture log close
********************************************************************************
******************Set up********************************************************
********************************************************************************
clear
clear all


import delimited "C:\Users\zhuku\OneDrive - Auburn University\python_project\wildfire_milling\data cleaning\merge_county\panel_county_milling_fire.csv"


log using "disturbance_wildfire_county.smcl", replace

duplicates report fips year




drop state
gen state = floor(fips/1000)
gen bio_grow_ac_num = real(bio_grow_ac)

* generate states with information
gen south_state=0
replace south_state=1 if inlist(state, 1, 5, 37, 40, 12, 13, 45, 47, 48, 51, 21, 22, 28)
* only include southern statates
drop if south_state==0


* data cleaning
* limited counties have milling facilities
sum 

* generate establishment
gen stdorgcd=stdorgcd_x

* backfill the FIA data and trade based IV
foreach v in tpa baa bio_acre area_total_total area_total_private saw_cf_acre bio_grow_ac_num reservcd stdage siteclcd stdszcd slope sicond rddistcd elev age30 stdorgcd estabs {
    gsort fips -year
    by fips: replace `v' = `v'[_n-1] if missing(`v')
}


xtset fips year



* fill na
foreach var in mtbs_acre nifc_acre mtbs_count nifc_count capacity capacity_mill open_c close_c open_o close_o milling_count open  forest_hansen  cc_private cc_total major_cc_private major_cc_total th_private th_total major_th_private major_th_total pt_private pt_total major_pt_private major_pt_total n_cc_private n_cc_total major_n_cc_private major_n_cc_total n_th_private n_th_total major_n_th_private major_n_th_total major_cc_area major_th_area major_pt_area major_n_cc_area major_n_th_area estabs fema_count estabs{
	replace `var'=0 if missing(`var')
}

* fillna for iv
local covars private_arti_s private_natural_s c_a_s t_a_s c_n_s t_n_s private_arti_se private_natural_se c_a_se t_a_se c_n_se t_n_se private_arti_c private_natural_c c_a_c t_a_c c_n_c t_n_c

* Forward fill
gsort fips year
foreach v of local covars {
    by fips (year): replace `v' = `v'[_n-1] if missing(`v')
}

* Backfill
gsort fips -year
foreach v of local covars {
    by fips: replace `v' = `v'[_n-1] if missing(`v')
}
xtset fips year

foreach x in private_arti private_natural c_a t_a c_n t_n {
    replace `x'_se = `x'_s if missing(`x'_se) & !missing(`x'_s)
}

foreach col in private_arti_c private_natural_c c_a_c t_a_c c_n_c t_n_c{
	replace `col'=0 if missing(`col')
}


* mtbs data change
replace mtbs_acre=mtbs_acre/4046.856
replace nifc_acre=nifc_acre/4046.856

* drop nifc>2018
replace nifc_acre  = . if year>2018
replace nifc_count  = . if year>2018


* fill by group means
foreach var in slope h_a stdorgcd tpa  saw_cf_acre {
    bysort fips : egen length_mean = mean(`var')
	replace `var'=length_mean if missing(`var')
	drop length_mean
	replace `var'=0 if missing(`var')
}

foreach var in house house_firm reservcd corporate_x corporate_arti saw_price pulp_price timber_price saw_price_firm pulp_price_firm timber_price_firm{
	bysort state: egen test = mean(`var')
	replace `var'=test if missing(`var')
	drop test
}

bysort state year: egen vdp_mean = mean(vdp_max)
replace vdp_max=vdp_mean if missing(vdp_max)
drop vdp_mean


gen private_forest_p=area_total_private/area_total_total

foreach var in private_forest_p bio_grow_ac_num siteclcd stdage age30 elev stdszcd sicond{
	bysort state year: egen mid_mean = mean(`var')
	replace `var'=mid_mean if missing(`var')
	drop mid_mean
	
	sum `var'
	replace `var'=r(mean) if missing(`var')
}

* generate event
gen mtbs_d=0
replace mtbs_d=1 if mtbs_acre>0

gen nifc_d=0 if year<=2018
replace nifc_d=1 if nifc_acre>0 &  year<=2018

* fema event
replace fema_count=1 if fema_count>0

* log transformation
foreach var in mtbs_acre nifc_acre capacity vdp_max tpa baa area_total_total area_total_private forest_hansen cc_private cc_total major_cc_private major_cc_total th_private th_total major_th_private major_th_total pt_private pt_total major_pt_private major_pt_total n_cc_private n_cc_total major_n_cc_private major_n_cc_total n_th_private n_th_total major_n_th_private major_n_th_total major_cc_area major_th_area major_pt_area major_n_cc_area major_n_th_area{
    gen log`var'=log(`var'+1)
}


* generate baseline data
foreach var in tpa area_total_total stdorgcd private_forest_p bio_grow_ac_num siteclcd stdage age30 house house_firm{
	bysort fips (year): egen `var'_base = mean(cond(year==2000,  `var', .))
}
foreach var in ves_val_yr ves_wgt_yr all_val_yr ves_val_yr_im ves_wgt_yr_im ves_val_yr_firm ves_wgt_yr_firm all_val_yr_firm ves_val_yr_im_firm ves_wgt_yr_im_firm{
	bysort fips (year): egen `var'_base = mean(cond(year==2000,  `var', .))
}

replace house=house/1000


label variable vdp_max 		   "Maximum VPD"
label variable tpa             "Trees per acre"
label variable area_total_total "Log total forest area"
label variable private_forest_p   "Private forest share"
label variable saw_cf_acre     "Sawtimber volume per acre"
label variable slope           "Average slope"
label variable elev            "Average elevation"
label variable stdage          "Average stand age"
label variable bio_grow_ac          "Bio growth rate"
label variable siteclcd          "Site productivity"
label variable stdszcd          "Tree diameter class"
label variable estabs          "Forestry and logging establishments "

* area_total_total bio_grow_ac_num
gl CONTROL "vdp_max tpa  saw_cf_acre bio_grow_ac_num slope elev stdage siteclcd"
replace private_forest_p=1 if private_forest_p>1

* state-eco shift delocalized
gen se_iv_harvest=(c_a_se-c_a_c)/(private_arti_se-private_arti_c)*L.private_arti_c+(c_n_se-c_n_c)/(private_natural_se-private_natural_c)*L.private_natural_c
gen se_iv_thin=(t_a_se-t_a_c)/(private_arti_se-private_arti_c)*L.private_arti_c+(t_n_se-t_n_c)/(private_natural_se-private_natural_c)*L.private_natural_c

replace se_iv_harvest=(c_n_se-c_n_c)/(private_natural_se-private_natural_c)*L.private_natural_c if missing(se_iv_harvest)
replace se_iv_thin=(t_n_se-t_n_c)/(private_natural_se-private_natural_c)*L.private_natural_c if missing(se_iv_thin)



* state shift
gen s_iv_harvest=(c_a_s-c_a_c)/(private_arti_s-private_arti_c)*L.private_arti_c+(c_n_s-c_n_c)/(private_natural_s-private_natural_c)*L.private_natural_c
gen s_iv_thin=(t_a_s-t_a_c)/(private_arti_s-private_arti_c)*L.private_arti_c+(t_n_s-t_n_c)/(private_natural_s-private_natural_c)*L.private_natural_c


replace s_iv_harvest=f.s_iv_harvest if year==1995

// gen s_iv_harvest_fill = L.s_iv_harvest
// replace s_iv_harvest_fill = s_iv_harvest if missing(s_iv_harvest_fill)
// replace s_iv_harvest=s_iv_harvest_fill


* undelocalized shift share instrument
gen se_iv_harvest_g=(c_a_se)/(private_arti_se)*L.private_arti_c+(c_n_se)/(private_natural_se)*L.private_natural_c
gen se_iv_thin_g=(t_a_se)/(private_arti_se)*L.private_arti_c+(t_n_se)/(private_natural_se)*L.private_natural_c


* drop fips with tpa
drop if inlist(fips, ///
    1057, 12103, 13015, 13157, 13171, 13227, 13237, 13257, 13265, 13307)

drop if inlist(fips, ///
    22023, 22051, 22075, 22113, 28013, 28097, 28117, 28145, 28155)

drop if inlist(fips, ///
    37001, 37003, 37015, 37029, 37033, 37037, 37041, 37053, 37057, 37063)

drop if inlist(fips, ///
    37065, 37069, 37077, 37083, 37101, 37105, 37107, 37109, 37123, 37131)

drop if inlist(fips, ///
    37135, 37139, 37145, 37157, 37163, 37177, 37181, 37185, 37195, 45001)

drop if inlist(fips, ///
    47011, 47127, 48113, 48361, 51001, 51025, 51049, 51053, 51057, 51081)

drop if inlist(fips, ///
    51083, 51089, 51093, 51101, 51111, 51115, 51119, 51127, 51131, 51147)

drop if inlist(fips, ///
    51149, 51159, 51175, 51181, 51183, 51193, 51550)

******************************** SS table **************************************
preserve
foreach var in  area_total_total{
	replace `var' = `var' / 1000
}

foreach var in n_cc_private n_th_private mtbs_acre s_iv_harvest s_iv_thin{
	replace `var' = `var' / 1000 
}

tabstat mtbs_acre mtbs_d n_cc_private n_th_private s_iv_harvest s_iv_thin ///
                    vdp_max tpa area_total_total ///
                    saw_cf_acre bio_grow_ac_num slope elev stdage siteclcd estabs if year >= 2000, ///
    stat(mean sd min max) ///
    col(stat) save

matrix A = r(StatTotal)'
matrix rownames A = mtbs_acre mtbs_d n_cc_private n_th_private s_iv_harvest s_iv_thin ///
                    vdp_max tpa area_total_total ///
                    saw_cf_acre bio_grow_ac_num slope elev stdage siteclcd estabs
									
					
matrix B = A

forvalues i = 1/`=rowsof(A)' {
    forvalues j = 1/`=colsof(A)' {
        matrix B[`i',`j'] = round(A[`i',`j'], .001)
    }
}

esttab matrix(B) using "table_A1.tex", replace ///
    fragment booktabs nomtitles nonumber ///
	label ///
    varlabels( ///
        mtbs_acre  "Wildfire area in MTBS (thousand acre)" ///
        mtbs_d "Wildfire occurrence in MTBS" ///
        n_cc_private "Harvest area (thousand acre)" ///
        n_th_private "Thinning area (thousand acre)" ///
        s_iv_harvest "Harvest shift-share (thousand acre)" ///
        s_iv_thin "Thinning shift-share (thousand acre)" ///
        vdp_max "Maximum VPD (hPa)" ///
        tpa "Trees per acre" ///
        area_total_total "Total forest area (thousand acre)" ///
        saw_cf_acre "Sawtimber volume per acre (ft$^3$/acre)" ///
		bio_grow_ac_num "Bio growth rate" ///
        slope "Slope (\%)" ///
        elev "Elevation (m)" ///
        stdage "Stand age" ///
        bio_grow_ac_num "Bio growth rate" ///
		estabs "Forestry and logging establishments" ///
    ) ///
    collabels("Mean" "Std. Dev." "Min" "Max") ///
    cells("B") ///
    postfoot("\midrule")
restore	
	


xtset fips year

* clean treatment
foreach var in cc_private cc_total major_cc_private major_cc_total th_private th_total major_th_private major_th_total{
    gen `var'_d=(`var'>0)
	gen `var'_perc=`var'/area_total_total
	quietly sum `var'_perc
	replace `var'_perc=r(mean) if missing(`var'_perc)
	replace `var'_perc=`var'_perc/(r(max)+1)
}

gen mtbs_acre_p=mtbs_acre/area

drop if south_state!=1
sum cc_total th_total

sum all_val_yr ves_val_yr ves_wgt_yr all_val_yr_1133 ves_val_yr_1133 ves_wgt_yr_1133 all_val_yr_im ves_val_yr_im ves_wgt_yr_im all_val_yr_1133_im ves_val_yr_1133_im ves_wgt_yr_1133_im house saw_price pulp_price timber_price ves_val_yr_firm ves_wgt_yr_firm all_val_yr_firm ves_val_yr_im_firm all_val_yr_1133_firm ves_val_yr_1133_firm ves_wgt_yr_1133_firm all_val_yr_1133_im_firm ves_val_yr_1133_im_firm ves_wgt_yr_1133_im_firm ves_wgt_yr_im_firm house_firm saw_price_firm pulp_price_firm timber_price_firm
egen state_eco_year = group(state na_l3code year)
egen state_eco = group(state na_l3code)


*****************************************************
*****************************************************
*****************************************************
*****************************************************
*****************************************************
*****************************************************
*****************************************************
*****************************************************


eststo clear
local mark "$\checkmark$"

preserve
keep if south_state == 1

*****************************************************
* Generate 5-year treatment variables
*****************************************************

cap drop logavg_harvest logavg_thin

gen logavg_harvest = log((L.n_cc_private + L2.n_cc_private + ///
                          L3.n_cc_private + L4.n_cc_private)/4 + 1)

gen logavg_thin = log((L.n_th_private + L2.n_th_private + ///
                       L3.n_th_private + L4.n_th_private)/4 + 1)

label variable logavg_harvest "Harvest"
label variable logavg_thin    "Thinning"

*****************************************************
* (1) Harvest FE
*****************************************************

qui reghdfe logmtbs_acre logavg_harvest $CONTROL if year >= 2000, ///
    absorb(fips year) ///
    vce(cluster fips)

eststo cc_fe
estadd scalar firstF = .
estadd local countyfe "`mark'"
estadd local yearfe   "`mark'"

scalar b_ols_cc = _b[logavg_harvest]

*****************************************************
* (2) Harvest IV
*****************************************************

qui ivreghdfe logmtbs_acre ///
    (logavg_harvest = L5.s_iv_harvest) ///
    $CONTROL if year >= 2000, ///
    absorb(fips year) ///
    cluster(fips)

eststo cc_iv5
capture estadd scalar firstF = e(widstat)
estadd local countyfe "`mark'"
estadd local yearfe   "`mark'"

scalar b_iv_cc = _b[logavg_harvest]

*****************************************************
* (3) Thinning FE
*****************************************************

qui reghdfe logmtbs_acre logavg_thin $CONTROL if year >= 2000, ///
    absorb(fips year) ///
    vce(cluster fips)

eststo th_fe
estadd scalar firstF = .
estadd local countyfe "`mark'"
estadd local yearfe   "`mark'"

scalar b_ols_th = _b[logavg_thin]

*****************************************************
* (4) Thinning IV
*****************************************************

qui ivreghdfe logmtbs_acre ///
    (logavg_thin = L4.s_iv_thin) ///
    $CONTROL if year >= 2000, ///
    absorb(fips year) ///
    cluster(fips)

eststo th_iv5
capture estadd scalar firstF = e(widstat)
estadd local countyfe "`mark'"
estadd local yearfe   "`mark'"

scalar b_iv_th = _b[logavg_thin]

*****************************************************
* (5) Joint IV: harvest and thinning together
*****************************************************

ivreghdfe logmtbs_acre ///
    (logavg_harvest logavg_thin = L5.s_iv_harvest L4.s_iv_thin ) ///
    $CONTROL if year >= 2000, ///
    absorb(fips year) ///
    cluster(fips)

eststo joint_iv
capture estadd scalar firstF = e(widstat)
estadd local countyfe "`mark'"
estadd local yearfe   "`mark'"

*****************************************************
* Compare harvest and thinning effects
*****************************************************

test logavg_harvest = logavg_thin

lincom logavg_harvest - logavg_thin

corr logavg_harvest logavg_thin

*****************************************************
* Export table
*****************************************************

esttab cc_fe cc_iv5 th_fe th_iv5 joint_iv ///
    using "Table_1.tex", replace ///
    fragment booktabs label ///
    varlabels(logavg_harvest "Harvest" ///
              logavg_thin "Thinning" ///
              bio_grow_ac_num "Bio growth rate") ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.4f) se(%9.4f) ///
    nonumbers nomtitles compress ///
    keep(logavg_harvest logavg_thin) ///
    order(logavg_harvest logavg_thin) ///
    mgroups("Harvest" "Thinning" "Joint IV", ///
        pattern(1 0 1 0 1) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span ///
        erepeat(\cmidrule(lr){@span})) ///
    collabels(none) ///
    posthead("& (1) & (2) & (3) & (4) & (5) \\" ///
             "& FE & IV & FE & IV & IV \\" ///
             "\midrule") ///
    postfoot("\midrule") ///
    stats(firstF N countyfe yearfe, ///
        fmt(%9.2f %9.0f %9s %9s) ///
        labels("Kleibergen-Paap Wald F statistic" ///
               "Observations" ///
               "County FE" ///
               "Year FE"))

restore

eststo clear
local mark "$\checkmark$"

*****************************************************
* Full sample: south only
*****************************************************

preserve
keep if south_state == 1

*****************************************************
* Harvest
*****************************************************

cap drop iv_house_ma3 logavg_treat
gen iv_house_ma3=L5.s_iv_harvest

label variable iv_house_ma3 "Shift-share instrument"

gen logavg_treat = log((L.n_cc_private + L2.n_cc_private + ///
                        L3.n_cc_private + L4.n_cc_private)/4 + 1)
label variable logavg_treat "Log average treatment"

* (1) FE: Harvest baseline
qui reghdfe logmtbs_acre logavg_treat $CONTROL if year >= 2000, ///
    absorb(fips year) ///
    vce(cluster state_eco)

eststo cc_fe
estadd scalar firstF = .
estadd local countyfe "`mark'"
estadd local yearfe   "`mark'"

* (2) First stage: Harvest baseline
qui reghdfe logavg_treat iv_house_ma3 $CONTROL if year >= 2000, ///
    absorb(fips year) ///
    vce(cluster state_eco)

eststo cc_fs
estadd scalar firstF = .
estadd local countyfe "`mark'"
estadd local yearfe   "`mark'"

* (3) IV: Harvest 5-year baseline
qui ivreghdfe logmtbs_acre ///
    (logavg_treat = iv_house_ma3) ///
    $CONTROL if year >= 2000, ///
    absorb(fips year) ///
    cluster(fips)

eststo cc_iv5
capture estadd scalar firstF = e(widstat)
estadd local countyfe "`mark'"
estadd local yearfe   "`mark'"

drop logavg_treat

*****************************************************
* Harvest
*****************************************************

gen logavg_treat = log((L.n_cc_private + L2.n_cc_private + ///
                        L3.n_cc_private)/3 + 1)
label variable logavg_treat "Log average treatment"

qui ivreghdfe logmtbs_acre ///
    (logavg_treat = L4.s_iv_harvest) ///
    $CONTROL if year >= 2000, ///
    absorb(fips year) ///
    cluster(fips)

eststo cc_iv4
capture estadd scalar firstF = e(widstat)
estadd local countyfe "`mark'"
estadd local yearfe   "`mark'"

drop logavg_treat

*****************************************************
* Harvest
*****************************************************

gen logavg_treat = log((L.n_cc_private + L2.n_cc_private + ///
                        L3.n_cc_private + L4.n_cc_private + ///
                        L5.n_cc_private )/5 + 1)
label variable logavg_treat "Log average treatment"

qui ivreghdfe logmtbs_acre ///
    (logavg_treat = L6.s_iv_harvest) ///
    $CONTROL if year >= 2000, ///
    absorb(fips year) ///
    cluster(fips)

eststo cc_iv6
capture estadd scalar firstF = e(widstat)
estadd local countyfe "`mark'"
estadd local yearfe   "`mark'"

drop logavg_treat


*****************************************************
* Thinning
*****************************************************

cap drop iv_house_ma3 
cap drop logavg_treat
gen iv_house_ma3 = L4.s_iv_thin
label variable iv_house_ma3 "Shift-share instrument"

gen logavg_treat = log((L.n_th_private + L2.n_th_private + ///
                        L3.n_th_private + L4.n_th_private )/4 + 1)
label variable logavg_treat "Log average treatment"

* (7) FE: Thinning baseline
qui reghdfe logmtbs_acre logavg_treat $CONTROL if year >= 2000, ///
    absorb(fips year) ///
    vce(cluster state_eco)

eststo th_fe
estadd scalar firstF = .
estadd local countyfe "`mark'"
estadd local yearfe   "`mark'"

* (8) First stage: Thinning baseline
qui reghdfe logavg_treat iv_house_ma3 $CONTROL if year >= 2000, ///
    absorb(fips year) ///
    vce(cluster state_eco)

eststo th_fs
estadd scalar firstF = .
estadd local countyfe "`mark'"
estadd local yearfe   "`mark'"

* (9) IV: Thinning 5-year baseline
qui ivreghdfe logmtbs_acre ///
    (logavg_treat = L4.s_iv_thin) ///
    $CONTROL if year >= 2000, ///
    absorb(fips year) ///
    cluster(fips)

eststo th_iv5
capture estadd scalar firstF = e(widstat)
estadd local countyfe "`mark'"
estadd local yearfe   "`mark'"

drop logavg_treat

*****************************************************
* Thinning
*****************************************************

gen logavg_treat = log((L.n_th_private + L2.n_th_private + ///
                        L3.n_th_private)/3 + 1)
label variable logavg_treat "Log average treatment"

qui ivreghdfe logmtbs_acre ///
    (logavg_treat = L3.s_iv_thin) ///
    $CONTROL if year >= 2000, ///
    absorb(fips year) ///
    cluster(fips)

eststo th_iv4
capture estadd scalar firstF = e(widstat)
estadd local countyfe "`mark'"
estadd local yearfe   "`mark'"

drop logavg_treat

*****************************************************
* Thinning
*****************************************************

gen logavg_treat = log((L.n_th_private + L2.n_th_private + ///
                        L3.n_th_private + L4.n_th_private + ///
                        L5.n_th_private)/5 + 1)
label variable logavg_treat "Log average treatment"

qui ivreghdfe logmtbs_acre ///
    (logavg_treat = L5.s_iv_thin) ///
    $CONTROL if year >= 2000, ///
    absorb(fips year) ///
    cluster(fips)

eststo th_iv6
capture estadd scalar firstF = e(widstat)
estadd local countyfe "`mark'"
estadd local yearfe   "`mark'"

drop logavg_treat

restore


*****************************************************
* No-COVID robustness, year < 2020
*****************************************************

preserve
keep if south_state == 1
keep if year < 2020
xtset fips year

*****************************************************
* Harvest no-COVID
*****************************************************

cap drop logavg_treat
gen logavg_treat = log((L.n_cc_private + L2.n_cc_private + ///
                        L3.n_cc_private+L4.n_cc_private)/4 + 1)
label variable logavg_treat "Log average treatment"

qui ivreghdfe logmtbs_acre ///
    (logavg_treat = L5.s_iv_harvest) ///
    $CONTROL if year >= 2000, ///
    absorb(fips year) ///
    cluster(fips)

eststo cc_iv5_2020
capture estadd scalar firstF = e(widstat)
estadd local countyfe "`mark'"
estadd local yearfe   "`mark'"

drop logavg_treat

*****************************************************
* Thinning no-COVID
*****************************************************

gen logavg_treat = log((L.n_th_private + L2.n_th_private + ///
                        L3.n_th_private +L4.n_th_private)/4 + 1)
label variable logavg_treat "Log average treatment"

qui ivreghdfe logmtbs_acre ///
    (logavg_treat = L4.s_iv_thin) ///
    $CONTROL if year >= 2000, ///
    absorb(fips year) ///
    cluster(fips)

eststo th_iv5_2020
capture estadd scalar firstF = e(widstat)
estadd local countyfe "`mark'"
estadd local yearfe   "`mark'"

tab year if e(sample)

drop logavg_treat
restore


*****************************************************
* Export table
*****************************************************

esttab cc_fe cc_fs cc_iv5 cc_iv4 cc_iv6 cc_iv5_2020 ///
       th_fe th_fs th_iv5 th_iv4 th_iv6 th_iv5_2020 ///
    using "table_A3.tex", replace ///
    fragment booktabs label ///
    varlabels(logavg_treat "Log average treatment" ///
              iv_house_ma3 "Shift-share instrument" ///
              bio_grow_ac_num "Bio growth rate") ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.4f) se(%9.4f) ///
    nonumbers nomtitles compress ///
    keep(logavg_treat iv_house_ma3 $CONTROL) ///
    order(logavg_treat iv_house_ma3 $CONTROL) ///
    mgroups("Harvest" "Thinning", ///
        pattern(1 0 0 0 0 0 1 0 0 0 0 0) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span ///
        erepeat(\cmidrule(lr){@span})) ///
    mlabels("Log wildfire area" "Log average treatment" "Log wildfire area" ///
            "Log wildfire area" "Log wildfire area" "Log wildfire area" ///
            "Log wildfire area" "Log average treatment" "Log wildfire area" ///
            "Log wildfire area" "Log wildfire area" "Log wildfire area", ///
            span prefix(\multicolumn{@span}{c}{) suffix(})) ///
    collabels(none) ///
    posthead("& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) & (9) & (10) & (11) & (12) \\" ///
             "& FE & First stage & IV: 4-year & IV: 3-year & IV: 5-year & IV no COVID & FE & First stage & IV: 4-year & IV: 3-year & IV: 5-year & IV no COVID \\" ///
             "\midrule") ///
    postfoot("\midrule") ///
    stats(firstF N countyfe yearfe, ///
        fmt(%9.2f %9.0f %9s %9s) ///
        labels("Kleibergen-Paap Wald F statistic" ///
               "Observations" ///
               "County FE" ///
               "Year FE"))
			   
			   
			   
			   
*==================================================*
* Table A4. Robustness tests for alternative wildfire indicators
*==================================================*

preserve
drop if south_state != 1

eststo clear
xtset fips year

local mark "$\checkmark$"

local outcomes1 logmtbs_acre mtbs_d mtbs_acre_p fema_count

*****************************************************
* Panel A. Harvest results: 4-year baseline
*****************************************************

capture drop ma5_logcc_total
gen ma5_logcc_total = log((L.n_cc_private + L2.n_cc_private + ///
                           L3.n_cc_private + L4.n_cc_private)/4 + 1)

label var ma5_logcc_total "Log average harvest area"

foreach y of local outcomes1 {

    qui ivreghdfe `y' ///
        (ma5_logcc_total = L5.s_iv_harvest) ///
        $CONTROL if year >= 2000, ///
        absorb(fips year) ///
        cluster(fips) first

    eststo cc_`y'
    capture estadd scalar firstF = e(widstat)
    estadd local countyfe "`mark'"
    estadd local yearfe   "`mark'"
}

*****************************************************
* Panel B. Thinning results: 4-year baseline
*****************************************************

capture drop ma5_logth_total
gen ma5_logth_total = log((L.n_th_private + L2.n_th_private + ///
                           L3.n_th_private + L4.n_th_private)/4 + 1)

label var ma5_logth_total "Log average thinning area"

foreach y of local outcomes1 {

    qui ivreghdfe `y' ///
        (ma5_logth_total = L4.s_iv_thin) ///
        $CONTROL if year >= 2000, ///
        absorb(fips year) ///
        cluster(fips) first

    eststo th_`y'
    capture estadd scalar firstF = e(widstat)
    estadd local countyfe "`mark'"
    estadd local yearfe   "`mark'"
}

*****************************************************
* Export Panel A: Harvest
*****************************************************

esttab ///
    cc_logmtbs_acre cc_mtbs_d cc_mtbs_acre_p ///
    using "table_A4_panelA.tex", replace ///
    fragment booktabs label ///
    coeflabels( ///
        ma5_logcc_total "Log average harvest area" ///
    ) ///
    keep(ma5_logcc_total) ///
    order(ma5_logcc_total) ///
    cells(b(fmt(%9.3f) star) se(par fmt(%9.3f))) ///
    nonumbers nomtitles noobs compress ///
    mlabels("Log wildfire area (MTBS)" ///
            "Wildfire event (MTBS)" ///
            "Percent fire (MTBS)", ///
            span prefix(\multicolumn{@span}{c}{) suffix(})) ///
    collabels(none) ///
    posthead("Panel A. Harvest results \\" ///
             "& (1) & (2) & (3) \\" ///
             "\midrule") ///
    stats(firstF N countyfe yearfe, ///
          fmt(%9.2f %9.0f %9s %9s) ///
          labels("Kleibergen-Paap Wald F statistic" ///
                 "Observations" ///
                 "County FE" ///
                 "Year FE")) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    postfoot("\midrule")

*****************************************************
* Export Panel B: Thinning
*****************************************************

esttab ///
    th_logmtbs_acre th_mtbs_d th_mtbs_acre_p ///
    using "table_A4_panelB.tex", replace ///
    fragment booktabs label ///
    coeflabels( ///
        ma5_logth_total "Log average thinning area" ///
    ) ///
    keep(ma5_logth_total) ///
    order(ma5_logth_total) ///
    cells(b(fmt(%9.3f) star) se(par fmt(%9.3f))) ///
    nonumbers nomtitles noobs compress ///
    mlabels("Log wildfire area (MTBS)" ///
            "Wildfire event (MTBS)" ///
            "Percent fire (MTBS)", ///
            span prefix(\multicolumn{@span}{c}{) suffix(})) ///
    collabels(none) ///
    posthead("Panel B. Thinning results \\" ///
             "& (4) & (5) & (6) \\" ///
             "\midrule") ///
    stats(firstF N countyfe yearfe, ///
          fmt(%9.2f %9.0f %9s %9s) ///
          labels("Kleibergen-Paap Wald F statistic" ///
                 "Observations" ///
                 "County FE" ///
                 "Year FE")) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    postfoot("\midrule")

restore


*****************************************************
* Robustness of dropping Texas and oklohoma
*****************************************************

eststo clear
local mark "$\checkmark$"

preserve
keep if south_state == 1

*****************************************************
* Generate 5-year treatment variables
*****************************************************

cap drop logavg_harvest logavg_thin

gen logavg_harvest = log((L.n_cc_private + L2.n_cc_private + ///
                          L3.n_cc_private + L4.n_cc_private)/4 + 1)

gen logavg_thin = log((L.n_th_private + L2.n_th_private + ///
                       L3.n_th_private + L4.n_th_private)/4 + 1)

label variable logavg_harvest "Harvest"
label variable logavg_thin    "Thinning"

*****************************************************
* (1) Harvest IV
*****************************************************

qui ivreghdfe logmtbs_acre ///
    (logavg_harvest = L5.s_iv_harvest) ///
    $CONTROL if year >= 2000, ///
    absorb(fips year) ///
    cluster(fips)

eststo cc_iv5
capture estadd scalar firstF = e(widstat)
estadd local countyfe "`mark'"
estadd local yearfe   "`mark'"

*****************************************************
* (2) Harvest IV
*****************************************************

qui ivreghdfe logmtbs_acre ///
    (logavg_harvest = L5.s_iv_harvest) ///
    $CONTROL if year >= 2000  &  !inlist(state, 48, 40), ///
    absorb(fips year) ///
    cluster(fips)

eststo cc_iv5_dropped
capture estadd scalar firstF = e(widstat)
estadd local countyfe "`mark'"
estadd local yearfe   "`mark'"


*****************************************************
* (3) Thinning IV
*****************************************************

qui ivreghdfe logmtbs_acre ///
    (logavg_thin = L4.s_iv_thin) ///
    $CONTROL if year >= 2000, ///
    absorb(fips year) ///
    cluster(fips)

eststo th_iv5
capture estadd scalar firstF = e(widstat)
estadd local countyfe "`mark'"
estadd local yearfe   "`mark'"

scalar b_iv_th = _b[logavg_thin]

*****************************************************
* (4) Thinning IV
*****************************************************

qui ivreghdfe logmtbs_acre ///
    (logavg_thin = L4.s_iv_thin) ///
    $CONTROL if year >= 2000 &  !inlist(state, 48, 40), ///
    absorb(fips year) ///
    cluster(fips)

eststo th_iv5_dropped
capture estadd scalar firstF = e(widstat)
estadd local countyfe "`mark'"
estadd local yearfe   "`mark'"

scalar b_iv_th = _b[logavg_thin]

*****************************************************
* Export table
*****************************************************

esttab cc_iv5 cc_iv5_dropped th_iv5 th_iv5_dropped ///
    using "Table_A6_noFLOK.tex", replace ///
    fragment booktabs label ///
    varlabels(logavg_harvest "Harvest" ///
              logavg_thin "Thinning" ///
              bio_grow_ac_num "Bio growth rate") ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.4f) se(%9.4f) ///
    nonumbers nomtitles compress ///
    keep(logavg_harvest logavg_thin) ///
    order(logavg_harvest logavg_thin) ///
    mgroups("Harvest" "Thinning", ///
        pattern(1 0 1 0) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span ///
        erepeat(\cmidrule(lr){@span})) ///
    collabels(none) ///
    posthead("& (1) & (2) & (3) & (4) \\" ///
             "\midrule") ///
    postfoot("\midrule") ///
    stats(firstF N countyfe yearfe, ///
        fmt(%9.2f %9.0f %9s %9s) ///
        labels("Kleibergen-Paap Wald F statistic" ///
               "Observations" ///
               "County FE" ///
               "Year FE"))


restore

