********************************************************************************
*******Clean the csv dataset constructed by python to a stata dta file**********
********************************************************************************
capture log close
********************************************************************************
******************Set up********************************************************
********************************************************************************
clear
clear all

import delimited "C:\Users\zhuku\OneDrive - Auburn University\python_project\wildfire_milling\data cleaning\fia_level\fia_family_owner_v5.csv"
log using "disturbance_wildfire_fia.smcl", replace


xtset plt_id year

* data cleaning
gen state = statecd
gen geo =statecd*1000+countycd

* generate states with information
gen south_state=0
replace south_state=1 if inlist(state, 1, 5, 37, 40, 12, 13, 45, 47, 48, 51, 21, 22, 28)
drop if south_state != 1


tab year
tab year if clearcut_srs==1

tab owngrpcd stdorgcd if clearcut_srs==1

drop if extend_year!=0


*only include private land owners
drop if owngrpcd !=40

tab mtbs_count_10 year
tab nifc_count_10 year

* drop reserved land
drop if reservcd==1

* generate code for ecoregion 3
encode na_l3code_x, gen(l3eco)
gen na_l2code = regexs(1) if regexm(na_l3code_x, "^([0-9]+\.[0-9]+)\.")
encode na_l2code, gen(l2eco)


* clean tpa
sum tpa if year>2000,d
local p90 = r(p90)
display r(p90)
gen tpa_cap=0
replace tpa_cap=1 if tpa>1000
replace tpa=r(p90) if tpa>`p90'
egen tpa_cap_max = max(tpa_cap), by(plt_id)
drop if tpa_cap_max==1

* clean stdage
replace stdage=200 if stdage>200


* fill na
* not included dia_grow ba_grow netvol_grow sawvol_grow bio_grow baa_grow netvol_grow_ac sawvol_grow_ac bio_grow_ac
foreach var in rddistcd prev_tpa spgrpcd  clearcut_srs thin_srs part_srs n_clearcut_srs n_thin_srs major_clearcut_srs major_thin_srs major_part_srs major_n_clearcut_srs major_n_thin_srs l3eco mtbs_count_2 nifc_count_2 mtbs_count_10 nifc_count_10 fire_srs cutting site_preparation arti_regen natural_regen silviculture private_arti private_natural c_a t_a c_n t_n private_arti_s private_natural_s c_a_s t_a_s c_n_s t_n_s private_arti_se private_natural_se c_a_se t_a_se c_n_se t_n_se estabs{
	replace `var'=0 if missing(`var')
}



* get whether management activities happens in the last 4 years
gen sitep_4 = ///
    (l4.site_preparation == 1) | ///
    (l1.site_preparation == 1) | ///
    (l2.site_preparation == 1) | ///
    (l3.site_preparation == 1)
gen silvi_4 = ///
    (l4.silviculture == 1) | ///
    (l1.silviculture == 1) | ///
    (l2.silviculture == 1) | ///
    (l3.silviculture == 1)
	
* fill na by mean
foreach var in stdage bio_grow_ac slope elev vdp_max_2 vdp_max_10{
	sum `var'
	replace `var'=r(mean) if missing(`var')
}

* drop nifc>2018
foreach var in nifc_count_2 nifc_count_10{
	replace `var'=1 if `var'>1 & missing(`var')==0
	replace `var'  = . if year>2018
}

foreach var in mtbs_count_2 mtbs_count_10{
	replace `var'=1 if `var'>1 & missing(`var')==0
}

* log transformation
foreach var in vdp_max_2 vdp_max_10 {
    gen log`var'=log(`var'+1)
}

* artifically generated
egen artifical_gen = max(stdorgcd), by(plt_id)

* tree types
gen SPGR =  .
* 1. Loblolly Pines
replace SPGR = 1 if spgrpcd == 2
* 2. Other Commercial Softwood
replace SPGR = 2 if inlist(spgrpcd, 1, 3, 4, 6)
* 3. Non-Commercial Softwood
replace SPGR = 3 if inlist(spgrpcd, 7, 8, 9, 23, 24)
* 4. Upland Hardwoods
replace SPGR = 4 if inlist(spgrpcd, ///
    28, 27, 25, 26, 29, 31, 33, 40, 38, 30)
* 5. Bottomland Hardwoods
replace SPGR = 5 if inlist(spgrpcd, ///
    34, 32, 35, 39, 36, 37, ///
    43, 41, 42, 48, 55, 54)
replace SPGR=0 if missing(SPGR)

gen rddist_ft=0
replace rddist_ft = 50       if rddistcd == 1   // 100 ft | less
replace rddist_ft = 200      if rddistcd == 2   // 101–300 ft
replace rddist_ft = 400      if rddistcd == 3   // 301–500 ft
replace rddist_ft = 750      if rddistcd == 4   // 501–1000 ft
replace rddist_ft = 3140     if rddistcd == 5   // 1001 ft–1/2 mile
replace rddist_ft = 3960     if rddistcd == 6   // 1/2–1 mile
replace rddist_ft = 10560    if rddistcd == 7   // 1–3 miles
replace rddist_ft = 21120    if rddistcd == 8   // 3–5 miles
replace rddist_ft = 31680    if rddistcd == 9   // greater than 5 miles, assigned 6 miles


* weighted by baseline area_total_private
bysort plt_id (year): gen area_total_private_s = area_total_private[1]


egen state_eco = group(state l3eco)
egen county_eco = group(fips l3eco)
********************************************************************************
******************Southern regions analysis ************************************
********************************************************************************


gen iv_saw=saw_price*private_arti/(private_natural+private_arti)
gen iv_pulp=pulp_price*private_arti/(private_natural+private_arti)

* state level shift share
gen state_clear=((c_a_s-c_a)/(private_arti_s-private_arti)*L.private_arti+(c_n_s-c_n)/(private_natural_s-private_natural)*L.private_natural)/(L.private_natural+L.private_arti)
gen state_thin=((t_a_s-t_a)/(private_arti_s-private_arti)*L.private_arti+(t_n_s-t_n)/(private_natural_s-private_natural)*L.private_natural)/(L.private_natural+L.private_arti)

* state_eco region level
gen se_clear=((c_a_se-c_a)/(private_arti_se-private_arti)*L.private_arti+(c_n_se-c_n)/(private_natural_se-private_natural)*L.private_natural)/(L.private_natural+L.private_arti)
gen se_thin=((t_a_se-t_a)/(private_arti_se-private_arti)*L.private_arti+(t_n_se-t_n)/(private_natural_se-private_natural)*L.private_natural)/(L.private_natural+L.private_arti)

gen c_clear=((c_a_c-c_a)/(private_arti_c-private_arti)*L.private_arti+(c_n_c-c_n)/(private_natural_c-private_natural)*L.private_natural)/(L.private_natural+L.private_arti)
gen c_thin=((t_a_c-t_a)/(private_arti_c-private_arti)*L.private_arti+(t_n_c-t_n)/(private_natural_c-private_natural)*L.private_natural)/(L.private_natural+L.private_arti)

foreach var in se_clear c_clear{
	replace `var'=state_clear if missing(`var')
}
foreach var in se_thin c_thin{
	replace `var'=state_thin if missing(`var')
}
	
*****************************************************
gl CONTROL "vdp_max_10 tpa saw_cf_acre stdage percent_arti slope elev bio_grow_ac estabs rddist_ft i.siteclcd"
*********************************************************
******************************** SS table **************************************


xtset plt_id year
preserve

foreach v in state_clear state_thin {

    capture drop L1_`v' L2_`v' L3_`v' L4_`v' ma3_`v'

    gen L1_`v' = L1.`v'
    gen L2_`v' = L2.`v'
    gen L3_`v' = L3.`v'
	gen L4_`v' =L4.`v'
	gen ma3_`v' = L1_`v'+ L2_`v'+ L3_`v'+ L4_`v'
}



foreach var in  area_total_private{
	replace `var' = `var'/1000
}
replace rddist_ft = rddist_ft / 5280

tabstat mtbs_count_10 major_n_clearcut_srs major_n_thin_srs state_clear state_thin area_total_private ///
                    vdp_max_10 tpa saw_cf_acre stdage percent_arti slope elev bio_grow_ac estabs rddist_ft  siteclcd if year >= 2000 & missing(ma3_state_clear)==0, ///
    stat(n mean sd min max) ///
    col(stat) save

matrix A = r(StatTotal)'
matrix rownames A = mtbs_count_10 major_n_clearcut_srs major_n_thin_srs state_clear state_thin area_total_private ///
                    vdp_max_10 tpa saw_cf_acre stdage percent_arti slope elev bio_grow_ac estabs rddist_ft  siteclcd
	
		
matrix B = A

forvalues i = 1/`=rowsof(A)' {
    forvalues j = 1/`=colsof(A)' {
        matrix B[`i',`j'] = round(A[`i',`j'], .001)
    }
}

esttab matrix(B) using "table_1.tex", replace ///
    fragment booktabs nomtitles nonumber ///
	label ///
    varlabels( ///
        mtbs_count_10 "Wildfire occurrence" ///
        major_n_clearcut_srs "Harvest" ///
        major_n_thin_srs "Thinning" ///
        state_clear "Harvest shift-share" ///
        state_thin "Thinning shift-share" ///
        area_total_private "Total forest area (thousand acre)" ///
        vdp_max_10 "Maximum VPD (hPa)" ///
        tpa "Trees per acre" ///
        saw_cf_acre "Sawtimber volume per acre (ft$^3$/acre)" ///
		bio_grow_ac "Bio growth rate" ///
        slope "Slope (\%)" ///
        elev "Elevation (m)" ///
        stdage "Stand age" ///
        percent_arti "Percentage artifical forest" ///
		rddist_ft "Distance to nearest road (mile)" ///
		siteclcd "Siteclass (Best:1 to Worst:7)" ///
		estabs "Forestry and logging establishments" ///
    ) ///
    collabels("Obs" "Mean" "Std. Dev." "Min" "Max") ///
    cells("B") ///
    postfoot("\midrule")
	
egen unit_tag = tag(plt_id) if year >= 2000 & missing(ma3_state_clear)==0
count if unit_tag == 1
display "Number of units in the sample = " r(N)
drop unit_tag

restore	

replace stdage=stdage/10
replace tpa=tpa/100
replace saw_cf_acre=saw_cf_acre/1000
replace rddist_ft = rddist_ft / 5280

********************************************************************************
* Plot fixed effects: IV using ivreghdfe first stage, also test statistics
********************************************************************************
preserve
eststo clear

drop if south_state != 1
xtset plt_id year

* Generate lag and MA3 treatment variables
foreach v in major_n_clearcut_srs major_n_thin_srs {

    capture drop L1_`v' L2_`v' L3_`v' L4_`v' ma3_`v'

    gen L1_`v' = L1.`v'
    gen L2_`v' = L2.`v'
    gen L3_`v' = L3.`v'
	gen L4_`v' = L4.`v'
	

    egen ma3_`v' = rowmax(L1_`v' L2_`v' L3_`v' L4_`v')
}


foreach v in state_clear state_thin {

    capture drop L1_`v' L2_`v' L3_`v' L4_`v' ma3_`v'

    gen L1_`v' = L1.`v'
    gen L2_`v' = L2.`v'
    gen L3_`v' = L3.`v'
	gen L4_`v' =L4.`v'
	gen ma3_`v' = L1_`v'+ L2_`v'+ L3_`v'+ L4_`v'
}


*****************************************************
* Harvest models
*****************************************************
* (3) Harvest IV
eststo cc_iv: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_clearcut_srs = ma3_state_clear) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) cluster(county_eco) endog(ma3_major_n_clearcut_srs)


capture estadd scalar firstF = e(cdf)
capture estadd scalar secF   = e(widstat)
estadd local PlotFE "\checkmark"
estadd local YearFE "\checkmark"
estadd local ForestGroupFE "\checkmark"
estadd local EcoFE "\checkmark"

gen sample=e(sample)

* (1) Harvest FE
eststo cc_fe: xtreg mtbs_count_10 ma3_major_n_clearcut_srs $CONTROL ///
    i.year i.SPGR i.l3eco ///
    [aweight = area_total_private_s] ///
    if sample== 1, ///
    fe vce(cluster county_eco)

estadd local PlotFE "\checkmark"
estadd local YearFE "\checkmark"
estadd local ForestGroupFE "\checkmark"
estadd local EcoFE "\checkmark"


* (2) Harvest first stage
eststo cc_fs: xtreg ma3_major_n_clearcut_srs ma3_state_clear $CONTROL ///
    i.year i.SPGR i.l3eco ///
    [aweight = area_total_private_s] ///
    if sample== 1, ///
    fe vce(cluster county_eco)

test ma3_state_clear

estadd local PlotFE "\checkmark"
estadd local YearFE "\checkmark"
estadd local ForestGroupFE "\checkmark"
estadd local EcoFE "\checkmark"




drop sample
*****************************************************
* Thinning models
*****************************************************
* (6) Thinning IV
eststo thin_iv: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_thin_srs = ma3_state_thin) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) cluster(county_eco) endog(ma3_major_n_thin_srs)


capture estadd scalar firstF = e(cdf)
capture estadd scalar secF   = e(widstat)
estadd local PlotFE "\checkmark"
estadd local YearFE "\checkmark"
estadd local ForestGroupFE "\checkmark"
estadd local EcoFE "\checkmark"



gen sample=e(sample)


* (4) Thinning FE
eststo thin_fe: xtreg mtbs_count_10 ma3_major_n_thin_srs $CONTROL ///
    i.year i.SPGR i.l3eco ///
    [aweight = area_total_private_s] ///
    if sample== 1, ///
    fe vce(cluster county_eco)

estadd local PlotFE "\checkmark"
estadd local YearFE "\checkmark"
estadd local ForestGroupFE "\checkmark"
estadd local EcoFE "\checkmark"


* (5) Thinning first stage
eststo thin_fs: xtreg ma3_major_n_thin_srs ma3_state_thin $CONTROL ///
    i.year i.SPGR i.l3eco ///
    [aweight = area_total_private_s] ///
    if sample== 1, ///
    fe vce(cluster county_eco)


test ma3_state_thin

estadd local PlotFE "\checkmark"
estadd local YearFE "\checkmark"
estadd local ForestGroupFE "\checkmark"
estadd local EcoFE "\checkmark"





*****************************************************
* Joint IV: harvest and thinning together
*****************************************************

* (7) Joint IV
eststo joint_iv: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_clearcut_srs ma3_major_n_thin_srs = ma3_state_thin ma3_state_clear) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) cluster(county_eco)

capture estadd scalar firstF = e(cdf)
capture estadd scalar secF   = e(widstat)
estadd local PlotFE "\checkmark"
estadd local YearFE "\checkmark"
estadd local ForestGroupFE "\checkmark"
estadd local EcoFE "\checkmark"


*****************************************************
* Thinning versus waiting / no treatment
*****************************************************

capture drop ma3_wait
gen ma3_wait = 1
replace ma3_wait = 0 if ma3_major_n_clearcut_srs == 1 | ma3_major_n_thin_srs == 1

* (8) Thinning and wait IV
eststo thin_wait_iv: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_thin_srs ma3_wait =  ma3_state_thin ma3_state_clear) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) cluster(county_eco)

capture estadd scalar firstF = e(cdf)
capture estadd scalar secF   = e(widstat)
estadd local PlotFE "\checkmark"
estadd local YearFE "\checkmark"
estadd local ForestGroupFE "\checkmark"
estadd local EcoFE "\checkmark"


*****************************************************
* Export LaTeX table
*****************************************************

esttab using "wildfire_iv_difference_3yr.csv", se ar2 label ///
    noconstant stats(firstF secF N) ///
    star(* .1 ** .05 *** .01) replace


esttab cc_fe cc_fs cc_iv thin_fe thin_fs thin_iv joint_iv thin_wait_iv ///
    using "Table_Ap1_4yr.tex", replace ///
    fragment booktabs se label ///
    noconstant nomtitles nonumber ///
    b(%9.4f) se(%9.4f) ///
    star(* .1 ** .05 *** .01) ///
    prehead("\begin{tabular}{lcccccccc}" ///
            "\toprule" ///
            "& \multicolumn{3}{c}{Harvest} & \multicolumn{3}{c}{Thinning} & \multicolumn{1}{c}{Joint IV} & \multicolumn{1}{c}{Thinning vs. Wait} \\" ///
            "\cmidrule(lr){2-4}\cmidrule(lr){5-7}\cmidrule(lr){8-8}\cmidrule(lr){9-9}") ///
    posthead("& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\" ///
			 "& \multicolumn{1}{c}{Wildfire event} & \multicolumn{1}{c}{Harvest event} & \multicolumn{1}{c}{Wildfire event} & \multicolumn{1}{c}{Wildfire event} & \multicolumn{1}{c}{Thinning event} & \multicolumn{1}{c}{Wildfire event} & \multicolumn{1}{c}{Wildfire event} & \multicolumn{1}{c}{Wildfire event} \\" ///
             "& FE & First stage & IV & FE & First stage & IV & IV & IV \\" ///
             "\midrule") ///
    keep(ma3_major_n_clearcut_srs ma3_major_n_thin_srs ma3_wait ///
         ma3_state_clear ma3_state_thin ///
         vdp_max_10 tpa saw_cf_acre stdage slope elev bio_grow_ac estabs) ///
    order(ma3_major_n_clearcut_srs ma3_major_n_thin_srs ma3_wait ///
          ma3_state_clear ma3_state_thin ///
          vdp_max_10 tpa saw_cf_acre stdage slope elev bio_grow_ac estabs) ///
    coeflabels( ///
        ma3_major_n_clearcut_srs "Harvest" ///
        ma3_major_n_thin_srs "Thinning" ///
        ma3_wait "No treatment / wait" ///
        ma3_state_clear "Harvest shift-share instrument" ///
        ma3_state_thin "Thinning shift-share instrument" ///
        vdp_max_10 "Maximum VPD" ///
        tpa "Trees per acre" ///
        saw_cf_acre "Sawtimber volume per acre" ///
        stdage "Stand age" ///
        bio_grow_ac "Bio growth rate" ///
        slope "Slope" ///
        elev "Elevation" ///
		estabs "Forestry establishments" ///		
    ) ///
    stats(secF N PlotFE YearFE ForestGroupFE EcoFE, ///
        labels("Kleibergen-Paap Wald F statistic" ///
               "Observations" ///
               "Plot FE" ///
               "Year FE" ///
               "Species group FE" ///
               "Ecoregion FE") ///
        fmt(%9.2f %9.0fc %s %s %s %s)) ///
    postfoot("\bottomrule" ///
             "\end{tabular}")


esttab cc_fe cc_iv thin_fe thin_iv joint_iv ///
    using "Table2_4yr.tex", replace ///
    fragment booktabs se label ///
    noconstant nomtitles nonumber ///
    b(%9.4f) se(%9.4f) ///
    star(* .1 ** .05 *** .01) ///
    prehead("\begin{tabular}{lccccc}" ///
            "\toprule" ///
            "& \multicolumn{2}{c}{Harvest} & \multicolumn{2}{c}{Thinning} & \multicolumn{1}{c}{Joint IV} \\" ///
            "\cmidrule(lr){2-3}\cmidrule(lr){4-5}\cmidrule(lr){6-6}") ///
    posthead("& (1) & (2) & (3) & (4) & (5) \\" ///
			 "& \multicolumn{1}{c}{Wildfire event} & \multicolumn{1}{c}{Wildfire event} & \multicolumn{1}{c}{Wildfire event} & \multicolumn{1}{c}{Wildfire event} & \multicolumn{1}{c}{Wildfire event} \\" ///
             "& FE & IV & FE & IV & IV \\" ///
             "\midrule") ///
    keep(ma3_major_n_clearcut_srs ma3_major_n_thin_srs) ///
    order(ma3_major_n_clearcut_srs ma3_major_n_thin_srs) ///
    coeflabels( ///
        ma3_major_n_clearcut_srs "Harvest" ///
        ma3_major_n_thin_srs "Thinning" ///
        ma3_state_clear "Harvest shift-share" ///
        ma3_state_thin "Thinning shift-share" ///
    ) ///
    stats(secF N PlotFE YearFE ForestGroupFE EcoFE, ///
        labels("Kleibergen-Paap Wald F statistic" ///
               "Observations" ///
               "Plot FE" ///
               "Year FE" ///
               "Species group FE" ///
               "Ecoregion FE") ///
        fmt(%9.2f %9.0fc %s %s %s %s)) ///
    postfoot("\bottomrule" ///
             "\end{tabular}")

restore


********************************************************************************
* IV dropping FL and OK
********************************************************************************
preserve
eststo clear

drop if south_state != 1
xtset plt_id year

* Generate lag and MA3 treatment variables
foreach v in major_n_clearcut_srs major_n_thin_srs {

    capture drop L1_`v' L2_`v' L3_`v' L4_`v' ma3_`v'

    gen L1_`v' = L1.`v'
    gen L2_`v' = L2.`v'
    gen L3_`v' = L3.`v'
	gen L4_`v' = L4.`v'
	

    egen ma3_`v' = rowmax(L1_`v' L2_`v' L3_`v' L4_`v')
}


foreach v in state_clear state_thin {

    capture drop L1_`v' L2_`v' L3_`v' L4_`v' ma3_`v'

    gen L1_`v' = L1.`v'
    gen L2_`v' = L2.`v'
    gen L3_`v' = L3.`v'
	gen L4_`v' =L4.`v'

//     egen ma3_iv_`v' = rowmax(L1_`v' L2_`v' L3_`v' L4_`v')
	gen ma3_`v' = L1_`v'+ L2_`v'+ L3_`v'+ L4_`v'
}


*****************************************************
* Harvest models
*****************************************************
* (1) Harvest IV
eststo cc_iv: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_clearcut_srs = ma3_state_clear) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) cluster(county_eco) endog(ma3_major_n_clearcut_srs)

capture estadd scalar firstF = e(cdf)
capture estadd scalar secF   = e(widstat)
estadd local PlotFE "\checkmark"
estadd local YearFE "\checkmark"
estadd local ForestGroupFE "\checkmark"
estadd local EcoFE "\checkmark"


* (2) Harvest IV
eststo cc_no: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_clearcut_srs = ma3_state_clear) ///
    [aweight = area_total_private_s] if year >= 2000 & !inlist(state, 48), ///
    absorb(plt_id year SPGR l3eco) cluster(county_eco) endog(ma3_major_n_clearcut_srs)

capture estadd scalar firstF = e(cdf)
capture estadd scalar secF   = e(widstat)
estadd local PlotFE "\checkmark"
estadd local YearFE "\checkmark"
estadd local ForestGroupFE "\checkmark"
estadd local EcoFE "\checkmark"


*****************************************************
* Thinning models
*****************************************************
* (3) Thinning IV
eststo thin_iv: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_thin_srs = ma3_state_thin) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) cluster(county_eco) endog(ma3_major_n_thin_srs)

capture estadd scalar firstF = e(cdf)
capture estadd scalar secF   = e(widstat)
estadd local PlotFE "\checkmark"
estadd local YearFE "\checkmark"
estadd local ForestGroupFE "\checkmark"
estadd local EcoFE "\checkmark"
* (3) Thinning IV
eststo thin_no: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_thin_srs = ma3_state_thin) ///
    [aweight = area_total_private_s] if year >= 2000 &  !inlist(state, 48), ///
    absorb(plt_id year SPGR l3eco) cluster(county_eco) endog(ma3_major_n_thin_srs)

	
capture estadd scalar firstF = e(cdf)
capture estadd scalar secF   = e(widstat)
estadd local PlotFE "\checkmark"
estadd local YearFE "\checkmark"
estadd local ForestGroupFE "\checkmark"
estadd local EcoFE "\checkmark"



*****************************************************
* Export LaTeX table
*****************************************************
esttab cc_iv cc_no thin_iv thin_no ///
    using "Table_A2_noTX.tex", replace ///
    fragment booktabs se label ///
    noconstant nomtitles nonumber ///
    b(%9.4f) se(%9.4f) ///
    star(* .1 ** .05 *** .01) ///
    prehead("\begin{tabular}{lcccc}" ///
            "\toprule" ///
            "& \multicolumn{2}{c}{Harvest} & \multicolumn{2}{c}{Thinning} \\" ///
            "\cmidrule(lr){2-3}\cmidrule(lr){4-5}") ///
    posthead("& (1) & (2) & (3) & (4)  \\" ///
	"& \multicolumn{1}{c}{Wildfire event} & \multicolumn{1}{c}{Wildfire event} & \multicolumn{1}{c}{Wildfire event} & \multicolumn{1}{c}{Wildfire event} \\" ///
             "\midrule") ///
    keep(ma3_major_n_clearcut_srs ma3_major_n_thin_srs) ///
    order(ma3_major_n_clearcut_srs ma3_major_n_thin_srs) ///
    coeflabels( ///
        ma3_major_n_clearcut_srs "Harvest" ///
        ma3_major_n_thin_srs "Thinning" ///
    ) ///
    stats(secF N PlotFE YearFE ForestGroupFE EcoFE, ///
        labels("Kleibergen-Paap Wald F statistic" ///
               "Observations" ///
               "Plot FE" ///
               "Year FE" ///
               "Species group FE" ///
               "Ecoregion FE") ///
        fmt(%9.2f %9.0fc %s %s %s %s)) ///
    postfoot("\bottomrule" ///
             "\end{tabular}")


restore





********************************************************************************
* IV dropping FL and OK
********************************************************************************
preserve
eststo clear

drop if south_state != 1
xtset plt_id year

* Generate lag and MA3 treatment variables
foreach v in major_n_clearcut_srs major_n_thin_srs {

    capture drop L1_`v' L2_`v' L3_`v' L4_`v' ma3_`v'

    gen L1_`v' = L1.`v'
    gen L2_`v' = L2.`v'
    gen L3_`v' = L3.`v'
	gen L4_`v' = L4.`v'
	

    egen ma3_`v' = rowmax(L1_`v' L2_`v' L3_`v' L4_`v')
}


foreach v in state_clear state_thin {

    capture drop L1_`v' L2_`v' L3_`v' L4_`v' ma3_`v'

    gen L1_`v' = L1.`v'
    gen L2_`v' = L2.`v'
    gen L3_`v' = L3.`v'
	gen L4_`v' =L4.`v'

//     egen ma3_iv_`v' = rowmax(L1_`v' L2_`v' L3_`v' L4_`v')
	gen ma3_`v' = L1_`v'+ L2_`v'+ L3_`v'+ L4_`v'
}


*****************************************************
* Harvest models
*****************************************************
* (1) Harvest IV
eststo cc_iv: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_clearcut_srs = ma3_state_clear) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) cluster(county_eco) endog(ma3_major_n_clearcut_srs)

capture estadd scalar firstF = e(cdf)
capture estadd scalar secF   = e(widstat)
estadd local PlotFE "\checkmark"
estadd local YearFE "\checkmark"
estadd local ForestGroupFE "\checkmark"
estadd local EcoFE "\checkmark"


* (2) Harvest IV
eststo cc_no: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_clearcut_srs = ma3_state_clear) ///
    [aweight = area_total_private_s] if year >= 2000 & !inlist(state, 48, 40), ///
    absorb(plt_id year SPGR l3eco) cluster(county_eco) endog(ma3_major_n_clearcut_srs)

capture estadd scalar firstF = e(cdf)
capture estadd scalar secF   = e(widstat)
estadd local PlotFE "\checkmark"
estadd local YearFE "\checkmark"
estadd local ForestGroupFE "\checkmark"
estadd local EcoFE "\checkmark"


*****************************************************
* Thinning models
*****************************************************
* (3) Thinning IV
eststo thin_iv: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_thin_srs = ma3_state_thin) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) cluster(county_eco) endog(ma3_major_n_thin_srs)

capture estadd scalar firstF = e(cdf)
capture estadd scalar secF   = e(widstat)
estadd local PlotFE "\checkmark"
estadd local YearFE "\checkmark"
estadd local ForestGroupFE "\checkmark"
estadd local EcoFE "\checkmark"
* (3) Thinning IV
eststo thin_no: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_thin_srs = ma3_state_thin) ///
    [aweight = area_total_private_s] if year >= 2000 &  !inlist(state, 48, 40), ///
    absorb(plt_id year SPGR l3eco) cluster(county_eco) endog(ma3_major_n_thin_srs)

	
capture estadd scalar firstF = e(cdf)
capture estadd scalar secF   = e(widstat)
estadd local PlotFE "\checkmark"
estadd local YearFE "\checkmark"
estadd local ForestGroupFE "\checkmark"
estadd local EcoFE "\checkmark"



*****************************************************
* Export LaTeX table
*****************************************************
esttab cc_iv cc_no thin_iv thin_no ///
    using "Table_A2_noFLOK.tex", replace ///
    fragment booktabs se label ///
    noconstant nomtitles nonumber ///
    b(%9.4f) se(%9.4f) ///
    star(* .1 ** .05 *** .01) ///
    prehead("\begin{tabular}{lcccc}" ///
            "\toprule" ///
            "& \multicolumn{2}{c}{Harvest} & \multicolumn{2}{c}{Thinning} \\" ///
            "\cmidrule(lr){2-3}\cmidrule(lr){4-5}") ///
    posthead("& (1) & (2) & (3) & (4)  \\" ///
	"& \multicolumn{1}{c}{Wildfire event} & \multicolumn{1}{c}{Wildfire event} & \multicolumn{1}{c}{Wildfire event} & \multicolumn{1}{c}{Wildfire event} \\" ///
             "\midrule") ///
    keep(ma3_major_n_clearcut_srs ma3_major_n_thin_srs) ///
    order(ma3_major_n_clearcut_srs ma3_major_n_thin_srs) ///
    coeflabels( ///
        ma3_major_n_clearcut_srs "Harvest" ///
        ma3_major_n_thin_srs "Thinning" ///
    ) ///
    stats(secF N PlotFE YearFE ForestGroupFE EcoFE, ///
        labels("Kleibergen-Paap Wald F statistic" ///
               "Observations" ///
               "Plot FE" ///
               "Year FE" ///
               "Species group FE" ///
               "Ecoregion FE") ///
        fmt(%9.2f %9.0fc %s %s %s %s)) ///
    postfoot("\bottomrule" ///
             "\end{tabular}")


restore





*****************************************************
*****************************************************
*****************************************************
*****************************************************
*****************************************************
*****************************************************


*** robustness
preserve
eststo clear

drop if south_state != 1
xtset plt_id year

local mark "\checkmark"

*****************************************************
* Generate max-event treatment variables
*****************************************************

foreach v in major_n_clearcut_srs major_n_thin_srs {

    capture drop L1_`v' L2_`v' L3_`v' L4_`v'
    capture drop ma2_`v' ma3_`v' ma4_`v'

    gen L1_`v' = L1.`v'
    gen L2_`v' = L2.`v'
    gen L3_`v' = L3.`v'
    gen L4_`v' = L4.`v'
    gen L5_`v' = L5.`v'

    egen ma3_`v' = rowmax(L1_`v' L2_`v' L3_`v')
    egen ma4_`v' = rowmax(L1_`v' L2_`v' L3_`v' L4_`v')
	egen ma5_`v' = rowmax(L1_`v' L2_`v' L3_`v' L4_`v' L5_`v')
}



foreach v in state_clear state_thin {

    capture drop L1_`v' L2_`v' L3_`v' L4_`v' ma3_`v'

    gen L1_`v' = L1.`v'
    gen L2_`v' = L2.`v'
    gen L3_`v' = L3.`v'
	gen L4_`v' =L4.`v'
    gen L5_`v' = L5.`v'

	gen ma3_`v' = L1_`v'+ L2_`v'+ L3_`v'
	gen ma4_`v' = L1_`v'+ L2_`v'+ L3_`v'+ L4_`v'
	gen ma5_`v' = L1_`v'+ L2_`v'+ L3_`v'+ L4_`v'+ L5_`v'
}



label variable ma5_major_n_clearcut_srs "Harvest"
label variable ma3_major_n_clearcut_srs "Harvest"
label variable ma4_major_n_clearcut_srs "Harvest"

label variable ma5_major_n_thin_srs "Thinning"
label variable ma3_major_n_thin_srs "Thinning"
label variable ma4_major_n_thin_srs "Thinning"


*****************************************************
* Harvest IV: 3-, 4-, and 5-year windows
*****************************************************

eststo cc_iv3: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_clearcut_srs = ma3_state_clear) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) cluster(county_eco)

capture estadd scalar secF = e(widstat)
estadd local PlotFE "`mark'"
estadd local YearFE "`mark'"
estadd local ForestGroupFE "`mark'"
estadd local EcoFE "`mark'"


eststo cc_iv4: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma4_major_n_clearcut_srs = ma4_state_clear) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) cluster(county_eco)

capture estadd scalar secF = e(widstat)
estadd local PlotFE "`mark'"
estadd local YearFE "`mark'"
estadd local ForestGroupFE "`mark'"
estadd local EcoFE "`mark'"


eststo cc_iv5: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma5_major_n_clearcut_srs = ma5_state_clear) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) cluster(county_eco)

capture estadd scalar secF = e(widstat)
estadd local PlotFE "`mark'"
estadd local YearFE "`mark'"
estadd local ForestGroupFE "`mark'"
estadd local EcoFE "`mark'"


*****************************************************
* Thinning IV: 2-, 3-, and 4-year windows
*****************************************************

eststo thin_iv3: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_thin_srs = ma3_state_thin) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) cluster(county_eco)

capture estadd scalar secF = e(widstat)
estadd local PlotFE "`mark'"
estadd local YearFE "`mark'"
estadd local ForestGroupFE "`mark'"
estadd local EcoFE "`mark'"


eststo thin_iv4: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma4_major_n_thin_srs = ma4_state_thin) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) cluster(county_eco)

capture estadd scalar secF = e(widstat)
estadd local PlotFE "`mark'"
estadd local YearFE "`mark'"
estadd local ForestGroupFE "`mark'"
estadd local EcoFE "`mark'"


eststo thin_iv5: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma5_major_n_thin_srs = ma5_state_thin) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) cluster(county_eco)

capture estadd scalar secF = e(widstat)
estadd local PlotFE "`mark'"
estadd local YearFE "`mark'"
estadd local ForestGroupFE "`mark'"
estadd local EcoFE "`mark'"


*****************************************************
* Joint IV: 2-, 3-, and 4-year windows
*****************************************************

eststo joint_iv3: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_clearcut_srs ma3_major_n_thin_srs = ///
        ma3_state_clear ma3_state_thin) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) cluster(county_eco)

capture estadd scalar secF = e(widstat)
estadd local PlotFE "`mark'"
estadd local YearFE "`mark'"
estadd local ForestGroupFE "`mark'"
estadd local EcoFE "`mark'"


eststo joint_iv4: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma4_major_n_clearcut_srs ma4_major_n_thin_srs = ///
        ma4_state_clear ma4_state_thin) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) cluster(county_eco)

capture estadd scalar secF = e(widstat)
estadd local PlotFE "`mark'"
estadd local YearFE "`mark'"
estadd local ForestGroupFE "`mark'"
estadd local EcoFE "`mark'"


eststo joint_iv5: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma5_major_n_clearcut_srs ma5_major_n_thin_srs = ///
        ma5_state_clear ma5_state_thin) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) cluster(county_eco)

capture estadd scalar secF = e(widstat)
estadd local PlotFE "`mark'"
estadd local YearFE "`mark'"
estadd local ForestGroupFE "`mark'"
estadd local EcoFE "`mark'"


*****************************************************
* Export LaTeX robustness table
*****************************************************

esttab cc_iv3 cc_iv4 cc_iv5 ///
       thin_iv3 thin_iv4 thin_iv5 ///
       joint_iv3 joint_iv4 joint_iv5 ///
    using "Table_A1_plot_window_345.tex", replace ///
    fragment booktabs se label ///
    noconstant nomtitles nonumber compress ///
    b(%9.4f) se(%9.4f) ///
    star(* .1 ** .05 *** .01) ///
    prehead("\begin{tabular}{lccccccccc}" ///
            "\toprule" ///
            "& \multicolumn{3}{c}{Harvest} & \multicolumn{3}{c}{Thinning} & \multicolumn{3}{c}{Joint IV} \\" ///
            "\cmidrule(lr){2-4}\cmidrule(lr){5-7}\cmidrule(lr){8-10}") ///
    posthead("& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) & (9) \\" ///
             "& Wildfire event & Wildfire event & Wildfire event & Wildfire event & Wildfire event & Wildfire event & Wildfire event & Wildfire event & Wildfire event \\" ///
             "\midrule") ///
    keep(ma3_major_n_clearcut_srs ma4_major_n_clearcut_srs ///
         ma5_major_n_clearcut_srs ///
         ma3_major_n_thin_srs ma4_major_n_thin_srs ///
         ma5_major_n_thin_srs) ///
    order(ma3_major_n_clearcut_srs ma4_major_n_clearcut_srs ///
         ma5_major_n_clearcut_srs ///
         ma3_major_n_thin_srs ma4_major_n_thin_srs ///
         ma5_major_n_thin_srs) ///
    coeflabels( ///
        ma3_major_n_clearcut_srs "Harvest (3-year)" ///
        ma4_major_n_clearcut_srs "Harvest (4-year)" ///
        ma5_major_n_clearcut_srs "Harvest (5-year)" ///
        ma3_major_n_thin_srs "Thinning (3-year)" ///
        ma4_major_n_thin_srs "Thinning (4-year)" ///
        ma5_major_n_thin_srs "Thinning (5-year)" ///
    ) ///
    stats(secF N PlotFE YearFE ForestGroupFE EcoFE, ///
        labels("Kleibergen-Paap Wald F statistic" ///
               "Observations" ///
               "Plot FE" ///
               "Year FE" ///
               "Species group FE" ///
               "Ecoregion FE") ///
        fmt(%9.2f %9.0fc %s %s %s %s)) ///
    postfoot("\bottomrule" ///
             "\end{tabular}")

restore





*****************************************************
* 4-year IV robustness: alternative instruments
*****************************************************

preserve
eststo clear

drop if south_state != 1
xtset plt_id year

*****************************************************
* Generate 3-year treatment variables
*****************************************************

foreach v in major_n_clearcut_srs major_n_thin_srs {

    capture drop L1_`v' L2_`v' L3_`v' L4_`v' ma3_`v'

    gen L1_`v' = L1.`v'
    gen L2_`v' = L2.`v'
    gen L3_`v' = L3.`v'
	gen L4_`v' = L4.`v'
	

    egen ma3_`v' = rowmax(L1_`v' L2_`v' L3_`v' L4_`v')
}


foreach v in iv_saw state_clear state_thin se_clear se_thin c_clear c_thin {

    capture drop L1_`v' L2_`v' L3_`v' L4_`v' ma3_`v'

    gen L1_`v' = L1.`v'
    gen L2_`v' = L2.`v'
    gen L3_`v' = L3.`v'
	gen L4_`v' =L4.`v'
	
	gen ma3_`v' = L1_`v'+ L2_`v'+ L3_`v'+ L4_`v'
}


*****************************************************
* Harvest IV models: alternative instruments
*****************************************************

* (1) Sawtimber-price IV
eststo cc_iv_saw: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_clearcut_srs = ma3_iv_saw) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) ///
    cluster(county_eco) endog(ma3_major_n_clearcut_srs)

capture estadd scalar firstF = e(cdf)
capture estadd scalar secF   = e(widstat)


* (2) State-level shift-share IV
eststo cc_iv_state: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_clearcut_srs = ma3_state_clear) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) ///
    cluster(county_eco) endog(ma3_major_n_clearcut_srs)

capture estadd scalar firstF = e(cdf)
capture estadd scalar secF   = e(widstat)


* (3) State-ecoregion shift-share IV
eststo cc_iv_se: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_clearcut_srs = ma3_se_clear) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) ///
    cluster(county_eco) endog(ma3_major_n_clearcut_srs)

capture estadd scalar firstF = e(cdf)
capture estadd scalar secF   = e(widstat)


* (4) County-ecoregion shift-share IV
eststo cc_iv_c: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_clearcut_srs = ma3_c_clear) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) ///
    cluster(county_eco) endog(ma3_major_n_clearcut_srs)

capture estadd scalar firstF = e(cdf)
capture estadd scalar secF   = e(widstat)


*****************************************************
* Thinning IV models: alternative instruments
*****************************************************

* (5) Sawtimber-price IV
eststo thin_iv_saw: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_thin_srs = ma3_iv_saw) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) ///
    cluster(county_eco) endog(ma3_major_n_thin_srs)

capture estadd scalar firstF = e(cdf)
capture estadd scalar secF   = e(widstat)


* (6) State-level shift-share IV
eststo thin_iv_state: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_thin_srs = ma3_state_thin) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) ///
    cluster(county_eco) endog(ma3_major_n_thin_srs)

capture estadd scalar firstF = e(cdf)
capture estadd scalar secF   = e(widstat)


* (7) State-ecoregion shift-share IV
eststo thin_iv_se: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_thin_srs = ma3_se_thin) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) ///
    cluster(county_eco) endog(ma3_major_n_thin_srs)

capture estadd scalar firstF = e(cdf)
capture estadd scalar secF   = e(widstat)


* (8) County-ecoregion shift-share IV
eststo thin_iv_c: ivreghdfe mtbs_count_10 ///
    $CONTROL ///
    (ma3_major_n_thin_srs = ma3_c_thin) ///
    [aweight = area_total_private_s] if year >= 2000, ///
    absorb(plt_id year SPGR l3eco) ///
    cluster(county_eco) endog(ma3_major_n_thin_srs)

capture estadd scalar firstF = e(cdf)
capture estadd scalar secF   = e(widstat)


*****************************************************
* Add fixed-effect indicators
*****************************************************

foreach m in cc_iv_saw cc_iv_state cc_iv_se cc_iv_c ///
             thin_iv_saw thin_iv_state thin_iv_se thin_iv_c {

    est restore `m'
    estadd local PlotFE "\checkmark"
    estadd local YearFE "\checkmark"
    estadd local ForestGroupFE "\checkmark"
    estadd local EcoFE "\checkmark"
}

*****************************************************
* Export table
*****************************************************

esttab cc_iv_state cc_iv_se cc_iv_c ///
       thin_iv_state thin_iv_se thin_iv_c ///
       using "table_ma4_alt_iv.tex", replace ///
       keep(ma3_major_n_clearcut_srs ma3_major_n_thin_srs) ///
       order(ma3_major_n_clearcut_srs ma3_major_n_thin_srs) ///
	   coeflabels( ///
			ma3_major_n_clearcut_srs "Harvest" ///
			ma3_major_n_thin_srs "Thinning" ///
		) ///
       label se star(* 0.10 ** 0.05 *** 0.01) ///
       b(%9.4f) se(%9.4f) ///
       stats( secF N PlotFE YearFE ForestGroupFE EcoFE, ///
             labels("Kleibergen-Paap Wald F statistic" ///
                    "Observations" ///
                    "Plot FE" ///
                    "Year FE" ///
                    "Species group FE" ///
                    "Ecoregion FE") ///
             fmt(%9.2f %9.0fc %s %s %s %s)) ///
       mtitles("State IV" "State-Eco IV" "County IV" ///
              "State IV" "State-Eco IV" "County IV") ///
       mgroups("Harvest" "Thinning", pattern(1 0 0 1 0 0) ///
               prefix(\multicolumn{@span}{c}{) suffix(}) ///
               span erepeat(\cmidrule(lr){@span})) ///
       booktabs compress

restore