global path "C:\Users\mmlin\OneDrive - Ecole Polytechnique\Desktop\Paris\Académico\Polytechnique\Courses\Deuxième année\Economics of migration\Project\report\replication"

version 15.1
set more off 
clear 

*import delimited "$path/data/data_hate_crime.csv" , clear
*save "$path/data/data_hate_crime.dta"
*use "$path/data/cross_section_trump_twitter.dta", clear
use "$path/data/data_hate_crime.dta" , clear


**************************************************************************
* SET-UP
**************************************************************************
program main
    * *** Add required packages from SSC to this list ***
    local ssc_packages "erepost reghdfe boottest weakivtest grstyle missings ppmlhdfe palettes colrspace gtools geodist binscatter estout ivreghdfe ivreg2 ranktest ftools twostepweakiv avar moremata acreg shp2dta spmap mif2dta coefplot"
    * *** Add required packages from SSC to this list ***

    if !missing("`ssc_packages'") {
        foreach pkg in `ssc_packages' {
        * install using ssc, but avoid re-installing if already present
            capture which `pkg'
            if _rc == 111 {                 
               dis "Installing `pkg'"
               quietly ssc install `pkg', replace
               }
        }
    }


end

main
*net install st0171_1.pkg // Installs rivtest 

/*==============================================================================================*/
/* after installing all packages, it may be necessary to issue the mata mlib index command */
/* This should always be the LAST command after installing all packages                    */

	mata: mata mlib index
**************************************************************************
* DEFINE GLOBALS
**************************************************************************
* Define globals
glo fe2 "i.state_code i.pop_deciles"
glo cons_empty_ln     	""
glo cons_empty_d      	""

glo dist   pop_density lnarea  
glo vote   vote_share_rep2012
glo demo7  share_20_24 share_25_29 share_30_34 share_35_39 share_40_44 share_45_49 share_50_over pop_gr
glo ethn   white_pop black_pop native_pop asian_pop hispanic_pop muslim_share
glo econed census_poverty bls_unemployment_rate census_o25_highschool census_o25_graduate 
glo crim    fbi_property_crime 
glo tv     fox_news_share tv_viewership_prime_pop 


glo cont1  ""
glo cont2  "$demo7"
glo cont3  "$demo7 $dist"
glo cont4  "$demo7 $dist $ethn"
glo cont5  "$demo7 $dist $ethn $econed" 
glo cont6  "$demo7 $dist $ethn $econed $tv"
glo cont7  "$demo7 $dist $ethn $econed $tv $vote"
glo cont8  "$demo7 $dist $ethn $econed $tv $vote $crim"
glo control_lab "Demographic controls = share_25_29" "Geographical controls = pop_density" "Race and religion controls = white_pop" "Socioeconomic controls = census_poverty" "Media controls = fox_news_share " "Election control = vote_share_rep2012" "Crime controls = fbi_property_crime" 
* New control set without pop_density
* keep lnarea (county size) but drop pop_density
glo cont8_nodensity "$demo7 lnarea $ethn $econed $tv $vote $crim"


***************************************************************************************
*********** TABLE 1 - FIRST STAGE
****************************************************************************************
est clear
label var twitter_user_unique_ln "Log(Twitter users)"

foreach c in cont1 cont2 cont3 cont4 cont5 cont6 cont7 cont8 {

* FS
	eststo temp_FS_`c': qui reghdfe twitter_user_unique_ln sxsw_treat $`c' sxsw_pre if year==2017, a($fe2) cluster(state_code) 
	estadd ysumm
	qui test sxsw_treat = sxsw_pre
	estadd scalar equiv = r(p): temp_FS_`c'
	*spatialcorr
}

*** Export tables
esttab temp_FS_cont*   using "$path/Tables/FS_CS_Baseline_owndata.tex",indicate(`r(indicate_fe)'  "$control_lab", labels(Yes)) keep(sxsw_treat sxsw_pre) fragment nomtitles nonumbers  nowrap booktabs  nogaps  nolines eqlabels("") style(tex) label replace stats(N r2 ymean equiv, labels("Observations" "\$R^2\$" "Mean of DV"  "p-value: March 2007 = Pre") fmt(%9.0fc %04.3f %04.3f %04.2f))  starlevels(* 0.10 ** 0.05 *** 0.01) collabels(none)  cells(b(star fmt(%9.3f)) se(par))   varlabels(,elist(sxsw_pre \midrule ))

**** COMMENTS:
** we don't have control variables on distance to the counties, census_GiniIndex , census_share_uninsured , census_median_house_incomeln , sh_emp_agric , sh_emp_information , sh_emp_manufm sh_emp_nontra sh_emp_consre sh_emp_utilit sh_emp_busise sh_emp_othese, fbi_viol_2016_pop
** Despite this, the results are largely similar
** The first stage results are restricted to the last year in the data, to what the authors refer as 'today'


**********************************************************************************
********* TABLE 2 - 
**********************************************************************************

* outcome variable : log change in hate crimes against muslims between 2010 and 2017
* three estimation strategies: OLS, reduced form and 2sls
* list of controls similar as above 
est clear

*******************************************************************************
* Generate the outcome variable following the original paper
******************************************************************************
keep panel_id year anti_muslim_attack
sort panel_id year

* 2. Reshape to wide format: one row per entity
reshape wide anti_muslim_attack, i(panel_id) j(year)

* Post-period (2015–2017) using denominator 2.5
gen post_mean_attacks = (anti_muslim_attack2015 + anti_muslim_attack2016 + anti_muslim_attack2017)/2.5

* Pre-period (2010–2015) using denominator 5.5
gen pre_mean_attacks2010 = (anti_muslim_attack2010 + anti_muslim_attack2011 + anti_muslim_attack2012 + anti_muslim_attack2013 + anti_muslim_attack2014 + anti_muslim_attack2015)/5.5

* Log difference
gen diff_lnav_anti_moslem2010y = log(1 + post_mean_attacks) - log(1 + pre_mean_attacks2010)

reshape long anti_muslim_attack, i(panel_id) j(year)

* Keep one row per entity with the computed averages
keep panel_id post_mean_attacks pre_mean_attacks2010 diff_lnav_anti_moslem2010y
duplicates drop panel_id, force
save "$path/Temp Data/temp_avg.dta", replace
* Go back to your original long panel
use "$path/data/data_hate_crime.dta" , clear

* Merge averages back
joinby panel_id using "$path/Temp Data/temp_avg.dta"

*******************************************************************************
* Estimate regressions
******************************************************************************
keep if year==2017

foreach c in cont1 cont2 cont3 cont4 cont5 cont6 cont7 cont8 {

	* 2SLS
	eststo temp_IV_`c': qui ivreghdfe diff_lnav_anti_moslem2010y (twitter_user_unique_ln = sxsw_treat) $`c' sxsw_pre , a($fe2) cluster(state_code)  
	qui gen sample=e(sample)
	estadd ysumm
	estadd local fst = string(`e(widstat)', "%3.2f") : temp_IV_`c'

	*spatialcorr
	qui xi: twostepweakiv 2sls diff_lnav_anti_moslem2010y (twitter_user_unique_ln = sxsw_treat) $`c' sxsw_pre $fe2 , cluster(state_code)
	*get_arcset
	local ar = r(ar_p)
	estadd local ar = "$ar" : temp_IV_`c'
	

	* RF
	eststo temp_RF_`c' : qui reghdfe diff_lnav_anti_moslem2010y sxsw_treat $`c' sxsw_pre , a($fe2) cluster(state_code) 
	estadd ysumm
	*qui test twitter_user_sxsw_fol_Mar07_ln = twitter_user_sxsw_fol_pre06_ln
	*estadd scalar equiv = r(p)
	*spatialcorr
	
	* OLS
	eststo temp_OLS_`c' : qui reghdfe diff_lnav_anti_moslem2010y twitter_user_unique_ln $`c', a($fe2) cluster(state_code) 
	
	qui drop sample

}

* Lasso Variable Selection

* LASSO 2SLS
eststo temp_SS_Lasso: poivregress diff_lnav_anti_moslem2010y (twitter_user_unique_ln = sxsw_treat) , controls((sxsw_pre)  i.pop_deciles i.state_code##c.($cont8)  ) vce(cluster state_code ) selection(cv)
qui gen sample=e(sample)
estadd ysumm

* LASSO RF
eststo temp_RF_Lasso : dsregress diff_lnav_anti_moslem2010y sxsw_treat , controls((sxsw_pre)  i.pop_deciles i.state_code##c.($cont8)  ) vce(cluster state_code ) selection(cv)
estadd ysumm


* LASSO OLS
eststo temp_OLS_Lasso : dsregress diff_lnav_anti_moslem2010y twitter_user_unique_ln , controls((sxsw_pre)  i.pop_deciles i.state_code##c.($cont8)  ) vce(cluster state_code ) selection(cv)
		
qui drop sample

*******************************************************************************
* Export tables 
******************************************************************************
esttab temp_IV_cont* temp_SS_Lasso using "$path/Tables/table2_IV_owndata.tex", indicate(`r(indicate_fe)' "$control_lab", labels(Yes )) keep(twitter_user_unique_ln sxsw_pre) fragment nomtitles nonumbers nowrap booktabs nogaps nolines eqlabels("") style(tex) label replace stats(N ymean fst ar, labels("Observations" "Mean of DV" "Robust F-stat." "Weak IV 95\% AR confidence set") fmt(%9.0fc %04.3f %04.2f %04.3f) ) starlevels(* 0.10 ** 0.05 *** 0.01) collabels(none) cells(b(star fmt(%9.3f)) se(par)) varlabels(,elist(sxsw_pre \midrule ))

esttab temp_RF_cont* temp_RF_Lasso using "$path/Tables/table2_RF_owndata.tex", keep(sxsw_treat) fragment nomtitles nonumbers  nowrap booktabs  nogaps  nolines eqlabels("") style(tex) label replace noobs starlevels(* 0.10 ** 0.05 *** 0.01) collabels(none)  cells(b(star fmt(%9.3f)) se(par)) 


esttab  temp_OLS_cont* temp_OLS_Lasso using "$path/Tables/table2_OLS_owndata.tex", keep(twitter_user_unique_ln) fragment nomtitles nonumbers  nowrap booktabs  nogaps  nolines eqlabels("") style(tex) label replace noobs starlevels(* 0.10 ** 0.05 *** 0.01) collabels(none)  cells(b(star fmt(%9.3f)) se(par)) 


**********************************************************************************
********* TABLE 3 - OTHER HATE CRIMES
**********************************************************************************
*******************************************************************************
* Generate the outcome variable following the original paper

******************************************************************************
use "$path/data/data_hate_crime.dta" , clear
keep panel_id year hate_crime_all
sort panel_id year

* 2. Reshape to wide format: one row per entity
reshape wide hate_crime_all, i(panel_id) j(year)

* Post-period (2015–2017) using denominator 2.5
gen post_mean_attacks = (hate_crime_all2015 + hate_crime_all2016 + hate_crime_all2017)/2.5

* Pre-period (2010–2015) using denominator 5.5
gen pre_mean_attacks2010 = (hate_crime_all2010 + hate_crime_all2011 + hate_crime_all2012 + hate_crime_all2013 + hate_crime_all2014 + hate_crime_all2015)/5.5

* Log difference
gen diff_lnav_hate_crime_all2010y = log(1 + post_mean_attacks) - log(1 + pre_mean_attacks2010)

reshape long hate_crime_all, i(panel_id) j(year)

* Keep one row per entity with the computed averages
keep panel_id post_mean_attacks pre_mean_attacks2010 diff_lnav_hate_crime_all2010y
duplicates drop panel_id, force
save "$path/Temp Data/temp_avg_allcrimes.dta", replace
* Go back to your original long panel
use "$path/data/data_hate_crime.dta" , clear

* Merge averages back
joinby panel_id using "$path/Temp Data/temp_avg_allcrimes.dta"

*******************************************************************************
* Estimate regressions
******************************************************************************
keep if year==2017

foreach c in cont1 cont2 cont3 cont4 cont5 cont6 cont7 cont8 {

	* 2SLS
	eststo temp_IVall_`c': qui ivreghdfe diff_lnav_hate_crime_all2010y (twitter_user_unique_ln = sxsw_treat) $`c' sxsw_pre , a($fe2) cluster(state_code)  
	qui gen sample=e(sample)
	estadd ysumm
	estadd local fst = string(`e(widstat)', "%3.2f") : temp_IVall_`c'

	*spatialcorr
	qui xi: twostepweakiv 2sls diff_lnav_hate_crime_all2010y (twitter_user_unique_ln = sxsw_treat) $`c' sxsw_pre $fe2 , cluster(state_code)
	*get_arcset
	local ar = r(ar_p)
	estadd local ar = "$ar" : temp_IVall_`c'
	

	* RF
	eststo temp_RFall_`c' : qui reghdfe diff_lnav_hate_crime_all2010y sxsw_treat $`c' sxsw_pre , a($fe2) cluster(state_code) 
	estadd ysumm
	*qui test twitter_user_sxsw_fol_Mar07_ln = twitter_user_sxsw_fol_pre06_ln
	*estadd scalar equiv = r(p)
	*spatialcorr
	
	* OLS
	eststo temp_OLSall_`c' : qui reghdfe diff_lnav_hate_crime_all2010y twitter_user_unique_ln $`c', a($fe2) cluster(state_code) 
	
	qui drop sample

}

* Lasso Variable Selection

* LASSO 2SLS
eststo temp_SSall_Lasso: poivregress diff_lnav_hate_crime_all2010y (twitter_user_unique_ln = sxsw_treat) , controls((sxsw_pre)  i.pop_deciles i.state_code##c.($cont8)  ) vce(cluster state_code ) selection(cv)
qui gen sample=e(sample)
estadd ysumm

* LASSO RF
eststo temp_RFall_Lasso : dsregress diff_lnav_hate_crime_all2010y sxsw_treat , controls((sxsw_pre)  i.pop_deciles i.state_code##c.($cont8)  ) vce(cluster state_code ) selection(cv)
estadd ysumm


* LASSO OLS
eststo temp_OLSall_Lasso : dsregress diff_lnav_hate_crime_all2010y twitter_user_unique_ln , controls((sxsw_pre)  i.pop_deciles i.state_code##c.($cont8)  ) vce(cluster state_code ) selection(cv)
		
qui drop sample

*******************************************************************************
* Export tables 
******************************************************************************
esttab temp_IVall_cont* temp_SSall_Lasso using "$path/Tables/table2_IV_owndata_all.tex", indicate(`r(indicate_fe)' "$control_lab", labels(Yes )) keep(twitter_user_unique_ln sxsw_pre) fragment nomtitles nonumbers nowrap booktabs nogaps nolines eqlabels("") style(tex) label replace stats(N ymean fst ar, labels("Observations" "Mean of DV" "Robust F-stat." "Weak IV 95\% AR confidence set") fmt(%9.0fc %04.3f %04.2f %04.3f) ) starlevels(* 0.10 ** 0.05 *** 0.01) collabels(none) cells(b(star fmt(%9.3f)) se(par)) varlabels(,elist(sxsw_pre \midrule ))

esttab temp_RFall_cont* temp_RFall_Lasso using "$path/Tables/table2_RF_owndata_all.tex", keep(sxsw_treat) fragment nomtitles nonumbers  nowrap booktabs  nogaps  nolines eqlabels("") style(tex) label replace noobs starlevels(* 0.10 ** 0.05 *** 0.01) collabels(none)  cells(b(star fmt(%9.3f)) se(par)) 


esttab  temp_OLSall_cont* temp_OLSall_Lasso using "$path/Tables/table2_OLS_owndata_all.tex", keep(twitter_user_unique_ln) fragment nomtitles nonumbers  nowrap booktabs  nogaps  nolines eqlabels("") style(tex) label replace noobs starlevels(* 0.10 ** 0.05 *** 0.01) collabels(none)  cells(b(star fmt(%9.3f)) se(par)) 







**********************************************************************************
******** MAPS REPRODUCTION
**********************************************************************************

***********************************************************
* (1) Prepare Map Data from Shape File
***********************************************************

*import delimited "$path/data/data_hate_crime.csv" , clear
*save "$path/data/data_hate_crime.dta"

* recover unique county identifier from the authors cross-sectional dataset 
use "$path/data/cross_section_trump_twitter.dta", clear 
tostring state_code, gen(state_str) format(%02.0f)
tostring FIPS_county_code, gen(county_str) format(%03.0f)
gen county_fips = state_str + county_str
destring county_fips, replace
save "$path/Temp Data/temp_ident.dta", replace

* Add it to our data
use "$path/data/data_hate_crime.dta" , clear
joinby panel_id using "$path/Temp Data/temp_ident.dta" 

egen total_attacks = total(hate_crime_all), by(county_fips)
gen twitter_users = exp(twitter_user_unique_ln) - 1
egen total_twitter_users=total(twitter_users), by(county_fips)
keep panel_id FIPS_county_code state_code county_fips total_attacks total_twitter_users
destring state_code county_fips, replace
duplicates drop county_fips , force
save "$path/Temp Data/temp_maps.dta", replace 
* 3108 observations


* Plot our data using authors shape file
* shape file with lakes excluded used to produce maps
shp2dta using "$path/data/tl_2017_us_county.shp", data("$path/Temp Data/us_county_shape_data_alt.dta") coordinates("$path/Temp Data/us_county_coordinate_data_alt.dta") genid(id) replace  



* load county shape data
use "$path/Temp Data/us_county_shape_data_alt.dta" ,clear
rename STATEFP state_str
rename COUNTYFP county_str
gen county_fips = state_str + county_str
destring county_fips, replace

*destring state_code county_code, replace

* 3,233 observations

***********************************************************
* (2) Merge with Data Files based on FIPS County Codes
***********************************************************


* merge to shape file to get hate crime variables and Twitter usage
merge 1:1 county_fips using "$path/Temp Data/temp_maps.dta" , keepusing( state_code panel_id county_fips total_attacks total_twitter_users) nogenerate



* recover population in 2016 to build shares
merge 1:1 county_fips using "$path/Temp Data/temp_ident.dta" , keepusing( pop2016) 

gen total_twitter_users_scal = total_twitter_users/pop2016
gen total_attacks_scal = total_attacks/pop2016

replace total_attacks_scal=. if missing(total_attacks)

*Drop territories

drop if state_str == "02" | state_str == "60" | state_str == "66" | state_str == "15" | state_str == "69" | state_str == "72" | state_str == "78"

***********************************************************
* (1)Produce Maps
***********************************************************


*** 1a) All Hate Crime 
spmap total_attacks_scal using "$path/Temp Data/us_county_coordinate_data_alt.dta", id(id) mopattern(dash) clmethod(quantile) clnumber(5) legenda(on)  fcolor(Blues2) ndfcolor(gs10)  legorder(lohi) mosize(vvthin)  ndsize(vvthin)  osize(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)  legstyle(3) legend(  label(6 "5th Quintile")  label(5 "4th Quintile") label(4 "3rd Quintile") label(3 "2nd Quintile") label(2 "1st Quintile") label(1 "No Reports")  size(medlarge))
graph export "$path/Graphs/map_hate_crime_pc_owndata.png" , replace width(4000)
graph export "$path/Graphs/map_hate_crime_pc_owndata.pdf" , replace 



*** 1b) Twitter Users Gesis 
spmap total_twitter_users_scal using "$path/Temp Data/us_county_coordinate_data_alt.dta", id(id) mopattern(dash) clmethod(quantile) clnumber(5) legenda(on)  fcolor(Blues2) legorder(lohi) mosize(vvthin)  ndsize(vvthin)  osize(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)  legstyle(3) legend( label(1 "No Reports") label(6 "5th Quintile")  label(5 "4th Quintile") label(4 "3rd Quintile") label(3 "2nd Quintile") label(2 "1st Quintile")  size(medlarge))
graph export "$path/Graphs/map_twitter_user_pc_owndata.png" , replace width(4000)
graph export "$path/Graphs/map_twitter_user_pc_owndata.pdf" , replace 


**********************************************************************************************************************************
* FIGURE 4
*************************************************************************************************

***********************************************************
**# (0) Read in main data and variable construction
***********************************************************

use "$path/data/cross_section_trump_twitter.dta", clear
keep panel_id pop2014
save "$path/Temp Data/pop2014.dta", replace
use "$path/data/data_hate_crime.dta" , clear



bysort panel_id: egen temp = total(hate_crime_all) if year<2015 
bysort panel_id: egen hate_crime_all_pre = min(temp)
drop temp

joinby panel_id using "$path/Temp Data/pop2014.dta"
replace hate_crime_all_pre = hate_crime_all_pre/pop2014

sum hate_crime_all_pre,d
gen hate_crime_all_pre_50 = hate_crime_all_pre >= `r(p50)' if hate_crime_all_pre!=.




***********************************************************
**#** 2) Produce Event Study Graphs (OLS and Reduced Form)
***********************************************************
*sxsw_neighbor missing
*splc_num_groups missing

******* 2a) OLS
preserve

	gcollapse (sum) anti_muslim_attack (first) state_code twitter_user_unique_ln sxsw_treat sxsw_pre pop_deciles  hate_crime_all_pre_50 $cont8, by(panel_id year)

	*** define yearly dummies for Twitter Usage
	forvalues i=2010/2017{
		gen twitter_user_ln_`i' = twitter_user_unique_ln if year==`i'
		replace twitter_user_ln_`i'= 0 if twitter_user_ln_`i'==.
	}

	*** Loop over hate crime and Tweet Variables and produce event studies 
	foreach var of varlist anti_muslim_attack {

	
		display as error "`var'"



		local filename = subinstr("`var'" , "anti_" , "", .)
		local filename = subinstr("`filename'" , "_attack" , "", .)
		local filename = subinstr("`filename'" , "_no_mu" , "", .)
		local filename = subinstr("`filename'" , "twitter_" , "", .)
		local filename = subinstr("`filename'" , "hate_crime_" , "", .)

		
		local post_label = ""


		
		if "`var'" =="hate_crime_all"{
			local type_label=""
		}		
		else if "`var'" =="anti_muslim_attack"{
			local type_label="anti-Muslim "
		}


	
		
		
		reghdfe `var' twitter_user_ln_2010-twitter_user_ln_2013 o.twitter_user_ln_2014 twitter_user_ln_2015 twitter_user_ln_2016 twitter_user_ln_2017 if  year>=2010, a(panel_id year ) vce(cluster panel_id) noconst
		

		coefplot , scheme(small) vertical yline(0) xline(5.5, lcolor(dkorange)) omitted baselevels     rename( "twitter_user_ln_" = ""   ,  regex) lcolor(navy)  mcolor(navy)    recast(connected)  ciopts(recast(rarea) color(navy%30)  lwidth(none) ) ytitle("") subtitle("Diff-in-diff estimate of Twitter on `type_label'hate crime`post_label'", margin(bottom) pos(11)) ylabel(-0.02(0.02)0.06, format(%9.2f) angle(horizontal) )   

		
		
		graph export "$path/Graphs/pretrend_graph_`filename'_yearly.png" , replace width(1920)
		graph export "$path/Graphs/pretrend_graph_`filename'_yearly.pdf" , replace
		* apply log transformation
		replace `var' = ln(1+ `var')
		

		reghdfe `var' twitter_user_ln_2010-twitter_user_ln_2013 o.twitter_user_ln_2014 twitter_user_ln_2015 twitter_user_ln_2016 twitter_user_ln_2017 if  year>=2010, a(panel_id year pop_deciles#year ) vce(cluster panel_id) noconst

		coefplot , scheme(small) vertical yline(0) xline(5.5, lcolor(dkorange)) omitted baselevels     rename( "twitter_user_ln_" = ""   ,  regex) lcolor(navy)  mcolor(navy)    recast(connected)  ciopts(recast(rarea) color(navy%30)  lwidth(none) ) ytitle("") subtitle("Diff-in-diff estimate of Twitter on `type_label'hate crime`post_label'", margin(bottom) pos(11)) ylabel(-0.02(0.02)0.06, format(%9.2f) angle(horizontal) )   

		
		
		graph export "$path/Graphs/pretrend_graph_log_`filename'_yearly.png" , replace width(1920)
		graph export "$path/Graphs/pretrend_graph_log_`filename'_yearly.pdf" , replace

		
		
		reghdfe `var' twitter_user_ln_2010-twitter_user_ln_2013 o.twitter_user_ln_2014 twitter_user_ln_2015 twitter_user_ln_2016 twitter_user_ln_2017 if  year>=2010, a(panel_id year pop_deciles#year c.($cont8)#year) vce(cluster panel_id) noconst
		

		coefplot , scheme(small) vertical yline(0) xline(5.5, lcolor(dkorange)) omitted baselevels     rename( "twitter_user_ln_" = ""   ,  regex) lcolor(navy)  mcolor(navy)    recast(connected)  ciopts(recast(rarea) color(navy%30)  lwidth(none) ) ytitle("") subtitle("Diff-in-diff estimate of Twitter on `type_label'hate crime`post_label'", margin(bottom) pos(11)) ylabel(-0.02(0.02)0.06, format(%9.2f) angle(horizontal) )   

		
		
		graph export "$path/Graphs/pretrend_graph_log_`filename'_yearly_cont.png" , replace width(1920)
		graph export "$path/Graphs/pretrend_graph_log_`filename'_yearly_cont.pdf" , replace
		
		
}


**********************************************************************************
********* EXTENSION - HETEROGENEITY BY URBANICITY (POPULATION DENSITY)
* Idea: Social media dynamics may differ between urban and rural counties.
* Urban areas have denser social networks; rural areas may be more isolated
* but potentially more susceptible to online radicalization signals.
**********************************************************************************

use "$path/data/data_hate_crime.dta", clear

*******************************************************************************
* Reconstruct outcome variable
*******************************************************************************
keep panel_id year anti_muslim_attack
sort panel_id year
reshape wide anti_muslim_attack, i(panel_id) j(year)

gen post_mean_attacks = (anti_muslim_attack2015 + anti_muslim_attack2016 + anti_muslim_attack2017)/2.5
gen pre_mean_attacks2010 = (anti_muslim_attack2010 + anti_muslim_attack2011 + ///
    anti_muslim_attack2012 + anti_muslim_attack2013 + ///
    anti_muslim_attack2014 + anti_muslim_attack2015)/5.5
gen diff_lnav_anti_moslem2010y = log(1 + post_mean_attacks) - log(1 + pre_mean_attacks2010)

reshape long anti_muslim_attack, i(panel_id) j(year)
keep panel_id post_mean_attacks pre_mean_attacks2010 diff_lnav_anti_moslem2010y
duplicates drop panel_id, force
save "$path/Temp Data/temp_avg_ext.dta", replace

use "$path/data/data_hate_crime.dta", clear
joinby panel_id using "$path/Temp Data/temp_avg_ext.dta"
keep if year == 2017

*******************************************************************************
* Create terciles of population density for richer heterogeneity picture
*******************************************************************************
xtile density_tercile = pop_density, nq(3)
label define dens_lbl 1 "Rural (First Tercile)" 2 "Suburban (Second Tercile)" 3 "Urban (Third Tercile)"
label values density_tercile dens_lbl

*******************************************************************************
* Estimate 2SLS by density group (terciles split)
*******************************************************************************
est clear



* Loop through each Tercile (1=Rural, 2=Suburban, 3=Urban)
forvalues t = 1/3 {
    local lab = cond(`t'==1, "Rural", cond(`t'==2, "Suburban", "Urban"))
    
    * 1. OLS
    eststo OLS_terc_`t': qui reghdfe diff_lnav_anti_moslem2010y ///
        twitter_user_unique_ln $cont8_nodensity ///
        if density_tercile == `t', a($fe2) cluster(state_code)
    estadd ysumm
    estadd local group "`lab'": OLS_terc_`t'
    
    * 2. Reduced Form (RF)
    eststo RF_terc_`t': qui reghdfe diff_lnav_anti_moslem2010y ///
        sxsw_treat $cont8_nodensity sxsw_pre ///
        if density_tercile == `t', a($fe2) cluster(state_code)
    estadd ysumm
    estadd local group "`lab'": RF_terc_`t'
    
    * 3. 2SLS (IV)
    eststo IV_terc_`t': qui ivreghdfe diff_lnav_anti_moslem2010y ///
        (twitter_user_unique_ln = sxsw_treat) $cont8_nodensity sxsw_pre ///
        if density_tercile == `t', a($fe2) cluster(state_code)
    estadd ysumm
    * Extract Kleibergen-Paap rk Wald F statistic for the table
    estadd local fst = string(`e(widstat)', "%3.2f"): IV_terc_`t'
    estadd local group "`lab'": IV_terc_`t'
}

*******************************************************************************
* Export Table 
*******************************************************************************
esttab OLS_terc_* RF_terc_* IV_terc_* ///
    using "$path/Tables/ext_heterogeneity_density.tex", ///
    keep(twitter_user_unique_ln sxsw_treat sxsw_pre) ///
    mgroups("OLS" "Reduced Form" "2SLS", ///
        pattern(1 0 0 1 0 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
    mtitles("Rural" "Suburban" "Urban" "Rural" "Suburban" "Urban" "Rural" "Suburban" "Urban") ///
    fragment nowrap booktabs nogaps nolines style(tex) label replace ///
    stats(N ymean fst, ///
        labels("Observations" "Mean of DV" "Robust F-stat.") ///
        fmt(%9.0fc %04.3f %04.2f)) ///
    starlevels(* 0.10 ** 0.05 *** 0.01) collabels(none) ///
    cells(b(star fmt(%9.3f)) se(par)) ///
    addnote("Counties split into population density terciles (T1 < 25.1; T2 25.1-79; T3 > 79)." ///
            "All specifications include full set of controls ($cont8)." ///
            "Standard errors clustered at state level.")
			


