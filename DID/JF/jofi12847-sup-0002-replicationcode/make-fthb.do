*******************************************************************************
*   Title: make-fthb.do
* Purpose: Produces the many data sets for the final published version. See the
*          main() program at the bottom for organization.
*   Owner: Eric Zwick
* Authors: David Berger, Nicholas Turner, Eric Zwick
*     RAs: Tom (Tianfang) Cui, Caleb Wroblewski
*    Date: 2019-02-01
*******************************************************************************

*******************************************************************************
* Helpers
*******************************************************************************
capture program drop seasonalize /* %< */
program define seasonalize, byable(onecall) sortpreserve
    syntax varname [if], datevar(varname) [period(integer 12)]

    marksample touse, novarlist
    tempvar s1 s2 s3

    sort `touse' `_byvars'
    by `touse' `_byvars': egen `s1' = mean(`varlist') `if'
    by `touse' `_byvars': egen `s2' = max(`s1')
    capture confirm var month
    if _rc {
        gen month = mod(`datevar',`period') + 1
    }
    bys `touse' `_byvars' month: egen `s3' = mean(`varlist')
    gen `varlist'_sa = `varlist' - `s3' + `s2'
    rename `varlist' `varlist'_nsa
    sort `touse' `_byvars' `datevar'

end /* %> */

capture program drop filter_data /*%<*/
program define filter_data
    syntax [if/]

    display "pre-filter count"
    display "---------------"
    count
    if "`if'" != "" {
        keep if `if'
    }
    display "post-filter count"
    display "---------------"
    count
end/*%>*/

*******************************************************************************
* Small data sets
*******************************************************************************
capture program drop build_nar/*%<*/
program define build_nar
    /* !!! This no longer works b/c of changes to FRED 
	Data Availability. NAR fail.                       !!!*/
	/* 
    display "BUILD nar"

    tempfile sales
    clear
    freduse EXHOSLUSM495N EXHOSLUSM495S
    replace EXH*S = EXHOSLUSM495S/1000
    gen mdate = mofd(daten)
    save `sales'

    clear
    freduse HOSINVUSM495N
    rename H*N inventory
    gen mdate = mofd(daten)
    gen month = month(dofm(mdate))
    egen m = mean(inventory)
    bys month: egen m_month = mean(inventory)
    gen inventory_sa = inventory - m_month + m
    sort mdate
    format mdate %tm
    
    label var mdate "Year/Month"
    label var inventory "Unadjusted"
    label var inventory_sa "Seasonally adjusted"
    replace inventory = inventory/1000
    replace inventory_sa = inventory_sa/1000

    keep inventory inventory_sa mdate
    merge 1:1 mdate using `sales', keep(1 3) nogen

    format mdate %tm
    save $analysis/nar_sales.dta, replace */

end/*%>*/

capture program drop build_google/*%<*/
program define build_google

    import delimited using $rawdatadir/GoogleSearch.csv, delimiter(",") clear
    gen mdate = date(substr(week, 1, 10), "YMD")
    keep term1 term2 total mdate
    label var term1 "first time home buyer"
    label var term2 "home buyer credit"
    save $analysis/google.dta, replace

end/*%>*/

capture program drop build_claims/*%<*/
program define build_claims

    /* These data come from the IRS internal environment */
    insheet using $rawdatadir/natl_claims.csv, comma names clear
    gen mdate = ym(year, month)
    format mdate %tm
    keep if total_claims > 500 

    save "$analysis/natl_claims.dta", replace

end/*%>*/

capture program drop build_age/*%<*/
program define build_age

    tempfile age_counts orig claims temp
    /* All these data come from IRS internal environment */
    * Develop FTHB take-up data.
    insheet using "$rawdatadir/AGE_COUNTS.csv", comma clear	
    keep if age_primary < 60
    save `age_counts'

    collapse (sum) count, by(tax_yr)
    rename count count_tot
    sort tax_yr
    list tax_yr count_tot
    save `temp'

    use `age_counts', clear
    merge m:1 tax_yr using `temp', keep(3) nogen
    gen share = count/count_tot
    gsort tax_yr age_primary
    gen claim_data = 0
    save `orig'

    insheet using "$rawdatadir/age_claims.csv", comma clear
    keep if year == 2009 | year == 2010
    bys year: egen count_tot = sum(total_claims)
    rename age age_primary
    rename year tax_yr
    rename total_claims count
    gen share = count/count_tot
    gen claim_data = 1
    gsort tax_yr age_primary
    save `claims'
    append using `orig'

    save "$analysis/age_counts.dta", replace

    insheet using "$rawdatadir/zipage_collapse.csv", comma clear 
    rename zip5 zip
    bys zip: gen nrows = _N
    keep if nrows == 12
    drop nrows

    save "$analysis/age_zip_counts.dta", replace

    insheet using "$rawdatadir/zipage_collapse_20171122.csv", clear 
    rename zip5 zip
    bys zip: gen nrows = _N
    drop nrows

    save "$analysis/age_zip_counts_uneven_20171122.dta", replace

    insheet using "$rawdatadir/zip_collapse_panel_20171122.csv", clear
    rename zipcode zip
    save "$analysis/zipcollapse_panel_20171122.dta", replace


end/*%>*/

*******************************************************************************
* Geo crosswalks
*******************************************************************************
capture program drop crosswalks/*%<*/
program define crosswalks
    syntax anything(name=xwalks), geo(name)

    * Convert CSVs to .dta files:
    * Note: CBSA file assigns to Met. Division wherever they exist.
	* Raw data comes from: https://www.huduser.gov/portal/datasets/usps_crosswalk.html
    tempfile zip_`geo'
    import delimited using `xwalks'/zip_`geo'_032010.csv, delimiter(",") clear
    save `zip_`geo''
    rename *_ratio zip_`geo'_*_ratio
    if "`geo'" == "cbsa" {
        replace cbsa = 99999 if missing(cbsa)
    }
    keep zip `geo' *res_ratio
    label var zip "USPS Zip Code"
    label var zip_`geo'_res_ratio "Residential Units in `geo'"
    saveold `xwalks'/zip_`geo'_crosswalk, replace

end/*%>*/

capture program drop build_crosswalks/*%<*/
program define build_crosswalks

    foreach geo in county cbsa {
        crosswalks $rawdatadir, geo(`geo')
    }

end/*%>*/

*******************************************************************************
* Covariates and exposure
*******************************************************************************
capture program drop reshape_raw_acszip/*%<*/
program define reshape_raw_acszip
    *** Import ACS 5-year period estimates (2006-2010)
    insheet using "$rawdatadir/acs_zip_06-10.csv", clear

    local varnames zip sumlev fips totpop medianage tothhs pctpoor ///
        pctunemp ownerocc pctownerocc totrentals rentvacancy medianrent

    rename v1-v15 (areaname state `varnames')

    label var totpop "(06-10) Total population"
    label var medianage "(06-10) Median age in years"
    label var tothhs "(06-10) Total households"
    label var pctpoor "(06-10) % persons below poverty"
    label var pctunemp "(06-10) % unemployed civilians"
    label var ownerocc "(06-10) Owner-occupied units"
    label var pctownerocc "(06-10) % owner-occupied units"
    label var totrentals "(06-10) Total rental units"
    label var rentvacancy "(06-10) Rental vacancy rate"
    label var medianrent "(06-10) Median rent"
 
    drop if _n <=2

    qui destring `varnames', replace force ignore(", $")
	
    * Merge in other ACS data (n:1 because master has zips straddling states)

    sort zip
    mmerge zip using "$rawdatadir/med_income_zip_06-10", type(n:1) unm(master)
    drop _m
    label var median_income "(06-10) Median household income"

    mmerge zip using "$rawdatadir/zip_pcturban", type(n:1) unm(master)
    drop _m
    rename pct_urban pcturban
    label var pcturban "Urban census tracts within zip"
    label var isurban "zip classified as urban in Census"

    * Drop observations for areas without zip codes 
    drop if substr(areaname, -12, 12) == " is assigned"	
    mmerge zip state using "$rawdatadir/_irszipdata_2005_noagi.dta", ///
        type(1:1) missing(nom) unm(none)
    drop _merge group year sumlev

    label var zip "Zip Code Tabulation Area (ZCTA)"
    label var fips "State FIPS code"
    label var state "State abbreviation"
    label var areaname "Area name"

    order state fips zip
    sort state zip
    *2 (?) ZIPs that straddle borders
    duplicates tag zip, gen(d)
    drop if d != 0
    drop d	
	ren (rentvacancy medianrent median_income totrentals) ///
	    (rentalvacrate mediangrossrent medianhhinc renterocc)
	export delimited zip state totpop medianage tothhs pctpoor ownerocc renterocc ///
	    rentalvacrate mediangrossrent medianhhinc pcturban isurban pctunemp using "$rawdatadir/zip_covars.csv", replace
	
end/*%>*/

capture program drop reshape_raw_censuszip/*%<*/
program define reshape_raw_censuszip
    import delimited "$rawdatadir\DEC_00_SF1_DP1_with_ann.csv", varnames(2) rowrange(2) clear 
	keep id2 numbertotalpopulation
	ren (id2 number) (zip totpop_2000)
	tempfile census1
	save `census1'
	
    import delimited "$rawdatadir\DEC_00_SF1_QTH2_with_ann.csv", varnames(2) rowrange(2) clear 
	keep id2 v6 v8
	ren (id2 v6 v8) (zip ownerocc_2000 rentalunits_2000)
	merge 1:1 zip using `census1', keep(3) nogen
	destring zip, replace force
	drop if missing(zip)
	saveold "$rawdatadir/census_2000_covariates", replace
end/*%>*/

capture program drop build_zip /* %< */
program define build_zip
    syntax anything(name=covars), irscovars(string)

    tempfile acs equifax
	
    ** Built from the reshape_raw_acszip program
    import delimited `covars'/zip_covars.csv,clear
    destring totpop-pcturban, replace i("\N")
    save `acs', replace
    
	** Built from equifax.sql file and equifax raw data at Fama-Miller Center, UChicago
    import delimited `covars'/equifax_subprime_update.csv, clear
	ren v1-v6 (zip card_subprime card_tot subprime tot year)
    keep if year==1996
    destring *subprime, replace i("\N")
	drop year
    save `equifax', replace

	** Pulled from internal IRS environment
    use `acs', clear
    mmerge zip state using "`irscovars'/_irszipdata_2005_noagi", ///
        type(1:1) missing(nom) unm(none)
    mmerge zip using `equifax', type(1:1) unm(master)
    drop _m
    saveold `covars'/zip.dta, replace

end/*%>*/

capture program drop make_covariates/*%<*/
program define make_covariates
    syntax, geo(name) datadir(string) [ANNual]
    * Merge in covariates
    * Sometimes we want to use annual ACS data, sometimes
    * 5-year averages. Decided by an option
    if "`annual'" != "" local ann "annual_"
    if "`annual'" != "" local yr "year"
      
    mmerge `geo' `yr' using "`datadir'/covariates_`ann'`geo'_final", ///
        type(n:1) unmatched(master)
    drop _m
end/*%>*/

capture program drop set_exposure/*%<*/
program define set_exposure
    syntax anything(name=irsdata), file2000(string) file2007(string)

    display "Loading IRS claims data"
	display "These collapses come from within IRS environment"
    * FTHB claims data.
    import delimited using `irsdata'/zip_fthb_collapse_20150307.csv, delimiter(",") clear
    drop v1
    * Add new claims data with amendeds.
    merge 1:1 zip using `irsdata'/FTHBC_ZIP_collapse_v1_masked_20170401.dta, ///
        keep(1 3) nogen keepusing(count_total)
    rename count_total total_claimsA 
    * FTHB counts of homeowners for computing ineligibles.
    mmerge zip using `irsdata'/zip_clunkers_collapse_20150327v2007, type(1:1)
    drop _m
    * FTHB counts of counterfactual homeowners in 2000.
    mmerge zip using `irsdata'/zip_ftcons_collapse_20150302v2000, type(1:1)
    drop _m

    * Discussion of instrument construction from admin data.
    #delimit ; /* %< */
     display
     "Short note on construction of these data, since this is now the main"        _newline
     "instrument. "        _newline
     "1. The collapse used here is a filtered version of a ZIP collapse"        _newline
     "   called zip_collapse_2000_20150228 produced in zip_age_agi_collapse.R"        _newline
     "   on the IRS server. The filter is to restrict to ZIPs with >= 10"        _newline
     "   ft_cons in 2000. The key measure is ft_cons and the denominator is"        _newline
     "   f_units."        _newline
     "2. zip_collapse_2000_20150228 is in turn produced by build_homeowner.py on"        _newline
     "   the IRS server. The PROC SQL query is:"        _newline
     "   ------------------------------------------------------------------------"        _newline
     "       create table home.ft_collapse_\$s as "        _newline
     "       select      "        _newline
     "           zipcode, sum(homeowner) as homeowners, sum(ft_con) as ft_cons, "        _newline
     "           count(tin) as f_units, fil_stat,"        _newline
     "           sum(past_homeowner) as past_homeowners, "        _newline
     "           sum(homeowner <> 1 and past_homeowner <> 1 and "        _newline
     "           ((fil_stat = 2 and adjgross > 150000) or"        _newline
     "           (fil_stat <> 2 and adjgross > 75000))) as income_ineligibles,"        _newline
     "           mort_med "        _newline
     "       from home.homeowner_derived_\$s "        _newline
     "       where age >= 18 and age < 100"        _newline
     "       group by zipcode, fil_stat"        _newline
     "   ------------------------------------------------------------------------"        _newline
     "   The key variables for our purposes are ft_cons and f_units. The code"        _newline
     "   that produces the homeowner_derived table is:"        _newline
     "   ------------------------------------------------------------------------"        _newline
     "       create table home.homeowner_derived_\$s as "        _newline
     "       select"        _newline
     "           tin, tin_pe, tax_yr, age, adjgross, zipcode, fil_stat,"        _newline
     "           claimed_total, "        _newline
     "           coalesce(homeowner, 0) as homeowner,"        _newline
     "           taxA = 1 and (homeowner_1 <> 1 and homeowner_2 <> 1) as ft_ultracon, "        _newline
     "           ---- KEY LINE ----"        _newline
     "           (taxA = 1 and (homeowner_1 <> 1 and homeowner_2 <> 1)) or "        _newline
     "           (points1098 = 1 and (homeowner_1 <> 1 and homeowner_2 <> 1)) as ft_con,"        _newline
     "           ---- KEY LINE ----"        _newline
     "           (taxA = 1 and (homeowner_1 <> 1 and homeowner_2 <> 1)) or"        _newline
     "           (points1098 = 1 and (homeowner_1 <> 1 and homeowner_2 <> 1)) or"        _newline
     "           (mort1098 = 1 and (homeowner_1 <> 1 and homeowner_2 <> 1)) as ft_aggr, "        _newline
     "           homeowner <> 1 and (homeowner_1 = 1 or homeowner_2 = 1) as past_homeowner"        _newline
     "       from home.selfmerge_\$s"        _newline
     "       where tax_yr = \$s;"        _newline
     "   ------------------------------------------------------------------------"        _newline
     "   The key variables from this table are homeowner, taxA, and points1098:"        _newline
     "   ------------------------------------------------------------------------"        _newline
     "       create table home.homeowner_\$s as        "        _newline
     "       select             "        _newline
     "           tin, tax_yr,"        _newline
     "           coalesce(morg_prsm_int, 0) +                 <MORG_INT + PRSMGINT from SCH A>"        _newline
     "           coalesce(est_tax, 0) +                       <EST_TAX from SCH A>"        _newline
     "           coalesce(mortgage_int, 0) +                  <MORTGAGE_INT from 1098>          "        _newline
     "           coalesce(points_paid, 0) > 0 as homeowner,   <POINTS_PAID from 1098>         "        _newline
     "           points_paid > 0 as points1098,"        _newline
     "           mortgage_int > 0 as mort1098,            "        _newline
     "           est_tax > 0 as taxA        "        _newline
     "       from work.combine_\$s;"        _newline
     "   ------------------------------------------------------------------------"        _newline
     "3. The 1098 data start in 1999, the SCH A data start in 1996."        _newline
     "  (a) We use 2 years of past data instead of 3."        _newline
     "  (b) For exposure, we conduct our collapses at the individual/H of H level, rather than"        _newline
     "      by looking at both members of the household."        _newline
     "  (c) We use 2000 instead of earlier or later years (see appendix for robustness).";
    #delimit cr /* %> */

    * 2000 level covariates (census)
    mmerge zip using `file2000', type(1:1) unm(master)
    drop _m
    
    * Covariates from ACS. No inner join unless producing the ZIP covars file.
    mmerge zip using `file2007', type(1:1)
    count if missing(state)

    // total housing stock including rentals
    gen totalunits_2007 = ownerocc + renterocc
    gen totalunits_2000 = ownerocc_2000 + rentalunits_2000

    * Poverty controls?
    gen numpoor_2007 = pctpoor/100*totpop

    *Unaffordables are part of the eligible ratio computation
    destring unaffordables25_2007 unaffordables50_2007 nonfilers_2007, replace force
    replace unaffordables25* = 0 if unaffordables25 == .
    replace unaffordables50* = 0 if unaffordables50 == .

    * Rename variables (MAY BE GONE LATER)
    rename (totpop ownerocc f_units_2007) (pop_2007 ownerocc_2007 filingunits_2007)
    rename (totpop_2000 f_units_2000) (pop_2000 filingunits_2000)

    *Assert uniqueness
    duplicates tag zip, gen(test)
    assert test == 0
    drop test

    * Tempfile because collapsing over all three levels
    saveold $analysis/covariates, replace

end/*%>*/

capture program drop gen_exante/*%<*/
program define gen_exante
    syntax varlist

    foreach totunits of varlist `varlist' {
        gen fthomebuyers_`totunits' = ft_con_2000/`totunits'
    }

end/*%>*/

capture program drop gen_eligibles/*%<*/
program define gen_eligibles
    syntax varlist

    #delimit;
    gen eligibles = ((filingunits_2007 - ineligibles_2007 - unaffordables25_2007
                  )*(1 - (ownerocc_2007 - homeowners_2007)/ownerocc_2007));

    #delimit cr
    foreach totunits of varlist `varlist' {
        gen fthomebuyers_`totunits' = eligibles/`totunits'
        gen total_claims_`totunits' = total_claims/`totunits'
        gen total_claimsA_`totunits' = total_claimsA/`totunits'
    }

end/*%>*/

capture program drop import_covars /*%<*/
program define import_covars
    syntax anything (name=covars), geo(name) xwalks(string)

    *Varlists for programs 
    local exposure_var ineligibles*  pop_2000 ownerocc* ///
        homeowners* filingunits* totalunits* numpoor* unaffordables25* ///
        unaffordables50* ft_con_2000
    local vars_2007 pop_2007 ownerocc_2007 homeowners_2007 filingunits_2007 totalunits_2007
    local vars_2000 pop_2000 ownerocc_2000 homeowners_2000 filingunits_2000 totalunits_2000

    use $analysis/covariates, clear

    tempfile zip_covars
    keep if _m == 3 // Inner join onto ACS ZIP data
    * Merge to county data first
    save `zip_covars', replace
    use `xwalks'/zip_county_crosswalk, clear
    * Take census list of ZIPs as the authoritative one, hence inner join
    mmerge `geo' using `zip_covars', type(n:n) unm(using)
    gsort + `geo' - zip_county_res_ratio
    replace county = 6053 if zip == 93921 // ZIP unmatched to crosswalk?
    duplicates drop `geo', force
    drop _m *res_ratio

    gen numhhspoor = (pctpoor/100)*tothhs

    gen_exante `vars_2000'
    gen_eligibles `vars_2007'

    *Match CBSAs to the ZIP dataset, associating each to the CBSA with the highest
    * respective res_ratio
    mmerge `geo' using "`xwalks'/zip_cbsa_crosswalk", ///
        type(1:n) ukeep(cbsa zip_cbsa_res_ratio) unm(master)
    egen `geo'_cluster = group(cbsa state) if cbsa == 99999
    replace `geo'_cluster = cbsa if cbsa != 99999
    replace `geo'_cluster = 41500 if zip == 93921 // ZIP unmatched to crosswalk?
    gsort + `geo' - zip_cbsa_res_ratio
    duplicates drop `geo', force
    drop _m *res_ratio

    /* RHS, geo-level variables below */

    gen log_avgagi = log(avgagi)
    gen log_pop = log(pop_2007)

    * for circumventing absorb()
    gen ones = 1

    * indicator for sandstate
    gen sandstate = (inlist(state, "CA", "AZ", "NV", "FL"))
    gen sandstateMS = sandstate
    replace sandstateMS = 0 if state == "FL"

    * export
    saveold $analysis/covariates_`geo', replace

end/*%>*/

capture program drop build_covariates/*%<*/
program define build_covariates
    reshape_raw_censuszip
    reshape_raw_acszip
	build_zip $rawdatadir, irscovars("$rawdatadir")
	
    set_exposure $rawdatadir, file2007($rawdatadir/zip) ///
        file2000($rawdatadir/census_2000_covariates)

    import_covars $rawdatadir, geo(zip) xwalks($rawdatadir)

end/*%>*/

capture program drop filter_exposure/*%<*/
program define filter_exposure

    * ISSUE: Exante share can be large, but not negative
    * KEEP THE GEOS WITH MISSING VALUES, for plotting purposes
    count if exante_share < 0 | (exante_share > 0.5 & !missing(exante_share))
    keep if exante_share < 0.5 | missing(exante_share)

end/*%>*/

capture program drop build_exposure/*%<*/
program define build_exposure
    syntax, [scale(name)]

    if ("`scale'" == "") {
        local scale filingunits
    }
    * For mapping.
    * from specific exposure measures to generic names
    
    * For analysis.
    use $analysis/covariates_zip, clear

    rename (total_claims_`scale'_2007 total_claimsA_`scale'_2007 fthomebuyers_`scale'_2007 fthomebuyers_`scale'_2000) ///
           (claims_scaled claimsA_scaled eligible_share exante_share)
    unique zip

    filter_exposure
    unique zip
    drop homeowners_2007-rentalunits_2000 total_claims* fthomebuyers*
    save $analysis/covariates_zip_final, replace

end/*%>*/

*******************************************************************************
* Prices
*******************************************************************************
capture program drop import_CL/*%<*/
program define import_CL
    syntax namelist, CLdata(string)

    * Like what follows, encode the filenames and levels we want reworked in the namelist.
    local i 1
    tokenize `namelist'
    while "``i''" != "" {
        local filename ``i''
        local ++i
        use `cldata'/`filename', replace
        if "``i''"=="state" | "``i''"=="natl" rename month mdate
        drop if mdate < ym(2004,1) | mdate >= ym(2013, 10)

        * Seasonality over each geographic unit
        qui {
        local geo ``i''
        sort `geo'
        foreach var of varlist cl - cl_exc {
            if "``i''" == "natl" seasonalize `var', datevar(mdate)
            else by `geo': seasonalize `var', datevar(mdate)
        }
        }
        drop *_nsa 

        * Delta house prices
        by `geo': egen hptemp = mean(cl_sa) if mdate >= ym(2006,1) & mdate <= ym(2006,12) 
        by `geo': egen hp_2006 = max(hptemp)
        drop hptemp
        by `geo': egen hptemp = mean(cl_sa) if mdate >= ym(2008,1) & mdate <= ym(2008,12) 
        by `geo': egen hp_2008 = max(hptemp)
        * Delta house prices
        by `geo': gen d_hp_CL = 100*(hp_2008-hp_2006)/hp_2006
        drop hp_2006 hp_2008 hptemp

        saveold $analysis/``i''_CL_prices, replace
        local ++i
    }
end/*%>*/

capture program drop import_prices_20170208/*%<*/
program define import_prices_20170208
    syntax anything(name=DQdata), geo(name) ///
        [salesfilter(integer 5) missfilter(integer 10) ]
    /* Dataquick prices datasets.
      Looking at the tails of the prices distribution: extreme outliers and
      zero-valued data. Produces a breakdown of missing price data stats by state.
    */

    tempfile precollapse averages

    * Load in data, filter like we had with houses
    import delimited using `DQdata'/`geo'_dataquick_prices_20170410.csv, ///
        delimiter(",") clear
    capture drop v1 // In case python index not deleted
    drop if mdate < ym(2000,1) | mdate >= ym(2013, 10)
    if "`geo'" == "zip" drop if zip == 0
    if "`geo'" == "cbsa" drop if cbsa == 99999
    drop if mdate > 1000 // two spurious mdates

    *Taking out obs with small values, for now less than 5 transactions.
    filter_data if `geo'_count >= `salesfilter'

    *Seasonalize.
    qui bys `geo': seasonalize med_price, datevar(mdate)
    qui bys `geo': seasonalize mean_price, datevar(mdate)

    *Winsorize to get rid of outliers. Before and after comparisons.
    sum med_price_sa, d
    winsor med_price_sa, gen(med_p_winsor) p(.01)
    sum mean_price_sa, d
    winsor mean_price_sa, gen(mean_p_winsor) p(.01)
    sum med_p_winsor, d

    * Delta house prices. Quarterly price indices generated by averaging over
    * multiple months. We do so for Q2 2006, 2008.
    bys `geo': egen hptemp = mean(med_price_sa) if mdate >= ym(2006,1) & mdate <= ym(2006,12)
    by `geo': egen hp_2006 = max(hptemp)
    drop hptemp
    bys `geo': egen hptemp = mean(med_price_sa) if mdate >= ym(2008,1) & mdate <= ym(2008,12)
    by `geo': egen hp_2008 = max(hptemp)
    by `geo': gen d_hp_DQ = 100*(hp_2008-hp_2006)/hp_2006
    drop if d_hp < -100 // Artifact of seasonalization

    * Add mean prices from repeat sales index data build with same filters.
    merge 1:1 mdate `geo' using $analysis/DQ_zip_mean_price_agg.dta, keep(1 3) nogen
    qui bys `geo': seasonalize mean_price_agg, datevar(mdate)
    winsor mean_price_agg_sa, gen(mean_p_agg_winsor) p(.01)

    keep `geo' mdate me*_p* d_hp `geo'_count count_agg // NOTE new variable included!
    saveold "$analysis/`geo'_DQ_prices_20170208", replace

end/*%>*/

capture program drop import_FHFA/*%<*/
program define import_FHFA
    syntax anything(name=FHFAdata), [startdate(integer 1998) ///
        enddate(integer 2007) betafilter(integer -1) RESIDualize]
    /* Code for generating transformed FHFA price data. */

    tempfile DQorig full HPI

    * Option reruns the beta-adjustment script in R
    if "`residualize'" != "" {
        !Rscript $localcodedir/R/HPI_betas.R `FHFAdata' `startdate' `enddate'
    }
    import delimited `FHFAdata'/HPI.csv, delimiter(",") clear
    rename five zip
    save `HPI'
    use `FHFAdata'/HPI_betas, clear

    * Regressions only on ZIPs whose betas are calculated over sufficiently
    * complete time series (default is complete over entire time interval)
    if `betafilter' == -1 local betafilter = `enddate' - `startdate' + 1
    drop if N < `betafilter'
    display "ZIPs with betas computed in FHFA data:"
    unique zip
    mmerge zip year using `HPI', type(1:n) unm(using)
    keep if year >= 2000 // Paper sample
    unique zip

    duplicates drop zip year, force
    xtset zip year

    * Build d_hp measure with these data
    by zip: gen hpdrop = (hpi - L.hpi)/L.hpi if year==2007
    by zip: egen d_hp_fhfa07 = max(hpdrop)
    drop hpdrop

    saveold "$analysis/zip_FHFA_prices", replace

end/*%>*/

cap program drop build_rawprices/*%<*/
program define build_rawprices

    local clfiles ZMcl zip CMcl county // MMcl cbsa SMcl state Mcl natl

    foreach geo in zip {
		import_prices_20170208 $rawdatadir, geo(`geo')
    }
    import_CL `clfiles', cl($rawdatadir) 

    * Chose 3 as thats when the cross sectional 
    * variance of res falls to a stable level
    import_FHFA $rawdatadir, resid betafilter(3)

end/*%>*/

capture program drop make_prices_20170208/*%<*/
program define make_prices_20170208
    syntax, geo(name) datadir(string) clvars(namelist) fhfavars(namelist)

    preserve
    tempfile prices
    * Merge in a master price file with three different sources
    use `datadir'/`geo'_DQ_prices_20170208, clear
    mmerge `geo' mdate using `datadir'/`geo'_CL_prices, type(1:1) ukeep(`clvars')
    if "`geo'" == "zip" {
            gen year = floor(mdate/12) + 1960
            mmerge `geo' year using `datadir'/`geo'_FHFA_prices, type(n:1) ///
            ukeep(`fhfavars')
            replace mdate = (year - 1960)*12 if missing(mdate)
    }
    * Export
    save `prices', replace
    restore
    mmerge `geo' mdate using `prices', type(n:1)

end/*%>*/

capture program drop build_prices_20170208 /*%<*/
program define build_prices_20170208
    syntax name(name=geo)

    local iv exante
    local grid 5

    use $analysis/`geo'_DQ_prices_20170208, clear
    make_prices_20170208, geo(`geo') datadir($analysis) clvars(cl_sa cl_exc_sa) ///
        fhfavars(annualchange res beta)
    make_covariates, datadir($analysis) geo(`geo')

    * For macro data, merge 2007 mean house sales onto dataset for regression weights.
    * Additionally, filter only on ZIPs in nodistress dataset.
    merge m:1 zip using $analysis/zip_`iv'_nostd, keep(3) nogen keepusing(totalhsales_base)

    prep_data cl_sa, noscale label(lnp) wgt(med_price_nsa) labelwgt(DQprice) ///
        baseyear(2008) geo(`geo') grid(`grid') `nostd'
    rename DQprice_ DQprice_mean2008
    save $analysis/`geo'_prices_`iv'_20170208, replace

end /*%>*/

*******************************************************************************
* Quantities
*******************************************************************************
***************************************************************************
* 1. Data setup
***************************************************************************
capture program drop load_dataquick/*%<*/
program define load_dataquick
    syntax, geo(name) datadir(string) [hetero(string)]
    * DQ data
    if ("`hetero'" != "") {
        use "`datadir'/DQsales_`geo'_`hetero'", clear
    }
    else {
        use "`datadir'/DQsales_`geo'_all", clear
    }
    capture keep if cbsa!=99999
end/*%>*/

capture program drop prep_DQ /* %< */
program define prep_DQ
    syntax varlist, output(string) geo(name) type(name) ///
    [xwalks(string) filterstart(integer 552) filterend(integer 644) ]
    
    * Shared operations for all DQ data work 

    preserve

    if "`type'" == "all" { // To allow aggregation over transaction types
        tokenize varlist
        macro shift 1
        local varlist `*'
    }

    *aggregate the data on geo and whatever else
    collapse (sum) hsales, by(`geo' mdate year month `varlist')
    replace hsales = . if hsales == 0
    egen zip_type = group(`geo' `varlist')

    * Hard-code missing values as zeroes for different model specification
    if "`type'" == "construction" {
        xtset zip_type mdate
        tsfill, full
        replace hsales = 0 if missing(hsales)
        * Carryforward the geo-level stats
        sort zip_type year
        by zip_type: carryforward `geo', replace
    }
    sort zip_type mdate
    if "`type'" != "distress" & "`type'" != "construction" {
        * create values for a geo level filter
        by zip_type: egen chsales = count(hsales) ///
            if mdate >= `filterstart' & mdate <= `filterend'
        gen xshr = chsales/(`filterend' - `filterstart' + 1)
        by zip_type: egen shr = max(xshr)
        drop if missing(shr)
        assert shr <= 1 if !missing(zip_type)
        drop chsales xshr
        local wshr shr
    }

    * Seasonality adjustment
    bys zip_type: seasonalize hsales, datevar(mdate)

    keep `geo' mdate year month `varlist' hsales* `wshr'
    * Export
    saveold `output'/DQsales_`geo'_`type', replace

end
* %>

capture program drop prep_distress/*%<*/
program define prep_distress
    syntax anything, geo(name) output(string)

    tempfile orig totsales filter

    preserve
    use $analysis/DQsales_`geo'_distress, clear

    * Generating the total sales per month denominator.
    mmerge `geo' mdate using `output'/DQsales_`geo'_all, type(n:1) unm(master) ///
        urename(hsales_nsa totsales)
    drop _m
    save `orig', replace

    * Divide into separate datasets for analysis
    * Could be made more general with a levelsof-foreach loop, but doubt
    * we'll move away from the three below.
    tokenize `anything'
    keep if distress_indicator == "A"
    saveold `output'/DQsales_`geo'_distress_`1', replace
    use `orig', clear
    keep if distress_indicator == "S"
    saveold `output'/DQsales_`geo'_distress_`2', replace
    use `orig', clear
    keep if distress_indicator == "I"
    saveold `output'/DQsales_`geo'_distress_`3', replace
    * Old distress dataset layout, without breakdowns by type
    use `orig', clear
    collapse (sum) hsales_nsa (first) totsales, by(`geo' mdate)
    saveold `output'/DQsales_`geo'_distress, replace

end/*%>*/

capture program drop import_DQ/*%<*/
program define import_DQ
    syntax namelist, DQdata(string) xwalks(string) [bedsplit(integer 3)]

    * Make temp datasets
    import delimited using `dqdata'/dq_month_zip_collapse.csv, delimiter(",") clear
    if regexm("`namelist'", "resales|dis") == 1 & regexm("`namelist'", "resales ") == 0 {
        tempfile resales
    }
    if regexm("`namelist'", "all") == 0 {
        local namelist `namelist' all
    }
    tempfile `namelist' errors

    * most data starts in 2004
    drop if year < 2000

    gen mdate = (year - 1960)*12 + month - 1
    format mdate %tm
    drop if month==0
	
    save `all', replace
    collapse (sum) hsales, by(zip county)
    bys zip: egen count = count(county)
    drop if count > 1 & hsales < 3
    save `errors', replace

    use `all', clear
    mmerge zip county using `errors', type(n:1) unm(none) ukeep(hsales)

    * 1. All transactions.
    save `all', replace
    * 2. Just resales.
    if regexm("`namelist'", "resales|dis") == 1 {
        keep if transactiontype == "R"
        save `resales', replace
    }
    * 3. No distress resales.
    if "`resales_nodistress'" != "" {
        keep if inlist(distress_indicator, "\N") 
        save `resales_nodistress', replace
    }
    * 4. Just construction.
    if "`construction'" != "" {
        use `all', clear
        keep if distress_indicator != "L" & transactiontype == "S"
        save `construction', replace
    }
    * 5. Distress resales.
    if "`distress'" != "" {
        use `resales', clear
        keep if !inlist(distress_indicator, "\N", "L", "O")
        save `distress'
    }

    *these are default settings (only need to tweak varlist for heterogeneity)
    local default transactiontype, output($analysis) xwalks(`xwalks')

    * Arguments to program decides which datasets we want to rebuild
    foreach transaction in `namelist' {
        if "`transaction'" != "bedrooms" {
            use ``transaction'', clear
           
            local geo zip
            if "`transaction'" != "distress" prep_DQ `default' ///
                geo(`geo') type(`transaction')
            else {
                count
                prep_DQ distress_indicator, output($analysis) ///
                    xwalks(`xwalks') geo(`geo') type(distress)
                count
                prep_distress Foreclosures REOs ShortSales, ///
                    geo(`geo') output($analysis)
                count
            }
        }


        else {
            *CSV with bedroom info
            import delimited using `dqdata'/dq_month_zip_collapse_bdrms.csv, ///
                delimiter(",") clear
            rename v1-v8 ///
                (hsales month year zip county ///
                 transactiontype distress_indicator bdrms)

            drop if year < 2004

            gen mdate = (year - 1960)*12 + month - 1
            format mdate %tm
            drop if month==0

            save `all', replace
            collapse (sum) hsales, by(zip county)
            bys zip: egen count = count(county)
            drop if count > 1 & hsales < 3
            save `errors', replace

            use `all', clear
            mmerge zip county using `errors', type(n:1) unm(none) ukeep(hsales)

            *Only look at non-distress resales
            keep if transactiontype == "R" & inlist(distress_indicator, "\N")

            * Convert bedroom counts into dummies
            destring bdrms, replace force
            drop if missing(bdrms)
            gen bdrm_effect = (bdrms > 0 & bdrms < 4)
            replace bdrm_effect = . if bdrms == 0

            prep_DQ bdrm_effect `default' geo(zip) type(bedrooms)
        }

    }
end/*%>*/

capture program drop build_sales/*%<*/
program define build_sales

    local dqfiles all resales resales_nodistress construction distress bedrooms
    import_DQ `dqfiles', dqdata($rawdatadir) xwalks($rawdatadir) 

end/*%>*/

capture program drop load_data /* %< */
program define load_data
    syntax, geo(name) datadir(string) [threshold(real 0.9) hetero(string) ANNual]
   
    * Wrapper. Ensure the counts and uniques are conditional
    * to get actual counts of how many geos are matched.

    qui load_dataquick, geo(`geo') datadir(`datadir') hetero(`hetero')
    display "POST LOAD"
    unique `geo'

    capture filter_data if shr > `threshold'
    if _rc display "Completeness flag shr not found, ignoring"
    unique `geo'

    if "`annual'" != "" local opt "ann"
    qui make_covariates, geo(`geo') datadir(`datadir') `opt'
    display "POST COVARS"
    unique `geo' if !missing(exante_share)

    qui make_prices_20170208, geo(`geo') datadir(`datadir') clvars(cl_sa d_hp_CL) ///
        fhfavars(annualchange)
    keep if inlist(_m,1,3)
    drop _m
    display "POST PRICES"
    unique `geo' if !missing(exante_share) & ///
        (!missing(med_price_sa) | !missing(annualchange))
   
end /* %> */

capture program drop make_lhs /* %< */
program define make_lhs
    syntax varname [if/], geo(name) label(string) [base(varname) over(name)]
    *This program constructs different LHS depending on the dataset we're using.
    * Also trims the generated variables at the end. Note that no arguments are optional
    * and if statements are specifiable.
   
    if "`over'" == "" local over mdate
    sort `geo' `over'
   
    * Logarithm of the variable of interest (from Best-Kleven (2015))
    gen `label'_1 = log(`varlist')
   
    * A first difference operator on some LHS. More work on this suggested.
    xtset `geo' `over'
    gen `label'_diff = `label'_1 - L.`label'_1
   
end /* %> */

capture program drop make_scaled_lhs /* %< */
program define make_scaled_lhs
    syntax varname [if/], geo(name) trim(real) ///
        [cumstart(integer 568) cumend(integer 640)]
   
    tempvar hsales_cum
    * Scaled LHS
    gen y = `varlist'/totalhsales_base
   
    * Cumulative house sales. Work in progress: how to filter it?
    by `geo': gen `hsales_cum' = sum(`varlist') if md >= `cumstart' & md <= `cumend'
    gen y_cum = `hsales_cum'/totalhsales_base

    *Generalized percentile trim
    _pctile y, p(`trim')
    gen y_trim = y if y <= r(r1) & y > 0
    gen y_trim_cum = y_cum if y_trim != . 

    * Logs of trims
    gen y_trim_log = log(y_trim)

end /* %> */

capture program drop make_discrete /* %< */
program define make_discrete
    syntax [if/], geo(name) grid(integer)

    tempfile instrument
    preserve
   
    * Construct bins for discrete instruments
    duplicates drop `geo', force
    keep `geo' e*_share pop_2007 totalhsales_base

    * Equal weight.
    xtile exante_sort = exante_share, n(`grid')

    * Weighted by 2007 population
    xtile exante_sort_wt = exante_share [aweight=pop_2007], n(`grid')

    * Weighted by 2007 total hsales NSA.
    xtile exante_sort_wt2 = exante_share [aweight=totalhsales_base], n(`grid')   

    keep `geo' exante_sort* 

    * Merge groups
    save `instrument'
    restore
    mmerge `geo' using `instrument', type(n:1) unmatched(master)
    drop _m

    * Discrete instruments; note capture statement, in case convolved
    * instruments were not generated.
    foreach weigh in "" "_wt" "_wt2" {
        gen treatment_exante`weigh' = cond(exante_sort`weigh' == 1, 0, ///
                                           cond(exante_sort`weigh' == `grid', 1, .))
    }
   
end/* %>*/

capture program drop prep_data /* %< */
program define prep_data
    syntax varlist [if/], geo(name) [grid(integer 5) noSCALE DISCRETE noSTD ///
        BRIDGE wgt(varname) labelwgt(string) label(string) baseyear(integer 2007) ///
        trim(real 97.5) cumstart(integer 568) cumend(integer 640)]
    * Wrapper function for data prep, including exposure standardization.

    * Construct denominator/scale for housing demand growth
    * The second mean is for weighing purposes 
    tempvar W1
    sort `geo' mdate
    if "`wgt'" != "" {
        if (`baseyear' > 0) {
            by `geo': egen `W1' = mean(`wgt') if year == `baseyear'
        }
        else {
            by `geo': egen `W1' = mean(`wgt')
        }
        by `geo': egen `labelwgt'_base = max(`W1')
    }

    *Most of the variable generation now in these two functions
    display "Constructing LHS/RHS variables"
    if "`scale'" == "" {
        make_scaled_lhs `varlist', geo(`geo') trim(`trim') cumstart(`cumstart') cumend(`cumend')
    }
    else {
        capture make_lhs `varlist', geo(`geo') label(`label')
        if _rc display "LHS construction unsuccessful, ignoring"
    }
    if "`bridge'" != "" make_bridge_loan

    if "`discrete'" != "" {
        display "Constructing discrete instruments"
        make_discrete, geo(`geo') grid(`grid')
    }
   
    if "`std'" == "" {
        ** scale the variables
        display "Standardizing exposure"
        foreach var of varlist claims_* e*_share {
                qui sum `var'
                replace `var' = `var'/r(sd)
        }
    }
   
end
*%>

capture program drop merged_housing/*%<*/
program define merged_housing
    syntax name(name=geo), datadir(string) [hetero(name) ///
    threshold(real 0.90) trim(real 97.5) NOSTD DISCRETE baseyear(integer 2007) ///
    BRIDGE NOSCALE grid(integer 5) cumstart(integer 568) cumend(integer 640)]

    clear
    if "`hetero'" == "" local hetero resales_nodistress
    load_data, geo(`geo') datadir(`datadir') hetero(`hetero')
    #delimit ;
    prep_data hsales_sa, geo(`geo') grid(`grid') wgt(hsales_nsa) trim(`trim')
        labelwgt(totalhsales) baseyear(`baseyear') cumstart(`cumstart') cumend(`cumend')
        `nostd' `bridge' `noscale' `discrete';
    #delimit cr

end/*%>*/

capture program drop make_price_impute/*%<*/
program define make_price_impute

    tempfile orig prices
    tempvar d_hp_DQwins d_hp_impute
    save `orig'

    duplicates drop zip, force
    winsor d_hp_DQ, gen(`d_hp_DQwins') p(.05)

    * Only use 95% winsorized guys to predict
    qui reg d_hp_CL `d_hp_DQwins' i.zip_cluster if `d_hp_DQwins' == d_hp_DQ
    predict `d_hp_impute' if `d_hp_DQwins' == d_hp_DQ, xb
    keep zip `d_hp_impute'
    save `prices'

    use `orig', clear
    merge m:1 zip using `prices', keep(1 3) nogen
    gen d_hp = d_hp_CL
    replace d_hp = `d_hp_impute' if d_hp == .

end/*%>*/

***************************************************************************
* Main quantities data set
***************************************************************************
capture program drop build_hsales/*%<*/
program define build_hsales

    * Builds on files created in sql/hsales.sql -> do/1-databuild_bygeo.do
    local iv exante

    merged_housing zip, datadir($analysis) discrete nostd 
    make_price_impute

    preserve
    keep zip county zip_c* state totalhsales d_hp claims* e*_share treatment_ex*
    duplicates drop zip, force
    save $analysis/zip_`iv'_nostd.dta, replace
    restore

    prep_data hsales_sa, geo(zip) noscale
    save $analysis/zip_housing_`iv'_cts.dta, replace

end/*%>*/

***************************************************************************
* Other data sets
***************************************************************************
capture program drop build_bedrooms /*%<*/
program define build_bedrooms 
    *  Bedroom triple diff dataset
    *  The additional option housein should be used if the housing data is already
    *  loaded, in which case there is no need to remerge those files in.

    * Now load in other data and merge with tempfile
    local iv exante
    load_data, geo(zip) datadir($analysis) hetero("bedrooms")
    merge m:1 zip using $analysis/zip_`iv'_nostd, keep(3) nogen ///
        keepusing(totalhsales_base d_hp)

    * Filter for "Triple difference" on bedrooms.
    bys zip: egen zeroes = total(hsales_sa) if missing(bdrm_effect)
    by zip: egen all = total(hsales_sa)
    by zip: gen propo = zeroes/all
    by zip: egen prop = max(propo)

    filter_data if transactiontype == "R" & prop < 0.2 & !inlist(state, ///
        "AR", "LA", "NJ", "NM", "WI")

    * Clean data from here
    prep_data hsales_sa, geo(zip)
    save $analysis/zip_housing_`iv'_bedrooms.dta, replace

end/*%>*/

capture program drop build_construction /*%<*/
program define build_construction 

    local iv exante
    merged_housing zip, datadir($analysis) hetero(construction) nostd noscale
    rename totalhsales_base totalhsales_cons_base
    merge m:1 zip using $analysis/zip_`iv'_nostd, keep(3) nogen ///
        keepusing(totalhsales_base d_hp)
        
    * Clean data from here
    prep_data hsales_sa, geo(zip)
    * Create a holder to enable filtering on incomplete data.
    bys zip year: gen month_countH = (year == 2007)*(_N)
    bys zip: egen month_count = max(month_countH)
    drop month_countH
    * Filter on sufficient number of average house starts in 2007
    keep if totalhsales_cons_base > 2

    save $analysis/zip_construction_`iv'_cts.dta, replace

end/*%>*/

capture program drop build_distress_counts/*%<*/
program define build_distress_counts

    * Composition of distress sales to resales on the national level,
    * following a Calculated Risk graph doing the same for Sacramento.
    tempfile REOs ShortSales Resales totals
    use $analysis/DQsales_zip_distress_Foreclosures, clear
    save `REOs', replace
    use $analysis/DQsales_zip_distress_REOs, clear
    append using `REOs'
    collapse (sum) hsales_nsa, by(mdate)
    save `REOs', replace

    use $analysis/DQsales_zip_distress_ShortSales, clear
    collapse (sum) hsales_nsa, by(mdate)
    save `ShortSales', replace 

    * One measure is to compare the distressed sales against our sample data...
    use $analysis/DQsales_zip_resales_nodistress, clear 
    collapse (sum) hsales_nsa, by(mdate)
    save `Resales', replace 

    * ... Or against all resales in DQ (including sales with some
    * unidentified distress status)
    use $analysis/DQsales_zip_resales, clear 
    collapse (sum) hsales_nsa, by(mdate)
    rename hsales_nsa total
    save `totals', replace 

    use `REOs', clear
    gen sale_type = 1
    append using `ShortSales'
    replace sale_type = 2 if missing(sale_type)
    append using `Resales'
    replace sale_type = 3 if missing(sale_type)

    mmerge mdate using `totals', type(n:1) unm(none) 
    drop _m
    replace hsales_nsa = hsales_nsa/total
    reshape wide hsales_nsa, i(mdate) j(sale_type)

    save $analysis/distress_counts.dta, replace

end
* %>

capture program drop build_reallocation/*%<*/
program define build_reallocation

    *Key outcomes dataset
    local iv exante
    tempfile orig oth_di realloc

    * First file contains collapsed counts of recently built, 
    * owned by developers, etc.
    import delimited $rawdatadir/dq_keycounts_zip_collapse.csv, clear
    gen mdate = (year-1960)*12 + month - 1
    format mdate %tm
    xtset zip mdate
    tsfill, full
    save `orig', replace

    * Left join on foreclosures and short sales combined.
    use $analysis/DQsales_zip_distress_Foreclosures, clear
    append using $analysis/DQsales_zip_distress_ShortSales
    collapse (sum) hsales_nsa, by(zip mdate)
    save `oth_di', replace

    use `orig', clear
    mmerge zip mdate using $analysis/DQsales_zip_all, type(1:1) ///
        ukeep(zip hsales_nsa)
    merge m:1 zip using $analysis/zip_`iv'_nostd, keep(3) nogen ///
        keepusing(totalhsales_base claims_ e*_share state zip_cluster)
    mmerge zip mdate using $analysis/DQsales_zip_distress_REOs, ///
        type(1:1) ukeep(hsales_nsa) urename(hsales_nsa reo) unm(master)
    mmerge zip mdate using `oth_di', type(1:1) ukeep(hsales_nsa) ///
        urename(hsales_nsa foreclo_shortsale) unm(master)

    * Cleaning. 
    drop year month
    order mdate, after(zip)
    foreach count of varlist recent_built-gse_seller reo foreclo_shortsale {
        replace `count' = 0 if missing(`count')
    }
    save $analysis/zip_reallocation_monthly.dta, replace

    * Collapse.
    local realloc_types recent_built developer_seller gse_seller reo foreclo_shortsale
    prep_longdiff zip mdate `realloc_types' hsales_nsa, label(`realloc_types' totsales)
    save `realloc', replace
    use $analysis/zip_reallocation_monthly.dta, clear
    prep_longdiff zip mdate `realloc_types' hsales_nsa, ///
        label(`realloc_types' totsales) avg
    mmerge zip period using `realloc', type(1:1) unm(none)
    sort zip period
    save $analysis/zip_reallocation_collapse.dta, replace

end/*%>*/

capture program drop prep_longdiff /*%<*/
program define prep_longdiff
    syntax anything, label(string asis) ///
        [period(numlist) control(varlist) AVGonly]

    * This program collapses the dataset to run a cross-sectional regression
    * Over delta growth of the LHS between the pre, policy and post periods.
    * Run the appropriate data_produce function before usage.

    * First input is the name of geo unit. Second is name of time variable.
    * Everything after are the variables needing aggregation. You want to
    * have as many space-delimited labels in label as the number of variables.
    tokenize `anything'
    local geo `1'
    macro shift
    local over `1'
    macro shift
    local varlist `*'

    local d = 1
    if "`over'" == "qdate" local d = 3
    * Choose a time frame for pre and policy and post.
        * Pre-period: 571-587 (17 months)
        * Policy period: 588-595 + 596-598 + 599-601 + 602-604 (17 months)
        * Post-period: 605-616 (reversal) + 617-621 (17 months), but adjustable
    if "`period'" == "" local period 554 570 571 587 588 604 605 621
    tokenize `period'
    * Build the period indicators
    gen period = .
    local i = 1
    while "``i''" != "" {
        local j = `i' + 1
        replace period = ceil(`i'/2) if `over' >= floor(``i''/`d') & `over' <= floor(``j''/`d')
        local i = `i' + 2
    }
    keep if !missing(period)
    tab `over' period // Check period generation accurate

    * Iterative process of taking the sum over periods of a variable of interest.
    sort `geo' period
    tokenize `label'
    local i = 1
    local first `1'
    if "`geo'" != "cbsa" local cluster `geo'_cluster
    if "`avgonly'" != "" {
        local func mean
        local varlab avg
    }
    else {
        local func total
        local varlab total
        local opts ,missing
    }
    foreach var of local varlist {
         by `geo' period: egen `varlab'_``i'' = `func'(`var') `opts'
         local ++i
    }
    local i = `i' - 1

    collapse (max) `varlab'_`first'-`varlab'_``i'' totalhsales_base claims_ e*_share ///
        `control' (first) state `cluster', by(`geo' period)
    xtset `geo' period

    * Delete zips/counties with missing data after a certain period. Assert
    * the panels are all "strongly balanced"
    qui unique period
    by `geo': keep if _N == _result(18)
    xtset `geo' period

end
* %>

capture program drop build_longdiffs/*%<*/
program define build_longdiffs
    * TODO: Use prep_longdiff for everything here. Ideally collapse twice
    * (once for sum, other for average) and then join them together.

    local iv exante
    tempfile housing price price_beta construction permit empl distress housing_all ///
        construction_post price_raw
    *************************************************************************
    * Housing./*%<*/
    *************************************************************************
    use $analysis/zip_housing_`iv'_cts.dta, clear
    preserve
    prep_longdiff zip mdate hsales_sa, control(log_avgagi d_hp pctunemp card_subprime log_pop pop_2007 sandstate*) label(sales)
    by zip: gen delta_sales = (total_sales[3] - total_sales[2])/totalhsales_base
    * Pre-period
    keep if period == 1
    save `housing'

    restore, preserve
    prep_longdiff zip mdate hsales_sa, label(sales) avg
    by zip: gen average_sales_pre = avg_sales[2]/totalhsales_base
    by zip: gen average_sales = avg_sales[3]/totalhsales_base
    by zip: gen average_delta_sales = (avg_sales[3]-avg_sales[2])/totalhsales_base
    by zip: gen average_delta_sales_post = (avg_sales[4]-avg_sales[2])/totalhsales_base
    mmerge zip period using `housing', type(1:1) unm(none)
    save `housing', replace 
    
    restore
    * Post goes 12 months instead of 17. I.e., 605 to 616 instead of 605 to 621.
    prep_longdiff zip mdate hsales_sa,  label(sales) avg ///
        period(554 570 571 587 588 604 605 616)
    by zip: gen average_delta_sales_postshort = (avg_sales[4]-avg_sales[2])/totalhsales_base
    mmerge zip period using `housing', type(1:1) unm(none)
    save `housing', replace /*%>*/

    *************************************************************************
    * Prices./*%<*/
    * Notice cumulative sum of percentages as LHS.
    *************************************************************************
    use $analysis/zip_prices_`iv'.dta, clear

    preserve
    prep_longdiff zip mdate lnp_diff, control(DQprice_mean2008) label(pricegrowth)
    by zip: gen delta_pricegrowth = 100*(total_pricegrowth[3] - total_pricegrowth[2])
    keep if period == 1
    save `price'

    restore, preserve
    prep_longdiff zip mdate lnp_diff, label(pricegrowth) avg
    by zip: gen average_pricegrowth_pre = 100*avg_pricegrowth[2]
    by zip: gen average_pricegrowth = 100*avg_pricegrowth[3]
    by zip: gen average_delta_pricegrowth = 100*(avg_pricegrowth[3]-avg_pricegrowth[2])
    mmerge zip period using `price', type(1:1) unm(none)
    save `price', replace 
    
    use "$analysis/zip_prices_exante_20170208.dta", clear
    drop lnp_diff*

    xtset zip mdate
    gen L_mean_p_winsor = L.mean_p_agg_winsor
    gen lnp_diff = log(mean_p_agg_winsor) - log(L_mean_p_winsor)
    drop L_mean_p_winsor
    prep_longdiff zip mdate lnp_diff, control(DQprice_mean2008) label(pricegrowth_raw)
    by zip: gen delta_pricegrowth_raw = 100*(total_pricegrowth_raw[3] - total_pricegrowth_raw[2])
    keep if period == 1
    save `price_raw'

    /*%>*/

    *************************************************************************
    * Prices from FHFA with mkt adjust./*%<*/
    * Notice period changes; variables already percentages, so no factor of 100.
    *************************************************************************
    restore
    duplicates drop zip year, force

    preserve
    prep_longdiff zip year res, period(2007 2008 2009 2010) label(pricegrowth_fhares)
    by zip: gen delta_pricegrowth_fhares = total_pricegrowth_fhares[2]-total_pricegrowth_fhares[1]
    * Pre-period, and also filtering all FHFA data based on beta completeness.
    keep if period == 1 & !missing(total_pricegrowth_fhares)
    save `price_beta'

    restore
    prep_longdiff zip year annualchange, period(2007 2008 2009 2010) label(pricegrowth_fha)
    by zip: gen delta_pricegrowth_fha = total_pricegrowth_fha[2]-total_pricegrowth_fha[1]
    mmerge zip period using `price_beta', type(1:1) unm(none)
    save `price_beta', replace /*%>*/

    *************************************************************************
    * Construction./*%<*/
    * Notice long post diffs in the third block.
    *************************************************************************
    use $analysis/zip_construction_`iv'_cts.dta, clear

    tempfile construction
    preserve
    prep_longdiff zip mdate hsales_sa,  label(built) 
    by zip: gen delta_built = (total_built[3] - total_built[2])/totalhsales_base
    by zip: gen average_delta_built = (1/17)*(total_built[3]-total_built[2])/((1/17)*total_built[2])
    * Pre-period
    keep if period == 1
    save `construction'

    restore, preserve
    prep_longdiff zip mdate hsales_sa,  label(built) avg
    by zip: gen average_built_pre = avg_built[2]/totalhsales_base
    by zip: gen average_built = avg_built[3]/totalhsales_base
    mmerge zip period using `construction', type(1:1) unm(none)
    save `construction', replace

    restore
    prep_longdiff zip mdate hsales_sa,  label(built) ///
        period(571 587 605 616 617 640)
    by zip: gen avg_delta_cons_postperiod = (total_built[2]/12 - total_built[1]/17)/totalhsales_base
    by zip: gen avg_delta_cons_postperiod2 = (total_built[3]/24 - total_built[1]/17)/totalhsales_base
    mmerge zip period using `construction', type(1:1) unm(none)
    save `construction', replace /*%>*/

    *************************************************************************
    * Distress categories./*%<*/
    * Collapse was in an earlier program, so just variable construction.
    *************************************************************************
    use $analysis/zip_reallocation_collapse.dta, clear

    by zip: gen average_delta_recent = (avg_recent_built[3] - avg_recent_built[2])/totalhsales_base
    by zip: gen average_delta_developer = (avg_developer_seller[3] - avg_developer_seller[2])/totalhsales_base
    by zip: gen average_delta_gse = (avg_gse_seller[3] - avg_gse_seller[2])/totalhsales_base
    by zip: gen average_delta_reo = (avg_reo[3] - avg_reo[2])/totalhsales_base
    by zip: gen average_delta_foreclo = (avg_foreclo_shortsale[3] - avg_foreclo_shortsale[2])/avg_foreclo_shortsale[2]
    by zip: gen average_delta_foreclo2 = (avg_foreclo_shortsale[3] - avg_foreclo_shortsale[1])/avg_foreclo_shortsale[1]
    * Pre-period (Only nonzero for the DQ longdiffs)
    keep if period == 1
    save `distress' /*%>*/

    * Combine datasets.
    use `housing', clear
    merge 1:1 zip using `construction', keep(1 3) nogen
    merge 1:1 zip using `price', keep(1 3) nogen
    merge 1:1 zip using `price_raw', keep(1 3) nogen
    merge 1:1 zip using `price_beta', keep(1 3) nogen
    merge 1:1 zip using `distress', keep(1 3) nogen

    save $analysis/zip_longdiffs.dta, replace
    
end/*%>*/

capture program drop raw_treat /* %< */
program define raw_treat
    syntax varname 

    local geo zip
    * Produces a spreadsheet of which zips belong in discrete treatment or control,
    * and possibly more. Useful for more granular IRS work.
    drop if missing(`varlist')
    keep zip county zip_cluster state `varlist'
    export excel using "$analysis/TreatControl.xlsx", firstrow(var) ///
        sheetrep sheet(`geo')
    
end /* %> */

capture program drop build_splitclaims/*%<*/
program define build_splitclaims

    use $analysis/zip_exante_nostd, clear
    merge 1:1 zip using $analysis/zip_longdiffs, keep(1 3) ///
        keepusing(d_hp log_pop log_avgagi pctunemp zip_cluster) nogen
    raw_treat treatment_exante_wt

end/*%>*/

capture program drop make_defaulters /* %< */
program define make_defaulters
    syntax, start(numlist) end(numlist) label(string)

    tokenize `start'
    local start_y = `1'
    local start_m = `2'
    
    tokenize `end'
    local end_y = `1'
    local end_m = `2'
    
    rename (v1-v4) (orig trans di property)
    gen start = mofd(date(orig, "YM"))
    drop orig
    rename start orig
    gen mdate = mofd(date(trans, "YM"))
    format orig %tm
    format mdate %tm
    drop if mdate < orig
    gsort mdate orig - di

    gen count = property
    replace count = 0 if orig != mdate & di == "\N" & ///
        mdate >= ym(`start_y',`start_m') & ///
        mdate <= ym(`end_y', `end_m')

    gen distress = sum(count) if di != "\N" & orig != mdate
    carryforward distress, replace
    gen denom = sum(count) if ((di == "\N") | (di != "\N" & orig == mdate)) ///
        & mdate <= ym(`end_y',`end_m')
    carryforward denom, replace
    gen rate = distress/denom*1000
    
    collapse (last) rate denom, by(mdate) 
    gen length = 1 in 1
    gen cohort = "`label'"
    replace length = length[_n-1] + 1 in 2/l
    
end

* %>

capture program drop build_defaulters /* %< */
program define build_defaulters 

    tempfile policy policy2 pre1 pre2 pre3 post

    import delimited $rawdatadir/dq_delin_2009_collapse.csv, clear
    make_defaulters, start(2009 1) end(2009 12) label(Policy cohort)
    save `policy', replace
    import delimited $rawdatadir/dq_delin_2010_collapse.csv, clear
    make_defaulters, start(2010 1) end(2010 12) label(Policy cohort, 2010)
    save `policy2', replace
    import delimited $rawdatadir/dq_delin_2006_collapse.csv, clear
    make_defaulters, start(2006 1) end(2006 12) label(Pre cohort, 2006)
    save `pre1', replace
    import delimited $rawdatadir/dq_delin_2007_collapse.csv, clear
    make_defaulters, start(2007 1) end(2007 12) label(Pre cohort, 2007)
    save `pre2', replace
    import delimited $rawdatadir/dq_delin_2008_collapse.csv, clear
    make_defaulters, start(2008 1) end(2008 12) label(Pre cohort, 2008)
    save `pre3', replace
    import delimited $rawdatadir/dq_delin_2011_collapse.csv, clear
    make_defaulters, start(2011 1) end(2011 12) label(Post cohort)
    save `post', replace

    use `policy', clear
    foreach names in policy2 pre1 pre2 pre3 post {
        append using ``names''
    }
    encode cohort, gen(coh) 
    drop mdate cohort
    reshape wide rate denom, i(length) j(coh)
    forval n = 1/6 {
        local denom_`n' = denom`n'[13]
    }
    drop denom*
    label var rate1 "2009 cohort (N=`denom_1')"
    label var rate2 "2010 cohort (N=`denom_2')"
    label var rate3 "2011 cohort (N=`denom_3')"
    label var rate4 "2006 cohort (N=`denom_4')"
    label var rate5  "2007 cohort (N=`denom_5')"
    label var rate6  "2008 cohort (N=`denom_6')"

    label var length "Months elapsed since start of cohort"

    save $analysis/DQ_defaulters, replace

end

* %>

*******************************************************************************
* Robustness and additional controls for appendix/referee analysis
*******************************************************************************
capture program drop create_fha_takeup/*%<*/
program define create_fha_takeup

    ** First, data from 2004-2010
    import delimited $rawdatadir/hmda_pull_all_loans_2010_purchase.csv, clear stringcols(1 2 3 4)
    rename (v1 v2 v3 v4 v5 v6 v7) ///
        (loan_type censustrct_raw county state loan_amt year purch)
	drop purch
    gen length_fips_code = length(county)
    replace county = "0" + county if length_fips_code==2
    replace county = "00" + county if length_fips_code==1   

    gen length_state = length(state)
    replace state = "0" + state if length_state==1

    replace censustrct = substr(censustrct, 1, length(censustrct)-3)
    gen census_tract     = subinstr(censustrct, ".", "",.)
    gen length_census = length(census_tract)
    replace census_tract = "0" + census_tract if length_census==5
    replace census_tract = "00" + census_tract if length_census==4
    replace census_tract = "000" + census_tract if length_census==3
    gen fips_tract_final = state + county + census_tract 
    gen length_fips_tract = length(fips_tract_final)
    tab length_fips_tract
    drop if length_fips_tract==12
    destring state county, replace
    rename (state county) (fips_state fips_code)
    drop length* census_tract
	save $rawdatadir/hmda_pull_loans_work_2010, replace
	
	** Next, data from 2011-2014
    import delimited $rawdatadir/hmda_pull_all_loans_2014_purchase.csv, clear stringcols(1 2 3 4)
    rename (v1 v2 v3 v4 v5 v6 v7) ///
        (loan_type censustrct_raw county state loan_amt year purch)
	drop purch
    gen census_tract = subinstr(censustrct, ".", "",.) 
    gen fips_tract_final = state + county + census_tract 
    gen length_fips_tract = length(fips_tract_final)
    tab length
    drop if length==12
    destring state county, replace
    rename (state county) (fips_state fips_code)
    drop length* census_tract
	save $rawdatadir/hmda_pull_loans_work_2014, replace
    
	** Append the two datasets together and save
	append using $rawdatadir/hmda_pull_loans_work_2010
	save $rawdatadir/hmda_all_loans, replace 
	
	import delimited $rawdatadir/fha_mortgage_map.csv, clear
	merge 1:m fips_state fips_code using $rawdatadir/hmda_all_loans.dta, ///
        keep(3) nogen
    gen exp1_indicator   = 1 if loan_amt<=one_unit_2008_test & ///
	                            loan_amt>one_unit_2007_test & ///
							    year<2008
    gen exp2_indicator   = 1 if loan_amt<=one_unit_2008_test & ///
	                            year<2008
	gen fha_loan_exp_ind = 1 if loan_amt>one_unit_2007_test & ///
	                            loan_amt<=one_unit_2008_test & ///
	                            loan_type=="2"
	gen fha_loan_ind     = 1 if loan_type=="2"
	gen non_fha_loan_ind = 1 if loan_type!="2"
	save $rawdatadir/hmda_all_loans_indvars, replace
	** Test some things out 
	tabstat fha_loan_exp_ind fha_loan_ind non_fha_loan_ind, ///
        statistics(sum) by(year)
	tabstat loan_amt, statistics(count) by(year)
	
	** Collapse at the tract_year level
	collapse (sum) fha_loan_exp_ind fha_loan_ind non_fha_loan_ind ///
	    (count) loan_amt, by(fips_tract_final fips_code fips_state year)
    save $rawdatadir/hmda_all_loans_indvars_collapse, replace
	tabstat loan_amt fha_loan_exp_ind fha_loan_ind non_fha_loan_ind, ///
        statistics(sum) by(year)
	
	** Now, move from tract-year to zip-year
	import excel $rawdatadir/TRACT_ZIP_032010.xlsx, sheet("TRACT_ZIP_032010") ///
        allstring firstrow clear
	rename TRACT fips_tract_final
    joinby fips_tract_final using "$rawdatadir/hmda_all_loans_indvars_collapse.dta"
    destring *RATIO, replace 
	
	tabstat loan_amt fha_loan_exp_ind fha_loan_ind non_fha_loan_ind, ///
        statistics(sum) by(year)
		
    foreach var in fha_loan_exp_ind fha_loan_ind non_fha_loan_ind loan_amt {
        gen `var'_adj = `var'*RES_RATIO
    }
	
    collapse (sum) *_adj, by(ZIP year)
	rename *_adj *
	rename ZIP zip
	tabstat loan_amt fha_loan_exp_ind fha_loan_ind non_fha_loan_ind, ///
        statistics(sum) by(year)
		
	rename (fha_loan_exp_ind fha_loan_ind non_fha_loan_ind loan_amt) ///
	    (expansion_fha_loans fha_loans non_fha_loans total_count)
		
    la var zip "ZIP-5"
    la var year "Year"
    la var total_count "Total 1-4 unit, owner-occupied, loan originations"
    la var fha_loans "Total FHA-insured loans"
    la var non_fha_loans "Total non-FHA-insured loans"
	la var expansion_fha_loans "Total FHA-insured loans in post-expansion limits"
	save $rawdatadir/zip_by_year_fha_hmda.dta, replace

end/*%>*/

capture program drop create_fha_exp_var/*%<*/
program define create_fha_exp_var

    ** First, data from 2004-2010
    import delimited $rawdatadir/hmda_pull_all_loans_2010_purchase.csv, clear stringcols(1 2 3 4)
    rename (v1 v2 v3 v4 v5 v6 v7)  ///
        (loan_type censustrct_raw county state loan_amt year purch)
	drop purch
    gen length_fips_code = length(county)
    replace county = "0" + county if length_fips_code==2
    replace county = "00" + county if length_fips_code==1   

    gen length_state = length(state)
    replace state = "0" + state if length_state==1

    replace censustrct = substr(censustrct, 1, length(censustrct)-3)
    gen census_tract     = subinstr(censustrct, ".", "",.)
    gen length_census = length(census_tract)
    replace census_tract = "0" + census_tract if length_census==5
    replace census_tract = "00" + census_tract if length_census==4
    replace census_tract = "000" + census_tract if length_census==3
    gen fips_tract_final = state + county + census_tract 
    gen length_fips_tract = length(fips_tract_final)
    tab length_fips_tract
    drop if length_fips_tract==12
    destring state county, replace
    rename (state county) (fips_state fips_code)
    drop length* census_tract
	save $rawdatadir/hmda_pull_loans_work_2010, replace
	
	** Next, data from 2011-2014
    import delimited $rawdatadir/hmda_pull_all_loans_2014_purchase.csv, clear stringcols(1 2 3 4)
    rename (v1 v2 v3 v4 v5 v6 v7) ///
        (loan_type censustrct_raw county state loan_amt year purch)
    gen census_tract = subinstr(censustrct, ".", "",.) 
    gen fips_tract_final = state + county + census_tract 
    gen length_fips_tract = length(fips_tract_final)
    tab length
    drop if length==12
    destring state county, replace
    rename (state county) (fips_state fips_code)
    drop length* census_tract
	save $rawdatadir/hmda_pull_loans_work_2014, replace
    
	** Append the two datasets together and save
	append using $rawdatadir/hmda_pull_loans_work_2010
	save $rawdatadir/hmda_all_loans, replace
	
    import delimited $rawdatadir/fha_mortgage_map.csv, clear
	merge 1:m fips_state fips_code using $rawdatadir/hmda_all_loans.dta, ///
        keep(3) nogen
    gen exp1_indicator   = 1 if loan_amt<=one_unit_2008_test & ///
	                            loan_amt>one_unit_2007_test & ///
							    year<2008
    gen exp2_indicator   = 1 if loan_amt<=one_unit_2008_test & ///
	                            year<2008
	gen fha_loan_exp_ind = 1 if loan_amt>one_unit_2007_test & ///
	                            loan_amt<=one_unit_2008_test & ///
	                            loan_type=="2"
	gen fha_loan_ind     = 1 if loan_type=="2"
	gen non_fha_loan_ind = 1 if loan_type!="2"
	save $rawdatadir/hmda_all_loans_indvars, replace
	
    keep if year<2008
	
    ** Test some things out 
	tabstat exp1_indicator exp2_indicator, ///
        statistics(sum)
	tabstat loan_amt, statistics(count)
	
	** Collapse at the tract_year level
	collapse (sum) exp1_indicator exp2_indicator ///
	    (count) loan_amt, by(fips_tract_final fips_code fips_state)
	tabstat loan_amt exp1_indicator exp2_indicator, stat(sum)
	save $rawdatadir/hmda_all_loans_fha_exposure_collapse, replace
	
    import excel $rawdatadir/TRACT_ZIP_032010.xlsx, sheet("TRACT_ZIP_032010") ///
        allstring firstrow clear
   
    rename TRACT fips_tract_final
    merge m:1 fips_tract_final using ///
	    "$rawdatadir/hmda_all_loans_fha_exposure_collapse", keep(3) nogen
	
	destring *RATIO, replace 

    foreach var in loan_amt exp1_indicator exp2_indicator {
        gen `var'_adj = `var'*RES_RATIO
    }
	
	collapse (sum) exp*indicator_adj loan_amt_adj, by(ZIP)
    
	* Create the exposure variable 
    gen new_exp_ratio = exp1_indicator_adj/loan_amt_adj
    gen exp_ratio     = exp2_indicator_adj/loan_amt_adj
	rename ZIP zip
    summ new_exp_ratio, det
    summ exp_ratio, det
    tabstat loan_amt_adj, stat(sum)
	destring zip, replace
	save $rawdatadir/hmda_fha_exp_estimate, replace

end/*%>*/

capture program drop create_gse_exposure/*%<*/
program define create_gse_exposure

    ** First, data from 2004-2010
    import delimited $rawdatadir/hmda_pull_all_loans_2010_purchase.csv, clear ///
	    stringcols(1 2 3 4)
    rename (v1 v2 v3 v4 v5 v6 v7) ///
        (loan_type censustrct_raw county state loan_amt year purch_type)
	gen length_fips_code = length(county)
    replace county = "0" + county if length_fips_code==2
    replace county = "00" + county if length_fips_code==1   

    gen length_state = length(state)
    replace state = "0" + state if length_state==1

    replace censustrct = substr(censustrct, 1, length(censustrct)-3)
    gen census_tract     = subinstr(censustrct, ".", "",.)
    gen length_census = length(census_tract)
    replace census_tract = "0" + census_tract if length_census==5
    replace census_tract = "00" + census_tract if length_census==4
    replace census_tract = "000" + census_tract if length_census==3
    gen fips_tract_final = state + county + census_tract 
    gen length_fips_tract = length(fips_tract_final)
    tab length_fips_tract
    drop if length_fips_tract==12
    destring state county, replace
    rename (state county) (fips_state fips_code)
    drop length* census_tract
	save $rawdatadir/hmda_pull_loans_work_2010_purch, replace
	
	** Next, data from 2011-2014
    import delimited $rawdatadir/hmda_pull_all_loans_2014_purchase.csv, clear ///
	    stringcols(1 2 3 4)
    rename (v1 v2 v3 v4 v5 v6 v7 v8) ///
        (loan_type censustrct_raw county state loan_amt year respid purch_type)
    gen census_tract = subinstr(censustrct, ".", "",.) 
    gen fips_tract_final = state + county + census_tract 
    gen length_fips_tract = length(fips_tract_final)
    tab length
    drop if length==12
    destring state county, replace
    rename (state county) (fips_state fips_code)
    drop length* census_tract
	save $rawdatadir/hmda_pull_loans_work_2014_purch, replace
    
	** Append the two datasets together and save
	append using $rawdatadir/hmda_pull_loans_work_2010_purch
	save $rawdatadir/hmda_all_loans_purchase, replace
	gen gse_indicator = 1 if purch_type==1 | ///
	                         purch_type==3
    collapse (sum) gse_indicator (count) loan_amt, ///
	    by(fips_tract_final fips_code fips_state year)
	save $rawdatadir/hmda_all_loans_gsevar_collapse, replace
	
    ** Now, move from tract-year to zip-year
	import excel $rawdatadir/TRACT_ZIP_032010.xlsx, sheet("TRACT_ZIP_032010") ///
        allstring firstrow clear
	rename TRACT fips_tract_final
    joinby fips_tract_final using "$rawdatadir/hmda_all_loans_gsevar_collapse.dta"
    destring *RATIO, replace 

	foreach var in gse_indicator loan_amt {
        gen `var'_adj = `var'*RES_RATIO
    }
    
	collapse (sum) *_adj, by(ZIP year)
	rename *_adj *
	rename ZIP zip
	ren loan_amt total_count
    la var zip "ZIP-5"
    la var year "Year"
    la var total_count "Total 1-4 unit, owner-occupied, loan originations"
	la var gse_indicator "Loan purchaser = Fannie or Freddie"
	save $rawdatadir/zip_by_year_gse, replace

end/*%>*/

capture program drop create_subprime_var/*%<*/
program define create_subprime_var

    * This program tests out the subprime exposure variable *
    ** First, data from 2004-2010
    import delimited $rawdatadir/censustract_hmda_subprime_pull_test_2010.csv, clear stringcols(3 4 5)
    rename (v1 v2 v3 v4 v5 v6) ///
        (subprime_count total_count censustrct_raw county state year)

	* format the census_tract_vars 
    gen length_fips_code = length(county)
    replace county = "0" + county if length_fips_code==2
    replace county = "00" + county if length_fips_code==1   

    gen length_state = length(state)
    replace state = "0" + state if length_state==1

    replace censustrct = substr(censustrct, 1, length(censustrct)-3)
    gen census_tract     = subinstr(censustrct, ".", "",.)
    gen length_census = length(census_tract)
    replace census_tract = "0" + census_tract if length_census==5
    replace census_tract = "00" + census_tract if length_census==4
    replace census_tract = "000" + census_tract if length_census==3
    gen fips_tract_final = state + county + census_tract 
    gen length_fips_tract = length(fips_tract_final)
    tab length_fips_tract
    drop if length_fips_tract==12
    destring state county, replace
    rename (state county) (fips_state fips_code)
    drop length* census_tract
	save $rawdatadir/subprime_test_2010, replace
	
	** Next, data from 2011-2014
    import delimited $rawdatadir/censustract_hmda_subprime_pull_test_2014.csv, clear stringcols(3 4 5)
    rename (v1 v2 v3 v4 v5 v6) ///
        (subprime_count total_count censustrct_raw county state year)
		
	* format the census_tract_vars 
    gen census_tract = subinstr(censustrct, ".", "",.) 
    gen fips_tract_final = state + county + census_tract 
    gen length_fips_tract = length(fips_tract_final)
    tab length
    drop if length==12
    destring state county, replace
    rename (state county) (fips_state fips_code)
    drop length* census_tract
	
	** Append the datasets
	append using $rawdatadir/subprime_test_2010
	save $rawdatadir/subprime_all_years, replace
	
    ** Now, move from tract-year to zip-year
	import excel $rawdatadir/TRACT_ZIP_032010.xlsx, sheet("TRACT_ZIP_032010") ///
        allstring firstrow clear
	rename TRACT fips_tract_final
    joinby fips_tract_final using "$rawdatadir/subprime_all_years.dta"
    destring *RATIO, replace 
	
	tabstat subprime_count total_count, ///
        statistics(sum) by(year)
		
    foreach var in total_count subprime_count {
        gen `var'_adj = `var'*RES_RATIO
    }
	
	* Create the subprime variable 
    collapse (sum) *_adj, by(ZIP year)
	rename *_adj *
	rename ZIP zip
	gen subprime_ratio = subprime_count/total_count
	summ subprime_ratio, det
    tabstat total_count, stat(sum)
	destring zip, replace
	save $rawdatadir/subprime_estimate_all_years, replace
    
end/*%>*/

capture program drop make_hamp_data/*%<*/
program define make_hamp_data

    import delimited $rawdatadir/receipt_info.csv, clear varnames(nonames)
    forvalues num=2/105 {
        replace v`num' = "y" + v`num' if _n==1
        local name = strtoname(v`num'[1])
        ren v`num' `name'
    }
    ren yprop* prop*
    drop in 1
    drop v1  
    drop if prop=="NA"
    destring *, replace 
    reshape long y, i(prop_geoc) j(date_m_string) string
    ren (y prop) (loan_count_hamp_treas msa)
    split date_m_string, parse("_")
    destring date_m_string*, replace
    gen date_m = ym(date_m_string1, date_m_string2)
    format date_m %tm
    drop date_m_string*
    preserve
        collapse (sum) loan_count, by(msa)
        ren msa cbsa
        tempfile receipt1
        save `receipt1'
    restore
    keep if date_m>=tm(2009m4) & date_m<=tm(2013m6)
    collapse (sum) loan_count, by(msa)
    ren msa cbsa
    ren loan_count loan_count_short
    merge 1:1 cbsa using `receipt1', keep(3) nogen
    tempfile receipt
    save `receipt'


    import excel $rawdatadir/ZIP_CBSA_032017.xlsx, clear allstring firstrow
    ren ZIP zip
    ren CBSA cbsa_orig
    gen cbsa = cbsa_orig
    destring cbsa zip cbsa_orig, replace
    replace cbsa = 14460 if inlist(cbsa, 14454, 15764, 40484) 
    replace cbsa = 16980 if inlist(cbsa, 16974, 20994, 23844, 29404)
    replace cbsa = 19100 if inlist(cbsa, 19124, 23104) 
    replace cbsa = 19820 if inlist(cbsa, 19804, 47664)
    replace cbsa = 31080 if inlist(cbsa, 11244, 31084)
    replace cbsa = 33100 if inlist(cbsa, 22744, 33124, 48424)
    replace cbsa = 35620 if inlist(cbsa, 20524, 35004, 35084, 35614)
    replace cbsa = 37980 if inlist(cbsa, 15804, 33874, 37964, 48864)
    replace cbsa = 41860 if inlist(cbsa, 36084, 41884, 42034)
    replace cbsa = 42660 if inlist(cbsa, 42644, 45104)
    replace cbsa = 47900 if inlist(cbsa, 43524, 47894)
    replace cbsa = 0 if cbsa==99999 
    merge m:1 cbsa using `receipt', keep(3) nogen
    gsort -loan_count_hamp_treas
    tempfile crosswalk
    save `crosswalk'
	
	use $rawdatadir/zip_by_year_fha_hmda.dta, clear
    keep if inlist(year, 2004, 2005, 2006, 2007)
    collapse (sum) total_count, by(zip)
    destring zip, replace
    tempfile zip_hmda
    save `zip_hmda'

    joinby zip using `crosswalk', unmatched(using)

    duplicates tag zip, gen(dup)
    destring RES_RATIO, replace
    egen max_res_ratio   = total(RES_RATIO), by(zip)
    gen cbsa_misalign    = (cbsa!=cbsa_orig)
    egen tot_cbsa_misal  = max(cbsa_misalign), by(zip) 
    replace loan_count_hamp_treas = 0 if cbsa==0
    replace loan_count_short = 0 if cbsa==0
	
	egen max_total_loans       = max(loan_count_hamp_treas), by(zip)
    egen max_total_loans_short = max(loan_count_short), by(zip) 
    gsort -loan_count_hamp_treas
    collapse (first) loan_count_hamp_treas cbsa loan_count_short (max) ///
	    max_total_loans max_total_loans_short total_count, by(zip)
    save $rawdatadir/hamp_data_alloc_test, replace
	
	tempfile tomerge
	use $analysis/zip_longdiffs, clear
    xtile price_level=DQprice_mean2008, nq(10) 
    keep zip sandstateMS price_level log_pop log_avgagi pctunemp ///
	    card_subprime exante_share
    save `tomerge'
	
	use "$rawdatadir/zip_reallocation_monthly_EZ0314", clear
    drop exante_share _merge
    merge m:1 zip using `tomerge', keep(1 3) nogen
	merge m:1 zip using $rawdatadir/hamp_data_alloc_test, ///
	    keep(1 3)
    gen imputed_zero = (_merge==1)
    replace max_total_loans = 0 if imputed_zero==1 & missing(max_total_loans)
    replace loan_count_short = 0 if imputed_zero==1 & missing(loan_count_short)
    collapse (max) loan_count_short max_total_loans imputed_zero total_count, ///
        by(zip cbsa)
    save $rawdatadir/hamp_data_collapse, replace
	
	use $rawdatadir/zip_by_year_fha_hmda.dta, clear
	ren total_count total_count_new
	keep if inlist(year, 2004, 2005, 2006, 2007)
    collapse (sum) total_count, by(zip)
    destring zip, replace
	merge 1:1 zip using $rawdatadir/hamp_data_collapse, keep(2 3) nogen
	keep zip total_count_new max_total_loans imputed_zero cbsa loan_count_short
	ren (total_count_new max_total_loans loan_count_short) (total_loan_count total_hamp_receipt total_hamp_receipt_short)
	tempfile final_merge
	save `final_merge'
	
	import delimited $rawdatadir/cbsa-est2016-alldata-census.csv, clear
	keep cbsa stcou census lsad mdiv
	keep if missing(mdiv) & missing(stcou)
	drop if missing(cbsa)
    drop mdiv stcou lsad 
	merge 1:m cbsa using `final_merge', keep(2 3) nogen
	saveold $rawdatadir/hamp_data_collapse_final, replace version(13)

end/*%>*/

capture program drop build_extra_covariates/*%<*/
program define build_extra_covariates

    create_subprime_var
	create_fha_exp_var
	create_gse_exposure
    create_fha_takeup
	make_hamp_data

end/*%>*/

*******************************************************************************
* Repeat sales and hedonic price index code for appendix analysis
*******************************************************************************
capture program drop import_repeatsales /* %< */
program define import_repeatsales
    syntax anything(name=pricedir)

    * Load the main dataset pulled from SQL server. We run
    * a few different specifications.
    tempfile orig
    import delimited `pricedir'/dataquick_rptsale_20170110.csv, clear
    save `orig'

    * Justification for this is that CoreLogic uses SF homes only, but we don't
    * know how they filter on SF homes. 
    use `orig', clear
    foreach var of varlist price_c price_f {
        drop if (`var' < 125e3 | `var' > 5e6) & `var' != .
    }
    rpt_index_main all, btm_q(5) top_q(6) variables(1) p(0.001 99.999) ///
        trimdv(.01) notransfilter qtype(EZv19) merge_sgl

end /* %> */

capture program drop import_singlesales /* %< */
program define import_singlesales
    syntax anything(name=pricedir)

    /*
       This program processes the transaction-level data of all single
       sales in DataQuick (properties with only one nontrivial transaction).
       Price indices are created from these data and the data are tabulated
       in other ways.
    */
    import delimited `pricedir'/dataquick_sglsale_20170110.csv, clear
    gen mdate = mofd(date(date_current, "YMD"))
    preserve

    * This collapse records, by month, all transactions made and the median and
    * mean nominal values for houses in that period.
    collapse (count) price_count=price_current ///
        (median) price_current (mean) price_curmean=price_current ///
        if mdate >= 40*12, by(mdate)

    * Generate mean and median indices for single sales
    * (NOT all sales)
    gen price_adj = price_current/(price_current[1])*100
    gen pricemean_adj = price_curmean/(price_curmean[1])*100
    gen month = mod(mdate, 12) + 1
    foreach var of varlist price*_adj* {
        bys month: egen `var'_mfe = mean(`var')
        sum `var', meanonly
        gen `var'_sa = `var' - `var'_mfe + `r(mean)'
    }
    saveold $analysis/DQ_sglsales_agg, replace

    * In addition, cut the single sales data a few ways...
    * 1) Single sales by house age.
    restore
    drop if missing(yr_built) | yr_built <= 1900
    gen year = year(dofm(mdate))
    drop if yr_built > year
    collapse (count) yr_count=price_current if year >= 2000, ///
         by(year yr_built)
    saveold $analysis/DQ_sglsales_agg_built, replace

end /* %> */

capture program drop rpt_indexprep /* %< */
program define rpt_indexprep
        syntax, Percentiles(numlist) [noDIFFFILTER noTRANSFILTER trimdv(real 0)]

        /* Processes data cuts and trims before the actual regression is run. */
        by property: gen mdate_diff = mdate_current[_n] - mdate_current[_n-1]
        foreach var of varlist price_f price_c {
            gen trim_`var' = .

            * Restriction to transactions made at least 6 months between each
            * other is a S&P Index guideline (mdate_diff is an approximation?)
            if ("`difffilter'" == "") {
                replace `var' = . if mdate_diff < 6
            }

            * Allow at most (1,2) - (2,3) pairs for one property
            if ("`transfilter'" == "") {
                replace `var' = . if trans > 3 & !missing(trans)
            }
        }
        
        levelsof trim_group, local(groups)
        gen trim_depvar = .
        gen rfilter = .
        set seed 10110

        * Trim raw prices.
        foreach grp of local groups {
            replace rfilter = runiform() if trim_group == `grp'
            foreach var of varlist price_f price_c {
                * A very specific trim range
                _pctile `var' if trim_group == `grp', p(`percentiles')
                replace trim_`var' = 1 if trim_group == `grp' & ///
                    (`var' <= `r(r1)' | `var' >= `r(r2)') & !missing(`var')
            }
        }

        gen depvar = log(price_future) - log(price_current)

        * Trim regression LHS.
        foreach grp of local groups {
            if ("`trimdv'" != "0") {
                local upper = 100*(1-`trimdv')
                local lower = 100*`trimdv'
                _pctile depvar if trim_group == `grp', p(`lower' `upper')
                replace trim_depvar = 1 if trim_group == `grp' & ///
                    (depvar <= `r(r1)' | depvar >= `r(r2)') & !missing(depvar)
            }
        }

        gen cumtime = mdate_future - mdate_current

end /* %> */

capture program drop rpt_runindex /* %< */
program define rpt_runindex
    syntax [namelist] [if], [qtype(string) outdir(string) Percentiles(numlist) ///
        noDIFFFILTER noTRANSFILTER noTRIM noREG base(integer 360) trimdv(real 0)]

    if "`if'" != "" keep `if'
    if "`trim'" == "" {
        if ("`namelist'" != "all") {
            egen trim_group = group(`namelist')
        }
        else {
            gen trim_group = 1
        }

        rpt_indexprep, p(`percentiles') `difffilter' `transfilter' trimdv(`trimdv')

        foreach var of varlist price_f price_c depvar {
            replace `var' = . if trim_`var' == 1
        }

        preserve
        keep if missing(price_current)
        keep date_current property
        gen trim_ind = 1
        saveold $analysis/DQ_rptsales_filtered_`qtype'_agg, replace
        restore

        keep if rfilter < 0.75
        drop trim_* rfilter
    }
    if "`reg'" == "" {
        index_regs depvar, starttime(mdate_current) endtime(mdate_future) ///
            cumtime(cumtime) base(`base') cs
    }

end /* %> */

capture program drop index_regs /* %< */
program define index_regs
    syntax varname, starttime(varname) endtime(varname) base(integer) [cumtime(varname) CS]

    /* This program creates the design matrix for the regression
       that retrieves monthly repeat-sale price indices. It is
       a little more general in that variables describing arbitrary
       lengths of time are entered as options.
       The index is normalized at 100 at a base date. See Silverstein
       (Philly Fed 2014) for a description of the procedure. */
    tempfile orig
    save `orig'
    tempvar resid cumtime2 res_wgt

    sum `starttime', meanonly
    replace depvar = depvar + log(100) if `starttime' == `base' //
    replace depvar = log(100) - depvar if `endtime' == `base' //
    local init = `r(min)' + 1

    sum `endtime', meanonly
    local end = `r(max)'

    display "Index range: `init'-`end'"

    forv i=`init'/`end' {
        gen DUMMY_`i' = (`endtime'  == `i')
        qui replace DUMMY_`i' = -1 if `starttime' ==  `i'
    }
    capture drop DUMMY_`base'
    reg depvar DUMMY_* if `endtime' <= `end', nocons

    * Weighted least squares under the FHFA/OFHEO method
    if "`cs'" != "" {
        predict `resid', res
        replace `resid' = `resid'^2
        gen `cumtime2' = `cumtime'^2
        sum `cumtime', d
        reg `resid' `cumtime' `cumtime2', nocons
        
        matrix var_coef = e(b)
        local beta_1 = var_coef[1,1]
        local beta_2 = var_coef[1,2]
        gen `res_wgt' = sqrt(`beta_1'*`cumtime' + `cumtime2'*`beta_2')
        reg `varlist' DUMMY_* if `endtime' <= `end' [pw=`res_wgt'], nocons
    }
    matrix log_ind = e(b)
    * Regression should be on log scale, so transform coefs
    mata: st_matrix("ind", exp(st_matrix("log_ind"))')
    scalar startdate = `init' - 2
    use `orig', clear

end /* %> */

capture program drop rpt_dataprep /* %< */
program define rpt_dataprep
    syntax, btm_q(integer) top_q(integer) variables(integer)

    gen mdate_current = mofd(date(date_current, "YMD"))
    gen mdate_future = mofd(date(date_future, "YMD"))
    gsort property -date_current

    * Look at all transactions from 2000 onwards and some transactions from 1990 -
    * properties which were sold once since 1990 but must have had its next
    * transaction after 2000.
    keep if mdate_future >= 40*12 & mdate_current >= 30*12
    * Count the number of transactions this property went through in the dataset
    sort property date_current
    by property: egen t = count(price_current) if mdate_future >= 40*12
    by property: egen trans = max(t)
    replace trans = 10 if trans > 10
    drop t
    * Tabulate top-coded transactions data
    tab trans

    * Either divide bins on both the price and exposure axes, or just split it
    * on the exposure axes.
    split_bins bins, btm_q(`btm_q') top_q(`top_q') variables(`variables')

end /* %> */

capture program drop rpt_rescale /* %< */
program define rpt_rescale
    syntax, base(integer)

    local baseval = 480 - `base' + 1
    clear
    svmat ind
    gen mdate = startdate + _n
    gen ind = ind1/ind1[`baseval']*100 // denom is 2000 value
    drop if mdate < 480 // 2000
    keep ind mdate

end /* %> */

capture program drop split_bins /* %< */
program define split_bins
    syntax anything(name=newvar), btm_q(integer) top_q(integer) variables(integer)

    /* Subdivide the sample into ZIP bins, which are delineated according
       to the level of FTHB exposure in the ZIP as well as its 2008 price index.
    */

    capture drop `newvar'
    * Either divide bins on both the price and exposure axes, or just split it
    * on the exposure axes.
    if `variables' == 2 {
        gen `newvar' = cond(price_level <= `btm_q' & treatment_level <= `btm_q', 1, cond( ///
                            price_level <= `btm_q' & treatment_level >= `top_q', 2, cond( ///
                            price_level >= `top_q' & treatment_level <= `btm_q', 3, cond( ///
                            price_level >= `top_q' & treatment_level >= `top_q', 4, -1))))
        capture label def groups 1 "Lo Price/Lo Exposure" 2 "Lo Price/Hi Exposure" ///
            3 "Hi Price/Lo Exposure" 4 "Hi Price/Hi Exposure"
    }
    else if `variables' == 1 {
        gen `newvar' = cond(treatment_level <= `btm_q', 1, cond( ///
                            treatment_level >= `top_q', 2, -1))
        capture label def groups 1 "Lo Exposure" 2 "Hi Exposure"
    }
    else {
        * Special case: estimate separate indices by exposure decile
        rename treatment_level `newvar'
    }
    tab `newvar'
    label val `newvar' groups

end /* %> */

capture program drop rpt_index_main /* %< */
program define rpt_index_main
    syntax anything(name=trimvar), btm_q(integer) top_q(integer) ///
    variables(integer) Percentiles(numlist) [qtype(string) outdir(string) ///
    noDIFFFILTER noTRANSFILTER merge_sgl trimdv(real 0)]

    * Master program for construction of the repeat sales price index.
    * The process has many degrees of freedom when it comes to data processing,
    * so the most important processing options have been included as options.

    tempfile indices

    * First, create a file in which each separate column
    * records the index for each bin of ZIPs
    preserve
    clear
    set obs 164
    gen mdate = 480 + _n
    save `indices', replace
    restore

    * Load the repeat sale file. This is a pull of every
    * DQ transaction in our period of interest, but just
    * including data on that transaction's value, then the
    * subsequent resale on that same property.
    * See pricepull_01102017.py for details
    rpt_dataprep, btm_q(`btm_q') top_q(`top_q') variables(`variables')
    gen year = year(dofm(mdate_current))
    gen quarter = quarter(dofm(mdate_current))
    gen month = mod(mdate_current, 12) + 1
    preserve

    if "`merge_sgl'" != "" {
        * The next two blocks of code will not function unless
        * import_singlesales is run first.
        * Merge to obtain proportion of single sale homes
        capture confirm file $analysis/DQ_sglsales_agg.dta
        if _rc == 0 {
            collapse (count) price_count_rpt=price_current, by(mdate_current)
            rename mdate_current mdate
            merge 1:1 mdate using $analysis/DQ_sglsales_agg, keep(3) ///
                keepusing(price_count)
            drop _m
            saveold $analysis/DQ_comp_agg, replace
            restore, preserve
        }
        * Merge to obtain housing age distributions for house types
        capture confirm file $analysis/DQ_sglsales_agg_built.dta
        if _rc == 0 {
            collapse (count) yr_count_rpt=price_current if year >= 2000, ///
                by(year yr_built)
            merge 1:1 year yr_built using $analysis/DQ_sglsales_agg_built, keep(3)
            sort year yr_built
            saveold $analysis/DQ_comp_agg_built, replace
            restore, preserve
        }
    }

    restore
    rpt_runindex `trimvar', p(`percentiles') qtype(`qtype') noreg ///
        `difffilter' `transfilter' trimdv(`trimdv')
    preserve

    * For the aggregate index, run a straight regression on aggregated data
    rpt_runindex, notrim
    tempfile index_temp
    rpt_rescale, base(360)
    save `index_temp'
    use `indices', clear
    merge 1:1 mdate using `index_temp', keep(3)
    drop _m
    rename ind (index0)
    sum
    save `indices', replace

    * Comment out for speed.

    * For each ZIP bin, get log differences of prices
    * (taking out the property FE), prune data a little,
    * then get the house index
    restore, preserve
    qui sum bins
    local max = r(max)
    forv i=1/`max' {
        rpt_runindex if bins == `i', notrim
        tempfile index_temp
        rpt_rescale, base(360)
        save `index_temp'
        use `indices', clear
        merge 1:1 mdate using `index_temp', keep(3)
        drop _m
        rename ind (index`i')
        sum
        save `indices', replace
        restore, preserve
    }
    restore

    use `indices', clear
    * Data after reshape has keys date, group, like other price datasets
    reshape long index, i(mdate) j(group)
    format mdate %tm

    saveold $analysis/DQ_rptsales`qtype'_agg, replace

end /* %> */

capture program drop import_zip_mean_prices /* %< */
program define import_zip_mean_prices
    syntax anything(name=pricedir)

    tempfile rpt sgl

    import delimited `pricedir'/dataquick_rptsale_20170110.csv, clear
    keep price_current zip date_current
    gen mdate = mofd(date(date_current, "YMD"))
    drop if price_c < 1e3 | price_c > 5e6
    gen here = 1

    collapse (mean) mean_price_aggR=price_c (sum) count_aggR=here, by(zip mdate)
    save `rpt'

    import delimited `pricedir'/dataquick_sglsale_20170110.csv, clear
    keep price_current zip date_current
    gen mdate = mofd(date(date_current, "YMD"))
    drop if price_c < 1e3 | price_c > 5e6
    gen here = 1

    collapse (mean) mean_price_aggS=price_c (sum) count_aggS=here, by(zip mdate)
    save `sgl'

    merge 1:1 zip mdate using `rpt'
    recode mean_price_agg* count_agg* (. = 0)
    gen count_agg = count_aggR + count_aggS
    gen mean_price_agg = (count_aggR * mean_price_aggR + count_aggS * mean_price_aggS)/count_agg
    drop _merge

    saveold $analysis/DQ_zip_mean_price_agg.dta, replace

end /* %> */

capture program drop build_repeatsales/*%<*/
program define build_repeatsales

    import_singlesales $rawdatadir
    import_repeatsales $rawdatadir
    import_zip_mean_prices $rawdatadir

end/*%>*/

capture program drop import_hedonic/*%<*/
program define import_hedonic

    * Hedonic code from Caleb, based on DeFusco and Stroebel papers.
    cap log close 

    /* Import the hedonic price dataset */
    import delimited "$rawdatadir/dquick_hedonic_full.csv", clear
    ren (v1-v22) (sr_unique_id sr_date_transfer sr_val_transfer distress_indicator ///
        sa_nbr_bedrms sa_architecture_code sa_nbr_stories sa_fin_sqft_tot sa_bldg_sqft ///
        sa_sqft sa_sqft_assr_tot sa_patio_porch_code sa_pool_code sa_cool_code sa_yr_blt_effect ///
        sa_nbr_bath sa_heat_code sa_lotsize sa_site_zip sa_yr_blt mm_fips_muni_code mm_fips_state_code)
    destring sa_sqft sa_lotsize sa_nbr_bath sa_yr_blt* sr_val_transfer, replace ig("\N")
    gen date_d = date(sr_date_transfer, "YMD")
    format date_d %td
    gen mmonth = month(date_d)
    gen yyear  = year(date_d)
    gen date_q = qofd(date_d)
    gen date_m = mofd(date_d)
    format date_q %tq
    drop if yyear<2003 | yyear>2012
    egen group = group(mm_fips_muni mm_fips_state)
    drop if sr_val_transfer<=0 | missing(sr_val_transfer)
    count
    egen count = count(sr_val_transfer), by(group)
    drop if count<5000
    count

    levelsof group, local(county)
    foreach var in sa_sqft sa_lotsize {
        winsor `var' , p(0.05) gen(`var'_w)
    }

    gen sr_val_transfer_w = .
    foreach county in `county' {
        di "`county'"
        winsor sr_val_transfer if group==`county', p(0.05) gen(temp)
        replace sr_val_transfer_w = temp if group==`county'
        drop temp
    }
    gen quad_sqft    = sa_sqft^2
    gen quad_lotsize = sa_lotsize^2
    gen log_val      = log(sr_val_transfer_w)

    xtile sqft_ventile = sa_sqft, n(20)
    xtile lot_decile   = sa_lotsize, n(10)

    gen age            = yyear - sa_yr_blt
    drop if age>=250
    count
    drop if age<=-1
    count
    destring sa_nbr_bed, replace ig("\N")
    drop if sa_nbr_bed==0 | sa_nbr_bath==0
    count
    drop if sa_nbr_bed>=16.5 | sa_nbr_bath>=16.5
    count
    xtile age_ventile = age, n(20)
    xtile bed_decile  = sa_nbr_bed, n(10)
    saveold "$analysis/test_price_full_zipsample", replace 

    /* Now, subset to zip list */
    import delimited "$rawdatadir/zip_list.csv", clear
    ren zip sa_site_zip
    merge 1:m sa_site_zip using "$analysis/test_price_full_zipsample"
    keep if _merge==3
    count
    drop _merge

    gen bed_round = round(sa_nbr_bed)
    drop if bed_round>=17
    gen bath_round = round(sa_nbr_bath)
    drop if bath_round>=17

    /* This is the estimate */
    reghdfe log_val i.lot_decile i.age_ventile i.bed_round i.bath_round i.sqft_ventile, absorb(fe_month = i.date_m#i.sa_site_zip)
    gen hpi_hedonic = 100*exp(fe_month)
    collapse (max) hpi* fe_month, by(sa_site_zip date_m)

    export delimited using "$analysis/hpi_indval_clichange_reduced_samp2_20181005_test.csv", replace

end/*%>*/

capture program drop build_hedonic/*%<*/
program define build_hedonic

    import_hedonic

end/*%>*/

*******************************************************************************
* Replication package data sets
*******************************************************************************
capture program drop build_replication_csv/*%<*/
program define build_replication_csv

    tempfile out_csv
    setup_exposure_robust xsection 
    save `out_csv'

    load_analysis_data longdiff_cont
    keep zip exante_share
    * To match units from the robustness exposures
    replace exante_share = .01 * exante_share
    rename exante_share exante_share_baseline
    merge 1:1 zip using `out_csv', keep(1 3) nogen
    
    export delimited using "$analysis/fthb_exposure_20190201.csv", replace

end/*%>*/

*******************************************************************************
* Main
*******************************************************************************
capture program drop main
program define main
	log using "$outputdir/make-fthb.log", replace

    ****************************************************************************
    * Geographic crosswalks.
    ****************************************************************************
    build_crosswalks

    ****************************************************************************
    * Covariates from IRS, Census, Equifax, plus exposure variables
    ****************************************************************************
	build_covariates
    build_exposure
	build_extra_covariates
	
    ****************************************************************************
    * Price data from Dataquick, CoreLogic, FHFA
    ****************************************************************************
    build_rawprices
    build_repeatsales
    build_hedonic
	
	****************************************************************************
    * Home sales from Dataquick, including default data from FHA subsample
    ****************************************************************************
    build_sales
    build_defaulters

    ****************************************************************************
    * Small datasets.
    ****************************************************************************
	build_nar
    build_google
    build_claims
    build_age
	
    ****************************************************************************
    * Main analysis datasets
    ****************************************************************************
    build_hsales
    build_prices_20170208 zip
	build_bedrooms
    build_construction
	
    ****************************************************************************
    * Derived datasets
    ****************************************************************************
	build_distress_counts
    build_reallocation
	build_longdiffs
	build_splitclaims

    build_replication_csv

    log close
end
