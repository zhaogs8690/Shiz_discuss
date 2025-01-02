*******************************************************************************
*   Title: programs.do
* Purpose: Project specific programs
*   Owner: Eric Zwick
* Authors: David Berger, Nicholas Turner, Eric Zwick
*     RAs: Tom (Tianfang) Cui, Caleb Wroblewski
*    Date: 2019-02-01
*******************************************************************************

* Formatting for Graphs
global graphconfig "graphregion(color(white)) bgcolor(white)"

*******************************************************************************
* Helpers
*******************************************************************************
capture program drop coeff_xml_write/*%<*/
program define coeff_xml_write
    syntax, coeff(real) se(real) fname(string) [n(real 0) r2(real 0) label(string)]
    * Purpose: Write coefficient to general XML file for multiple output purposes.

    !echo "<estimate>" >> `fname'
    !echo "  <coeff>`coeff'</coeff>" >> `fname'
    !echo "  <se>`se'</se>" >> `fname'
    * Optional fields.
    if `n' != 0 {
        !echo "  <N>`n'</N>" >> `fname'
    }
    if `r2' != 0 {
        !echo "  <r2>`r2'</r2>" >> `fname'
    }
    if "`label'" != "" {
        !echo "  <label>`label'</label>" >> `fname'
    }
    !echo "</estimate>" >> `fname'
    display "COEFF XML WRITE complete"
end

capture program drop coeff_xml_open
program define coeff_xml_open
    syntax, fname(string) [replace(real 1)]

    * In most cases, we want to overwrite the file, so that's the default.
    if `replace' != 0 {
        !rm `fname'
        !echo "<doc>" > `fname'
    }
    display "COEFF XML OPEN complete"
end

capture program drop coeff_xml_close
program define coeff_xml_close
    syntax, fname(string)
    !echo "</doc>" >> `fname'
    !cat `fname'
    display "COEFF XML CLOSE complete"
end

capture program drop coeff_tex_compile
program define coeff_tex_compile
    syntax, input(string) output(string) [sigfig(integer 3)]
    !python $localcodedir/py/coeff_xml2tex2.py `input' `output' `sigfig' 
    !cat `output'
    display "COEFF XML COMPILE complete"
end/*%>*/

capture program drop trim_tails /* %< */
program define trim_tails
    syntax varlist, level(real) [TRIM]

    foreach x of varlist `varlist' {
        rename `x' `x'_ut
        winsor `x'_ut, generate(`x') p(`level')
        gen `x'_tr = cond(`x' == `x'_ut, `x'_ut, .)
        if "`trim'" != "" rename (`x'_tr `x') (`x' `x'_w)
    }

end /* %> */

capture program drop impose_bounds/*%<*/
program define impose_bounds

    ** To make upper and lower bounds optional, use fake numbers.
    syntax varname, [upper(real 9999) lower(real 9999)]
    rename `varlist' `varlist'_ub
    quietly gen `varlist' = `varlist'_ub
    if `upper' != 9999 {
        quietly replace `varlist' = . if `varlist' > `upper'
    }
    if `lower' != 9999 {
        quietly replace `varlist' = . if `varlist' < `lower'
    }

end/*%>*/

capture program drop set_controls /* %< */
program define set_controls
    syntax anything(name=control)
	
    local controls1 STUB_pop STUB_avgagi STUB_unemp
    local controls2 log_pop pctunemp log_avgagi card_subprime ///
        subprime_ratio fha_expansion_exposure gse_ratio hamp_exposure

    if "`control'" == "spline" {
        global controls `controls1' 
    }
	
    else if "`control'" == "ez2019" {
        global controls `controls2'
    }

end/*%>*/

capture program drop load_analysis_data/*%<*/
program define load_analysis_data
        
	tempfile subprime
    use "$rawdatadir/subprime_estimate_all_years.dta", clear
    keep if inrange(year,2004,2007)
    collapse (mean) subprime_ratio [aw=total_count], by(zip)
    save `subprime'
	
    tempfile fha_expansion
    use "$rawdatadir/hmda_fha_exp_estimate.dta", clear
    keep zip new_exp_ratio exp_ratio 
    rename new_exp_ratio fha_expansion_exposure
    rename exp_ratio fha_total_exposure
    save `fha_expansion'

	tempfile gse
    use "$rawdatadir/zip_by_year_gse.dta", clear
    keep if inrange(year,2004,2007)
    gen gse_ratio = gse_indicator/total_count
    collapse (mean) gse_ratio [aw=total_count], by(zip)
    destring zip, force replace
    save `gse'
	
	tempfile fha_takeup
    use "$rawdatadir/zip_by_year_fha_hmda.dta", clear
    destring zip, force replace
    gen post = year >= 2008
    collapse (sum) total_count *fha_loans, by(zip post)
    bys zip: gen count = _N
    drop if count != 2
    drop count
    sort zip post
    gen fha_share = fha_loans/total_count 
    gen fha_expansion_share = expansion_fha_loans/total_count
    by zip: gen delta_fha_share = fha_share[2] - fha_share[1]
    by zip: gen delta_fha_expansion_share = fha_expansion_share[2] - fha_expansion_share[1]
    by zip: gen initial_fha_share = fha_share[1]
    by zip: gen initial_fha_expansion_share = fha_expansion_share[1]
    keep if post == 1
    keep zip total_count fha_loans initial_fha_*share fha_share delta_fha_*share 
    rename total_count total_post_loans
    rename fha_loans fha_post_loans
    rename fha_share final_fha_share
    save `fha_takeup'

    * Goal here is to try to allocate CBSA level HAMP claim counts to ZIP level while
    * avoiding mechanical correlations driven by the method of imputation.
	tempfile hamp
    use "$rawdatadir/hamp_data_collapse_final", clear
    bys cbsa: egen cbsa_loans = total(total_loan_count)
    gen zip_hamp_ct = total_loan_count/cbsa_loans * total_hamp_receipt_short
    gen loan_share = total_loan_count/cbsa_loans
    keep zip zip_hamp_ct loan_share total_hamp_receipt_short cbsa census2010pop
    save `hamp'
    
	use $analysis/covariates_zip, clear
    keep zip homeowners_2007 pop_2007 ownerocc_2007 filingunits_2007 totalunits_2007
    merge 1:1 zip using `hamp', keep(1 3) nogen
    bys cbsa: egen total_occ = total(ownerocc_2007)
    bys cbsa: egen total_fu = total(filingunits_2007)
	
    gen hamp_exposure = ((ownerocc_2007/total_occ) * total_hamp_receipt_short)/homeowners_2007

    replace hamp_exposure = . if hamp_exposure > 1
    keep zip hamp_exposure
    save `hamp', replace

    if ("`1'" == "nar") {
        use $analysis/nar_sales, clear
        keep if mdate >= ym(2004,1) & mdate <= ym(2013,9)
    }
	
    if ("`1'" == "distress") {
        use $analysis/distress_counts, clear
    }

    if ("`1'" == "hsales") {
        use $analysis/zip_housing_exante_cts, clear
        keep if exante_share != .
        keep if mdate >= ym(2004,1) & mdate <= ym(2013,9)
    }
	
    if ("`1'" == "hsales_cont") {
	    tempfile highltv
		/* From Consumer Credit Panel, for referee response */
        use "$rawdatadir/rev_v2_collapse", clear
        gen highltv_share = ltv_gt80_jan2010 / count_mortgages 
        replace highltv_share = 0 if highltv_share > 1
        keep zip highltv_share
        save `highltv'

        load_analysis_data hsales
        merge m:1 zip using `subprime', keep(1 3) nogen
        merge m:1 zip using `fha_expansion', keep(1 3) nogen
        merge m:1 zip using `gse', keep(1 3) nogen
        merge m:1 zip using `hamp', keep(1 3) nogen
        merge m:1 zip using `highltv', keep(1 3) nogen
    }
	
    if ("`1'" == "google") {
        use $analysis/google, clear
    }
	
    if ("`1'" == "geocounty") {
        use $analysis/covariates_county_final, clear
    }
	
    if ("`1'" == "geozip") {
        use $analysis/covariates_zip_final, clear
    }
	
    if ("`1'" == "zipsample") {
        use $analysis/zip_exante_nostd, clear
        merge 1:1 zip using $analysis/zip_longdiffs, keep(1 3) ///
            keepusing(d_hp log_pop log_avgagi pctunemp zip_cluster card_subprime DQprice_mean2008) nogen
        merge 1:1 zip using $analysis/covariates_zip, keep(1 3) ///
            keepusing(filingunits_2007) nogen
        merge 1:1 zip using $analysis/covariates_zip_final, keep(1 3) ///
            keepusing(claimsA_scaled) nogen

        xtile price_level=DQprice_mean2008, nq(10) 
    }
	
    if ("`1'" == "zipsample_cont") {
        load_analysis_data zipsample
        merge 1:1 zip using `subprime', keep(1 3) nogen
        merge 1:1 zip using `fha_expansion', keep(1 3) nogen
        merge 1:1 zip using `gse', keep(1 3) nogen
        merge 1:1 zip using `hamp', keep(1 3) nogen
    }
	
    if ("`1'" == "claims") {
        use $analysis/natl_claims, clear
    }
	
	if ("`1'" == "split_claims") {
	    /* Import from IRS collapse. */
	    insheet using $rawdatadir/split_claims.csv, comma names clear
        gen mdate = ym(year, month)
        format mdate %tm
	}
	
    if ("`1'" == "logspec") {
        tempfile tomerge
        use $analysis/zip_longdiffs, clear
        xtile price_level=DQprice_mean2008, nq(10) 
        keep zip sandstateMS sandstate price_level log_pop log_avgagi pctunemp card_subprime exante_share
        save `tomerge'

        * TODO: clean
        use "$rawdatadir/zip_reallocation_monthly_EZ0314", clear
        drop exante_share _merge
        merge m:1 zip using `tomerge', keep(1 3) nogen
    }

    if ("`1'" == "logspec_cont") {
        load_analysis_data logspec
        merge m:1 zip using `subprime', keep(1 3) nogen
        merge m:1 zip using `fha_expansion', keep(1 3) nogen
        merge m:1 zip using `gse', keep(1 3) nogen
        merge m:1 zip using `hamp', keep(1 3) nogen
    }
	
    if ("`1'" == "age") {
        use $analysis/age_counts, clear
    }
	
    if ("`1'" == "agezip") {
        use $analysis/age_zip_counts, clear
        merge m:1 zip using $analysis/zip_longdiffs, keep(3) nogen ///
            keepusing(totalhsales_base log_pop log_avgagi ///
                      pctunemp card_subprime zip_cluster)
        merge m:1 zip using $analysis/zip_exante_nostd, keep(3) nogen ///
            keepusing(exante_share)
        merge m:1 zip using $analysis/covariates, keep(1 3) nogen ///
            keepusing(filingunits_2000 filingunits_2007)
        keep if exante_share != .
    }
	
    if ("`1'" == "agetdec") {
	    /* From IRS collapse */
        use $rawdatadir/AGE_YR_tdec_incdec, clear
    }
	
    if ("`1'" == "longdiff") {
        use $analysis/zip_longdiffs, clear
        keep if exante_share != .
        xtile price_level=DQprice_mean2008, nq(10) 

        * Adjust SD for raw price growth in appendix
        sum delta_pricegrowth
        local dpg_sd = `r(sd)'
        sum delta_pricegrowth_raw
        gen delta_pricegrowth_raw_sdnorm = delta_pricegrowth_raw * `dpg_sd' / `r(sd)'
    }

    if ("`1'" == "longdiff_cont") {
        load_analysis_data longdiff
        merge 1:1 zip using `subprime', keep(1 3) nogen
        merge 1:1 zip using `fha_expansion', keep(1 3) nogen
        merge 1:1 zip using `gse', keep(1 3) nogen
        merge 1:1 zip using `hamp', keep(1 3) nogen
    }
	
    if ("`1'" == "prices_fha") {
        use $analysis/zip_FHFA_prices, clear
        keep zip year hpi res beta annualchange 
        merge m:1 zip using $analysis/zip_longdiffs, keep(3) nogen ///
            keepusing(totalhsales_base log_pop log_avgagi card_subprime ///
                      pctunemp zip_cluster e*_share DQprice_mean2008)
        keep if exante_share != .
    }

    if ("`1'" == "prices_fha_cont") {
        load_analysis_data prices_fha
        merge m:1 zip using `subprime', keep(1 3) nogen
        merge m:1 zip using `fha_expansion', keep(1 3) nogen
        merge m:1 zip using `gse', keep(1 3) nogen
        merge m:1 zip using `hamp', keep(1 3) nogen
    }
	
    if ("`1'" == "defaulters") {
        use $analysis/DQ_defaulters, clear
    }
	
	if ("`1'" == "zip_panel") {
        use $analysis/zipcollapse_panel_20171122.dta, clear
        merge m:1 zip using $analysis/zip_longdiffs, keep(3) nogen ///
            keepusing(totalhsales_base log_pop log_avgagi ///
                      pctunemp card_subprime zip_cluster)
        merge m:1 zip using $analysis/zip_exante_nostd, keep(3) nogen ///
            keepusing(exante_share)
        merge m:1 zip using $analysis/covariates, keep(1 3) nogen ///
            keepusing(filingunits_2000 filingunits_2007)
        keep if exante_share != .
        rename year tax_yr
    }
	
	if ("`1'" == "fha_xp") {
        load_analysis_data zipsample
        merge 1:1 zip using `fha_expansion', keep(1 3) nogen
        merge 1:1 zip using `fha_takeup', keep(1 3) nogen	
	}
	
    if ("`1'" == "bedrooms") {
        use $analysis/zip_housing_exante_bedrooms, clear
        keep if exante_share != .
        keep if mdate >= ym(2004,1) & mdate <= ym(2013,9)
    }

    if ("`1'" == "bedrooms_cont") {
        load_analysis_data bedrooms
        merge m:1 zip using `subprime', keep(1 3) nogen
        merge m:1 zip using `fha_expansion', keep(1 3) nogen
        merge m:1 zip using `gse', keep(1 3) nogen
        merge m:1 zip using `hamp', keep(1 3) nogen
    }
	
end/*%>*/
*******************************************************************************

*******************************************************************************
* Graph helpers
*******************************************************************************
capture program drop agg_plot /*%<*/
program define agg_plot
    syntax, dname(name) [filename(string) outdir(string) ytitle(string)]
    * Aggregate home sales graphs.

    tempfile orig
    save `orig'
   
    if "`ytitle'" == "" local ytitle "Monthly Home Sales"
    preserve
   
    if "`filename'" != "" {
        use `filename', clear
    }

    drop if month == . | year == .
    collapse (sum) hsales_sa, by(mdate year month)
    replace hsales_sa = 12*hsales_sa/1000

    #delimit ;
    twoway (line hsales_sa mdate, lcolor(blue) xline(580 588 598 605, lp(dash) lc(gray))),
        xtitle("") xlabel(539 580 605 635)
        ytitle("Annualized Home Sales (000s)")
        graphregion(color(white)) bgcolor(white);
    #delimit cr

    graph export agg_home_sales_`dname'.pdf, replace

    #delimit ;
    twoway (line hsales_sa mdate if year>=2008 & year<=2010, lcolor(blue)
        xline(580 588 598 605, lp(dash) lc(gray))),
        xlabel(580 588 598 605)
        xtitle("") ytitle("Annualized Home Sales (000s)")
        graphregion(color(white)) bgcolor(white);
    #delimit cr

    graph export agg_home_sales_closeup_`dname'.pdf, replace

    use `orig', clear

end
* %>

capture program drop geoPlots /* %< */
program define geoPlots
    syntax, [county(numlist) countyExcl(numlist) othZip(numlist) save(string) ///
             expVar(name) nq(integer 4)]
    /* Program for building irregular, metro area maps.
     
    ARGUMENTS:
        county: Numeric, list of county FIPS codes for a set of counties that
        cover the area being mapped.
        countyExcl: Numeric, list of ZIP codes for a set of ZIPs in the above
        counties that should be excluded. Program will not run if countyExcl but
        not county is specified.
        othZip: ZIP codes outside the county FIPS that should also be added.
        save: Name for graph to be exported.
        expVar: Exposure variable being plotted. Default is exante (2000 data)
        nq: Number of colours in the palette (same argument in maptile), default 4.
    */
    foreach num of local county {
        local countyRef `countyRef' `num',
    }
    foreach num of local countyExcl {
        local ExclRef `ExclRef' `num',
    }    
    foreach num of local othZip {
        local ZipRef `ZipRef' `num',
    }
    foreach lists in county Excl Zip {
        local `lists'Ref = regexr("``lists'Ref'", ",$", "")
    }
    
    if "`county'" != "" & "`countyExcl'" != "" & "`othZip'" != "" {
        local cond (inlist(county, `countyRef') & ~inlist(zip5, `ExclRef')) ///
                   | inlist(zip5, `ZipRef')
    }
    else if "`county'" != "" & "`othZip'" != "" {
        local cond inlist(county, `countyRef') | inlist(zip5, `ZipRef')
    }
    else if "`county'" != "" & "`countyExcl'" != "" {
        local cond inlist(county, `countyRef') & ~inlist(zip5, `ExclRef')
    }
    else if "`county'" != "" {
        local cond inlist(county, `countyRef')
    }
    else if "`othZip'" != "" {
        local cond inlist(zip5, `ZipRef')
    }
    else {
        exit
    }
    display "Geo selection macro:"
    display "`cond'"
    
    if "`expVar'" == "" local expVar exante_share
    if ("`save'" != "") {
        maptile `expVar', geo(zip5) mapif(`cond') n(`nq') fcolor(Blues) legd(3) ///
            savegraph(`save') res(.5) replace
    }
    else {
        maptile `expVar', geo(zip5) mapif(`cond') n(`nq') fcolor(Blues) legd(3) 
    }
    
end
*%>

capture program drop price_data_prep/*%<*/
program define price_data_prep
    syntax, q(integer) [PERIOD]
    * A small program containing shared data cleaning maneuvers 

    tempfile orig xtiles divisions
    use "$analysis/zip_prices_exante_20170208.dta", clear
    drop state
    gen state = floor(county/1000)
    if "`period'" != "" {
        keep if inlist(mdate, 554, 588, 605, 621)
        gen period = 0 if mdate==554
        replace period = ceil((mdate - 588)/17) + 1 if mdate > 554
    }
    save `orig', replace
    duplicates drop zip, force
    drop if missing(DQprice_mean2008) | missing(exante_share)
    keep zip DQprice exante_s pop_2007
    xtile price_level = DQprice_mean2008, nq(`q')
    xtile treatment_level = exante_share [aw=pop_2007], nq(`q')
    save `xtiles', replace

    use `orig', clear
    merge n:1 zip using `xtiles', keep(3) nogen
    sort zip mdate
    format mdate %tm

end
* %>

capture program drop treat_series /* %< */
program define treat_series
    syntax varlist(fv) [if/] [aweight], fname(name) over(name) ///
    [absorb(name) cluster(name) trim(real 0) start(integer 2000) end(integer 2010) ///
     INTERact report(numlist) geo(name) ENDOGenous coeff_file(string) label(string)]

    version 13
    preserve

    * An adjusted version of treat_series in fthb_programs, where the main
    * regressions are 2SLS/GMM instead of OLS. Input variables into this
    * program in the order {LHS, endogeneous policy, exposure instrument, controls}

    * Modify data.
    if "`if'" != "" {
        keep if `if'
    }

    * More informative local names
    local treatment `2'
    local lhs `1'
    local cross `3'

    if `trim' != 0 {
        trim_tails `1', level(`trim')
    }

    * For each period, figure out treatment coefficient for the groups.
    gen treatment_coeff_`fname' = .
    gen std_err_`fname' = .
    gen rsquared_`fname' = .
    gen treatment_coeff_`fname'int = .
    gen std_err_`fname'int = .

    * Generates regressions over each monthly period.
    forvalues i = `start'/`end' {
        display `i'
        reg_internal `varlist' if `over' == `i' [`weight' `exp'], ///
            absorb(`absorb') cluster(`cluster') `endogenous'
        noisily display "Sample: `e(N)'"
        display "Treatment name: `2'"
        replace treatment_coeff_`fname' = _b[`treatment'] if `over' == `i'
        replace std_err_`fname' = _se[`treatment'] if `over' == `i'
        *Pulling out the interaction marginal effect and std. err
        *(The order matters; the interaction term goes after the main!)
        if "`interact'" != "" {
        * TC: What may be necessary is for the user to explicitly
        * call the interaction as being made up of factors, even if
        * # is called. Doing so allows the program to adjust labels
        * so it knows what coefficient to retrieve and where to place them
        local m = regexm("`4'", "i\.")
        local m_i = regexm("`3'", "i\.")
        if `m' == 1 local 4 = substr("`4'", 3,.)
        if `m_i' == 1 {
            local call_ind = "1."
            local ind_condition = "`over' == `i' &"
        }
        levelsof `4', local(int_levels)
        local int_var `4'
        foreach level of local int_levels {
            replace treatment_coeff_`fname'int = _b[`call_ind'`2'#`level'.`4'] ///
                if `ind_condition' `4' == `level'
            replace std_err_`fname'int = _se[`call_ind'`2'#`level'.`4'] if ///
                `ind_condition' `4' == `level'
        }
        }


        replace rsquared_`fname' = `e(r2)' if `over' == `i'
    }

    *Generate regressions pooled over time
    *This looks complicated because we allow for arbitrary numbers of periods over which
    *the pooled coefficients are computed. Each new beta, se and r^2 is imputed as a new row
    *in the dataset; Mdate is a high number so the row is preserved after
    *collapse command. 
    *
    * See Page 1128 of Mian Sufi. We need to take averages for these pooled
    * regressions.
    tokenize `report'
    local i = 1
    tempfile pre_collapse
    save `pre_collapse'
    while "``i''" != "" {
        local j = `i' + 1
        display `j'

        use `pre_collapse', clear
        * Zoom in on relevant periods
        keep if `over' >= ``i'' & `over' <= ``j''
        display "`lhs'"
        * Generate means for each group. Because varlist is a static object,
        * Replace the LHS instead of creating a new one.
        bys `geo': egen mean_`lhs' = mean(`lhs')
        duplicates drop `geo', force
        replace `lhs' = mean_`lhs'
        reg_internal `varlist' [`weight' `exp'], absorb(`absorb') ///
            cluster(`cluster') `endogenous'
        use `pre_collapse', clear
        set obs `=_N + 1'
        replace mdate = 10000*``i''+``j'' in l
        replace treatment_coeff_`fname' = _b[`treatment'] in l
        replace std_err_`fname' = _se[`treatment'] in l
        local b = _b[`treatment']
        local se = _se[`treatment']

        *Computing the interaction marginal effect and std. err by hand
        *(The order matters; the interaction term goes after the main!)
        if "`interact'" != "" {
            replace treatment_coeff_`fname'int = _b[1.`cross'] in l
            replace std_err_`fname'int = _se[1.`cross'] in l
            local b = _b[1.`cross']
            local se = _se[1.`cross']
        }

        replace rsquared_`fname' = `e(r2)' in l
        local i = `i' + 2
        unique mdate
    }

    * Collapse by group.
    if "`interact'" != "" {
        collapse (first) treatment_coeff* std_err* rsquared* , by(`over' `int_var')
        * Pulling out zero-valued base level parameters
        drop if treatment_coeff_`fname'int == 0
    }
    else {
        collapse (first) treatment_coeff* std_err* rsquared* , by(`over')
    }
    qui count if !missing(treatment_coeff_`fname')
    display "Total rows in table: `r(N)'"

    *Cumulative coefficients over some short period (only supports one interval)
    capture gen cum_`fname' = sum(treatment_coeff_`fname') ///
        if `over' >= `1' & `over' <= `2'

    *save to file (another program must delete it)
    save `fname', replace

end
* %>

capture program drop reg_internal /* %< */
program define reg_internal
    syntax varlist(fv) [if] [aweight fweight pweight iweight], [absorb(name) ///
        cluster(name) ENDOGenous]

    * Wraps the control flow of choosing which Stata regression
    * to run, such as including FE or including an endogenous regressor,
    * into one command. Makes programs in which the regression is just one
    * step easier to read.

    * clustering
    if "`cluster'" != "" {
        local vce "cluster(`cluster')"
    }
    else {
        local vce "r"
    }

    if "`absorb'" != "" & "`endogenous'" != "" {
        fastivreg `varlist' `if' [`weight' `exp'], absorb(`absorb') `vce'
    }
    else if "`absorb'" != "" {
        areg `varlist' `if' [`weight' `exp'], absorb(`absorb') `vce'
    }
    else if "`endogenous'" != "" {
        tokenize `varlist'
        local lhs `1'
        local endog `2'
        local exog `3'
        macro shift 3
        local varlist `*'
        ivregress 2sls `lhs' `varlist' (`endog' = `exog') `if' ///
            [`weight' `exp'], `vce' nohe first
    }
    else {
        reg `varlist' `if' [`weight' `exp'], `vce'
    }

end
* %>

capture program drop graph_mian_sufi /* %< */
program define graph_mian_sufi
    syntax varlist [if/] [aweight], [scalevar(name) over(name) ivtype(name) ///
        control(name) geo(name) cluster(name) ENDOGenous ///
        absorb(name) hetero(string) fig5 fig6 outdir(string) cum(numlist) raj(integer 0) ///
        start(integer 568) end(integer 622)]

    tempfile orig
    save `orig'

    * Variables in main regression
    tokenize `varlist'
    local lhs `1'
    local endog `2'
    local exog `3'
    macro shift 1
    local regressors `*'

    if "`exog'" == "" & "`endogenous'" != "" {
        di "Too few variables specified for regression with an endogenous" ///
            "treatment. Did you forget to add a variable?"
        exit
    }
    if "`exog'" != "" & "`endogenous'" == "" {
        di "Too many variables specified for reduced-form regression." ///
           " Did you forget to add a variable?"
        exit
    }

    * Modify data.
    if "`if'" != "" {
        keep if `if'
    }

    *Time period of evaluation. Default is monthly
    if "`over'" == "" local over mdate
    local xline 580 588 598 605
    if "`over'" == "qdate" local xline 196.3 199.7 202

    *dealing with controls. Default is discrete with controls
    set_controls `control'
    local controls $controls
    global controls // Keep register clean

    * clustering and FE
    local Cluster "cluster(`cluster')"
    local Absorb "absorb(`absorb')"

    * Different routines based on which one of the figure suboptions is given.
    * Non-regression functions disabled for this 2SLS version
    if "`fig5'" == "" & "`fig6'" == "" & "`endogenous'" == "" {
    * Just one scaled variable with controls.
    ddplot_yagan `varlist' `controls' [`weight' `exp'], ///
        fname("`outdir'/`lhs'_`geo'_`scalevar'_`ivtype'_`hetero'_controls`control'.pdf") ///
        over(`over') start(`start') end(`end') treatlab("Treatment") contlab("Control") ///
        xlabel("Month") ylabel("Scaled Sales") raj(`raj') xline(`xline') cum(`cum') ///
        `Absorb'
    }

    * For figures of coefficients over time. Runs a second regression with cumulative hsales
    * as the numerator of dep. var: have to generate this in another program.
    else if "`fig5'" != "" {

        tempfile analysis
        save `analysis'

        local output "`outdir'/`lhs'_`geo'_`scalevar'_`ivtype'_`hetero'_fig5.pdf"

        if "`cum'" == "" {
            treat_series `varlist' `controls' [`weight' `exp'], fname(c) over(`over') ///
                `Cluster' `Absorb' `endogenous' start(`start') end(`end')
            use c, clear
            erase c.dta
            gen pos_conf_c = treatment_coeff_c + 1.96*std_err_c
            gen neg_conf_c = treatment_coeff_c - 1.96*std_err_c

            outsheet treatment_coeff_c pos_conf_c neg_conf_c `over' using ///
                coeffplot_estimate_save.csv, comma replace

            #delimit ;
            twoway
                (connect treatment_coeff_c `over',
                    mcolor(red*1.6) lpattern(dash) lcolor(red*.8) msymbol(O))
                (rarea pos_conf_c neg_conf_c `over',
                    lcolor(red*.2) lpattern(dash) fcolor(none))
					
                if `over' >= `start' & `over' <= `end',
                xline(`xline', lcolor(gs9) lp(dash)) xscale(r(`start' `end'))
                $graphconfig xtitle("Month") ytitle("Sales impact coefficient")
                yline(0, lc(gs4)) ylab(#5, nogrid) xlab(#8)
                legend(label(1 "With Controls")
                       label(2 "95% Confidence Interval")
                       col(2)) xsize(6);
            #delimit cr
            graph export `output', replace
        }

        use `analysis', clear

        if "`cum'" != "" {
            treat_series `lhs'_cum `regressors' `controls' [`weight' `exp'],  fname(c_cum) ///
                over(`over') `Cluster' `Absorb' `endogenous' start(`start') end(`end')

            use c_cum, clear
            erase c_cum.dta
            gen pos_conf_c_cum = treatment_coeff_c_cum + 1.96*std_err_c_cum
            gen neg_conf_c_cum = treatment_coeff_c_cum - 1.96*std_err_c_cum

            #delimit ;
            twoway
                (connect treatment_coeff_c_cum `over', mcolor(red*1.6) lpattern(dash)
                    lcolor(red*.8) msymbol(O))
                (rarea pos_conf_c_cum neg_conf_c_cum `over',
                    lcolor(red*.2) lpattern(dash) fcolor(none))
                if `over' >= `start' & `over' <= `end',
                xline(`xline', lcolor(gs9) lp(dash)) xscale(r(`start' `end'))
                $graphconfig xtitle("Month") yline(0, lc(gs4)) ylab(#5,nogrid)
                ytitle("Sales impact coefficient, cumulative purchases")
                legend(label(1 "With controls")
                       label(2 "95% Confidence Interval")) xsize(6);
               local leng = length("`output'");
               local ftemp = substr("`output'",1,`leng'-4);
               local fname2 = "`ftemp'" + "cumul.pdf";
            graph export "`fname2'", replace;
            #delimit cr
        }
    }

    * These are placebo tests. Run regressions two years forward from Feb 08- Jul 11
    * (mdates 577-618) Policy period and three years back.
    else {
        forvalues i=1/6{
            local startp = 541+(`i'-1)*12
            local endp = 541+`i'*12+29
            treat_series `varlist' `controls' [`weight' `exp'],  fname(p_`i') ///
                over(mdate) `Cluster' `Absorb' `endogenous' ///
                start(`startp') end(`endp')
        }

        *Load data, overlap over months
        use p_1, clear
        erase p_1.dta
        gen base = 552 if !missing(treatment_coeff_p_1)
        forvalues i=2/6{
            append using p_`i'
            erase p_`i'.dta
            replace base = 552+(`i'-1)*12 if !missing(treatment_coeff_p_`i')
        }
        *Translate month 
        gen Month = mdate - base
        recast long Month
        drop mdate

        *Creates graph with all six series for appendix
        #delimit ;
        local output
        "`outdir'/`lhs'_`geo'_`scalevar'_`ivtype'_`hetero'_fig6.pdf";
        twoway
            (connect treatment_coeff_p_4 Month,
                lwidth(thick) lcolor(maroon*1.1) msymbol(none))
            (connect treatment_coeff_p_1 Month,
               lpattern(longdash) lc(teal) msymbol(none))
            (connect treatment_coeff_p_2 Month,
               lpattern(longdash_dot) msymbol(none))
            (connect treatment_coeff_p_3 Month,
               lpattern(shortdash_dot) msymbol(none))
            (connect treatment_coeff_p_5 Month,
               lpattern(dot) lc(maroon*0.8) msymbol(none))
            (connect treatment_coeff_p_6 Month,
               lpattern(shortdash) msymbol(none)),
            xline(-8 0 10 17, lcolor(gs9) lp(dash)) xscale(r(-11 31)) $graphconfig
            xtitle("Month") ytitle("Sales impact coefficient") ylab(, nogrid)
            legend(label(1 "2008-2011") label(2 "2005-2008")
                   label(3 "2006-2009") label(4 "2007-2010")
                   label(5 "2009-2012") label(6 "2010-2013")) xsize(6);
        #delimit cr
        graph export `output', replace
    }

    use `orig', clear
end
* %>

capture program drop graph_longdiff/*%<*/
program define graph_longdiff
    syntax anything(name=geo) [aweight], v(name) [control(name)]

    tempfile orig
    save `orig'
    
    set_controls `control'
    local controls $controls
    global controls // Keep register clean

    winsor `v', p(.01) gen(`v'W)
    replace `v' = `v'W
    sum `v', d

    if "`v'" == "delta_sales" {
        local ytitle "Delta Total Home Sales, Policy vs. Pre"
    }
    else if "`v'" == "delta_pricegrowth" | "`v'" == "delta_pricegrowth_fhares" | ///
            "`v'" == "delta_pricegrowth_fha" | "`v'" == "delta_pricegrowth_raw" {
        local ytitle "Delta Total Price Growth, Policy vs. Pre"
    }
    else if "`v'" == "delta_construction" {
        local ytitle "Change in Total New Construction, Policy vs. Pre"
    }
    else if "`v'" == "average_delta_sales" {
        local ytitle "Delta Average Home Sales, Policy vs. Pre"
    }
    else if "`v'" == "average_delta_construction" | "`v'" == "average_delta_built" {
        local ytitle "Delta Average New Construction, Policy vs. Pre"
    }
    else if "`v'" == "average_delta_foreclo" {
        local ytitle "Delta Average Foreclosures/Short Sales, Policy vs. Pre"
    }

    gen v1 = .25*`r(sd)'
    expand 2
    bys `geo': replace v1 = -.25*`r(sd)' if _n == 2
    gen v2 = .5*v1
    levelsof v1, local(v1list)
    sum v2, meanonly
    local v2min = `r(min)'
    local v2max = `r(max)'
    drop v1 v2
    duplicates drop `geo', force
    
    #delimit;
    binscatter `v' exante_share [aweight=totalhsales_base], 
        reportreg control(`controls') noaddmean
        absorb(zip_cluster) yscale(range(`v1list')) 
        ylab(`v2min' "-.125{&sigma}" `v2max' ".125{&sigma}", nogrid)
        ytitle("`ytitle'") xtitle("Place-based Exposure")
        savegraph("macro_binscatter_`v'.pdf") replace
        $graphconfig;
    #delimit cr

    use `orig', clear

end/*%>*/

capture program drop graph_HPICoeffPlots/*%<*/
program define graph_HPICoeffPlots
    syntax varlist [if/] [aweight], startyr(integer) endyr(integer) ///
        [control(name) absorb(varname) cluster(varname) hetero(name) ENDOGenous]

    * Content in this program is mostly unchanged. Added some control
    * flow commands so its interface is consistent with other functions.
    tempfile orig
    save `orig'

    tokenize `varlist'
    local lhs `1'
    local treatment `2'
    local exog `3'
    macro shift 1
    local regressors `*'

    if "`exog'" == "" & "`endogenous'" != "" {
        di "Too few variables specified for regression with an endogenous" ///
            "treatment. Did you forget to add a variable?"
        exit
    }
    if "`exog'" != "" & "`endogenous'" == "" {
        di "Too many variables specified for reduced-form regression." ///
           " Did you forget to add a variable?"
        exit
    }

    set_controls `control'
    local controls $controls
    global controls // Keep register clean

    tempfile orig
    save `orig'

    matrix drop _all
    
    if "`if'" != "" local if "& `if'"

    forvalues yr=`startyr'/`endyr' {
         display "Year: `yr'"
         reg_internal `varlist' `controls' if year == `yr' `if' ///
             [`weight' `exp'], absorb(`absorb') cluster(`cluster') `endogenous'
         matrix b = (nullmat(b) , _b[`treatment'])
         matrix se = (nullmat(se) , _se[`treatment'])
         loc yrlab = `yr' - 0.45
         local Nc = e(N)
         local Nloc `Nloc' (scatteri 0.95 `yrlab' "N= `Nc'", ///
             msymbol(i) mlabs(vsmall) mlabc(black) mlabangle(-25))
    }

    * Construct coefficients plot by calling the stored coefficients
    matrix b = b'
    matrix se = se'
    preserve
    clear
    local yrs = `endyr' - `startyr' + 1
    set obs `yrs'
    svmat b
    svmat se
    egen year = seq(), from(`startyr') to(`endyr')
    * 95% confidence interval
    gen b1neg = b1 - 1.96*se1
    gen b1pos = b1 + 1.96*se1

    * Plot the graph (with N counts per regression)
    #delimit ;
    twoway  
        (rcap b1pos b1neg year, lc(navy*0.4) lp("--."))
        (connect b1 year, ms(o) lc(navy) mc(navy)), /* `Nloc', */
        legend(off) xti("Year") yti("Coefficient Estimate") $graphconfig
        xlab(`startyr'(1)`endyr') ylab(,nogrid) yline(0, lc(black));
    #delimit cr
    graph export "hpcoeffplot_`lhs'_`hetero'.pdf", replace

    use `orig', clear

end/*%>*/

*******************************************************************************
* Table helpers
*******************************************************************************
capture program drop load_counts /*%<*/
program define load_counts
    syntax, geo(name) datadir(string) [hetero(string) threshold(real 0.9)]
 
    * These are stats from the SQL query stage
    if "`geo'" == "zip" {
        matrix b = (2716338, 2540700)
        matrix se = (124.4, 70.51)
        matrix u = (19240, 18351)
    }
    else if "`geo'" == "county" {
        matrix b = (150859, 145776)
        matrix se = (124.4, 70.51)
        matrix u = (973, 970)
    }
    else if "`geo'" == "cbsa" {
        matrix b = (55348, 53575)
        matrix se = (117.5, 67.26)
        matrix u = (295, 295)
    }

    foreach mat in b se u {
        matrix colnames `mat' = "-2" "-1"
    }
    
    qui load_dataquick, geo(`geo') datadir(`datadir') hetero(all)
    collapse (sum) hsales*, by(`geo' mdate)
    display "POST LOAD"
    unique `geo'
    matrix tmp = (`r(N)')
    matrix no = (_result(18))
    sum hsales_nsa, meanonly
    matrix see = (`r(sum)'/(1e06))
    foreach mat in tmp no see {
        matrix colnames `mat' = "1"
    }
    matrix b = b, tmp
    matrix se = se, see
    matrix u = u, no
    
    qui load_dataquick, geo(`geo') datadir(`datadir') hetero(`hetero')
    display "POST LOAD `hetero'"
    unique `geo'
    matrix tmp = (`r(N)')
    matrix no = (_result(18))
    sum hsales_nsa, meanonly
    matrix see = (`r(sum)'/(1e06))
    foreach mat in tmp no see {
        matrix colnames `mat' = "2"
    }
    matrix b = b, tmp
    matrix se = se, see
    matrix u = u, no
    
    filter_data if transactiontype == "R" & shr > `threshold'
    display "POST FILTER"
    unique `geo'
    matrix tmp = (`r(N)')
    matrix no = (_result(18))
    sum hsales_nsa, meanonly
    matrix see = (`r(sum)'/(1e06))
    foreach mat in tmp no see {
        matrix colnames `mat' = "3"
    }
    matrix b = b, tmp
    matrix se = se, see
    matrix u = u, no
    
    qui make_covariates, geo(`geo') datadir(`datadir') `opt'
    display "POST COVARS"
    unique `geo' if !missing(exante_share)
    matrix no = (_result(18))
    sum hsales_nsa if !missing(exante_share), meanonly
    matrix tmp = (`r(N)')
    matrix see = (`r(sum)'/(1e06))
    foreach mat in tmp no see {
        matrix colnames `mat' = "4"
    }
    matrix b = b, tmp
    matrix se = se, see
    matrix u = u, no
    
    ereturn post b
    quietly estadd matrix se, replace
    quietly estadd matrix u, replace
    matrix drop se u
    
    eststo `geo'
end/*%>*/

capture program drop summary_stats/*%<*/
program define summary_stats
    syntax name(name=geo)
    * Table of summary statistics. Produces a panel for RHS and LHS.

    *Capitalizes abbreviations
    local lv = upper("`geo'")

    *Slight definition changes for percentage urban
    local urban
    if "`geo'" == "zip" local urban "Fraction classified as urban"
    
    *These are our summary statistics for both sides
    replace pop_2007 = pop_2007/1000
    local RHS exante_share pop_2007 /* d_hp */ ///
        pctunemp avgagi card_subprime  ///
        subprime_ratio fha_expansion_exposure gse_ratio hamp_exposure ///
        medianage mediangrossrent pctpoor pcturban 
    local LHS hsales_sa y_trim 
    
    *Summarize twice, over geo and geo month
    preserve
    duplicates drop `geo', force
    eststo tab1: estpost sum `RHS', detail
    restore
    eststo tab2: estpost sum `LHS', d
*    eststo tab3: estpost sum y3_trim_cum if mdate == ym(2009, 11), d
    qui unique `geo'
    local geos = _result(18)

    *Make graph
    local out "table1_`geo'.tex"

    #delimit ;
    estout tab2 using "`out'", 
        cells("mean(fmt(%9.1f %9.2f)) p10 p50 p90 count(fmt(%9.0f))")
        coll(Mean 10th Median 90th N) 
        posthead(\cmidrule(lr){2-6} \multicolumn{6}{l}{\textbf{Housing Transactions}} \\)
        varl(
            hsales_sa 
                "Monthly Home Sales (SA)"
            y_trim 
                "Home Sales/Average Monthly Sales, 2007",
        blist(hsales_sa "\quad " y_trim "\quad ")) 
        postfoot("\addlinespace")
        mlabels(,none) style(tex) replace;

    estout tab1 using "`out'", cells("mean(fmt(%9.2f)) p10 p50 p90 count(fmt(%9.0f))")
        posthead(\multicolumn{6}{l}{\textbf{Program Exposure (`lv')}} \\)
        coll(, none)
        varl(
            exante_share 
                "First-Time Buyers/Tax Units, 2000 (IRS)"
            pop_2007 
                "Population, 000s (ACS)"
            pctunemp 
                "Unemployment Rate, 06-10 Average (ACS)" 
            avgagi 
                "Average Gross Income, 2005 (IRS)" 
            card_subprime 
                "Subprime Cardholder Share, 1996 (Equifax)"
            subprime_ratio 
                "Subprime Share of Originations (HMDA)"
            fha_expansion_exposure 
                "FHA Expansion Exposure (HMDA)"
            gse_ratio
                "HARP Exposure (HMDA)"
            hamp_exposure
                "HAMP Exposure (HAMP/HMDA)"
            medianage 
                "Median Age, 06-10 Avg. (ACS)" 
            mediangrossrent 
                "Median Rent, 06-10 Avg. (ACS)" 
            pctpoor 
                "Fraction below Poverty Line (ACS)" 
            pcturban 
                "Urban Share of Census Blocks (Census)", 
        blist(exante_share "\quad " pop_2007 "\quad " pctunemp "\quad "
              avgagi "\quad " card_subprime "\quad " 
              subprime_ratio "\quad " fha_expansion_exposure "\quad "
              gse_ratio "\quad " hamp_exposure "\quad " medianage "\quad "
              mediangrossrent "\quad " pctpoor "\quad " pcturban "\quad ")
        elist(exante_share 
            " \addlinespace \multicolumn{6}{l}{\textbf{Cross Sectional Characteristics (`lv')}} \\"))
        mlabels(,none) style(tex) append;
    #delimit cr

    est clear
    
end/*%>*/

capture program drop tabII/*%<*/
program define tabII
    syntax namelist [aweight], lhs(varname) outdir(string) ///
        [geo(name) labels(string asis) AREG]
    * This is a program for generating table II in FTHB. The hard part is "rotating" the table
    * so each row reports a regression result. This is done iteratively in Stata. 
    * NOW with an areg option.
   
    *useful for headings
    local firstvar `1'
    matrix drop _all
    *Parsing on some non-space character, then going through the list
    *(the plus signs are themselves an element, so we count twice each iteration)
    tokenize `labels', parse(+)
    local i 0   
    *Four statistics for each row: coefficient, std. error, R-squared, N in sample
    foreach covar of local namelist {
        local ++i
        local lab ``i''
            * Standardize the RHS
            sum `covar'
            replace `covar' = `covar'/r(sd)

            if "`areg'" != "" {
                areg `lhs' `covar' [`weight' `exp'], absorb(`geo'_cluster) ///
                    cluster(`geo'_cluster)
            }
            else {
                reg `lhs' `covar' [`weight' `exp'], cluster(`geo'_cluster)
            }
            matrix tmp = (_b[`covar'])
            matrix colnames tmp = "`lab'"
            matrix b = nullmat(b), tmp
            matrix see = (_se[`covar'])
            matrix colnames see = "`lab'"
            matrix se = nullmat(se), see
            matrix tmp = (`e(r2)')
            matrix colnames tmp = "`lab'"
            matrix r2 = nullmat(r2), tmp
            matrix tmp = (`e(N)')
            matrix colnames tmp = "`lab'"
            matrix total = nullmat(total), tmp
            local ++i
    }
    sum `lhs' `namelist'
    * Store estimates in e()
    ereturn post b
    quietly estadd matrix se, replace
    quietly estadd matrix r2, replace
    quietly estadd matrix total, replace
    matrix drop se r2 total
    *Print out the table
    #delimit ;
    estout using "`outdir'/table2_`lhs'`areg'.tex", cells((b(star fmt(%9.3f))
        r2(fmt(%9.4f)) total(fmt(%9.0f))) se(par(( ))))
        prehead(" &\multicolumn{3}{c}{LHS is Exposure}\\ \cmidrule(lr){2-4}")
        posthead("\midrule" "\multicolumn{4}{l}{\textbf{Exposure Correlates}:} \\")
        collabel(Coefficient "$ R^2$" "N", lhs(""))
        varl(,elist("Fraction Classified as Urban" "\multicolumn{4}{l}{\textbf{Controls}:} \\")
        prefix("\quad "))
        starlevels(+ 0.10 * 0.05 ** 0.01) mlabel(,none) style(tex) replace;
    #delimit cr
    est clear

end/*%>*/

capture program drop ddregression /* %< */
program define ddregression
    syntax varlist(fv) [if/] [aweight], over(name)  ///
        report(numlist) reportname(name) ///
        [absorb(name) trim(real 0) cluster(name) ENDOGenous ///
        treatlab(string) contlab(string) xline(numlist) ///
         coeff_file(string) label(string) fama(integer 0) geo(name)]

    * Purpose: Implements regression and generates version of MS Table V, following syntax
    *   of ddyagan_plot.
    * Dependencies: lincom

    * Save original data.
    tempfile orig
    save `orig'

    * Modify data.
    if "`if'" != "" {
        keep if `if'
    }

    if `trim' != 0 {
        trim_tails `1', level(`trim')
    }

    if `fama' != 0 {

        tempfile pre_collapse
        save `pre_collapse'

        tokenize `varlist'
        local lhs `1'
        local treatment `2' // EITHER EXOGENOUS OR ENDOGENOUS VARIABLE
        tokenize `report'
        display "FMB collapse on `reportname'"
        if "`reportname'" == "" {
            continue
        }
        use `pre_collapse', clear
        keep if `over' >= `1' & `over' <= `2'
        bys `geo': egen mean_`lhs' = mean(`lhs')
        duplicates drop `geo', force
        replace `lhs' = mean_`lhs'

        reg_internal `varlist' [`weight' `exp'], absorb(`absorb') ///
            cluster(`cluster') `endogenous'

        * Write to an outfile using XML for general parsing.
        local b = _b[`treatment']
        local se = _se[`treatment']
        display `e(r2)'
        coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_file') label(`label')

    }
    else {
        * Collect and analyze coefficients for windows:
        *   (pre, policy, post)
        * 1. Build spec.
        local ddspec "i.`over'##c.`treatment'"

        if "`absorb'" != "" {
            local spec "`varlist' `ddspec' [`weight' `exp'], absorb(`absorb')"
            areg `spec' `vce'
        }
        else {
            local spec "`varlist' `ddspec' [`weight' `exp'], "
            reg `spec' `vce'
        }

        * 2. Run spec.

        * 3. Lincom the cumulative via programmatic lincom expansion.
        tokenize `report'
        display "FMB collapse on `reportname'"
        if "`reportname'" == "" {
            continue
        }

        * Lincom expansion based on input numlist of start and end points
        * for pertinent interval.
        local lincom_arg "`1'.`over'#c.`treatment'"
        forval i=`1'/`2' {
            local lincom_arg `lincom_arg' + "`i'.`over'#c.`treatment'"
        }
        * Lincom.
        lincom "`lincom_arg'"

        * Write to an outfile using XML for general parsing.
        coeff_xml_write, coeff(`r(estimate)') se(`r(se)') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_file') label(`label')

    }

    use `orig', clear
end/*%>*/

capture program drop table_longdiff /*%< */
program define table_longdiff
    syntax varname [aweight], geo(name) outdir(string) ///
        [control(name) ENDOGenous]
    * This is similar to regressions_v2, but only for the long difference
    * cross section regressions like M/S, table 6. We run more robustness checks
    * than they do. DDregression will not work here, so opt for something simpler.

    tempfile orig
    save `orig'

    if "`geo'" == "zip" {
        local absorb_val absorb(zip_cluster)
        local cluster_val cluster(zip_cluster)
        local absorb_labels "CBSA FE"
    }
    if "`geo'" != "zip" {
        local absorb_val absorb(state)
        local cluster_val vce(r)
        local absorb_labels "State FE"
    }

    * Minor trimming and sample definitions.
    gen longdiff = `varlist'
    sum longdiff, d
    gen trim_sample = cond(longdiff < `r(p95)' & longdiff > `r(p5)', 1, 0)
    sum longdiff if trim_sample == 1, d

    winsor longdiff, p(.01) gen(longdiffW)
    replace longdiff = longdiffW

    if "`endogenous'" != "" {
        local regressors claims_scaled exante_share
    }
    else {
        local regressors exante_share
    }
    tokenize `regressors'
    local treatment `1'

    sum totalhsales_base, d
    gen big_sample = cond(totalhsales_base > `r(p10)', 1, 0)

    set_controls `control'
    local controls $controls
    global controls // Keep register clean

    * Start coeff doc.
    local coeff_xml "`outdir'/longdiff.xml"

    coeff_xml_open, fname(`coeff_xml')
    local coeff_tex "`outdir'/longdiff_`geo'_`varlist'.tex"

    * cbsa FE
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'], `absorb_val' ///
        `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
        fname(`coeff_xml') label(`absorb_labels')

    * All specs below here have FE
    * no weights
    reg_internal longdiff `regressors' `controls', `absorb_val' ///
        `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("No wgts")

    * excluding sand states
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'] if ///
        sandstate != 1, `absorb_val' `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("Ex sand")

    * trim sample
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'] if ///
        trim_sample == 1, `absorb_val' `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("Trimmed")

    * big sample
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'] if ///
        big_sample == 1, `absorb_val' `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("$\overline{\text{Sales}}$ > P10")

    * low prices
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'] if ///
        inlist(price_level,1,2,3), `absorb_val' `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("Low p")

    * high prices
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'] if ///
        inlist(price_level,8,9,10), `absorb_val' `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("High p")

    * Close coeff doc.
    coeff_xml_close, fname(`coeff_xml')

    * Turn coeff xml doc into a tex table.
    coeff_tex_compile, input(`coeff_xml') output(`coeff_tex')

    use `orig', clear

    !python $localcodedir/py/tab_longdiff.py

end/*%>*/

capture program drop regressions_v2 /*%<*/
program define regressions_v2
    syntax varlist [if/] [aweight], geo(name) report(numlist) reportname(namelist) ///
        outdir(string) start(integer) end(integer) [ENDOGenous ///
        scalevar(name) ivtype(name) cluster(name) hetero(string) control(name)]

    tempfile orig
    save `orig'

    tokenize `varlist'
    local lhs `1'
    local endog `2'
    local exog `3'
    macro shift 1
    local regressors `*'

    if "`exog'" == "" & "`endogenous'" != "" {
        di "Too few variables specified for regression with an endogenous" ///
            "treatment. Did you forget to add a variable?"
        exit
    }
    if "`exog'" != "" & "`endogenous'" == "" {
        di "Too many variables specified for reduced-form regression." ///
           " Did you forget to add a variable?"
        exit
    }

    if "`if'" != "" {
        keep if `if'
    }

    set_controls `control'
    local controls $controls
    global controls // Keep register clean

    * clustering
    capture local Cluster "cluster(`cluster')"

    * Start coeff doc.
    local coeff_xml "`outdir'/coeff_`geo'_`start'_`scalevar'_`ivtype'_`hetero'_controls`control'.xml"
    local coeff_tex_base "`outdir'/coeff_`geo'_`start'_`scalevar'_`ivtype'_`hetero'_controls`control'"

    *ddregression will spit out values, the xml functions draws them as columns in a table.

    tokenize `report'
    local j = 1
    foreach x of local reportname {
        coeff_xml_open, fname(`coeff_xml')
        local coeff_tex "`outdir'/coeff_`geo'_`start'_`scalevar'_`ivtype'_`hetero'_controls`control'_`x'.tex"

        * Run a bunch of regressions, they will be added to this file.

        * Basic syntax is similar to ddyagan.
        * You put the main treatment indicator in an argument.
        * Policy is a numlist of length two, which is
        * the period over which the lincom will run.
        * Pre and post are similar to policy but
        * optional arguments.

        local jplus = `j' + 1
        #delimit ;
        * No controls;
        ddregression `varlist'  [`weight' `exp']
            if mdate >= `start' & mdate <= `end',
            over(mdate) report(``j'' ``jplus'') reportname(`x') `Cluster'
            geo(`geo') coeff_file(`coeff_xml') label("No Controls") 
            `endogenous' fama(1);

        * baseline controls;
        ddregression `varlist'  `controls' [`weight' `exp']
            if mdate >= `start' & mdate <= `end',
            over(mdate) report(``j'' ``jplus'') reportname(`x') `Cluster'
            geo(`geo') coeff_file(`coeff_xml') label("Controls")
            `endogenous' fama(1);

        * cbsa FE;
        ddregression `varlist'  `controls' [`weight' `exp']
            if mdate >= `start' & mdate <= `end',
            absorb(zip_cluster)
            over(mdate) report(``j'' ``jplus'') reportname(`x') `Cluster'
            geo(`geo') coeff_file(`coeff_xml') label("CBSA FE")
            `endogenous' fama(1);

        * All specs below here have cbsa FE
        * no weights;
        ddregression `varlist'  `controls'
            if mdate >= `start' & mdate <= `end',
            absorb(zip_cluster)
            over(mdate) report(``j'' ``jplus'') reportname(`x') `Cluster'
            geo(`geo') coeff_file(`coeff_xml') label("No wgts")
            `endogenous' fama(1);

        * excluding sand states;
        ddregression `varlist'  `controls' [`weight' `exp']
            if mdate >= `start' & mdate <= `end' & sandstate !=1,
            absorb(zip_cluster)
            over(mdate) report(``j'' ``jplus'') reportname(`x') `Cluster'
            geo(`geo') coeff_file(`coeff_xml') label("Ex sand")
            `endogenous' fama(1);

        #delimit cr

        * Close coeff doc.
        coeff_xml_close, fname(`coeff_xml')

        * Turn coeff xml doc into a tex table.
        coeff_tex_compile, input(`coeff_xml') output(`coeff_tex') 

        local j = `j' + 2
    }
    use `orig', clear

    if ("`endogenous'" != "") {
        local endogenouslabel "endog"
    }

    !python $localcodedir/py/tab_monthly.py `coeff_tex_base' "`endogenouslabel'"

end/*%>*/

capture program drop table_longdiff_alt1 /*%< */
program define table_longdiff_alt1
    syntax varname [aweight], geo(name) outdir(string) ///
        [control(name) ENDOGenous]
    * This is similar to regressions_v2, but only for the long difference
    * cross section regressions like M/S, table 6. 

    tempfile orig
    save `orig'

    if "`geo'" == "zip" {
        local absorb_val absorb(zip_cluster)
        local cluster_val cluster(zip_cluster)
        local absorb_labels "CBSA FE"
    }
    if "`geo'" != "zip" {
        local absorb_val absorb(state)
        local cluster_val vce(r)
        local absorb_labels "State FE"
    }

    * Trimming and sample definitions.
    gen longdiff = `varlist'
    sum longdiff, d
    gen trim_sample = cond(longdiff < `r(p95)' & longdiff > `r(p5)', 1, 0)
    sum longdiff if trim_sample == 1, d

    winsor longdiff, p(.01) gen(longdiffW)
    replace longdiff = longdiffW

    if "`endogenous'" != "" {
        local regressors claims_scaled exante_share
    }
    else {
        local regressors exante_share
    }
    tokenize `regressors'
    local treatment `1'

    sum totalhsales_base, d
    gen big_sample = cond(totalhsales_base > `r(p10)', 1, 0)

    set_controls `control'
    local controls $controls
    global controls // Keep register clean

    * Start coeff doc.
    local coeff_xml "`outdir'/longdiff.xml"

    coeff_xml_open, fname(`coeff_xml')
    local coeff_tex "`outdir'/longdiff_`geo'_`varlist'_alt1.tex"

    * cbsa FE
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'], `absorb_val' ///
        `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
        fname(`coeff_xml') label(`absorb_labels')

    * All specs below here have FE
    * no weights
    reg_internal longdiff `regressors' `controls', `absorb_val' ///
        `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("No wgts")

    * excluding sand states
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'] if ///
        sandstate != 1, `absorb_val' `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("Ex sand")

    * trim sample
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'] if ///
        trim_sample == 1, `absorb_val' `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("Trimmed")

    * big sample
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'] if ///
        big_sample == 1, `absorb_val' `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("$\overline{\text{Sales}}$ > P10")

    * low prices
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'] if ///
        inlist(price_level,1,2,3), `absorb_val' `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("Low p")

    * high prices
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'] if ///
        inlist(price_level,8,9,10), `absorb_val' `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("High p")

    * Close coeff doc.
    coeff_xml_close, fname(`coeff_xml')

    * Turn coeff xml doc into a tex table.
    coeff_tex_compile, input(`coeff_xml') output(`coeff_tex')

    use `orig', clear

    !python $localcodedir/py/tab_longdiff_alt1.py

end/*%>*/

capture program drop table_longdiff_alt2 /*%< */
program define table_longdiff_alt2
    syntax varname [aweight], geo(name) outdir(string) ///
        [control(name) ENDOGenous]
    * This is similar to regressions_v2, but only for the long difference
    * cross section regressions like M/S, table 6. 

    tempfile orig
    save `orig'

    if "`geo'" == "zip" {
        local absorb_val absorb(zip_cluster)
        local cluster_val cluster(zip_cluster)
        local absorb_labels "CBSA FE"
    }
    if "`geo'" != "zip" {
        local absorb_val absorb(state)
        local cluster_val vce(r)
        local absorb_labels "State FE"
    }

    * Minor trimming and sample definitions.
    gen longdiff = `varlist'
    sum longdiff, d
    gen trim_sample = cond(longdiff < `r(p95)' & longdiff > `r(p5)', 1, 0)
    sum longdiff if trim_sample == 1, d

    winsor longdiff, p(.01) gen(longdiffW)
    replace longdiff = longdiffW

    if "`endogenous'" != "" {
        local regressors claims_scaled exante_share
    }
    else {
        local regressors exante_share
    }
    tokenize `regressors'
    local treatment `1'

    sum totalhsales_base, d
    gen big_sample = cond(totalhsales_base > `r(p10)', 1, 0)

    set_controls `control'
    local controls $controls
    global controls // Keep register clean

    * Start coeff doc.
    local coeff_xml "`outdir'/longdiff.xml"

    coeff_xml_open, fname(`coeff_xml')
    local coeff_tex "`outdir'/longdiff_`geo'_`varlist'_alt2.tex"

    * cbsa FE
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'], `absorb_val' ///
        `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
        fname(`coeff_xml') label(`absorb_labels')

    * All specs below here have FE
    * no weights
    reg_internal longdiff `regressors' `controls', `absorb_val' ///
        `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("No wgts")

    * excluding sand states
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'] if ///
        sandstate != 1, `absorb_val' `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("Ex sand")

    * trim sample
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'] if ///
        trim_sample == 1, `absorb_val' `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("Trimmed")

    * big sample
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'] if ///
        big_sample == 1, `absorb_val' `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("$\overline{\text{Sales}}$ > P10")

    * low prices
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'] if ///
        inlist(price_level,1,2,3), `absorb_val' `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("Low p")

    * high prices
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'] if ///
        inlist(price_level,8,9,10), `absorb_val' `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("High p")

    * Close coeff doc.
    coeff_xml_close, fname(`coeff_xml')

    * Turn coeff xml doc into a tex table.
    coeff_tex_compile, input(`coeff_xml') output(`coeff_tex')

    use `orig', clear

    !python $localcodedir/py/tab_longdiff_alt2.py

end/*%>*/
