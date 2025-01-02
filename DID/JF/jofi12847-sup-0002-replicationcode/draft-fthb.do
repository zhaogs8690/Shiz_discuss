*******************************************************************************
*   Title: draft-fthb.do
* Purpose: Produces final graphs and tables for main draft, excluding appendix.
*          See the main() program at the bottom for organization.
*   Owner: Eric Zwick
* Authors: David Berger, Nicholas Turner, Eric Zwick
*     RAs: Tom (Tianfang) Cui, Caleb Wroblewski
*    Date: 2019-02-01
*******************************************************************************

*******************************************************************************
* 1. Aggregate charts, time series.
*******************************************************************************
capture program drop graph_narinventories/*%<*/
program define graph_narinventories

    graph twoway (line inventory_sa mdate, lwidth(medthick)), ///
        ytitle("Housing inventories (000s)", margin(medsmall)) ///
        xtitle(,margin(small)) ylab(,labsize(small) nogrid) xlab(,labsize(small)) ///
        legend(off) $graphconfig xline(580 588 598 605, lp(dash) lc(gray))
    
    graph export "nar_inventories.pdf", replace

end/*%>*/

capture program drop graph_narsales/*%<*/
program define graph_narsales

    graph twoway (line EXH*S mdate, lw(0.65) yaxis(1)), ///
        yscale(axis(1)) $graphconfig ///
        yti("Annualized Existing Home Sales (000s)") xti("") legend(off) ///
        xline(580 588 598 605, lp(dash) lc(gray))
    
    graph export "nar_sales.pdf", replace

end/*%>*/

capture program drop graph_distress_share/*%<*/
program define graph_distress_share

    gen hsales_nsa0 = 0
    replace hsales_nsa2 = hsales_nsa1 + hsales_nsa2
    replace hsales_nsa3 = 1 // Subtracting all resales from distress categories
	drop if mdate<528
    #delimit ;
    twoway (rbar hsales_nsa0 hsales_nsa1 mdate, barwidth(.6))
       (rbar hsales_nsa1 hsales_nsa2 mdate, barwidth(.6))
       (rbar hsales_nsa2 hsales_nsa3 mdate, color(khaki*0.7) barwidth(.6))
       , ytitle("Share of Existing Home Sales") $graphconfig
       ylab(,nogrid) xlab(528(24)630 644, labsize(small)) xti("") legend(col(3)
       label(1 "REO & Foreclosure") label(2 "Short Sales") label(3 "Other Sales")
       region(lcolor(white)));
    #delimit cr

    graph export "dq_distress_resales.pdf", as(pdf) replace
end/*%>*/

capture program drop graph_aggregate/*%<*/
program define graph_aggregate

    agg_plot, dname(insample)

end/*%>*/

capture program drop graph_google/*%<*/
program define graph_google
    
    * Date formatting.
    rename mdate rawdate
    format rawdate %td
    gen double mdate =  mofd(rawdate)
    format mdate %tm

    * Graphs.
    #delimit ;
    keep if (year(rawdate) > 2007) &
            (year(rawdate) < 2011);

    collapse term1 term2, by(mdate);
    label var term1 "first time home buyer";
    label var term2 "home buyer credit";

    twoway (line term1 mdate, lcolor(blue))
            (line term2 mdate, lcolor(red)), 
            xline(580 589 598 605, lp(dash) lc(gray))
            xtitle("") xlabel(580 589 598 605) 
            ytitle("Search Activity") 
            $graphconfig;
    #delimit cr
    graph export "google.pdf", replace

end/*%>*/

capture program drop graph_claims/*%<*/
program define graph_claims

    #delimit ;
    twoway (bar total_claims mdate, color(blue*.8) barwidth(.8)
        xline(580 589 598 605, lp(dash) lc(gray))), 
        xlabel(580 589 598 605)
        xtitle("") ytitle("Monthly Tax Credit Claims") 
        $graphconfig;
    #delimit cr

    graph export "natl_claims.pdf", replace

end/*%>*/

*******************************************************************************
* 2. Exposure visuals.
*******************************************************************************
capture program drop graph_map_exante /*%<*/
program define graph_map_exante  

	replace county=12025 if county==12086 // Miami-Dade

	* Mapping with maptile
	* Ensure county1990 is referenced in both maptile DTA and ADo
	* Country 
	maptile exante_share, geo(county1990) n(4) fcolor(Blues) ndfcolor(gs11) ///
		res(.75) legd(3) savegraph("exante_us_county.tif") replace
		
end/*%>*/

capture program drop graph_exposure_maps/*%<*/
program define graph_exposure_maps

    rename zip zip5 // Maptile compatibility
    /*
        Map of Suffolk County (incl. Boston) minus Revere but plus Cambridge.
    */
    #delimit ;
    geoPlots, county(25025) countyExcl(2151) othZip(2138 2139 2140 2141 2142)
        save(exante_bos_zip.pdf);
    #delimit cr

    /*
        Map around San Francisco Bay, south of Berkeley. Includes SF,
        most incorporated parts of Alameda/San Mateo, Palo Alto,
        the Googolplex ZIP and Los Altos Hills.
    */
    #delimit ;
    geoPlots, county(6075 6081 6001) countyExcl(94019 94062 94074 94020 94021 94028
        94060 94550 94552 94586 94566 94588 94568)
        othZip(94301 94303 94304 94305 94306 94309 94022 94043 95002)
        save(exante_bay_zip.pdf);
    #delimit cr

    /*
       This map is a truncated version of Cook County, without the far western
        tip of the panhandle and some of the smaller southwest towns truncated.
        North Shore expanded into view a bit.
    */
    #delimit ;
    geoPlots, county(17031) countyExcl(60107 60120 60192 60411 60439 60443 60462 60464
        60466 60467 60471 60475 60477 60484) othZip(46393 60035 60015 60089 60069)
        save(exante_chi_zip.pdf);
    #delimit cr

end /*%>*/

capture program drop graph_firststage/*%<*/
program define graph_firststage

    tempfile orig
    save `orig'

    local control "`1'"
    local geo zip

    set_controls `control'
    local controls $controls
    global controls // Keep register clean
    local claims_scaled_lab "Total Credits Claimed/Tax Filers in 2007"

    trim_tails claims_scaled, level(.01)
    trim_tails claimsA_scaled, level(.01)

    #delimit;
    binscatter claimsA_scaled exante_share, reportreg 
        savegraph("exante_claimsA_`geo'_filingunits.pdf") replace
        ytitle("`claims_scaled_lab'") $graphconfig nq(100) ylab(, nogrid)
        xtitle("Place-based Exposure");

    binscatter claimsA_scaled exante_share,
        absorb(zip_cluster) control(`controls') 
        savegraph("exante_claimsA_`geo'_filingunits_controls.pdf") replace
        ytitle("`claims_scaled_lab'") $graphconfig nq(100) ylab(, nogrid)
        xtitle("Place-based Exposure");
		
	binsreg claimsA_scaled exante_share, deriv(0) dots(0,0) cb(2,2);
		
	binsreg claimsA_scaled exante_share `controls' i.zip_cluster,
	    line(3,3) ci(3,3) 
		ytitle("`claims_scaled_lab'") $graphconfig ylab(, nogrid)
        xtitle("Place-based Exposure") vce(cl zip_cluster);

    reg claimsA_scaled exante_share `controls',
        absorb(zip_cluster) cluster(zip_cluster);
    #delimit cr

    use `orig', clear

end/*%>*/

capture program drop graph_splitclaims/*%<*/
program define graph_splitclaims

    gsort treatmentvar mdate
    #delimit ;
    twoway (connected total_claims mdate if treatmentvar == 1, 
            mcolor(red*1.6) lpattern(dash) lcolor(red*.8) msymbol(O))
           (connected total_claims mdate if treatmentvar == 0, 
            msymbol(s) lpattern(dash) mcolor(blue*1.6) lcolor(blue*.8)),
        xline(580 589 598 605, lp(dash) lc(gray))
        xlabel(580 589 598 605)
        xtitle("") ytitle("Monthly Tax Credit Claims") 
        $graphconfig
        legend(label(1 "High Exposure ZIPs") 
               label(2 "Low Exposure ZIPs")) xsize(6);
    #delimit cr

    graph export "split_claims_zip.pdf", replace

end /* %> */

*******************************************************************************
* 3. Non-distress sales/"Main" results.
*******************************************************************************
capture program drop graph_heatmap/*%<*/
program define graph_heatmap
    * Requires Tom Cui heatmap package from github.

    local varlist zip
    local weight aweight
    local exp "=totalhsales_base"
    local grid 100

    tempfile orig
    save `orig'

    keep if mdate >= ym(2007,8) & mdate <= ym(2011,10)
    #delimit ;
    heatmap y_trim exante_share mdate [`weight' `exp'], n(`grid') id(`varlist')
        tperiod(yearmon) polbreak(May 2008, Jan 2009, Dec 2009, Jul 2010)
        save(heatmap_zip.pdf) ztitle("Scaled Sales");
    heatmap y_trim exante_share mdate [`weight' `exp'], n(`grid') id(`varlist')
        tperiod(yearmon) polbreak(May 2008, Jan 2009, Dec 2009, Jul 2010)
        control(log_pop pctunemp log_avgagi card_subprime) absorb(zip_cluster)
        save(heatmap_zip_control.pdf) ztitle("Scaled Sales");
    #delimit cr
   
    use `orig', clear

end/*%>*/

capture program drop xp_claims_sales/*%<*/
program define xp_claims_sales

    load_analysis_data hsales
    tempfile sales claims estimates

    drop if month == . | year == .
    collapse (sum) hsales_sa, by(mdate year month)
    gen hsales_monthly = hsales_sa
    replace hsales_sa = 12*hsales_sa/1000
    save `sales'

    load_analysis_data claims
    save `claims'
    merge 1:1 mdate using `sales', keep(1 3) nogen
    save `sales', replace

    insheet using coeffplot_estimate_save.csv, clear
    gen mdate_n = mofd(date(subinstr(mdate, "m", "-", 1), "YM"))
    drop mdate
    rename mdate_n mdate
    save `estimates'
    merge 1:1 mdate using `sales', keep(1 3) nogen
    save `sales', replace

    replace total_claims = total_claims * 1e-3
    #delimit ;
    local start 552;
    local end 640;
    local xline 580 588 598 605;
    format mdate %tm;
    twoway
        (bar total_claims mdate, color(gray*1.5) barwidth(.3) yaxis(2)) 
        (connect treatment_coeff_c mdate,
            mcolor(red*1.6) lpattern(dash) lcolor(red) msymbol(O)
            )
        (rarea pos_conf_c neg_conf_c mdate,
            lcolor(red*.4) lpattern(dash) fcolor(none))
        if mdate >= `start' & mdate <= `end',
        xline(`xline', lcolor(gs9) lp(dash)) xscale(r(`start' `end'))
        $graphconfig xtitle("Month") ytitle("Sales impact coefficient")
        ytitle("Monthly Tax Credit Claims (000s)", axis(2))
        yline(0, lc(gs4)) ylab(#5, nogrid) xlab(#8)
        yscale(range(-80(80)240) axis(2))
        ylab(-80 0 80 160 240, axis(2))
        legend(label(2 "With Controls")
               label(3 "95% Interval")
               label(1 "Total Claims")
              col(3)) xsize(6);
    graph export "coefplot_claims_overlay.pdf", replace;
    #delimit cr

    corr total_claims treatment_coeff_c if mdate >= 588 & mdate <= 605

end/*%>*/

capture program drop graph_impact_coefficients/*%<*/
program define graph_impact_coefficients
    * Runs a cross-sectional regression in every period and plots the derived
    * coefficients. One series for no control, the other with control.
    * Defined roughly over the same period as graph_discrete.

    local geo zip
    local hetero exante
    if ("`1'" == "") {
        local control ez3
    }
    else {
        local control "`1'"
    }
    local weight weight
    local exp "=totalhsales_base"

    * Current versions of monthly and cum
    #delimit ; 
    graph_mian_sufi y_trim exante_share [`weight' `exp'],
        geo(`geo') fig5 control(`control') hetero("`hetero'_msaFE")
        outdir(".") absorb(zip_cluster) cluster(zip_cluster) start(552) end(640);
    graph_mian_sufi y_trim exante_share [`weight' `exp'],
        geo(`geo') fig5 control(`control') hetero("`hetero'_msaFE")
        outdir(".") absorb(zip_cluster) cluster(zip_cluster) cum(1) end(640);
    #delimit cr

    * make a graph that combines the coeff plots with the claims.
    xp_claims_sales

end/*%>*/

*******************************************************************************
* 4. Log specs 
*******************************************************************************
capture program drop regression_logspec/*%<*/
program define regression_logspec

    tempfile orig
    save `orig'

    set matsize 11000

    drop if mdate == 480
	gen year = year(dofm(mdate))
	
    * Best and Kleven spec is
    * log(t)_it = a_0 Pre_t (2 years) + a_H Hol_t (holiday period) 
    *             + a_R Rev_t (1 year) + a_P Post_t (2 year post)
    *             + a_T D_treat + B_H Hol_t D_treat + B_R Rev_t D_treat
    *             + B_P Post_t D_treat

    * Generate outcomes
    gen log_hsales = log(hsales_nsa_nodistress)
    gen log_hsales_all = log(hsales_nsa_all)
    gen log_construction = log(construction)
    gen log_foreclo = log(foreclo_shortsale)

    * Generate Dummies policy period is 588 to 604
    gen pre = cond(mdate >= 564 & mdate <= 587, 1, 0) 
    gen policy = cond(mdate >= 588 & mdate <= 604, 1, 0) 
    gen rev = cond(mdate >= 605 & mdate <= 616, 1, 0) 
    gen post = cond(mdate >= 617 & mdate <= 641, 1, 0) 
    egen msa_month = group(zip_cluster mdate)

	* Create controls and dummy locals
	local controls_graph c.($controls)#mdate
	local dummies_graph mdate
    local start_yr 2004
	
    * Log sales with specs from paper.
    sum totalhsales_base, d
    gen big_sample = cond(totalhsales_base > `r(p10)', 1, 0)

    sum log_hsales, d
    gen trim_sample = cond(log_hsales < `r(p95)' & log_hsales > `r(p5)', 1, 0)
    
	la var log_avgagi "Avg. Inc"
	la var log_pop "Pop."
	la var pctunemp "Unemp."
	la var card_subprime "Subprime"
	la var exante_share "Exante"
	la var subprime_ratio "House Sub."
	la var fha_expansion_exposure "FHA Inst."
	la var hamp_exposure "HAMP Exp."
	gen treatment_coeff_c = .
	gen std_error_c = .

    gen log_hsalesW = log_hsales
    replace log_hsales = . if log_hsalesW > 4.585 /* Top winsorized at 99% */

	reghdfe log_hsalesW `controls_graph' c.exante_share#`dummies_graph' ///
	    [aw=totalhsales_base] if year >= `start_yr' & mdate < 641, absorb(zip msa_month) vce(cluster zip)
		
	forvalues mmonth = 528/640 {
	    replace treatment_coeff_c = _b[`mmonth'.mdate#c.exante_share] if mdate==`mmonth'
		replace std_error_c = _se[`mmonth'.mdate#c.exante_share] if mdate==`mmonth'
	}
	
    collapse (max) treatment_coeff* std_error*, by(mdate)
	export delimited if mdate>=504 using ///
	    "interaction_coeffs_se_final_graph_20171219.csv", nolabel datafmt replace

    use `orig', clear

end/*%>*/

capture program drop graph_logspec/*%<*/
program define graph_logspec

    regression_logspec

    insheet using "interaction_coeffs_se_final_graph_20171219.csv", clear
    gen mdate2 = mofd(date(subinstr(mdate, "m", "-", 1), "YM"))
    drop mdate
    rename mdate2 mdate
    format %tm mdate

	gen pos_conf_c = treatment_coeff_c + 1.96*std_error_c
	gen neg_conf_c = treatment_coeff_c - 1.96*std_error_c

    twoway (connect treatment_coeff_c mdate, mcolor(red*1.6) lpattern(dash) ///
	    lcolor(red*.8) msymbol(O)) (rarea pos_conf_c neg_conf_c mdate, ///
        lcolor(red*.2) lpattern(dash) fcolor(none)) if mdate>=552 & mdate<=640, ///
	    xscale(r(552 640)) xtitle("Month") ytitle("Month-by-Month Coefficients") ///
		yline(0, lc(gs4)) xlab(#8) ylab(#5, nogrid) legend(order(1 "With Controls" ///
		2 "95% Confidence Interval") col(2)) xsize(12) ///
		xline(580 588 598 605, lp(dash) lc(gray)) $graphconfig
	graph export "logspec_coeffplot.pdf", replace

end/*%>*/

*******************************************************************************
* 5. Age distribution 
*******************************************************************************
capture program drop graph_agedist/*%<*/
program define graph_agedist

    #delimit ;
    twoway
        scatter share age_primary if tax_yr==2003, c(l) ms(none) lw(medthick) lc(gs13) lpattern(longdash) ||
        scatter share age_primary if tax_yr==2004, c(l) ms(none) lw(medthick) lc(gs13) lpattern(longdash) ||
        scatter share age_primary if tax_yr==2005, c(l) ms(none) lw(medthick) lc(gs13) lpattern(longdash) ||
        scatter share age_primary if tax_yr==2006, c(l) ms(none) lw(medthick) lc(gs13) lpattern(longdash) ||
        scatter share age_primary if tax_yr==2007, c(l) ms(none) lw(medthick) lc(gs13) lpattern(longdash) ||
        scatter share age_primary if tax_yr==2008, c(l) ms(none) lw(medthick) lc(gs13) lpattern(longdash) ||
        scatter share age_primary if tax_yr==2010 & claim_data == 0, c(l) ms(none) lw(medthick) lc(gs8) lpattern(shortdash) ||
        scatter share age_primary if tax_yr==2011, c(l) ms(none) lw(medthick) lc(gs8) lpattern(shortdash) ||
        scatter share age_primary if tax_yr==2012, c(l) ms(none) lw(medthick) lc(gs8) lpattern(shortdash) ||
        scatter share age_primary if tax_yr==2013, c(l) ms(none) lw(medthick) lc(gs8) lpattern(shortdash) ||
        scatter share age_primary if tax_yr==2009 & claim_data == 0, c(l) ms(none) lw(medthick) lc(navy)
        legend(order(1 "Pre-Policy" 7 "Post-Policy") col(3))
        xtitle("Age of Primary Taxpayer") 
        ytitle("Share of First-Time Buyers") ylab(, nogrid)
        ylabel(.01(.01).05) 
        xlabel(20(10)60) $graphconfig;
    graph export "age_distribution_f.pdf", replace;
    #delimit cr

end/*%>*/

cap program drop age_distribution_hilo/*%<*/
program define age_distribution_hilo

    tempfile orig
    save `orig'

    collapse (sum) count, by(tax_yr age_primary treat_decile)
    *NT: note shares defined by yearly total
    egen total=sum(count), by(tax_yr)
    gen share=count/total

    rename tax_yr fthb_year
    rename age_primary fthb_age

    twoway ///
        line share fthb_age if fthb_year==2009 & treat_==1, c(l) ms(none) lw(medthick) lpattern(solid) lc(red*1.2)|| ///
        line share fthb_age if fthb_year==2007 & treat_==1, c(l) ms(none) lw(medthick) lpattern(longdash) lc(gs13)|| ///
        line share fthb_age if fthb_year==2005 & treat_==1, c(l) ms(none) lw(medthick) lpattern(longdash) lc(gs13)|| /// 
        line share fthb_age if fthb_year==2003 & treat_==1, c(l) ms(none) lw(medthick) lpattern(longdash) lc(gs13)|| ///
        line share fthb_age if fthb_year==2001 & treat_==1, c(l) ms(none) lw(medthick) lpattern(longdash) lc(gs13)|| ///
        line share fthb_age if fthb_year==2011 & treat_==1, c(l) ms(none) lw(medthick) lpattern(shortdash) lc(gs8)|| ///
        line share fthb_age if fthb_year==2013 & treat_==1, c(l) ms(none) lw(medthick) lpattern(shortdash) lc(gs8)|| ///
        line share fthb_age if fthb_year==2009 & treat_==10, c(l) ms(none) lw(medthick) lpattern(solid) lc(navy)|| ///
        line share fthb_age if fthb_year==2007 & treat_==10, c(l) ms(none) lw(medthick) lpattern(longdash) lc(gs13)|| ///
        line share fthb_age if fthb_year==2005 & treat_==10, c(l) ms(none) lw(medthick) lpattern(longdash) lc(gs13)|| ///
        line share fthb_age if fthb_year==2003 & treat_==10, c(l) ms(none) lw(medthick) lpattern(longdash) lc(gs13)|| ///
        line share fthb_age if fthb_year==2001 & treat_==10, c(l) ms(none) lw(medthick) lpattern(longdash) lc(gs13)|| ///
        line share fthb_age if fthb_year==2011 & treat_==10, c(l) ms(none) lw(medthick) lpattern(shortdash) lc(gs8)|| ///
        line share fthb_age if fthb_year==2013 & treat_==10, c(l) ms(none) lw(medthick) lpattern(shortdash) lc(gs8) ///
        xtitle("Age of Primary Taxpayer") ytitle("Share of First-Time Buyers") ///
        ylab(, nogrid) legend(order(2 "Pre-Policy"  6 "Post-Policy") row(1)) ///
        ylabel(0(.002).01) $graphconfig
    graph export "age_distribution_hilo.pdf", replace

    use `orig', clear

end/*%>*/

*******************************************************************************
* 6. Longdiffs, reallocation and heterogeneity..
*******************************************************************************
capture program drop graph_price_coeff/*%<*/
program define graph_price_coeff

    if ("`1'" == "") {
        local control ez3
    }
    else {
        local control "`1'"
    }

    set_controls `control'
    local controls $controls

    matrix drop _all
    areg average_delta_sales price_level##c.exante_share ///
        `controls' ///
        [weight=totalhsales_base], ///
        absorb(zip_cluster) vce(cluster zip_cluster)
    matrix b = (nullmat(b), _b[exante_share])
    matrix se = (nullmat(se), _se[exante_share])
    forv i = 2/10 {
        lincom exante_share + `i'.price_level#c.exante_share
        matrix b = (nullmat(b), `r(estimate)')
        matrix se = (nullmat(se), `r(se)')
    }
    matrix b = b'
    matrix se = se'
    clear
    set obs 10
    svmat b
    svmat se
    egen price_level = seq()
    gen b1neg = b1 - 1.96*se1
    gen b1pos = b1 + 1.96*se1

    #delimit ;
    twoway  
        (rcap b1pos b1neg price_level, lc(navy*0.4) lp("--."))
        (scatter b1 price_level, ms(o) lc(navy) mc(navy)), 
        legend(off) xti("Price Level Bin") yti("Sales Longdiff Coefficient") $graphconfig
        xlab(1(1)10) ylab(,nogrid) yline(0, lc(black));
    #delimit cr
    graph export "coeffplot_sales_pricelevel.pdf", replace

end/*%>*/

*******************************************************************************
* 7. RK 
*******************************************************************************
capture program drop rk_graphs /*%<*/
program define rk_graphs

    local rkdir "$rawdatadir"

    * First stage graph.
    insheet using "`rkdir'/kink1_9_firststage_nonzero.csv", comma clear
    #delimit ;
    twoway 
        (scatter fthbc2 dist_bin)
        (line fthbc2_hat dist_bin if dist_bin >= 0)
        (line fthbc2_hat dist_bin if dist_bin <= 0, lc(maroon)),
        ytitle("FTHC Amount (Nominal USD)") $graphconfig ylab(, nogrid)
        xtitle("Adjusted Gross Income Relative to Kink")
        xline(0, lp(dash) lc(gray))
        ysca(range(5000 9000)) ylab(#5)
        xsca(range(-9000 9000)) xlab(-8000(4000)8000)
        legend(off);
    graph export "rk_firststage.pdf", replace;
    #delimit cr

    * Reduced form graphs.
    tempfile rf_yr
    insheet using "`rkdir'/kink1_9_reduced_form.csv", comma clear
    #delimit ;
    twoway 
        (scatter fthb dist_bin)
        (line fthb_hat dist_bin if dist_bin >= 0)
        (line fthb_hat dist_bin if dist_bin <= 0, lc(maroon)),
        ytitle("P(First-Time Homebuyer)") $graphconfig ylab(, nogrid)
        xtitle("Adjusted Gross Income Relative to Kink")
        xline(0, lp(dash) lc(gray))
        ysca(range(.010 .018)) ylab(.010(.002).018)
        xsca(range(-9000 9000)) xlab(-8000(4000)8000)
        legend(off);
    graph export "rk_reducedform.pdf", replace;
    #delimit cr
    gen year = 2009
    save `rf_yr'

    * Predicted P(FTHB).
    insheet using "`rkdir'/kink1_9_predicted.csv", comma clear
    #delimit ;
    twoway 
        (scatter fthb_hat dist_bin)
        (line fthb_hat_hat dist_bin if dist_bin >= 0)
        (line fthb_hat_hat dist_bin if dist_bin <= 0, lc(maroon)),
        ytitle("Predicted P(First-Time Homebuyer)") $graphconfig ylab(, nogrid)
        xtitle("Adjusted Gross Income Relative to Kink")
        xline(0, lp(dash) lc(gray))
        ysca(range(.010 .018)) ylab(.010(.002).018)
        xsca(range(-9000 9000)) xlab(-8000(4000)8000)
        legend(off);
    graph export "rk_predicted.pdf", replace; 
    #delimit cr

    * Placebo graphs.
    use "`rkdir'/PLBO_yr_all", clear
    replace year = 2000 + year

    foreach v of varlist ci_h ci_l b_rf {
        replace `v' = `v'*1e4
    }
	
    forv y = 2005/2013 {
        
        * Skip 2009
        if (`y' == 2009) {
            continue
        }

        insheet using "`rkdir'/kink1_reduced_form_`y'.csv", comma clear
        gen year = `y'
        append using `rf_yr'
        save `rf_yr', replace
    }

    #delimit ;
    twoway 
        (scatter fthb dist_bin if year == 2005, mc(gray) msize(small))
        (line fthb_hat dist_bin if dist_bin >= 0 & year == 2005, lc(gray))
        (line fthb_hat dist_bin if dist_bin <= 0 & year == 2005, lc(gray))
        (scatter fthb dist_bin if year == 2006, mc(gray) msize(small))
        (line fthb_hat dist_bin if dist_bin >= 0 & year == 2006, lc(gray))
        (line fthb_hat dist_bin if dist_bin <= 0 & year == 2006, lc(gray))
        (scatter fthb dist_bin if year == 2007, mc(gray) msize(small))
        (line fthb_hat dist_bin if dist_bin >= 0 & year == 2007, lc(gray))
        (line fthb_hat dist_bin if dist_bin <= 0 & year == 2007, lc(gray))
        (scatter fthb dist_bin if year == 2008, mc(gray) msize(small))
        (line fthb_hat dist_bin if dist_bin >= 0 & year == 2008, lc(gray))
        (line fthb_hat dist_bin if dist_bin <= 0 & year == 2008, lc(gray))
        (scatter fthb dist_bin if year == 2010, mc(gray) msize(small))
        (line fthb_hat dist_bin if dist_bin >= 0 & year == 2010, lc(gray))
        (line fthb_hat dist_bin if dist_bin <= 0 & year == 2010, lc(gray))
        (scatter fthb dist_bin if year == 2011, mc(gray) msize(small))
        (line fthb_hat dist_bin if dist_bin >= 0 & year == 2011, lc(gray))
        (line fthb_hat dist_bin if dist_bin <= 0 & year == 2011, lc(gray))
        (scatter fthb dist_bin if year == 2012, mc(gray) msize(small))
        (line fthb_hat dist_bin if dist_bin >= 0 & year == 2012, lc(gray))
        (line fthb_hat dist_bin if dist_bin <= 0 & year == 2012, lc(gray))
        (scatter fthb dist_bin if year == 2013, mc(gray) msize(small))
        (line fthb_hat dist_bin if dist_bin >= 0 & year == 2013, lc(gray))
        (line fthb_hat dist_bin if dist_bin <= 0 & year == 2013, lc(gray))
        (scatter fthb dist_bin if year == 2009, mc(navy) msym(s))
        (line fthb_hat dist_bin if dist_bin >= 0 & year == 2009, lc(navy))
        (line fthb_hat dist_bin if dist_bin <= 0 & year == 2009, lc(navy))
        ,
        ytitle("P(First-Time Homebuyer)") $graphconfig ylab(, nogrid)
        xtitle("Adjusted Gross Income Relative to Kink")
        xline(0, lp(dash) lc(gray))
        xsca(range(-9000 9000)) xlab(-8000(4000)8000)
        legend(off);
    graph export "rk_reducedform_placebos.pdf", replace;
    #delimit cr

    * For ordering labels in tikz
    tabstat fthb, by(year) 

end/*%>*//*%>*/

*******************************************************************************
* 8. Prices & Defaults 
*******************************************************************************
capture program drop graph_hpcoeffs/*%<*/
program define graph_hpcoeffs

    local wgtv totalhsales_base
    if ("`1'" == "") {
        local control ez3
    }
    else {
        local control "`1'"
    }
    local startyr 2002
    local endyr 2012

    xtset zip year
    gen resD = D.res
    gen hpiD = D.annualchange

    keep if year >= `startyr'
    sort zip year
    by zip: gen cum_resD = sum(resD)

    graph_HPICoeffPlots resD exante_share [weight=`wgtv'], absorb(zip_cluster) ///
        startyr(`startyr') endyr(`endyr') cluster(zip_cluster) control(`control') ///
        hetero(basic)

end/*%>*/

capture program drop graph_defaulters /*%<*/
program define graph_defaulters

    local qualcol color("141 211 199" "190 186 218")
    #delimit ;
    twoway (connect rate3 length, mc(gs6) lc(gs10) m(dh)) 
        (connect rate1-rate2 length, msize(small small) `qualcol' m(S s)) 
        if length <= 60, name(twoplot, replace)
        xline(12, lcolor(maroon)) ylab(,nogrid) legend(off) xlab(none) xti("")
        xscale(range(0 60)) $graphconfig
        title(Policy cohorts with 2011 post cohort, size(normal));

    twoway (connect rate3-rate6 length, mcolor(gs6 gs6 gs6 gs6)
        m(dh Th oh X) lc(gs10 gs10 gs10 gs10))
        (connect rate1-rate2 length, msize(small small) `qualcol' m(S s))
        if length <= 60, name(allplot, replace)
        xline(12, lcolor(maroon)) ylab(,nogrid) fysize(55) legend(rows(2))
        bgcolor(white) $graphconfig legend(size(small))
        title(Including pre-policy cohorts, size(normal));
        

    graph combine twoplot allplot, col(1)
        imargin(0 0 0 0) graphregion(c(white))
        subtitle("Rate of distress sales per 1000 mortgages               ", position(10)
        size(small) orientation(vertical))
        caption("Denominator is a running sum of new sales in each month up to the gray line," 
        "after which it remains constant.", size(small));
    #delimit cr
    graph export "DQ_distress_plot.pdf", as(pdf) replace

end/*%>*/

*******************************************************************************
* 9. Tables 
*******************************************************************************
capture program drop table_summary_stats/*%<*/
program define table_summary_stats
    
    summary_stats zip

end/*%>*/

capture program drop table_covariates/*%<*/
program define table_covariates

    local geo zip

    tempfile orig
    save `orig'
    duplicates drop `geo', force

    #delimit;
    local RHSII medianage mediangrossrent 
        pctpoor pcturban log_pop pctunemp log_avgagi card_subprime
        subprime_ratio fha_expansion_exposure gse_ratio hamp_exposure; 
    local RHSII_desc "Median Age+Median Rent+
        Fraction below Poverty Line+Fraction Classified as Urban+
        Log(Population)+Unemployment Rate+Log(Average Gross Income)+
        Subprime Cardholder Fraction+Subprime in 2004-2007+
        FHA Expansion Exposure+HARP Exposure+HAMP Exposure"; 

    tabII `RHSII', lhs(exante_share) outdir(".")
        labels("`RHSII_desc'") geo(`geo');
    #delimit cr

    use `orig', clear

end/*%>*/

capture program drop table_policies/*%<*/
program define table_policies
    * Regresses the instrument against average of the LHS over specified
    * periods, for greater efficiency than a period-by-period regression.
    * Closest analogue in M-S is Panel B, Table V.

    local geo zip
    local hetero exante
    if ("`1'" == "") {
        local control ez2019
    }
    else {
        local control "`1'"
    }
    local weight weight
    local exp "=totalhsales_base"

    tempfile orig
    save `orig'

    if "`if'" != "" {
        keep if `if'
    }
    #delimit ;
    regressions_v2 y_trim exante_share [`weight' `exp'], outdir(".")
        start(572) end(628) cluster(zip_cluster) geo(`geo')
        control(`control') hetero(`hetero')
        report(572 587 588 604 605 628 588 595 596 598 602 604)
        reportname(pre policy post earlypolicy spike1 spike2);
    #delimit cr

    use `orig', clear

end/*%>*/

capture program drop graphtable_longdiff/*%<*/
program define graphtable_longdiff

    local wgtv totalhsales_base
    if ("`1'" == "") {
        local control ez2019
    }
    else {
        local control "`1'"
    }
    
    foreach x of varlist ///
            average_delta_sales average_delta_built ///
            delta_pricegrowth_fhares delta_pricegrowth_fha delta_pricegrowth ///
            delta_pricegrowth_raw delta_pricegrowth_raw_sdnorm ///
            average_delta_foreclo average_delta_foreclo2 {
        graph_longdiff zip [aweight=`wgtv'], v(`x') control(`control')
        table_longdiff `x' [aweight=`wgtv'], ///
            geo(zip) outdir(".") control(`control')
    }

end/*%>*/

*******************************************************************************
* Main
*******************************************************************************
capture program drop main
program define main

    capture log c
	log using "$outputdir/draft-fthb.log", replace
    cd $outputdir
	local control ez2019
    set_controls `control'
	
    *************************************************************
    * Graphs
    *************************************************************
	* Aggregate Graphs 
	load_analysis_data nar
	graph_narinventories
	graph_narsales
	
	load_analysis_data distress
    graph_distress_share
	
	load_analysis_data hsales
    graph_aggregate
	
	load_analysis_data google
    graph_google 

	* Exposure Graphs 
	load_analysis_data geocounty
    graph_map_exante
	
	load_analysis_data geozip
	graph_exposure_maps 
	
	load_analysis_data zipsample_cont
    graph_firststage `control' 
	
	load_analysis_data split_claims
	graph_splitclaims 
	
	load_analysis_data claims
    graph_claims
	
	* Main Results 
	load_analysis_data hsales
	graph_heatmap 
	
	load_analysis_data hsales_cont
	graph_impact_coefficients `control' 
	
	** !! Big Matrix !!
	load_analysis_data logspec_cont 
	graph_logspec 
	
	* Age Distribution 
	load_analysis_data age
    graph_agedist
	
	load_analysis_data agetdec
    age_distribution_hilo 
	
	* Heterogeneity, Longdiffs, etc. 
	load_analysis_data longdiff_cont
    graph_price_coeff `control' 
	
	* RK Graphs 
	* RK Graphs use data pulled internally at IRS, which is loaded as 
	* part of the rk_graphs program.
	rk_graphs
	
	* Prices 
	load_analysis_data prices_fha_cont
    graph_hpcoeffs `control'
	
	* Defaults 
	load_analysis_data defaulters
    graph_defaulters

	*************************************************************
    * Tables
    *************************************************************
	load_analysis_data hsales_cont
    table_summary_stats
    table_covariates
	table_policies 
	
    * Uses micro IRS data, available to researchers with approval
    *table_rk

    load_analysis_data longdiff_cont
	** NB: Graphs for Appendix D.5 Are Constructed Here
    graphtable_longdiff `control' 

    cd $localcodedir
    log close
end
