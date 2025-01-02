*******************************************************************************
*   Title: appendix-fthb.do
* Purpose: Produces final graphs and tables for online appendix.
*          See the main() program at the bottom for organization.
*   Owner: Eric Zwick
* Authors: David Berger, Nicholas Turner, Eric Zwick
*     RAs: Tom (Tianfang) Cui, Caleb Wroblewski
*    Date: 2019-02-01
*******************************************************************************

capture program drop setup_exposure_robust/*%<*/
program define setup_exposure_robust

    load_analysis_data zip_panel 

    rename ft_cons total_buyers

    * Generate three alternative exposure measures (used in robustness analysis
    * for appendix).
    tempfile orig
    gen exante_share_yr = total_buyers/f_units
    gen exante_share_inst = total_buyers/filingunits_2000
    keep if exante_share_yr < .5
    save `orig'

    tempfile exposure_alt
    use `orig', clear
    keep if tax_yr == 2007
    gen exante_share_alt1 = exante_share_yr
    keep zip exante_share_alt1 
    save `exposure_alt'

    use `orig', clear
    keep if tax_yr < 2008
    collapse (mean) exante_share_yr [aw=f_units], by(zip)
    rename exante_share_yr exante_share_alt2
    merge 1:1 zip using `exposure_alt', keep(1 3) nogen
    save `exposure_alt', replace

    if ("`1'" == "panel") {
        use `orig', clear
        merge m:1 zip using `exposure_alt', keep(1 3) keepusing(exante_share_alt1 exante_share_alt2) nogen
    } 
    else if ("`1'" == "xsection") {
        use `exposure_alt', clear
    }

end/*%>*/

capture program drop graph_exposure_robust/*%<*/
program define graph_exposure_robust 

    setup_exposure_robust panel

    * 1. Show correlation between exante_share and alternatives is high.
    gen basic_corr_alt1 = .
	gen basic_corr_alt2  = .
	gen iv_corr_alt1     = .
	gen iv_corr_alt2   = .
    forv year=2000/2007 {
        * Basic correlation
        corr exante_share exante_share_yr [aw=totalhsales_base] if tax_yr == `year'
        replace basic_corr_alt1 = `r(rho)' if tax_yr == `year'

        * Removes measurement error
        ivreg2 exante_share (exante_share_yr=exante_share_inst) [aw=totalhsales_base] if tax_yr == `year'
        replace iv_corr_alt1 = _b[exante_share_yr] if tax_yr == `year'

        * Basic correlation
        corr exante_share exante_share_alt2 [aw=totalhsales_base] if tax_yr == `year'
        replace basic_corr_alt2 = `r(rho)' if tax_yr == `year'

        * Removes measurement error
        ivreg2 exante_share (exante_share_alt2=exante_share_alt1) [aw=totalhsales_base] if tax_yr == `year'
        replace iv_corr_alt2 = _b[exante_share_alt2] if tax_yr == `year'
    }

	drop if tax_yr > 2007
	collapse (max) *_corr_*, by(tax_yr)
	gsort - tax_yr
	gen row = _n
	gsort tax_yr
	label define ylabels 1 "2007" 2 "2006" 3 "2005" 4 "2004" 5 "2003" 6 "2002" 7 "2001" 8 "2000"
	label values row ylabels

	twoway ///
	       (connect basic_corr_alt1 tax_yr, mc(navy) m(t) lcolor(navy)) ///
	       (connect iv_corr_alt1 tax_yr, mc(navy) m(t) lcolor(navy) lpattern(dash)) ///
	       (connect basic_corr_alt2 tax_yr, mc(maroon) m(s) lcolor(maroon)) ///
		   (connect iv_corr_alt2 tax_yr, mc(maroon) lc(maroon) m(s) lpattern(dash)), ///
		   plotregion(fcolor(white) lcolor(white)) graphregion(fcolor(white) ///
		   lcolor(white)) legend(order(1 "Basic Correlation Yearly" 2 "IV Correlation Yearly" ///
		        3 "Basic Correlation AVG" 4 "IV Correlation AVG")) ytitle("Correlation") ///
		   xtitle("Year") yscale(range(0 1)) ylab(0(0.25)1, nogrid) xscale(range(2000 2007)) ///
           xlab(#8) xsize(8)
	graph export correlations_connect.pdf, replace

end/*%>*/

capture program drop table_exposure_robust/*%<*/
program define table_exposure_robust

    if ("`1'" == "") {
        local control ez2019
    }
    else {
        local control "`1'"
    }

    set_controls `control'

    tempfile exposure_alt
    setup_exposure_robust xsection 
    save `exposure_alt'

    load_analysis_data zipsample_cont
    merge 1:1 zip using `exposure_alt', keep(1 3) nogen
    rename exante_share exante_share_old

    forv i=1/2 {
        rename exante_share_alt`i' exante_share 
        !mkdir alt`i'
        cd "alt`i'"
        graph_firststage `control'
        rename exante_share exante_share_alt`i'
        #delimit ;
        binscatter exante_share_alt`i' exante_share_old, absorb(zip_cluster)
            savegraph("exante_share_corr_alt`i'.pdf") replace $graphconfig
            ytitle("Place-based Exposure (Alternative)") nq(100) ylab(, nogrid)
            xtitle("Place-based Exposure (Original)");
        #delimit cr
        cd ..
    } 

    load_analysis_data longdiff_cont
    merge 1:1 zip using `exposure_alt', keep(1 3) nogen
    rename exante_share exante_share_old

    forv i=1/2 {
        sum exante_share_alt`i', d
        gen exante_share = exante_share_alt`i'/`r(sd)'
        !mkdir alt`i'
        cd "alt`i'"
        graphtable_longdiff `control'
        drop exante_share
        cd ..
    }

end/*%>*//*%>*/

capture program drop graph_placebo/*%<*/
program define graph_placebo

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
    
    #delimit ;
    graph_mian_sufi y_trim exante_share [`weight' `exp'],
        outdir(".") geo(`geo') fig6 control(`control') hetero("`hetero'_placebo") ///
        absorb(zip_cluster) cluster(zip_cluster);
    #delimit cr
end/*%>*/

capture program drop graph_age_exposure/*%<*/
program define graph_age_exposure

    drop if tax_yr == 2007 
    gen policy = tax_yr == 2009
    gen young_share = young_buyers/total_buyers
    bys zip policy: egen mean_young_share = mean(young_share)
    bys tax_yr: egen total_buyers_natl = total(total_buyers)
    gen young_share_natl = young_buyers/total_buyers_natl
    bys zip policy: egen mean_young_share_natl = mean(young_share_natl)
    duplicates drop zip policy, force

    sort zip policy
    by zip: gen nrows = _N
    assert nrows == 2
    drop nrows

    by zip: gen excess_young = mean_young_share[2] - mean_young_share[1]
    by zip: gen excess_young_natl = mean_young_share_natl[2] - mean_young_share_natl[1]
    keep if policy == 1

    tempfile excess
    save `excess'

    xtile ebin=exante_share, nq(20)
    collapse (sum) excess_young_natl (mean) exante_share, by(ebin)

    #delimit ;
    binscatter excess_young_natl exante_share, reportreg
        savegraph("excess_young_exante.pdf") replace ylab(, nogrid)
        ytitle("Excess Mass of Young Buyers") $graphconfig
        xtitle("Place-based Exposure");
    #delimit cr

end/*%>*/

cap program drop age_distribution_treat_2009/*%<*/
program define age_distribution_treat_2009

    ** Import Data
    *use "$data/AGE_YR_tdec_incdec", clear
    tempfile orig 
    save `orig'
    
    collapse (sum) count, by(tax_yr treat_decile age_primary)
	*NT: note shares defined by yearly total
    egen total=sum(count), by(tax_yr treat_decile)
    gen share=count/total
    rename age_primary fthb_age

	keep if tax_yr==2009
	twoway ///
	    line share fthb_age if treat_==1, c(l) ms(none) lw(medthick) lpattern(solid) lc(navy*0.1) || ///
		line share fthb_age if treat_==2, c(l) ms(none) lw(medthick) lpattern(solid) lc(navy*0.2) || ///
		line share fthb_age if treat_==3, c(l) ms(none) lw(medthick) lpattern(solid) lc(navy*0.3)  || ///
		line share fthb_age if treat_==4, c(l) ms(none) lw(medthick) lpattern(solid) lc(navy*0.4)  || ///
		line share fthb_age if treat_==5, c(l) ms(none) lw(medthick) lpattern(solid) lc(navy*0.5)  || ///
		line share fthb_age if treat_==6, c(l) ms(none) lw(medthick) lpattern(solid) lc(navy*0.6) || ///
		line share fthb_age if treat_==7, c(l) ms(none) lw(medthick) lpattern(solid) lc(navy*0.7) || ///
		line share fthb_age if treat_==8, c(l) ms(none) lw(medthick) lpattern(solid) lc(navy*0.8) || ///
		line share fthb_age if treat_==9, c(l) ms(none) lw(medthick) lpattern(solid) lc(navy*0.9) || ///
		line share fthb_age if treat_==10, c(l) ms(none) lw(medthick) lpattern(solid) lc(navy)  ///
		xtitle("Age of Primary Taxpayer") ytitle("Share of First-Time Buyers") ///
        ylab(, nogrid) $graphconfig legend(off)
	graph export "age_distribution_treatment.pdf", replace

    use `orig', clear

end/*%>*/

capture program drop xp_fha/*%<*/
program define xp_fha

    * FTHC exposure is correlated with initial FHA share
    binscatter exante_share initial_fha_share, absorb(zip_cluster)

    * Initial FHA share is negatively correlated with house prices
    binscatter initial_fha_share price_level, absorb(zip_cluster) 

    * FHA expansion exposure is always less than FHA total exposure
    twoway (scatter fha_expansion_exposure fha_total_exposure) (line fha_total_exposure fha_total_exposure)

    #delimit ;
    * FHA expansion exposure predicts change in FHA share of eligible loans;
    binscatter delta_fha_expansion_share fha_expansion_exposure, absorb(zip_cluster)
        savegraph("fha_first_stage.pdf") replace $graphconfig
        ytitle("Change in Share of Loans in FHA Expansion Range") nq(100) ylab(, nogrid)
        xtitle("FHA Expansion Exposure");

    binscatter card_subprime fha_expansion_exposure, absorb(zip_cluster)
        savegraph("fha_subprime.pdf") replace $graphconfig
        ytitle("Subprime Cardholder Fraction, 1996") nq(100) ylab(, nogrid)
        xtitle("FHA Expansion Exposure");
    #delimit cr

    * FHA share of eligible loans is very low in pre-period
    sum initial_fha_expansion_share, d

    #delimit ;
    * FHA expansion exposure is positively correlated with house prices;
    preserve;
    collapse (mean) fha_expansion_exposure, by(price_level);
    twoway bar fha_expansion_exposure price_level, $graphconfig
        ytitle("FHA Expansion Exposure") ylab(, nogrid) barw(.7)
        xtitle("Decile in Mean House Price Level (2008)");
    graph export "fha_expansion_prices.pdf", replace;
    restore;

    * FTHC exposure is negatively correlated with FHA expansion exposure;
    binscatter exante_share fha_expansion_exposure, absorb(zip_cluster)
        savegraph("fha_negative_correlation.pdf") replace $graphconfig
        ytitle("Place-based Exposure") nq(100) ylab(, nogrid)
        xtitle("FHA Expansion Exposure");
    #delimit cr

end/*%>*/

capture program drop graph_price_index/*%<*/
program define graph_price_index 

    **************************************************************************
    * 1. National graph/*%<*/
    ***************************************************************************
    tempfile orig
    local btm_q = 5 // or 3
    local top_q = 6 // or 8
    local raw_type med // or "med"
    local sgl_type  // or blank (for median)
    local qtype EZv19 // Pilot, blank, alt, alt5, altrep2, NoTransFilter,
                     // OthTrimFilter, EZv(1-12)

    price_data_prep, q(10)

    foreach var in med_price cl cl_exc {
        gen `var'_base = `var'_sa if mdate == ym(2000,1)
        bys zip: egen `var'_Base = max(`var'_base)
        gen `var'_adj = `var'_sa/`var'_Base*100
    }
    drop group

    preserve
    gen here = 1
    collapse (mean) me*_price_*sa me*winsor (sum) count=here [iw=zip_count], by(mdate)
    gen group = 0
    merge 1:1 mdate using $analysis/DQ_comp_agg, keep(1 3)
    gen sgl_prop = price_count/(price_count+price_count_rpt)
    drop _m
    save `orig'
    restore

    gen group = cond(price_level <= `btm_q' & treatment_level <= `btm_q', 1, cond( ///
                     price_level <= `btm_q' & treatment_level >= `top_q', 2, cond( ///
                     price_level >= `top_q' & treatment_level <= `btm_q', 3, cond( ///
                     price_level >= `top_q' & treatment_level >= `top_q', 4, -1))))
    tab group
    collapse (mean) me*_price_*sa* me*winsor *_adj [iw=zip_count] if group > 0, by(mdate group)
    append using `orig'
    save `orig', replace

    mmerge mdate using $rawdatadir/Mcl, unm(master) ///
        ukeep(cl cl_exc) um(month)
    drop _m
    merge n:1 mdate using $analysis/DQ_sglsales_agg, keep(1 3)
    drop _m
    merge 1:1 mdate group using $analysis/DQ_rptsales`qtype'_agg, keep(1 3)
    drop _m
    save `orig', replace
    save $analysis/DQ_repeat_holder.dta, replace

    use $analysis/DQ_repeat_holder.dta, clear
    keep if group == 0
    tsset mdate
    tssmooth ma index_smooth=index, window(2)

    keep if mdate >= ym(2004, 1) & mdate != .
    sort mdate
    gen c = cl/(cl[1])*100
    gen index_repeat = index/(index[1])*100
    gen index_repeat_smooth = index_smooth/(index_smooth[1])*100
    gen mean_prices = mean_price_agg_sa/(mean_price_agg_sa[1])*100

    #delimit ;
    twoway (connect c index_repeat index_repeat_smooth mean_prices mdate, 
              lc(gs14 navy maroon black)
              lp(solid solid solid "_") 
              lw(thick thin medium thin)
              m(none x none o) msize(none small small medium)
              mc(gs14 navy maroon black)), 
            xline(580 589 598 605, lp(dash) lc(gray)) 
            xtitle("") xlabel(528(24)656) ylab(90(10)140,nogrid)
            ytitle("Normalized Index, 100=2004m1 value")
            yscale(range(90(10)140))
            xsize(8)
            $graphconfig
        legend(
            label(1 "CoreLogic National Index") 
            label(2 "DataQuick Raw Repeat Index") 
            label(3 "DataQuick MA Repeat Index") 
            label(4 "DataQuick Raw Mean Prices") 
            );
    graph export hp_ts_agg_combined.pdf, replace;
    #delimit cr/*%>*/

    ***************************************************************************
    * 2. Single sales share /*%<*/
    ***************************************************************************
    tempfile sgl
    use $analysis/DQ_comp_agg, clear 
    gen sgl_prop = price_count/(price_count_rpt + price_count)

    format mdate %tm
    #delimit ;
    keep if mdate >= ym(2004, 1) & mdate != .;
    twoway (line sgl_prop mdate), $graphconfig
         yti("Share of Sales Excluded from Repeat Index")
            xline(580 589 598 605, lp(dash) lc(gray)) 
            xtitle("") xlabel(528(24)656) ylab(,nogrid); 
    #delimit cr
    graph export hp_share_agg.pdf, replace/*%>*/

    ***************************************************************************
    * 3. Year built distribution/*%<*/
    ***************************************************************************
    use $analysis/DQ_comp_agg_built, clear
    tempfile orig
    save `orig'

    * 1. Check data.
    tabstat yr_count yr_count_rpt, by(year) s(sum)

    * 2. Make bar graph
    keep if yr_built > 1900 & !missing(yr_built)
    
    gen mdate_dummy = (inlist(year, 2009, 2010))
    * Sanity check
    tab year mdate_dummy 

    keep if mdate_dummy == 1 
    collapse (sum) yr_tot_sgl=yr_count yr_tot_rpt=yr_count_rpt , by(yr_built)
    egen total_transactions_sgl = total(yr_tot_sgl)
    egen total_transactions_rpt = total(yr_tot_rpt)
    gen yr_prop_sgl = yr_tot_sgl/total_transactions_sgl
    gen yr_prop_rpt = yr_tot_rpt/total_transactions_rpt

    gen yr_built2 = yr_built + 0.5
    twoway (bar yr_prop_sgl yr_built, barwidth(0.35)) ///
            (bar yr_prop_rpt yr_built2, barwidth(0.35)) ///
            if yr_built > 1998 & yr_built < 2011, graphregion(c(white)) ///
            xlab(1999(1)2010, angle(-60)) yti("Share of Category Sales in 2009-10") ///
            xtitle("Year Home Built") ylab(, nogrid) ///
            legend(label(1 "Single Sales") label(2 "Repeat Sales"))
    graph export hp_yrdist_zoom.pdf, as(pdf) replace

    tabstat yr_tot_sgl yr_tot_rpt, s(sum)
    * 935708   1346082 => 41% of total transactions.

    * 3. Compute statistic on share of single sales (TC code).
    use $analysis/DQ_comp_agg, clear
    gen mdate_dummy = inrange(mdate, 588, 604)
    keep if mdate_dummy == 1
    collapse (sum) price_count_rpt price_count
    gen single_share = price_count/(price_count + price_count_rpt)
    list
        /*+-----------------------------------+
          | price_~pt   price_c~nt   single~e |
          |-----------------------------------|
       1. | 1065159     814275       .4332554 |
          +-----------------------------------+*//*%>*/
    
end/*%>*/

capture program drop table_logspec /*%<*/
program define table_logspec

    tempfile orig
    save `orig'

    drop if mdate == 480
	gen year = year(dofm(mdate))
	
	* Generate outcomes
    gen log_hsales = log(hsales_nsa_nodistress)
	
	* Generate time vars
	gen early_pre = cond(mdate>=555 & mdate<=579, 1, 0)
	gen late_pre  = cond(mdate>=580 & mdate<=587, 1, 0)
    gen policy    = cond(mdate >= 588 & mdate <= 604, 1, 0) 
	gen rev       = cond(mdate>=605 & mdate<=616, 1, 0)
	gen post      = cond(mdate>=617 & mdate<=640, 1, 0)
    egen msa_month = group(zip_cluster mdate)

	
	* Create controls and dummy locals
	local controls_ckw c.($controls)#mdate
	local dummies_ckw (1.early_pre 1.late_pre 1.policy 1.rev 1.post)
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
	la var hamp_exposure "HAMP exp."

    gen log_hsalesW = log_hsales
    replace log_hsales = . if log_hsalesW > 4.585 /* Top winsorized at 99% */

    * CBSA FE
    eststo: reghdfe log_hsalesW `controls_ckw' c.exante_share#`dummies_ckw' ///
	    [aw=totalhsales_base] if year >= `start_yr' & mdate<641, absorb(zip msa_month) vce(cluster zip)

	foreach coeff in early_pre late_pre policy rev post {
	    local b_`coeff'_base  = _b[c.exante_share#1.`coeff']
        local se_`coeff'_base = _se[c.exante_share#1.`coeff']
		local n_`coeff'_base  = `e(N)'
		local r2_`coeff'_base = `e(r2)'
	}
	
	* No weights
    eststo: reghdfe log_hsalesW `controls_ckw' c.exante_share#`dummies_ckw'  ///
        if year >= `start_yr' & mdate<641, absorb(zip msa_month) vce(cluster zip)
		
    foreach coeff in early_pre late_pre policy rev post {
	    local b_`coeff'_noweight  = _b[c.exante_share#1.`coeff']
        local se_`coeff'_noweight = _se[c.exante_share#1.`coeff']
	    local n_`coeff'_noweight  = `e(N)'
		local r2_`coeff'_noweight = `e(r2)'
	}

    * excluding sand states
    eststo: reghdfe log_hsalesW `controls_ckw' c.exante_share#`dummies_ckw' ///
        if sandstate != 1 & year>=`start_yr' & mdate<641 [aw=totalhsales_base], ///
		absorb(zip msa_month) vce(cluster zip)
    
	foreach coeff in early_pre late_pre policy rev post {
	    local b_`coeff'_sand  = _b[c.exante_share#1.`coeff']
        local se_`coeff'_sand = _se[c.exante_share#1.`coeff']	
		local n_`coeff'_sand  = `e(N)'
		local r2_`coeff'_sand = `e(r2)'
	}
	
    * big sample
    eststo: reghdfe log_hsalesW `controls_ckw' c.exante_share#`dummies_ckw'  ///
        if big_sample == 1 & year>= `start_yr' & mdate<641 [aw=totalhsales_base], ///
		absorb(zip msa_month) vce(cluster zip)
	
	foreach coeff in early_pre late_pre policy rev post {
	    local b_`coeff'_big  = _b[c.exante_share#1.`coeff']
        local se_`coeff'_big = _se[c.exante_share#1.`coeff']	
	    local n_`coeff'_big  = `e(N)'
		local r2_`coeff'_big = `e(r2)'
	}
		
	foreach coeff in early_pre late_pre policy rev post {
	    local coeff_xml "new_table_`coeff'.xml"
        local coeff_tex "new_table_`coeff'.tex"
        coeff_xml_open, fname(`coeff_xml')	
        coeff_xml_write, coeff(`b_`coeff'_base') se(`se_`coeff'_base') ///
		   n(`n_`coeff'_base') r2(`r2_`coeff'_base') fname(`coeff_xml') ///
		   label("Base")
        coeff_xml_write, coeff(`b_`coeff'_noweight') se(`se_`coeff'_noweight') ///
		   n(`n_`coeff'_noweight') r2(`r2_`coeff'_noweight') fname(`coeff_xml') ///
		   label("No wgts")
        coeff_xml_write, coeff(`b_`coeff'_sand') se(`se_`coeff'_sand') ///
		   n(`n_`coeff'_sand') r2(`r2_`coeff'_sand') fname(`coeff_xml') ///
		   label("Ex sand")	
        coeff_xml_write, coeff(`b_`coeff'_big') se(`se_`coeff'_big') ///
		   n(`n_`coeff'_big') r2(`r2_`coeff'_big') fname(`coeff_xml') ///
		   label("$\overline{\text{Sales}} >$ P10")	
		   
        * Close coeff doc.
        coeff_xml_close, fname(`coeff_xml')

        * Turn coeff xml doc into a tex table.
        coeff_tex_compile, input(`coeff_xml') output(`coeff_tex')
	}

    !python $localcodedir/py/tab_logspecs.py new_table ""
    use `orig', clear
	
end/*%>*/

capture program drop table_waterfall/*%<*/
program define table_waterfall

    * Actual labels for filtered rows
    local labm2 "Matched between assessor and transaction data"
    local labm1 "$ +$ Arm's length transactions w/valid geo, month"
    local lab1 "$ +$ Cleaned resales \& new sales over 2004-2013"
    local lab2 "$ +$ Non-distress resales"
    local lab3 "$ +$ Time series 90\%$ +$ complete over 2006-2013"
    local lab4 "$ +$ Matched to exposure variables and covariates"

    foreach geo in zip county cbsa {
        load_counts, geo(`geo') datadir($analysis) hetero("resales_nodistress")
    }
    
    * Table built in two panels below
    #delimit ;
    estout zip county cbsa using waterfall.tex, 
        cells(b(fmt(%11.2gc)) se(fmt(%6.4g) par("(" " M)")))
        mlabels(ZIPs Counties CBSAs) collabel(,none) varl(
        "-2" "`labm2'" "-1" "`labm1'" 1 "`lab1'" 2 "`lab2'" 3 "`lab3'" 4 "`lab4'")
        prehead("\newcolumntype{C}[1]{>{\hsize=#1\hsize\centering\arraybackslash}X}"
        "\begin{tabularx}{\textwidth}{ XC{0.21}C{0.21}C{0.21} }"  "\toprule")
        posthead("\hline" "\multicolumn{4}{l}{\textbf{Geo-month observations 
        (transaction counts in parentheses)}} \\")
        style(tex) replace;
        
    estout zip county cbsa using waterfall.tex, cells(u) 
        mlabels(,none) collabel(,none) varl(
        "-2" "`labm2'" "-1" "`labm1'" 1 "`lab1'" 2 "`lab2'" 3 "`lab3'" 4 "`lab4'")
        posthead( "\multicolumn{4}{l}{} \\"
        "\multicolumn{4}{l}{\textbf{Unique geographic units in dataset}} \\")
        postfoot("\bottomrule"  "\end{tabularx}") style(tex) append;
    #delimit cr
    
end/*%>*/

capture program drop regressions_bedrooms/*%<*/
program define regressions_bedrooms
    syntax varlist [if/] [aweight], geo(name) report(numlist) reportname(namelist) ///
        outdir(string) start(integer) end(integer) [ENDOGenous ///
        scalevar(name) ivtype(name) cluster(name) hetero(string) control(name)]
    * Identical to regressions_v2, but with fewer models/columns in table.

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

        * cbsa FE;
        ddregression `varlist'  `controls' [`weight' `exp']
            if mdate >= `start' & mdate <= `end',
            absorb(zip_cluster)
            over(mdate) report(``j'' ``jplus'') reportname(`x') `Cluster'
            geo(`geo') coeff_file(`coeff_xml') label("CBSA FE")
            `endogenous' fama(1);

        #delimit cr

        * Close coeff doc.
        coeff_xml_close, fname(`coeff_xml')

        * Turn coeff xml doc into a tex table.
        coeff_tex_compile, input(`coeff_xml') output(`coeff_tex')

        local j = `j' + 2
    }
    use `orig', clear

end/*%>*/

capture program drop table_bedrooms/*%<*/
program define table_bedrooms

    local geo zip
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

    keep if prop <= .05

    #delimit ;
    regressions_bedrooms y_trim exante_share if bdrm_effect == 1 [`weight' `exp'],
        outdir(".") start(572) end(628) cluster(zip_cluster) geo(`geo')
        control(`control') hetero(beds13_exante)
        report(572 587 588 604 605 628 588 595 596 598 602 604)
        reportname(pre policy post earlypolicy spike1 spike2);
    regressions_bedrooms y_trim exante_share if bdrm_effect == 0 [`weight' `exp'],
        outdir(".") start(572) end(628) cluster(zip_cluster) geo(`geo')
        control(`control') hetero(beds4_exante)
        report(572 587 588 604 605 628 588 595 596 598 602 604)
        reportname(pre policy post earlypolicy spike1 spike2);
    #delimit cr

    !python $localcodedir/py/tab_bedrooms.py ""

    use `orig', clear

end/*%>*/

capture program drop table_longdiff_reversal /*%< */
program define table_longdiff_reversal
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
    local coeff_xml "`outdir'/longdiff_reversal.xml"

    coeff_xml_open, fname(`coeff_xml')
    local coeff_tex "`outdir'/longdiff_`geo'_`varlist'_reversal.tex"

    * cbsa FE
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'], `absorb_val' ///
        `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
        fname(`coeff_xml') label(`absorb_labels')

    estimates clear

    * low prices
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'] if ///
        inlist(price_level,1,2,3), `absorb_val' `cluster_val' `endogenous'
	estimates store low
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("Low p")

    * high prices
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'] if ///
        inlist(price_level,8,9,10), `absorb_val' `cluster_val' `endogenous'
	estimates store high
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("High p")

    * low starter
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'] if ///
        inlist(starter_level,1,2,3), `absorb_val' `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("Low Start")

    * high starter
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'] if ///
        inlist(starter_level,8,9,10), `absorb_val' `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("High Start")

    * Low duration
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'] if ///
        inlist(duration_level,1,2,3), `absorb_val' `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("Low Hold")

    * High duration
    reg_internal longdiff `regressors' `controls'  [`weight' `exp'] if ///
        inlist(duration_level,8,9,10), `absorb_val' `cluster_val' `endogenous'
    local b = _b[`treatment']
    local se = _se[`treatment']
    coeff_xml_write, coeff(`b') se(`se') n(`e(N)') r2(`e(r2)') ///
            fname(`coeff_xml') label("High Hold")

	/* Tests for equality of coefficients */
	estimates clear
	set matsize 11000
	** low
	reg longdiff `regressors' `controls' i.zip_cluster [`weight' `exp'] if ///
        inlist(price_level,1,2,3)
	estimate store low
	
	** high
	reg longdiff `regressors' `controls' i.zip_cluster [`weight' `exp'] if ///
        inlist(price_level,8,9,10)
	estimate store high
	suest low high, cluster(zip_cluster)
	nlcom test: (_b[low_mean:exante_share]) - (_b[high_mean:exante_share]), post
	
	matrix b = r(b)
    matrix V = r(V)
    local std_err = sqrt(V[1,1])
    local z = b[1,1]/`std_err'
    local pvalue = 2*normal(-abs(`z'))
	di `pvalue'
	!echo "  <pval>`pvalue'</pval>" >> `coeff_xml'
	
	estimates clear
	set matsize 11000
	** low starter
	reg longdiff `regressors' `controls' i.zip_cluster [`weight' `exp'] if ///
        inlist(starter_level,1,2,3)
	estimate store low
	
	** high starter
	reg longdiff `regressors' `controls' i.zip_cluster [`weight' `exp'] if ///
        inlist(starter_level,8,9,10)
	estimate store high
	suest low high, cluster(zip_cluster)
	suest low high, cluster(zip_cluster)
	nlcom test: (_b[low_mean:exante_share]) - (_b[high_mean:exante_share]), post
	
	matrix b = r(b)
    matrix V = r(V)
    local std_err = sqrt(V[1,1])
    local z = b[1,1]/`std_err'
    local pvalue = 2*normal(-abs(`z'))
	di `pvalue'
	!echo "  <pval>`pvalue'</pval>" >> `coeff_xml'
	
	estimates clear
	fvset base 22220 zip_cluster
	set matsize 11000
	** low duration
	reg longdiff `regressors' `controls' i.zip_cluster [`weight' `exp'] if ///
        inlist(duration_level,1,2,3)
	estimate store low
	
	** high duration
	fvset base 22220 zip_cluster
	reg longdiff `regressors' `controls' i.zip_cluster [`weight' `exp'] if ///
        inlist(duration_level,8,9,10)
	estimate store high
	suest low high, cluster(zip_cluster)
	suest low high, cluster(zip_cluster)
	nlcom test: (_b[low_mean:exante_share]) - (_b[high_mean:exante_share]), post
	
	matrix b = r(b)
    matrix V = r(V)
    local std_err = sqrt(V[1,1])
    local z = b[1,1]/`std_err'
    local pvalue = 2*normal(-abs(`z'))
	di `pvalue'
	!echo "  <pval>`pvalue'</pval>" >> `coeff_xml'	
   
    * Close coeff doc.
    coeff_xml_close, fname(`coeff_xml')

    * Turn coeff xml doc into a tex table.
    coeff_tex_compile, input(`coeff_xml') output(`coeff_tex')

    use `orig', clear

    !python $localcodedir/py/tab_longdiff_reversal.py

end/*%>*/

cap program drop longdiff_reversal/*%<*/
program define longdiff_reversal

    tempfile orig
    save `orig'

    local wgtv totalhsales_base
    if ("`1'" == "") {
        local control ez2019
    }
    else {
        local control "`1'"
    }
    set_controls `control'
    local controls $controls

    * Make variable for starter home share
    tempfile bedroom_share
    load_analysis_data bedrooms_cont
    keep if bdrm_effect != . & year < 2009
    bys zip mdate: gen rowcount = _N
    keep if rowcount == 2
    collapse (sum) hsales_sa, by(zip bdrm_effect)
    bys zip: egen total_hsales = total(hsales_sa)
    keep if bdrm_effect == 1
    gen starter_share = hsales_sa/total_hsales
    keep zip starter_share
    impose_bounds starter_share, upper(1) lower(0)
    xtile starter_level = starter_share, nq(10)
    save `bedroom_share'

    * Measure of average churn from Nick credit panel
    tempfile duration
    use $rawdatadir/fthb_duration_deciles, clear
    keep if year == 2004
    keep duration_decile zip
    rename duration_decile duration_level
    save `duration'

    use `orig', clear
    merge 1:1 zip using `duration', keep(1 3) nogen
    merge 1:1 zip using `bedroom_share', keep(1 3) nogen

    table_longdiff_reversal average_delta_sales_postshort [aweight=`wgtv'], ///
        geo(zip) outdir(".") control(`control')

    use `orig', clear

end/*%>*/

capture program drop table_longdiff_hedonic /*%< */
program define table_longdiff_hedonic
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
    local coeff_xml "`outdir'/longdiff_hedonic.xml"

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
        sandstateMS != 1, `absorb_val' `cluster_val' `endogenous'
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

    !python $localcodedir/py/tab_longdiff_hedonic.py

end/*%>*/

capture program drop xp_hedonic/*%<*/
program define xp_hedonic

    clear
    set matsize 11000
    
	load_analysis_data longdiff_cont
    keep zip zip_cluster totalhsales_base claims_* exante* state
    tempfile join
    save `join'

    * From z server replication of Caleb.
    import delimited "$rawdatadir/hpi_indval_clichange_reduced_samp2_20181005.csv", clear
    format date_m %tm  
    gen yyear = year(dofm(date_m))
    gen hpi_hedonic2_ma = hpi_hedonic
    * 5% winsorizing looks like standard for these indexes
    winsor hpi_hedonic, p(0.05) gen(wins)
    replace hpi_hedonic2_ma = wins
    xtset sa_site_zip date_m  
    ren (sa_site_zip) (zip)
    gen logP = log(hpi_hedonic2_ma)
    gen log_change = (D.logP)
    merge m:1 zip using `join', keep(3) nogen

    prep_longdiff zip date_m log_change, label(pricegrowth_hedonic)
    sort zip period
    by zip: gen longdiff_hed = 100*(total_pricegrowth_hedonic[3] - total_pricegrowth_hedonic[2])
    collapse (max) longdiff_hed, by(zip)
    rename longdiff_hed delta_pricegrowth_hed
    * Some crazy outliers (roughly equivalent to trimming growth at .01)
    replace delta_pricegrowth_hed = . if abs(delta_pricegrowth_hed) > 100
    tempfile price_new
    save `price_new'  

    load_analysis_data longdiff_cont
    merge 1:1 zip using `price_new', keep(1 2 3) nogen

	local wgtv totalhsales_base
    local control ez2019
    set_controls `control'
    local controls $controls
    local wgtv totalhsales_base

    foreach x of varlist delta_pricegrowth_hed {
	    summ `x', det		
    }

    table_longdiff_hedonic delta_pricegrowth_hed [aweight=`wgtv'], ///
		    geo(zip) outdir(".") control(`control')
	
    !python $localcodedir/py/longdiff_price_combine.py

end/*%>*//*%>*/

*******************************************************************************
* Main
*******************************************************************************
capture program drop main
program define main

	log using "$outputdir/appendix-fthb.log", replace
    cd $outputdir
	local control ez2019
    set_controls `control'
	
	** Exposure Robustness
	* Data is loaded within program 
	graph_exposure_robust
	table_exposure_robust 
	
	load_analysis_data hsales_cont
    graph_placebo `control' 
	
	** Additional Age Tables
	load_analysis_data agezip
    graph_age_exposure
	
    load_analysis_data agetdec
    age_distribution_treat_2009   
	
	** FHA & Price Index Analysis
	* Data Loaded Within Programs
	load_analysis_data fha_xp
	xp_fha 
	graph_price_index 
	
	** Log spec Table
	* NB: Run on a serious computer
	load_analysis_data logspec_cont
	table_logspec 
	
	load_analysis_data hsales_cont
	table_waterfall 
    load_analysis_data bedrooms_cont
    table_bedrooms `control'  
	
	* Alternative Exposure Longdiff Measures & Reversal 
    load_analysis_data longdiff_cont
    longdiff_reversal `control'  

	** Data Loaded W/in Program
	xp_hedonic

    cd $localcodedir
    log close
end
