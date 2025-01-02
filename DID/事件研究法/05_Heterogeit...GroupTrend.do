
clear all

**********************
**       图13       **
**********************
use simu_data_DGP04, clear

preserve 
collapse (mean) y y0 y1, by(treat time rel_date)
tw  (sc y time if treat==2, color(plb1) lp(solid) lw(medthick) c(l) m(O)) (line y0 time if treat==2&rel_date>=-1, lc(plb1) lp(dash) lw(thick)) ///
    (sc y time if treat==3, color(plr1) lp(solid) lw(medthick) c(l) m(T)) (line y0 time if treat==3&rel_date>=-1, lc(plr1) lp(dash) lw(thick)) ///
    (sc y time if treat==4, color(plg1) lp(solid) lw(medthick) c(l) m(S)) (line y0 time if treat==4&rel_date>=-1, lc(plg1) lp(dash) lw(thick)) ///
    (sc y time if treat==1, color(ply1) lp(solid) lw(medthick) c(l) m(D)), ///
    ylabel(0(20)80) ///
    legend(order(1 "组群1（T*= 5）" 3 "组群2（T*=10）" 5 "组群3（T*=15）" 7 "控制组" ) $blgd col(2)) xtitle("时期") ytitle("结果变量Y") scale(1.2)
restore
gr export "figure/ES_HTNPdata.png", width(3000) replace
gr export "figure/ES_HTNPdata.svg", replace

preserve 
collapse (mean) tauit, by(treat rel_date)
tw  (sc tauit rel_date if treat==2, c(l) m(s)) ///
    (sc tauit rel_date if treat==3, c(l) m(t)) ///
    (sc tauit rel_date if treat==4, c(l) m(d)), xlabel(-15(5)15) ///
    xtitle("相对处理时期") ytitle("平均处理效应 (ATT)") ///
    legend(order(1 "组群1 (T*=  5, ATT{sub:t}=0.5t)" 2 "组群2 (T*=10, ATT{sub:t}=1t)" 3 "组群3 (T*=15, ATT{sub:t}=2t)") $tllgd col(1)) 
restore

**********************
**       图14       **
**********************
use simu_data_DGP04, clear
est clear

* true ATT
matrix btrue = J(1,21,.)
matrix colnames btrue = relb10 relb9 relb8 relb7 relb6 relb5 relb4 relb3 relb2 relb1 relf0 relf1 relf2 relf3 relf4 relf5 relf6 relf7 relf8 relf9 relf10
qui forvalues l = 1/10 {
    matrix btrue[1,`l']=0
}
qui forvalues h = 0/10 {
    sum tauit if rel_date==`h'&D==1
    matrix btrue[1,`h'+11]=r(mean)
}
matrix list btrue

* TWFE event study
reghdfe y relb_14 relb_13 relb_12 relb_11 relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 o.rel_base relf_*, a(id time) cluster(id)
eststo TWFE

* Sun & Abraham (2021)
gen never = treat==1
eventstudyinteract y relb_14 relb_13 relb_12 relb_11 relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 relf_* rel_base, vce(cluster id) absorb(id time) cohort(timing) control_cohort(never)
matrix sa_b = e(b_iw) // storing the estimates for later
matrix sa_v = e(V_iw)

* Callaway & Sant'Anna (2021)
gen csind = timing
replace csind = 0 if mi(csind)
csdid y, ivar(id) time(time) gvar(csind) notyet agg(event)
eststo csdid

* Dube et al.(2023)
xtset id time
cap drop t *_lpdid*
gen t = _n-15 if _n<=30
gen b_lpdid_fe = .
gen se_lpdid_fe = .

forval j =14(-1)2 {
    qui: reghdfe y relb_`j' if (rel_date==-`j' | rel_base==1 & treat!=1) | treat==1, a(id time) cluster(id)    
    replace b_lpdid_fe   =  _b[relb_`j']  if t==-`j'
    replace se_lpdid_fe  = _se[relb_`j']  if t==-`j'
}

forval j =0(1)15 {
    qui: reghdfe y relf_`j' if (rel_date==`j' | rel_base==1 & treat!=1) | treat==1, a(id time) cluster(id) 
    replace b_lpdid_fe   = _b[relf_`j']  if t==`j'
    replace se_lpdid_fe  = _se[relf_`j']  if t==`j'
}
replace  b_lpdid_fe  = 0 if t==-1
replace se_lpdid_fe  = 0 if t==-1
tostring t, replace
replace t = "T" + t
mkmat b_lpdid_fe, mat(lpdid_b) rownames(t) nomissing
mkmat se_lpdid_fe, mat(lpdid_se) rownames(t) nomissing

* Borusyak et al.(2023)
did_imputation y id time timing, allhorizons  pretrend(10) 
eststo bjs

* Gardner(2022)
did2s y, first_stage(i.id i.time) second_stage(relb* rel_base relf*) treatment(D) cluster(id)
eststo twostage

* Liu et al.(2022)
fect y, treat(D) unit(id) time(time) method("fe") se nboots(30)
matrix fect_results = e(ATTs)
matrix rownames fect_results = relb_13 relb_12 relb_11 relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 relb_1 relf_0 relf_1 relf_2 relf_3 relf_4 relf_5 relf_6 relf_7 relf_8 relf_9 relf_10 relf_11
matrix fect_b = fect_results[4..24,3] // storing the estimates for later
matrix fect_v = fect_results[4..24,4]

* Cengiz et al.(2019)
stackedev y relb_14 relb_13 relb_12 relb_11 relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 relf_* rel_base, cohort(timing) time(time) never_treat(never) unit_fe(id) clust_unit(id)
eststo stack

event_plot stack, stub_lag(relf_#) stub_lead(relb_#) plottype(scatter) ciplottype(rcap) 

event_plot btrue# TWFE sa_b#sa_v csdid lpdid_b#lpdid_se bjs twostage stack, ///
    stub_lag(relf# relf_# relf_# Tp# T# tau# relf_# relf_#) ///
    stub_lead(relb# relb_# relb_# Tm# T-# pre# relb_# relb_#) ///
    plottype(scatter) ciplottype(rspike) ///
    together perturb(-0.35(0.1)0.35) trimlead(10) trimlag(10) noautolegend ///
    graph_opt(xtit("相对处理时期") ytitle("估计系数") xlabel(-10(1)10) ylabel(-5(5)15) ///
        legend(order(1 "真实系数" 2 "事件研究法TWFE" 4 "Sun & Abraham（2021）"  6 "Callaway & Sant'Anna（2021）" 8 "Dube et al.（2023）" 10 "Borusyak et al.（2022）" ///
         12 "Gardner（2022）" 14 "Cengiz et al.（2019）") col(2) $tllgd ) ///
        xline(-0.5, lcolor(gs8) lp(dash)) yline(0, lcolor(gs0) lp(solid)) ///
        xsize(10) ysize(5) scale(1.3) ///
    ) ///
    lag_opt1(msymbol(Oh) color(plb1)) lag_ci_opt1(color(cranberry)) ///
    lag_opt2(msymbol(o) color(plr1)) lag_ci_opt2(color(plr1)) ///
    lag_opt3(msymbol(d) color(navy)) lag_ci_opt3(color(navy)) ///
    lag_opt4(msymbol(t) color(forest_green)) lag_ci_opt4(color(forest_green)) ///
    lag_opt5(msymbol(s) color(dkorange)) lag_ci_opt5(color(dkorange)) ///
    lag_opt6(msymbol(th) color(purple)) lag_ci_opt6(color(purple)) ///
    lag_opt7(msymbol(dh)  color(plb2)) lag_ci_opt7(color(plb2)) ///
    lag_opt8(msymbol(sh)  color(plr2)) lag_ci_opt8(color(plr2)) 
gr export "figure/ES_HTNPest.png", width(3000) replace
gr export "figure/ES_HTNPest.svg", replace
