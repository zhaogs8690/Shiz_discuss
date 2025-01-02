
clear all

* 设定glolab
gl leadall "relb_14 relb_13 relb_12 relb_11 relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 rel_base" // 事前全部系数
gl lagall "relf_0 relf_1 relf_2 relf_3 relf_4 relf_5 relf_6 relf_7 relf_8 relf_9 relf_10 relf_11 relf_12 relf_13 relf_14 relf_15" // 事后全部系数
gl lead10 "relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 rel_base" // 事前10期系数
gl lag10 "relf_0 relf_1 relf_2 relf_3 relf_4 relf_5 relf_6 relf_7 relf_8 relf_9 relf_10" // 事后10期系数

***********************
**   1.观察原始数据    **
***********************
use simu_data_DGP03, clear

preserve 
collapse (mean) y, by(treat time)
tw  (sc y time if treat==2, c(l) m(O)) ///
    (sc y time if treat==3, c(l) m(S)) ///
    (sc y time if treat==4, c(l) m(T)) ///
    (sc y time if treat==1, c(l) m(D)), ylabel(0(10)30) ///
    legend(order(1 "组群1 (T*=  5, ATT{sub:t}=0.5t)" 2 "组群2 (T*=10, ATT{sub:t}=1t)" 3 "组群3 (T*=15, ATT{sub:t}=2t)" 4 "控制组" ) $blgd col(2)) xtitle("时期") ytitle("结果变量Y") name(HTdata1)
restore

preserve 
collapse (mean) tauit, by(treat rel_date)
tw  (sc tauit rel_date if treat==2, c(l) m(s)) ///
    (sc tauit rel_date if treat==3, c(l) m(t)) ///
    (sc tauit rel_date if treat==4, c(l) m(d)), xlabel(-15(5)15) ///
    xtitle("相对处理时期") ytitle("平均处理效应 (ATT)") ///
    legend(order(1 "组群1 (T*=  5, ATT{sub:t}=0.5t)" 2 "组群2 (T*=10, ATT{sub:t}=1t)" 3 "组群3 (T*=15, ATT{sub:t}=2t)") $blgd col(2)) name(HTdata2)
restore

gr combine HTdata1 HTdata2, xsize(10) scale(1.5)


**********************
**       图10       **
**********************
* Sun & Abraham(2021)权重分解
*   - 手动进行S&A的权重分解
*   - 全部权重结果输出在weightfiles.xlsx文件中
*   - 基期为-1期
use simu_data_DGP03, clear
local b_idc "relb_14 relb_13 relb_12 relb_11 relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 relf_0 relf_1 relf_2 relf_3 relf_4 relf_5 relf_6 relf_7 relf_8 relf_9 relf_10 relf_11 relf_12 relf_13 relf_14 relf_15"
* prepare partial out
hdfe `b_idc', absorb(id time) generate(res_)
levelsof timing, local(cohort_list) // missing cohort includes the never treated units
levelsof rel_date, local(rel_time_list) 
* Loop over cohort and relative times
tempname bb bb_w
foreach yy of local cohort_list {
    foreach rr of local rel_time_list { 
    tempvar catt
        qui count if timing == `yy' & rel_date == `rr'
        if r(N) >0 {
            qui gen `catt'  = (timing == `yy') * (rel_date == `rr')
            qui regress `catt' res_*, nocons
            mat `bb_w' = e(b)
            mat `bb_w' = `yy', `rr', `bb_w'
            matrix `bb'  = nullmat(`bb') \ `bb_w'
        }
    }
}
matrix colnames `bb' = "timing" "rel_date" `b_idc'
matrix ES_weights = `bb'
mat list ES_weights
putexcel set "weightfiles", replace 
putexcel A1=matrix(ES_weights), colnames

*- 导入weightfiles.xlsx文件并画图
import excel "weightfiles.xlsx", clear firstrow
local period "relf_3"
keep timing rel_date `period'
reshape wide `period', i(rel_date) j(timing) 

gr bar `period'5 `period'10 `period'15, over(rel_date) ///
    bar(1, color(gs0)) bar(2, color(gs6))  bar(3, color(gs12))  ///
    yline(0, lw(medium)) ///
    ytit("权重") b1tit("相对处理时期") ///
    legend(order(1 "组群1（T*=5）" 2 "组群2（T*=10）" 3 "组群3（T*=15）") col(1) $tllgd)  ysize(3) scale(1.3)

gr export "figure/ES_SAweights.png", width(3000) replace
gr export "figure/ES_SAweights.svg", replace

/*- 检查权重分解结果
1.使用S&A提供的stata包进行权重分解和交叉检验
    use simu_data_DGP03.dta,clear
    local b_idc "relf_3" // 设定目标时期
    eventstudyweights relb_14 relb_13 relb_12 relb_11 relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 relf*, absorb(i.id i.time) cohort(timing) rel_time(rel_date) saveweights("weights") 
    import excel "weights.xlsx", clear firstrow
    keep `b_idc' timing rel_date
    reshape wide `b_idc', i(rel_date) j(timing)
    egen w_sum = rowtotal(`b_idc'*)
    egen w_sumt = total(w_sum)
    egen w_g = total(w_sum) if rel_date==-2
    egen w_incl = total(w_sum) if inrange(rel_date,-14,-3) | rel_date>=0
    egen w_excl = total(w_sum) if rel_date==-1
    egen w_pretrend = total(w_sum) if rel_date>=0
    sum w_pretrend
    scalar w_c = r(mean)
    local w_c = round(w_c,0.001)
    
    gr bar `b_idc'5 `b_idc'10 `b_idc'15, over(rel_date) ///
        bar(1, color(gs0)) bar(2, color(gs6))  bar(3, color(gs12))  ///
        yline(0, lc(plr1) lw(medium)) ylab(, format(%5.1f)) ///
        ytit("权重") b1tit("相对处理时期") ///
        legend(order(1 "组群1（T*=5）" 2 "组群2（T*=10）" 3 "组群3（T*=15）") col(1) $tllgd)  ysize(3) scale(1.3)

2.手动检查第4期权重
    use simu_data_DGP03, clear
    gen c1_relf_4 = (timing==5)*(rel_date==4)
    gen c2_relf_4 = (timing==10)*(rel_date==4)
    gen c3_relf_4 = (timing==15)*(rel_date==4)

    cap drop *_b
    reghdfe c1_relf_4 relb_* relf_*,a(id time) nocons
    gen c1_relf_4_b = _b[relf_3]
    reghdfe c2_relf_4 relb_* relf_*,a(id time) nocons
    gen c2_relf_4_b = _b[relf_3]
    reghdfe c3_relf_4 relb_* relf_*,a(id time) nocons
    gen c3_relf_4_b = _b[relf_3]

    gr bar (mean) c1_relf_4_b c2_relf_4_b c3_relf_4_b, bargap(100) blabel(bar, format(%6.4f)) ///
        legend(order(1 "组群1" 2 "组群2" 3 "组群3")) ylabel(-0.1(0.05)0.1) ///
        title("各组群第4期ATT对{&beta}{sub:3}的权重")
*/


**********************
**       图11       **
**********************
* 异质性处理效应稳健估计量
*   - 组群-时间ATT估计量
*       - De Chaisemartin & D’Haultfoeuille (2020)
*       - Sun & Abraham (2021)
*       - Callaway & Sant'Anna (2021)
*       - Dube et al.(2023)
*   - 插补法Imputation
*       - Borusyak et al.(2023)
*       - Gardner(2022)
*       - Liu et al.(2022)
*   - 堆叠法 Stacked DID
*       -Cengiz et al.(2019)
use simu_data_DGP03, clear

* true ATT_l
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
est clear
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
    qui: replace b_lpdid_fe   =  _b[relb_`j']  if t==-`j'
    qui: replace se_lpdid_fe  = _se[relb_`j']  if t==-`j'
}

forval j =0(1)15 {
    qui: reghdfe y relf_`j' if (rel_date==`j' | rel_base==1 & treat!=1) | treat==1, a(id time) cluster(id) 
    qui: replace b_lpdid_fe   = _b[relf_`j']  if t==`j'
    qui: replace se_lpdid_fe  = _se[relf_`j']  if t==`j'
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

* 建议先用event_plot随便画一个图，否则可能报错(原因未知)
event_plot stack, stub_lag(relf_#) stub_lead(relb_#) plottype(scatter) ciplottype(rcap) 

* 画图
* eventplot只支持8个估计结果
* 真实系数+TWFE+6个稳健估计量
event_plot btrue# TWFE sa_b#sa_v csdid lpdid_b#lpdid_se bjs twostage stack, ///
    stub_lag(relf# relf_# relf_# Tp# T# tau# relf_# relf_#) ///
    stub_lead(relb# relb_# relb_# Tm# T-# pre# relb_# relb_#) ///
    plottype(scatter) ciplottype(rspike) ///
    together perturb(-0.35(0.1)0.35) trimlead(10) trimlag(10) noautolegend ///
    graph_opt(xtit("相对处理发生时间") ytitle("估计系数") xlabel(-10(1)10) ylabel(-3(3)9) ///
        legend(order(1 "真实系数" 2 "事件研究法TWFE" 4 "Sun & Abraham（2021）"  ///
                     6 "Callaway & Sant'Anna（2021）" 8 "Dube et al.（2023）" 10 "Borusyak et al.（2022）" ///
                     12 "Gardner（2022）" 14 "Cengiz et al.（2019）") col(2) $tllgd) ///
        xline(-0.5, lcolor(gs8) lp(dash)) yline(0, lcolor(gs0) lp(solid)) ///
        xsize(10) ysize(5) scale(1.3) ///
    ) ///
    lag_opt1(msymbol(Oh) color(plb1))         lag_ci_opt1(color(cranberry)) ///
    lag_opt2(msymbol(o)  color(plr1))         lag_ci_opt2(color(plr1)) ///
    lag_opt3(msymbol(d)  color(navy))         lag_ci_opt3(color(navy)) ///
    lag_opt4(msymbol(t)  color(forest_green)) lag_ci_opt4(color(forest_green)) ///
    lag_opt5(msymbol(s)  color(dkorange))     lag_ci_opt5(color(dkorange)) ///
    lag_opt6(msymbol(th) color(purple))       lag_ci_opt6(color(purple)) ///
    lag_opt7(msymbol(dh) color(plb2))         lag_ci_opt7(color(plb2)) ///
    lag_opt8(msymbol(sh) color(plr2))         lag_ci_opt8(color(plr2)) 

gr export "figure/ES_HTest1.png", width(3000) replace
gr export "figure/ES_HTest1.svg", replace


**********************
**       图12       **
**********************
*** 使用未处理样本检验平行趋势
use simu_data_DGP03, clear

* true ATT_l
matrix btrue = J(1,30,.)
matrix colnames btrue = $leadall $lagall
qui forvalues l = 1/14 {
    matrix btrue[1,`l']=0
}
qui forvalues h = 0/15 {
    sum tauit if rel_date==`h'&D==1
    matrix btrue[1,`h'+15]=r(mean)
}
matrix list btrue

* TWFE
reghdfe y relb_14 relb_13 relb_12 relb_11 relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 o.rel_base relf_*, a(id time) cluster(id)
eststo TWFE
testparm $leadall

* TWFE，使用未处理子样本
reghdfe y relb_14 relb_13 relb_12 relb_11 relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 o.rel_base if D==0, a(id time) cluster(id)
eststo TWFE_pre
testparm $leadall


coefplot (matrix(btrue[1]), recast(scatter) msym(Oh) ) (TWFE, m(s) ) (TWFE_pre, m(t)) , keep($leadall) omitted baselevel vertical ///
    recast(con) ciopts(recast(rcap)) yline(0, lc(gs0) lp(solid)) ///
    xline(14, lc(gs8) lp(dash)) ylabel(-6(2)4) xtit("相对处理时期") ytit("估计系数") ///
    legend(order(2 "真实系数" 4 "全样本" 6 "未处理子样本") $blgd) ///
    text(-5 1 "全样本：F=12.78  p=0.00***", size(medium) placement(e)) ///
    text(2.5 1 "子样本：F=0.78  p=0.69", size(medium) placement(e)) scale(1.2)

gr export "figure/ES_HTpretest.png", width(3000) replace
gr export "figure/ES_HTpretest.svg", replace

