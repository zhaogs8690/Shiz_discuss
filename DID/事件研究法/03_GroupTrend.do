
clear all

**********************
**       图7        **
**********************
use simu_data_DGP02, clear

cap gr drop GT*
preserve 
collapse (mean) y, by(treat time)
tw  (sc y time if treat==2, c(l) m(o) lp(solid)) ///
    (sc y time if treat==3, c(l) m(s) lp(solid)) ///
    (sc y time if treat==4, c(l) m(t) lp(solid)) ///
    (sc y time if treat==1, c(l) m(d) lp(solid)) ///
    (lfit y time if treat==2&time<5, range(1 20) lc(plb1) lp(shortdash)) ///
    (lfit y time if treat==3&time<10, range(1 20) lc(plr1) lp(shortdash)) ///
    (lfit y time if treat==4&time<15, range(1 20) lc(plg1) lp(shortdash)) , ylabel(0(10)50) ///
    xtitle("时期") ytitle("结果变量Y") ///
    legend(order(1 "组群1（趋势=1t）" 2 "组群2（趋势=0.5t）" 3 "组群3（趋势=0.2t）" 4 "控制组") $blgd col(2)) ///
    tit("（a）模拟数据", pos(6)) scale(1.2) name(GT_data1)
restore

preserve 
    reghdfe y if D==0, a(id time groupFE=i.treat#c.time) cluster(id)
    ereplace groupFESlope1 = mean(groupFESlope1), by(id)
    gen grouptrend = time*groupFESlope1
    gen y_hat = y - grouptrend
    collapse (mean) y_hat, by(treat time)
    tw  (sc y_hat time if treat==2, c(l) m(oh) lp(dash)) ///
        (sc y_hat time if treat==3, c(l) m(sh) lp(dash)) ///
        (sc y_hat time if treat==4, c(l) m(th) lp(dash)) ///
        (sc y_hat time if treat==1, c(l) m(dh) lp(dash)), ylabel(0(10)35) ///
        xtitle("时期") ytitle("结果变量Y") legend(order(1 "组群1" 2 "组群2 (T*=10, 趋势=0.5t)" 3 "组群3 (T*=15, 趋势=0.2t)" 4 "控制组") $blgd row(2)) ///
        tit("（b）剔除时间趋势", pos(6)) scale(1.2) name(GT_data2)
restore

grc1leg2 GT_data1 GT_data2, xsize(10) scale(1.3)  lr(1) labsize(small)
gr export "figure/ES_GTdata.png", width(3000) replace
gr export "figure/ES_GTdata.svg", replace

**********************
**       图8        **
**********************
use simu_data_DGP02, clear

reghdfe y relb_* o.rel_base relf_*, a(id time) cluster(id)
eststo TWFE

reghdfe y  relb_14 relb_13 relb_12 relb_11 relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 o.rel_base relf_0 relf_1 relf_2 relf_3 relf_4 relf_5 relf_6 relf_7 relf_8 relf_9 relf_10 relf_11 relf_12 relf_13 relf_14 relf_15, a(id time i.treat#c.time) cluster(id)
eststo TWFE_wtrend

gl leadall "relb_14 relb_13 relb_12 relb_11 relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 rel_base"
gl lagall "relf_0 relf_1 relf_2 relf_3 relf_4 relf_5 relf_6 relf_7 relf_8 relf_9 relf_10 relf_11 relf_12 relf_13 relf_14 relf_15"
gl lead10 "relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 rel_base"
gl lag10 "relf_0 relf_1 relf_2 relf_3 relf_4 relf_5 relf_6 relf_7 relf_8 relf_9 relf_10"

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

cap gr drop GT_est*
coefplot (matrix(btrue[1]), msym(Oh)) TWFE TWFE_wtrend, keep($lead10 $lag10) order($lead10 $lag10) omitted baselevel vertical nooffsets ///
    recast(connect) ciopts(recast(rcap)) yline(0, lc(gs8) lp(solid)) ///
    xline(10.5, lc(gs8) lp(dash)) xtitle("相对处理时期") ytitle("估计系数") ///
    legend(order(2 "真实系数" 4 "事件研究法" 6 "控制组别时间趋势") $blgd col(3)) ///
    tit("（a）控制组别时间趋势", pos(6)) scale(1.2) name(GT_est1)

est clear
reghdfe y  relb_14 relb_13 relb_12 relb_11 o.relb_10 relb_9 relb_8 relb_7 relb_6 o.relb_5 relb_4 relb_3 relb_2 rel_base relf_0 relf_1 relf_2 relf_3 relf_4 relf_5 relf_6 relf_7 relf_8 relf_9 relf_10 relf_11 relf_12 relf_13 relf_14 relf_15, a(id time i.treat#c.time) cluster(id)
eststo TWFE_wtrend1
reghdfe y  relb_14 relb_13 relb_12 relb_11 relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 o.rel_base relf_0 relf_1 relf_2 relf_3 relf_4 o.relf_5 relf_6 relf_7 relf_8 relf_9 relf_10 relf_11 relf_12 relf_13 relf_14 relf_15, a(id time i.treat#c.time) cluster(id)
eststo TWFE_wtrend2

coefplot (matrix(btrue[1]), msym(Oh)) TWFE_wtrend1 TWFE_wtrend2, keep($lead10 $lag10) order($lead10 $lag10) omitted baselevel vertical recast(con) ///
    ciopts(recast(rcap)) yline(0, lc(gs8) lp(solid)) ///
    xline(14.5, lc(gs8) lp(dash)) xtitle("相对处理时期") ytitle("估计系数") ///
    legend(order(2 "真实系数" 4 "基期=(-10, -5)" 6 "基期=(-1, 5)") $blgd col(3)) ///
    tit("（b）更换基期", pos(6)) scale(1.2) name(GT_est2)

gr combine GT_est1 GT_est2, xsize(10) scale(1.3)
gr export "figure/ES_GTest.png", width(3000) replace
gr export "figure/ES_GTest.svg", replace


**********************
**       表1        **
**********************
* 组别时间趋势系数估计
use simu_data_DGP02, clear
forvalues i = 1/4{
    gen group`i'_time = (treat==`i') * time
}
* 由于组别时间趋势与时间固定效应共线，故以时间趋势为0的控制组（group1）为参照组
* 系数代表各组群的线性时间趋势系数与参照组的差异，由于参照组本身无时间趋势，故系数代表各组本身的线性时间趋势斜率
est clear
reghdfe y group2_time group3_time group4_time, a(id time) cluster(id)
eststo model_aux1
reghdfe y group2_time group3_time group4_time if D==0, a(id time) cluster(id)
eststo model_aux2
esttab model_aux2 model_aux1, keep(group*) r2 se b(4) star(* 0.10 ** 0.05 *** 0.01) 
/*------------------------------------------
                      (1)             (2)   
                        y               y   
--------------------------------------------
group2_time        1.0421***       1.9330***
                 (0.1556)        (0.0170)   

group3_time        0.5090***       1.1024***
                 (0.0471)        (0.0168)   

group4_time        0.2375***       0.4470***
                 (0.0276)        (0.0173)   
--------------------------------------------
N                    4700            8000   
R-sq                0.785           0.953   
--------------------------------------------*/
* esttab model_aux2 model_aux1 using "table\model_aux.rtf", keep(group*) r2 se b(4) star(* 0.10 ** 0.05 *** 0.01) nogap replace //导出结果


**********************
**       图9        **
**********************
* 正确控制组间时间趋势
*   - detrend
*   - imputation

use simu_data_DGP02, clear
gl leadall "relb_14 relb_13 relb_12 relb_11 relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 rel_base"
gl lagall "relf_0 relf_1 relf_2 relf_3 relf_4 relf_5 relf_6 relf_7 relf_8 relf_9 relf_10 relf_11 relf_12 relf_13 relf_14 relf_15"

*- True ATT
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

*- TWFE event study
reghdfe y relb_* o.rel_base relf_*, a(id time) cluster(id)
eststo TWFE_trend

*- Detrend (Good-man Bacon, 2021)
reghdfe y if D==0, a(id time groupFE=i.treat#c.time) cluster(id)
ereplace groupFESlope1 = mean(groupFESlope1), by(id)
gen grouptrend = time*groupFESlope1
gen y_hat = y - grouptrend
reghdfe y_hat relb_* o.rel_base relf_*, a(id time) cluster(id)
eststo TWFE_detrend

*- Generalized SCM (Liu et al., 2021)
forvalues i = 1/4{
    gen group`i'_time = (treat==`i') * time
}
fect y, treat(D) unit(id) time(time) cov(group1_time group2_time group3_time group4_time) method("fe") se nboots(100)
matrix fect_results = e(ATTs)
matrix rownames fect_results = relb_13 relb_12 relb_11 relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 relb_1 relf_0 relf_1 relf_2 relf_3 relf_4 relf_5 relf_6 relf_7 relf_8 relf_9 relf_10 relf_11
matrix fect_b = fect_results[4..24,3]  //storing the estimates for later
matrix fect_v = fect_results[4..24,4]
matrix fect = [fect_b , fect_v]'

*- Two-stage DiD (Gardner, 2021)
did2s y, first_stage(i.id i.time i.treat#c.time) second_stage(relb* rel_base relf*) treatment(D) cluster(id)
eststo twostage

gl lead10 "relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 rel_base"
gl lag10 "relf_0 relf_1 relf_2 relf_3 relf_4 relf_5 relf_6 relf_7 relf_8 relf_9 relf_10"
coefplot (matrix(btrue[1]), msym(Oh) recast(scatter))  ///
         (TWFE_trend, m(t)) ///
         (TWFE_detrend, m(s)) ///
         (twostage, m(d)) ///
         (matrix(fect[1]), se(2) m(X)), ///
         keep($lead10 $lag10) omitted baselevel vertical ///
         ciopts(recast(rspike)) yline(0, lc(gs8) lp(solid)) ///
         xline(10.5, lc(gs8) lp(dash)) xtitle("相对处理时期") ytitle("估计系数") ///
         legend(order(2 "真实系数" 4 "事件研究法（TWFE）" 6 "去事前趋势（Goodman-Bacon, 2021）" 8 "两阶段DiD （Gardner, 2022）" 10 "广义合成控制法 （Liu等, 2022）") $tllgd col(1)) ///
         xsize(8)  scale(1.3) 
gr export "figure/ES_GTest3.png", width(3000) replace
gr export "figure/ES_GTest3.svg", replace



/*** 错误控制组间趋势的影响
use simu_data_DGP01, clear //导入不存在组间趋势差异的数据
est clear
reghdfe y relb_* o.rel_base relf_*, a(id time) cluster(id)
eststo TWFE

reghdfe y relb_14 relb_13 relb_12 relb_11 relb_10 relb_9 relb_8 relb_7 relb_6 o.relb_5 relb_4 relb_3 relb_2 o.rel_base relf_0 relf_1 relf_2 relf_3 relf_4 relf_5 relf_6 relf_7 relf_8 relf_9 relf_10 relf_11 relf_12 relf_13 relf_14 relf_15, a(id time i.treat##c.time) cluster(id)
eststo TWFE_wtrend1

reghdfe y relb_14 relb_13 relb_12 relb_11 relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 o.rel_base relf_0 relf_1 relf_2 relf_3 relf_4 o.relf_5 relf_6 relf_7 relf_8 relf_9 relf_10 relf_11 relf_12 relf_13 relf_14 relf_15, a(id time i.treat##c.time) cluster(id)
eststo TWFE_wtrend2

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

coefplot (matrix(btrue[1]), msym(Oh)) TWFE_wtrend1 TWFE_wtrend2, keep($leadall $lagall) order($leadall $lagall) omitted baselevel vertical ///
    ciopts(recast(rcap)) yline(0, lc(gs8) lp(solid)) ///
    xline(14.5, lc(gs8) lp(dash)) xtitle("相对处理时期") ytitle("估计系数") ///
    legend(order(2 "真实系数" 4 "基期=（-5, -1）" 6 "基期=（-1, 5）") $blgd col(3)) xsize(8) scale(1.2)

