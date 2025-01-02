
clear all

**********************
**       图2        **
**********************

* 运行DGP文件生成数据
use simu_data_DGP01, clear

preserve 
collapse (mean) y, by(treat time)
tw  (sc y time if treat==2, c(l) m(o)) ///
    (sc y time if treat==3, c(l) m(s)) ///
    (sc y time if treat==4, c(l) m(t)) ///
    (sc y time if treat==1, c(l) m(d)), ylabel(0(10)40) ///
    subtit("（a）模拟数据", pos(6)) ///
    legend(order(1 "组群1 (T*=5)" 2 "组群2 (T*=10)" 3 "组群3 (T*=15)" 4 "控制组" ) $tllgd col(1)) xtitle("时期") ytitle("结果变量Y") scale(1) name(Homo1)
restore

* 图2b
preserve 
collapse (mean) att=tauit, by(treat rel_date)
replace att = att+0.5 if treat==3
replace att = att+1 if treat==2
tw  (sc att rel_date if treat==2, c(l) m(o)) ///
    (sc att rel_date if treat==3, c(l) m(s)) ///
    (sc att rel_date if treat==4, c(l) m(t)), xlabel(-15(5)15) ///
    xline(-0.5, lc(gs8) lp(dash)) ///
    xtitle("相对处理时期") ytitle("平均处理效应 (ATT)") ///
    subtit("（b）真实平均处理效应", pos(6)) ///
    legend(order(1 "组群1 (T*=5)" 2 "组群2 (T*=10)" 3 "组群3 (T*=15)") $tllgd col(1)) scale(1) name(Homo2)
restore
gr combine Homo1 Homo2, xsize(10) scale(1.5)
gr export "figure/ES_ATT.png", width(3000) replace
gr export "figure/ES_ATT.svg", replace

**********************
**       图3        **
**********************
use simu_data_DGP01, clear

* TWFE
reghdfe y relb_14 relb_13 relb_12 relb_11 relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 o.rel_base relf_0 relf_1 relf_2 relf_3 relf_4 relf_5 relf_6 relf_7 relf_8 relf_9 relf_10 relf_11 relf_12 relf_13 relf_14 relf_15, a(id time) cluster(id)
eststo ES_TWFE

gl leadall "relb_14 relb_13 relb_12 relb_11 relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 rel_base"
gl lagall "relf_0 relf_1 relf_2 relf_3 relf_4 relf_5 relf_6 relf_7 relf_8 relf_9 relf_10 relf_11 relf_12 relf_13 relf_14 relf_15"
testparm $leadall //事前系数联合检验

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

*omitted不选的话，画出来的图没有这一基期
*vertical，图片横纵轴
coefplot (matrix(btrue[1]), m(Oh)) (ES_TWFE, recast(connect)), drop(_cons) order(relb* rel_base relf*) omitted baselevel vertical ///
    ciopts(recast(rcap)) yline(0, lc(gs0) lp(solid)) ///
    xline(14.5, lc(gs8) lp(dash)) xtitle("相对处理发生时间") ytitle("估计系数") ///
    text(4 6 "事前系数联合检验", place(c)) ///
    text(2 6 "F=0.83 P=0.6256", place(c)) ///
    legend(order(2 "真实系数" 4 "估计系数" ) $brlgd col(1)) ///
    xsize(8) scale(1.3)
gr export "figure/ES_TWFE.png", width(3000) replace
gr export "figure/ES_TWFE.svg",  replace


**********************
**       图5        **
**********************
* 单一系数检验和联合检验

* TWFE估计
gr drop _all
use simu_data_DGP01, clear
reghdfe y relb_14 relb_13 relb_12 relb_11 relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 o.rel_base relf_*, a(id time) cluster(id)
eststo ES_TWFE1

reghdfe y relb_14 relb_13 relb_12 relb_11 relb_10 relb_9 o.relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 rel_base relf_*, a(id time) cluster(id)
eststo ES_TWFE2

gl leadall "relb_14 relb_13 relb_12 relb_11 relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 rel_base"
coefplot ES_TWFE1 (ES_TWFE2, lp(solid)), keep($leadall) omitted baselevel vertical ///
    recast(con) lw(thick) ciopts(recast(rspike) color(%40)) ///
    yline(0, lc(gs0) lp(solid) lw(medthick)) ylab(-2(1)2) ///
    xtitle("相对处理发生时间") ytitle("估计系数")  tit("（a）单一系数显著性", pos(6))  ///
    legend(order(2 "基期 = -1" 4 "基期 = -8") $brlgd) scale(1.2)  name(PTT1)


*- 事前系数联合检验结果的不变性
*--- 分别以-14到-1期为基期进行回归，计算每一个回归的联合检验P值
*--- 结果：无论以哪一期为基期，联合检验的P值均保持不变
local leadall "relb_14 relb_13 relb_12 relb_11 relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 rel_base"
tempvar t P 
gen `t' = _n-15 if _n<=14
gen `P' = .
forvalues i = 14(-1)2 {
    local base_period "relb_`i'"
    local xvar: list leadall - base_period
    reghdfe y `xvar' o.`base_period' relf_*, a(id time) cluster(id)
    testparm `leadall'
    replace `P' = r(p) if `t'==-`i'
}
reghdfe y `xvar' o.`base_period' relf_*, a(id time) cluster(id)
testparm `leadall'
replace `P' = r(p) if `t'==-1
sum `P',d
scatter `P' `t', c(l) ylab(0(0.2)1, format(%6.1f)) xlab(-14/-1) ///
    xtit("基期") ytitle("P值") tit("（b）联合检验显著性", pos(6)) scale(1.2) name(PTT2)

gr combine PTT1 PTT2, xsize(10) scale(1.5)

gr export "figure/ES_preTest.png", width(3000) replace
gr export "figure/ES_preTest.svg",  replace


**********************
**       图6        **
**********************
* 归并与截断

*** 在[-5,10]期内归并、截断
use simu_data_DGP01, clear
cap drop relb_* rel_base relf_*

gen rel_date_cut = rel_date
local pre_cut  = 5
local post_cut = 10
replace rel_date_cut = -`pre_cut'  if rel_date<-`pre_cut'
replace rel_date_cut = `post_cut' if rel_date>`post_cut'
tab rel_date_cut

forvalues k = `pre_cut'(-1)2 {
   gen relb_`k' = rel_date_cut == -`k'
   label var relb_`k' "-`k'"
}
gen rel_base = rel_date_cut==-1
label var rel_base "-1"
forvalues k = 0/`post_cut' {
    gen relf_`k' = rel_date_cut == `k'
    label var relf_`k' "`k'"
}
for var relb_* relf_* rel_base: replace X = 0 if group1==1 // 控制组的相对数量全是0

reghdfe y relb_* o.rel_base relf_*, a(id time) cluster(id)
eststo ES_bin

reghdfe y relb_* o.rel_base relf_* if inrange(rel_date,-5,10), a(id time) cluster(id)
eststo ES_trim

* 设置真实系数矩阵btrue
gl leadall "relb_5 relb_4 relb_3 relb_2 rel_base"
gl lagall "relf_0 relf_1 relf_2 relf_3 relf_4 relf_5 relf_6 relf_7 relf_8 relf_9 relf_10 "

matrix btrue = J(1,16,.)
matrix colnames btrue = $leadall $lagall
qui forvalues l = 1/5 {
    matrix btrue[1,`l']=0
}
qui forvalues h = 0/10 {
    sum tauit if rel_date==`h'&D==1
    matrix btrue[1,`h'+6]=r(mean)
}
matrix list btrue

coefplot (matrix(btrue[1]), m(Oh)) ES_bin ES_trim, drop(_cons) order(relb* rel_base relf*) omitted baselevel vertical  ///
    ciopts(recast(rcap)) yline(0, lc(gs0) lp(solid)) ///
    xline(5.5, lc(gs8) lp(dash)) xtitle("相对处理时期") ytitle("估计系数") ///
    legend(order(2 "真实系数" 4 "归并样本 (binned)" 6 "截断样本 (trimmed)") $blgd) ///
    title("（a）事件窗口期[-5,10]", pos(6)) name(BT1)


***在[-10,5]期内归并、截断
use simu_data_DGP01, clear
cap drop relb_* rel_base relf_*

gen rel_date_cut = rel_date
local pre_cut  = 10
local post_cut = 5
replace rel_date_cut = -`pre_cut'  if rel_date<-`pre_cut'
replace rel_date_cut = `post_cut' if rel_date>`post_cut'
tab rel_date_cut

forvalues k = `pre_cut'(-1)2 {
   gen relb_`k' = rel_date_cut == -`k'
   label var relb_`k' "-`k'"
}
gen rel_base = rel_date_cut==-1
label var rel_base "-1"
forvalues k = 0/`post_cut' {
    gen relf_`k' = rel_date_cut == `k'
    label var relf_`k' "`k'"
}
for var relb_* relf_* rel_base: replace X = 0 if group1==1 // 控制组的相对数量全是0
reghdfe y relb_* o.rel_base relf_*, a(id time) cluster(id)
eststo ES_bin

reghdfe y relb_* o.rel_base relf_* if inrange(rel_date,-10,5), a(id time) cluster(id)
eststo ES_trim

gl leadall "relb_10 relb_9 relb_8 relb_7 relb_6 relb_5 relb_4 relb_3 relb_2 rel_base"
gl lagall "relf_0 relf_1 relf_2 relf_3 relf_4 relf_5"
matrix btrue = J(1,16,.)
matrix colnames btrue = $leadall $lagall
qui forvalues l = 1/10 {
    matrix btrue[1,`l']=0
}
qui forvalues h = 0/5 {
    sum tauit if rel_date==`h'&D==1
    matrix btrue[1,`h'+11]=r(mean)
}
matrix list btrue

coefplot (matrix(btrue[1]), m(Oh)) ES_bin ES_trim, drop(_cons) order(relb* rel_base relf*) omitted baselevel vertical ///
    ciopts(recast(rcap)) yline(0, lc(gs0) lp(solid)) ///
    xline(10.5, lc(gs8) lp(dash)) ylabel(0(3)9) xtitle("相对处理时期") ytitle("估计系数") ///
    legend(order(2 "真实系数" 4 "归并样本 (binned)" 6 "截断样本 (trimmed)") $blgd) ///
    title("（b）事件窗口期[-10,5]", pos(6)) name(BT2)

grc1leg2 BT1 BT2, xsize(10) scale(1.5) labsize(small)

gr export "figure/ES_BinTrim.png", width(3000) replace
gr export "figure/ES_BinTrim.svg", replace
