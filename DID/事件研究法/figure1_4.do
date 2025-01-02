
* 设定主路径
cd "$root"

clear all

**********************
**       图1        **
**********************
use Currie_2020, clear
tw (line DD year, lw(thick)) (line ES year, lw(thick)), ///
    xlab(2005(5)2020) ylab(0(0.05)0.25, format(%6.2f)) ///
    legend(order(1 "双重差分法" 2 "事件研究法")) ///
    xtitle("") ytitle("占比") subtitle("（a）英文经济学五大刊", pos(6)) scale(1.2) name(top5)

use cntop, clear
keep if year>=2004
gen DD_share = dd/total
gen ES_share = es/total
gen ESPT_share = espt/total
two (line DD_share year, lw(thick)) ///
    (line ES_share year, lw(thick)) ///
    (line ESPT_share year, lw(thick) lp(shortdash)), ///
    xlab(2005(5)2020) ylab(0(0.05)0.25, format(%6.2f)) ///
    legend(order(1 "双重差分法" 2 "事件研究法" 3 "事件研究法+平行趋势") size(medsmall)) ///
    xtitle("") ytitle("占比") subtitle("（b）中文权威期刊", pos(6)) scale(1.2) name(cntop)

grc1leg2 top5 cntop, ytol legendfrom(cntop) xsize(8) scale(1.5)
gr export "figure/topshare.png", width(3000) replace
gr export "figure/topshare.svg",  replace


**********************
**       图4        **
**********************

set seed 20230101
clear all    
set obs 400
gen id = _n
expand 20
bysort id: gen time = _n  

* 生成个体固定效应
gen  unit_c     = runiform(0,10) if time==1
egen unit_fe  = mean(unit_c), by(id)

gen     indicator_c = unit_c 
qui sum indicator_c,d
scalar  threshold1   = r(p25)
scalar  threshold2   = r(p50)
scalar  threshold3   = r(p75)
egen    indicator   = mean(indicator_c), by(id)
gen     treat = 1
replace treat = 2 if indicator>threshold1
replace treat = 3 if indicator>threshold2
replace treat = 4 if indicator>threshold3
tab treat, gen(group)

gen timing = .
replace timing = 5 if treat==2
replace timing = 10 if treat==3
replace timing = 15 if treat==4

* 定义潜在结果y0和y1
gen rel_date = time-timing
replace rel_date = 0 if mi(rel_date)
gen post = rel_date>=0
gen group_level = 15*group2 + 10*group3 +5*group4
gen group_trend = 0
egen X_cf = mean(unit_c), by(treat)

gen y0 = group_level + group_trend   + runiform(0,10)
gen y11 = group_level + group_trend   + (5*group2 + 5*group3 + 5*group4)*post + runiform(0,10) // static effect
gen y12 = group_level + group_trend   + (1*group2 + 1*group3 + 1*group4)*post*(rel_date+1) + runiform(0,10) // dynamic effect
gen y13 = group_level + group_trend   + (0.5*group2 + 1*group3 + 2*group4)*post*(rel_date+1) + runiform(0,10) // dynamic effect

* 生成观测样本y 
cap drop y
gen D = 0
replace D = 1 if rel_date>=0&inlist(treat,2,3,4)
gen y1 = (1-D)*y0 + D*y11
gen y2 = (1-D)*y0 + D*y12
gen y3 = (1-D)*y0 + D*y13

preserve 
collapse (mean)  y1 y2 y3 y0, by(treat time)
tw  (line y1 time if treat==2, lw(thick)) ///
    (line y1 time if treat==3, lw(thick)) ///
    (line y1 time if treat==4, lw(thick)) ///
    (line y1 time if treat==1, lw(thick)), ylabel(0(10)30) ///
    legend(order(1 "组群1（ATT{sub:t}=5）" 2 "组群2（ATT{sub:t}=5）" 3 "组群3（ATT{sub:t}=5）" 4 "控制组") $blgd col(2)) xtitle("") ytitle("结果变量Y")  ///
    tit("（a）同质性处理效应", pos(6)) name(TEA1)
restore

preserve 
collapse (mean)  y1 y2 y3 y0, by(treat time)
tw  (line y2 time if treat==2, lw(thick)) ///
    (line y2 time if treat==3, lw(thick)) ///
    (line y2 time if treat==4, lw(thick)) ///
    (line y2 time if treat==1, lw(thick)), ylabel(0(10)30) ///
    legend(order(1 "组群1（ATT{sub:t}=1t）" 2 "组群2（ATT{sub:t}=1t）" 3 "组群3（ATT{sub:t}=1t）" 4 "控制组") $blgd col(2)) xtitle("") ytitle("结果变量Y")   ///
    tit("（b）同质性处理效应路径", pos(6)) name(TEA2)
restore

preserve 
collapse (mean)  y1 y2 y3 y0, by(treat time)
tw  (line y3 time if treat==2, lw(thick)) ///
    (line y3 time if treat==3, lw(thick)) ///
    (line y3 time if treat==4, lw(thick)) ///
    (line y3 time if treat==1, lw(thick)), ylabel(0(10)30) ///
    legend(order(1 "组群1（ATT{sub:t}=0.5t）" 2 "组群2（ATT{sub:t}=1t）" 3 "组群3（ATT{sub:t}=2t）" 4 "控制组") $blgd col(2)) xtitle("") ytitle("结果变量Y") ///
    tit("（c）异质性处理效应", pos(6)) name(TEA3)
restore

gr combine TEA1 TEA2 TEA3, ysize(4) xsize(12) row(1) scale(1.5)
gr export "figure/ES_TEA.png", width(3000) replace
gr export "figure/ES_TEA.svg", replace

