
clear all

set seed 20230101

***********************
**       DGP1        **
***********************
*  - 交错处理时点
*  - 同质性处理效应路径
*  -满足平行趋势

* N=400, T=20, G=4
clear all    
set obs 400
gen id = _n
expand 20
bysort id: gen time = _n  

* 生成个体固定效应
gen  unit_c     = runiform(0,10) if time==1
egen unit_spec  = mean(unit_c), by(id)

gen x = runiform(0,1)
gen     indicator_c = unit_c + 1*x
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
gen group_level = 10*group2 + 3*group3 -3*group4
gen group_trend = 0
gen tauit =  (1*group2 + 1*group3 + 1*group4)*post*(rel_date+1)
gen y0 = group_level + group_trend + unit_spec + runiform(0,10)
gen y1 = group_level + group_trend + unit_spec + tauit + runiform(0,10)
* 生成观测样本y 
cap drop y
gen D = 0
replace D = 1 if rel_date>=0&inlist(treat,2,3,4)
gen y = (1-D)*y0 + D*y1 

forvalues k = 14(-1)2 {
   gen relb_`k' = rel_date == -`k'  // 事前相对时点
   label var relb_`k' "-`k'"        // label用于画图
}

gen rel_base = rel_date==-1 //基期
label var rel_base "-1"

forvalues k = 0/15 {
    gen relf_`k' = rel_date == `k'  // 事后相对时点
    label var relf_`k' "`k'"            // label用于画图
}
for var relb_* relf_* rel_base: replace X = 0 if group1==1 // 控制组的相对时点全部定义为0
save simu_data_DGP01, replace




***********************
**       DGP2        **
***********************
*  -交错处理时点
*  -同质性处理效应路径
*  -存在组间趋势差异，不满足平行趋势

* 设定随机数种子
set seed 20230101

clear all    
set obs 400
gen id = _n
expand 20
bysort id: gen time = _n  

* 生成个体固定效应
gen  unit_c     = runiform(0,10) if time==1
egen unit_spec  = mean(unit_c), by(id)
gen x = runiform(0,1)

gen     indicator_c = unit_c + 1*x
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
gen group_level = 10*group2 + 3*group3 -3*group4
gen group_trend = (0*group1 + 1*group2 + 0.5*group3 + 0.2*group4)*time
gen tauit =  (1*group2 + 1*group3 + 1*group4)*post*(1 + rel_date) //short-run and dynamic effects
gen y0 = group_level + group_trend + unit_spec + runiform(0,10)
gen y1 = group_level + group_trend + unit_spec + tauit + runiform(0,10)
* 生成观测样本y 
cap drop y
gen D = 0
replace D = 1 if rel_date>=0&inlist(treat,2,3,4)
gen y = (1-D)*y0 + D*y1 

forvalues k = 14(-1)2 {
   gen relb_`k' = rel_date == -`k'  // 事前相对时点
   label var relb_`k' "-`k'"        // label用于画图
}

gen rel_base = rel_date==-1 //基期
label var rel_base "-1"

forvalues k = 0/15 {
    gen relf_`k' = rel_date == `k'  // 事后相对时点
    label var relf_`k' "`k'"            // label用于画图
}
for var relb_* relf_* rel_base: replace X = 0 if group1==1 // 控制组的相对时点全部定义为0

save simu_data_DGP02, replace



***********************
**       DGP3        **
***********************
*  -交错处理时点
*  -异质性处理效应
*  -满足平行趋势

* 设定随机数种子
set seed 20230101

clear all    
set obs 400
gen id = _n
expand 20
bysort id: gen time = _n  

* 生成个体固定效应
gen  unit_c     = runiform(0,10) if time==1
egen unit_spec  = mean(unit_c), by(id)

gen x = runiform(0,1)
gen     indicator_c = unit_c + 1*x
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
gen group_level = 10*group2 + 3*group3 -3*group4
gen group_trend = 0
gen tauit =  (0.5*group2 + 1*group3 + 2*group4)*post*(rel_date+1)
gen y0 = group_level + group_trend + unit_spec + runiform(0,10)
gen y1 = group_level + group_trend + unit_spec + tauit + runiform(0,10)

* 生成观测样本y 
cap drop y
gen D = 0
replace D = 1 if rel_date>=0&inlist(treat,2,3,4)
gen y = (1-D)*y0 + D*y1 

gen rel_date_cut = rel_date
local pre_cut  = 14
local post_cut = 15
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

save simu_data_DGP03, replace


***********************
**       DGP4        **
***********************
*  -交错处理时点
*  -异质性处理效应
*  -存在组间趋势差异，不满足平行趋势

* 设定随机数种子
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
gen group_level = 40*group2 + 25*group3 +10*group4
gen group_trend = 0
egen X_cf = mean(unit_c), by(treat)

gen tauit =  (2*group2 + 1*group3 + 0.5*group4)*post*(rel_date+1)
gen y0 = group_level + group_trend  - 0.1*X_cf*rel_date*post + runiform(0,10)
gen y1 = group_level + group_trend  - 0.1*X_cf*rel_date*post + tauit + runiform(0,10)
* 生成观测样本y 
cap drop y
gen D = 0
replace D = 1 if rel_date>=0&inlist(treat,2,3,4)
gen y = (1-D)*y0 + D*y1 

gen rel_date_cut = rel_date
local pre_cut  = 14
local post_cut = 15
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
save simu_data_DGP04, replace
