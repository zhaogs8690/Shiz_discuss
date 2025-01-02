**入门模型
use "http://fmwww.bc.edu/repec/bocode/c/CardKrueger1994.dta", clear  //联网获取

**# Bookmark #1
*实施新法案后，美国两个州（新泽西州AJ=1，宾夕法尼亚州PA=0）快餐店就业人数
*t：取值为 1 时表示时间为 1992 年 11月，否则为 1992 年 2 月。
*D: AJ1992年通过法案，提高最低工资，对快餐店雇佣人数的影响。


 *生成实验组和法案实施时期的交互项
 gen did = t*treated
 duplicates drop id t, force  //删除重复出现的观测值
 *进行DID估计，并使用稳健标准误
 drop in 101/818 //删除101到第818个观测值
 
reg fte t treated did,r 
estimates store model1
*reg 解释变量y 时间变量t 处理变量treated 交乘项，稳健标准误
reg fte i.t##i.treated,r //结果与上行相同
estimates store model2

reg fte t treated did bk kfc roys,r
estimates store model3
diff fte, t(treated) p(t) cov(bk kfc roys) //结果同上，但是不能加入稳健标准误
estimates store model4

* 使用esttab将四个结果输出到同一张表
esttab model1 model2 model3 model4 using myresults.rtf, replace se r2 ar2 nogaps



clear all
. set obs 60  //设置obs数量
. set seed 10101  //设置种子，重现数据
. gen id =_n //创建id

/// 每一个数值的数量扩大11倍，再减去前六十个观测值，即60*11-60 = 600，为60个体10年的面板数据
. expand 11 
. drop in 1/60
. count

///以id分组生成时间标识
. bys id: gen time = _n+1999
. xtset id time

///生成协变量以及个体和时间效应
. gen x1 = rnormal(1,7)
. gen x2 = rnormal(2,5) 


. sort time id
. by time: gen ind = _n
. sort id time  //等同于reg y x i.id
. by id: gen T = _n //等同于 i.time



///生成处理变量,此时D为D*t,设定1-30在2004年接受冲击，31-60未受到冲击
. gen D = 0
. gen birth_date = 0

forvalues i = 1/30{
	replace D = 1 if id  == `i' & time >= 2004
	replace birth_date = 2004 if id == `i'
}

sort id time
//gen treat=1 if id<31
//replace treat=0 if treat==.
//gen post=1 if time>2003
//replace post=0 if post==.

//此刻再看数据，会发现四个部分，第一个部分是前30个人在2004年前未被冲击的部分，
/// 第二部分是前30个人在2004年后被冲击的部分，
///第3部分是从31个人到第60个人在2004年以前未被冲击，
///第4部分是从31个人到第60个人在2004年以后未被冲击

//需要说明的是这里的D其实就是Treat_i和Post_t的交互项，意思是只有受到冲击的处理组
//到了冲击时期之后才取值为1，其他取值都是0（0包括冲击时期之前的处理组和冲击时期前后都是0的控制组）
//构建被解释变量
. gen y = 0


///Y的生成，使得接受冲击的个体的政策真实效果为10
bysort id: gen y0 = 10  + 5 * x1 + 3 * x2 + T + ind  + rnormal()
bysort id: gen y1 = 10  + 5 * x1 + 3 * x2 + T + ind  + 10 + rnormal() if time >= 2004 & id >= 1 & id <= 30
bysort id: replace y1 = 10  + 5 * x1 + 3 * x2 + T + ind  + rnormal() if y1 == .
replace y = y0 + D * (y1 - y0)


//双向固定效应
reg y D x1 x2 i.time i.id, r
eststo reg   
xtreg y D x1 x2 i.time, r fe
eststo xtreg_fe  
areg y D x1 x2 i.time, absorb(id) robust
eststo areg
reghdfe y D x1 x2, absorb(id time) vce(robust)  //可以做高频，现在非常常用
eststo reghdfe

estout *, title("The Comparison of Actual Parameter Values") ///
		 cells(b(star fmt(%9.3f)) se(par)) ///
		 stats(N N_g, fmt(%9.0f %9.0g) label(N Groups)) ///
		 legend collabels(none) varlabels(_cons Constant) keep(x1 x2 D)
		 
//平行趋势检验//

gen treated=1 if id<=30 //前30个人受到了冲击
replace treated=0 if treated==.
gen post =(time>=2004)& !missing(time) //当time变量的值大于等于 2004并且time变量不缺失时，post变量的值设置为 1


gen period =time-2004
*数据集共包含10年，其中政策实施前4年，实施后5年
 *将政策实施前的年份与treated做交互
 forvalues i =4(-1)1{
 gen pre_`i' = (period == -`i' & treated ==1)
 }
 *将政策实施的年份（2004）与treated做交互
 gen current = (period == 0 & treated ==1)  // gen current=1 if period==0 & treated==1

 *将政策实施后的年份与treated做交互
 forvalues j =1(1)5{
 gen time_`j' = (period == `j'&treated ==1)
 }

xtset id time
xtreg y  pre_4 pre_3 pre_2 current time_* x1 x2 i.time, r fe  //选择一期作为参照组，通常选第一期或者政策前1期，或者当期（很少）

 
 
*//平行趋势图//*
reghdfe y  pre_4 pre_3 pre_2 current time_* x1 x2 , absorb(id time) vce(robust)
eststo reghdfe
coefplot reghdfe, keep(pre_* current time_*) vertical recast(connect) yline(0) xline(4,lp(dash)) /// 
ytitle("Coef") ///
xtitle("Time （pre_*policy，current policy，time_*post-policy）")

//理论上来说想画出平行趋势检验图要满足两个条件，第一个，因为我们要选择omitted掉一期，所以事件发生前至少需要2期，事件发生后需要1期，所以4期是极限（有的审稿人4期都不认可）。

