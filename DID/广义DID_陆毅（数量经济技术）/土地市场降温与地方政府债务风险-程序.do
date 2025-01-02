
****** 对原始数据的清洗与变量设置 ******

clear
use "土地市场降温与地方政府债务风险-数据.dta"

gen ybyssr0=ybyssr
gen zyxsr0=zyxsr
gen zfxjjsr0=zfxjjsr
gen gyzbsr0=gyzbsr

replace ybyssr0=. if ybyssr<0
replace zyxsr0=. if zyxsr<0
replace zfxjjsr0=. if zfxjjsr<0
replace gyzbsr0=. if gyzbsr<0

replace ybyssr=0 if ybyssr==.
replace zyxsr=0 if zyxsr==.
replace zfxjjsr=0 if zfxjjsr==.
replace gyzbsr=0 if gyzbsr==.


replace landsr=. if landsr<0
replace zyxsr=. if zyxsr<0
replace gyzbsr=. if gyzbsr<0

*处理强度变量
gen treatment=.
replace treatment=landsr/(ybyssr+zyxsr+zfxjjsr+gyzbsr)
replace treatment=. if treatment>1

bys county(year):egen treatment_a=mean(treatment) if year<2018
bys county(year):replace treatment_a=treatment_a[1]

egen mediantreat_a=median(treatment_a)
gen hightreat=.
replace hightreat=1 if treatment_a>=mediantreat_a
replace hightreat=0 if treatment_a<mediantreat_a
replace hightreat=. if treatment_a==.

*四个前定变量
bys county(year):egen rjgdp_a=mean(rjgdp) if year<2018
bys county(year):replace rjgdp_a=rjgdp_a[1]
gen lnrjgdp_a=ln(1+rjgdp_a)

bys county(year):egen czzjl_a=mean(czzjl) if year<2018
bys county(year):replace czzjl_a=czzjl_a[1]
replace czzjl_a=czzjl_a/100

bys county(year):egen ybyszc_a=mean(ybyszc) if year<2018
bys county(year):replace ybyszc_a=ybyszc_a[1]
gen lnybyszc_a=ln(ybyszc_a+1)

replace zfxjjzc=. if zfxjjzc<0
bys county(year):egen zfxjjzc_a=mean(zfxjjzc) if year<2018
bys county(year):replace zfxjjzc_a=zfxjjzc_a[1]
gen lnzfxjjzc_a=ln(zfxjjzc_a+1)

*其他备选变量
bys county(year):egen gyzcz_a=mean(gyzcz) if year<2018
bys county(year):replace gyzcz_a=gyzcz_a[1]
gen lngyzcz_a=ln(gyzcz_a+1)

bys county(year):egen secondind_a=mean(secondind) if year<2018
bys county(year):replace secondind_a=secondind_a[1]
gen lnsecondind_a=ln(secondind_a+1)

bys county(year):egen gdzctz_a=mean(gdzctz) if year<2018
bys county(year):replace gdzctz_a=gdzctz_a[1]
gen lngdzctz_a=ln(gdzctz_a+1)

bys county(year):egen jckze_a=mean(jckze) if year<2018
bys county(year):replace jckze_a=jckze_a[1]
gen lnjckze_a=ln(jckze_a+1)

bys county(year):egen dfzfzwxe_a=mean(dfzfzwxe) if year<2018
bys county(year):replace dfzfzwxe_a=dfzfzwxe_a[1]
gen lndfzfzwxe_a=ln(dfzfzwxe_a+1)

bys county(year):egen zxzxe_a=mean(zxzxe) if year<2018
bys county(year):replace zxzxe_a=zxzxe_a[1]
gen lnzxzxe_a=ln(zxzxe_a+1)


*基准回归所需变量

gen did=treatment_a*post
gen t=.
replace t=1 if year==2014
replace t=2 if year==2015
replace t=3 if year==2016
replace t=4 if year==2017
replace t=5 if year==2018
replace t=6 if year==2019
replace t=7 if year==2020
replace t=8 if year==2021

gen lnrjgdp_a_t=lnrjgdp_a*t
gen lnrjgdp_a_t2=lnrjgdp_a*t*t
gen lnrjgdp_a_t3=lnrjgdp_a*t*t*t

gen czzjl_a_t=czzjl_a*t
gen czzjl_a_t2=czzjl_a*t*t
gen czzjl_a_t3=czzjl_a*t*t*t

gen lnybyszc_a_t=lnybyszc_a*t
gen lnybyszc_a_t2=lnybyszc_a*t*t
gen lnybyszc_a_t3=lnybyszc_a*t*t*t

gen lnzfxjjzc_a_t=lnzfxjjzc_a*t
gen lnzfxjjzc_a_t2=lnzfxjjzc_a*t*t
gen lnzfxjjzc_a_t3=lnzfxjjzc_a*t*t*t

gen lnrjgdp_a_p=lnrjgdp_a*post
gen czzjl_a_p=czzjl_a*post
gen lnybyszc_a_p=lnybyszc_a*post
gen lnzfxjjzc_a_p=lnzfxjjzc_a*post

//其他回归所需变量
gen ctztreat=.
replace ctztreat=rzptyxzw/(dfzfzwye+rzptyxzw)
bys county(year):egen ctztreat_a=mean(ctztreat) if year<2017
bys county(year):replace ctztreat_a=ctztreat_a[1]
gen taxtreat=.
gen post_ctz=0
replace post_ctz=1 if year>=2017
gen reform_ctz=ctztreat_a*post_ctz

replace taxtreat=taxsr/(ybyssr+zyxsr+zfxjjsr+gyzbsr)
replace taxtreat=. if taxtreat>1
bys county(year):egen taxtreat_a=mean(taxtreat) if year<2018
bys county(year):replace taxtreat_a=taxtreat_a[1]
gen post_tax=0
replace post_tax=1 if year>=2018
gen reform_tax=taxtreat_a*post_tax

gen post_covid=0
replace post_covid=1 if year>=2020
norm 累计确诊, method(mmx)
gen covid=mmx_累计确诊*post_covid

replace zxzxe=. if (zxzxe!=.&zxzye==.)
replace zxzye=. if (zxzxe==.&zxzye!=.)
replace zxzye=. if (zxzxe==0&zxzye!=0)
replace zxzxe=. if (zxzxe==0&zxzye!=0)
replace zxzye=. if (zxzye>zxzxe)
replace zxzxe=. if (zxzye>zxzxe)
gen space=zxzxe-zxzye
gen lnspace=ln(space+1)

bys county(year):egen rzptyxzw_a=mean(rzptyxzw) if year<2018
bys county(year):replace rzptyxzw_a=rzptyxzw_a[1]

bys county(year):egen zxzye_a=mean(zxzye) if year<2018
bys county(year):replace zxzye_a=zxzye_a[1]

gen taxzb=.
replace taxzb=taxsr/(ybyssr+zyxsr+zfxjjsr+gyzbsr)
replace taxzb=. if taxzb>1
bys county(year):egen taxzb_a=mean(taxzb) if year<2018
bys county(year):replace taxzb_a=taxzb_a[1]
gen did_taxzba=did*taxzb_a
gen treat_taxzba=treatment_a*taxzb_a
gen post_taxzba=post*taxzb_a


****** 附表1 前定变量分析 ******
reg treatment_a lnrjgdp_a lngyzcz_a  lnsecondind_a lngdzctz_a lnjckze_a  czzjl_a lnybyszc_a lnzfxjjzc_a lndfzfzwxe_a lnzxzxe_a if year==2014
outreg2 using 附表1.word,replace


****** 附表2 平衡性检验 ******
** Panel A
*  Cols 1-2
sum lnrjgdp_a czzjl_a lnybyszc_a lnzfxjjzc_a if hightreat==1
sum lnrjgdp_a czzjl_a lnybyszc_a lnzfxjjzc_a if hightreat==0
*  Col. 3
reg lnrjgdp_a treatment_a if year==2014,r
outreg2 using 附表2_A.word,replace
reg czzjl_a treatment_a if year==2014,r
outreg2 using 附表2_A.word,append
reg lnybyszc_a treatment_a if year==2014,r
outreg2 using 附表2_A.word,append
reg lnzfxjjzc_a treatment_a if year==2014,r
outreg2 using 附表2_A.word,append

** Panel B
*  Cols 1-2
sum lngyzcz_a lnsecondind_a lngdzctz_a lnjckze_a lndfzfzwxe_a lnzxzxe_a if hightreat==1
sum lngyzcz_a lnsecondind_a lngdzctz_a lnjckze_a lndfzfzwxe_a lnzxzxe_a if hightreat==0
*  Col. 3
reg lngyzcz_a treatment_a if year==2014,r
outreg2 using 附表2_B.word,replace
reg lnsecondind_a treatment_a if year==2014,r
outreg2 using 附表2_B.word,append
reg lngdzctz_a treatment_a if year==2014,r
outreg2 using 附表2_B.word,append
reg lnjckze_a treatment_a if year==2014,r
outreg2 using 附表2_B.word,append
reg lndfzfzwxe_a treatment_a if year==2014,r
outreg2 using 附表2_B.word,append
reg lnzxzxe_a treatment_a if year==2014,r
outreg2 using 附表2_B.word,append
*  Col. 4
reg lngyzcz_a treatment_a lnrjgdp_a czzjl_a lnybyszc_a lnzfxjjzc_a if year==2014,r
outreg2 using 附表2_B4.word,replace
reg lnsecondind_a treatment_a lnrjgdp_a czzjl_a lnybyszc_a lnzfxjjzc_a if year==2014,r
outreg2 using 附表2_B4.word,append
reg lngdzctz_a treatment_a lnrjgdp_a czzjl_a lnybyszc_a lnzfxjjzc_a if year==2014,r
outreg2 using 附表2_B4.word,append
reg lnjckze_a treatment_a lnrjgdp_a czzjl_a lnybyszc_a lnzfxjjzc_a if year==2014,r
outreg2 using 附表2_B4.word,append
reg lndfzfzwxe_a treatment_a lnrjgdp_a czzjl_a lnybyszc_a lnzfxjjzc_a if year==2014,r
outreg2 using 附表2_B4.word,append
reg lnzxzxe_a treatment_a lnrjgdp_a czzjl_a lnybyszc_a lnzfxjjzc_a if year==2014,r
outreg2 using 附表2_B4.word,append

tab hightreat if year==2014


****** 附表3 描述性统计 ******
sum y1 dfzfzwye ybzye zxzye rzptyxzw ybyssr0 zyxsr0 zfxjjsr0 gyzbsr0 taxsr tdcj tdlp tdtc landsr intrrate_a space gdzctz gdprate term_a zrzzqzb treatment_a post rjgdp_a czzjl_a ybyszc_a zfxjjzc_a reform_ctz reform_tax covid rzptyxzw_a zxzye_a taxzb_a


****** 表1 基准回归：土地市场降温对地方政府债务风险的影响 ******
xi:areg y1 did lnrjgdp_a_t lnrjgdp_a_t2 lnrjgdp_a_t3 czzjl_a_t czzjl_a_t2 czzjl_a_t3 lnybyszc_a_t lnybyszc_a_t2 lnybyszc_a_t3 lnzfxjjzc_a_t lnzfxjjzc_a_t2 lnzfxjjzc_a_t3 i.year,absorb(id) cluster(id)
outreg2 using 表1.word,replace

xi:areg y1 did lnrjgdp_a_p czzjl_a_p lnybyszc_a_p lnzfxjjzc_a_p i.year,absorb(id) cluster(id)
outreg2 using 表1.word,append

xi:areg y1 did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表1.word,append

*xi：是generate expanded variables的缩写。它通常用于在回归模型中生成虚拟变量或交互项。例如，如果有一个分类变量，xi可以将其转换为一组虚拟变量，以便在回归中使用。
*areg：全称是absorbing regression。它用于进行带有固定效应的回归分析。固定效应可以是个体固定效应（如公司、地区等）或时间固定效应。


****** 图4 平行趋势检验：事件研究法 ******
gen base=0

gen time=year-2018
gen reform_pre4=treatment_a*(time==-4)
gen reform_pre3=treatment_a*(time==-3)
gen reform_pre2=treatment_a*(time==-2)
gen reform_pre1=treatment_a*(time==-1)
gen reform_post0=treatment_a*(time==0)
gen reform_post1=treatment_a*(time==1)
gen reform_post2=treatment_a*(time==2)
gen reform_post3=treatment_a*(time==3)

xi:areg y1 reform_pre4 base reform_pre2 reform_pre1 reform_post0 reform_post1 reform_post2 reform_post3 i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)

est store Dynamic
coefplot Dynamic, keep(reform_pre4 base reform_pre2 reform_pre1 reform_post0 reform_post1 reform_post2 reform_post3) vertical levels(90) omit ///
yscale(range(-5 5)) ylabel(-5(1)5) mcolor(black)  lcolor(black) ///
xtitle("距离政策冲击的年份")  xscale(range(1 8)) xlabel(1 "-4" 2 "-3" 3 "-2" 4 "-1" 5 "0" 6 "1" 7 "2" 8 "3", labcolor(black) tposition(crossing) tlcolor(black)) ///
ytitle("coefficient")   ///
addplot(scatteri  0  2, mcolor(black)) ///
ciopts(lpattern(-) recast(rcap) msize(medium) lcolor(gray)) ///
text(-0.4 2 "base year", size(small)) ///
recast(connect) lcolor(black) lpattern(solid) mcolor(black) ///
mlcolor(black) mfcolor(white) msize(*1.2) msymbol(h) ///
yline(0,lcolor(black) lpattern(dash) lwidth(*1.0)) ///
ylabel(,nogrid tposition(crossing) tlcolor(black)) ///
graphregion(color(gs16)) ///
plotr(lcolor(black) lpattern(1) lwidth(*1.5)) 


****** 附表4 稳健性检验：更换处理强度变量设置方式 ******
*平均数分组
egen meantreat_a=mean(treatment_a)
gen treatment2=.
replace treatment2=1 if treatment_a>=meantreat_a
replace treatment2=0 if treatment_a<meantreat_a
replace treatment2=. if treatment_a==.
gen did2=treatment2*post
xi:areg y1 did2 i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表4.word,replace

*中位数分组
xtile nq2=treatment_a,nq(2)
gen treatment3=.
replace treatment3=1 if nq2==2
replace treatment3=0 if nq2==1
replace treatment3=. if treatment_a==.
gen did3=treatment3*post
xi:areg y1 did3 i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表4.word,append

*三分位数分组-较高1/3是处理组
xtile nq3=treatment_a,nq(3)
gen treatment4=.
replace treatment4=1 if nq3==3
replace treatment4=0 if (nq3==2|nq3==1)
replace treatment4=. if treatment_a==.
gen did4=treatment4*post
xi:areg y1 did4 i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表4.word,append

*四分位数分组-较高1/4是处理组
xtile nq4=treatment_a,nq(4)
gen treatment5=.
replace treatment5=1 if nq4==4
replace treatment5=0 if (nq4==3|nq4==2|nq4==1)
replace treatment5=. if treatment_a==.
gen did5=treatment5*post
xi:areg y1 did5 i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表4.word,append


****** 附表5 稳健性检验：排除同期政策干扰 ******
*城投债约束
xi:areg y1 did reform_ctz i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表5.word,replace

*减税降费
xi:areg y1 did reform_tax i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表5.word,append

*COVID-19
xi:areg y1 did covid i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表5.word,append

*同时控制
xi:areg y1 did reform_ctz reform_tax covid i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表5.word,append


****** 附表6 稳健性检验：排除2015年干扰 ******
*仅删除2015年样本
xi:areg y1 did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year if year!=2015,absorb(id) cluster(id)
outreg2 using 附表6.word,replace

*删除2015年及以前样本
xi:areg y1 did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year if year>2015,absorb(id) cluster(id)
outreg2 using 附表6.word,append

*处理强度变量计算时排除2015年，且仅删除2015年样本
gen treatment6=.
replace treatment6=landsr/(ybyssr+zyxsr+zfxjjsr+gyzbsr)
replace treatment6=. if treatment6>1
bys county(year):egen treatment6_a=mean(treatment6) if year<2018&year!=2015
bys county(year):replace treatment6_a=treatment6_a[1]
gen did6=treatment6_a*post

xi:areg y1 did6 i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year if year!=2015,absorb(id) cluster(id)
outreg2 using 附表6.word,append

*处理强度变量计算时排除2015年，且删除2015年及以前样本
xi:areg y1 did6 i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year if year>2015,absorb(id) cluster(id)
outreg2 using 附表6.word,append


****** 附表7 稳健性检验：将30个特定城市作为处理组 ******
gen shiqu=ustrregexm(county,"区")
gen treat30=0
replace treat30=1 if city=="北京市"|city=="上海市"|city=="广州市"|city=="深圳市"|city=="天津市"|city=="南京市"|city=="苏州市"|city=="无锡市"|city=="杭州市"|city=="合肥市"|city=="福州市"|city=="厦门市"|city=="济南市"|city=="郑州市"|city=="武汉市"|city=="成都市"|city=="长沙市"|city=="重庆市"|city=="西安市"|city=="昆明市"|city=="佛山市"|city=="徐州市"|city=="太原市"|city=="海口市"|city=="宁波市"|city=="宜昌市"|city=="哈尔滨市"|city=="长春市"|city=="兰州市"|city=="贵阳市"
gen post30=0
replace post30=1 if year>=2018
gen did30=treat30*post30

*  Cols 1-3
xi:areg y1 did30 lnrjgdp_a_t lnrjgdp_a_t2 lnrjgdp_a_t3 czzjl_a_t czzjl_a_t2 czzjl_a_t3 lnybyszc_a_t lnybyszc_a_t2 lnybyszc_a_t3 lnzfxjjzc_a_t lnzfxjjzc_a_t2 lnzfxjjzc_a_t3 i.year if shiqu==1,absorb(id)
outreg2 using 附表7.word,replace

xi:areg y1 did30 lnrjgdp_a_p czzjl_a_p lnybyszc_a_p lnzfxjjzc_a_p i.year if shiqu==1,absorb(id)
outreg2 using 附表7.word,append

xi:areg y1 did30 i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year if shiqu==1,absorb(id)
outreg2 using 附表7.word,append

*  Cols 4-6
xi:areg y1 did did30 lnrjgdp_a_t lnrjgdp_a_t2 lnrjgdp_a_t3 czzjl_a_t czzjl_a_t2 czzjl_a_t3 lnybyszc_a_t lnybyszc_a_t2 lnybyszc_a_t3 lnzfxjjzc_a_t lnzfxjjzc_a_t2 lnzfxjjzc_a_t3 i.year,absorb(id) cluster(id)
outreg2 using 附表7.word,append

xi:areg y1 did did30 lnrjgdp_a_p czzjl_a_p lnybyszc_a_p lnzfxjjzc_a_p i.year,absorb(id) cluster(id)
outreg2 using 附表7.word,append

xi:areg y1 did did30 i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表7.word,append


****** 附表8 稳健性检验：数据缩尾与缩小样本范围 ******
*  Cols 1-3
clear
use "土地市场降温与地方政府债务风险-数据.dta"

gen ybyssr0=ybyssr
gen zyxsr0=zyxsr
gen zfxjjsr0=zfxjjsr
gen gyzbsr0=gyzbsr

replace ybyssr0=. if ybyssr<0
replace zyxsr0=. if zyxsr<0
replace zfxjjsr0=. if zfxjjsr<0
replace gyzbsr0=. if gyzbsr<0

replace ybyssr=0 if ybyssr==.
replace zyxsr=0 if zyxsr==.
replace zfxjjsr=0 if zfxjjsr==.
replace gyzbsr=0 if gyzbsr==.

replace landsr=. if landsr<0
replace zyxsr=. if zyxsr<0
replace gyzbsr=. if gyzbsr<0

gen treatment=.
replace treatment=landsr/(ybyssr+zyxsr+zfxjjsr+gyzbsr)
replace treatment=. if treatment>1

bys county(year):egen treatment_a=mean(treatment) if year<2018
bys county(year):replace treatment_a=treatment_a[1]

winsor2 y1 rjgdp czzjl ybyszc zfxjjzc, cut (5 95) replace

bys county(year):egen rjgdp_a=mean(rjgdp) if year<2018
bys county(year):replace rjgdp_a=rjgdp_a[1]
gen lnrjgdp_a=ln(1+rjgdp_a)

bys county(year):egen czzjl_a=mean(czzjl) if year<2018
bys county(year):replace czzjl_a=czzjl_a[1]
replace czzjl_a=czzjl_a/100

bys county(year):egen ybyszc_a=mean(ybyszc) if year<2018
bys county(year):replace ybyszc_a=ybyszc_a[1]
gen lnybyszc_a=ln(ybyszc_a+1)

replace zfxjjzc=. if zfxjjzc<0
bys county(year):egen zfxjjzc_a=mean(zfxjjzc) if year<2018
bys county(year):replace zfxjjzc_a=zfxjjzc_a[1]
gen lnzfxjjzc_a=ln(zfxjjzc_a+1)

gen did=treatment_a*post
gen t=.
replace t=1 if year==2014
replace t=2 if year==2015
replace t=3 if year==2016
replace t=4 if year==2017
replace t=5 if year==2018
replace t=6 if year==2019
replace t=7 if year==2020
replace t=8 if year==2021

gen treat_t=treatment_a*t

gen lnrjgdp_a_t=lnrjgdp_a*t
gen lnrjgdp_a_t2=lnrjgdp_a*t*t
gen lnrjgdp_a_t3=lnrjgdp_a*t*t*t

gen czzjl_a_t=czzjl_a*t
gen czzjl_a_t2=czzjl_a*t*t
gen czzjl_a_t3=czzjl_a*t*t*t

gen lnybyszc_a_t=lnybyszc_a*t
gen lnybyszc_a_t2=lnybyszc_a*t*t
gen lnybyszc_a_t3=lnybyszc_a*t*t*t

gen lnzfxjjzc_a_t=lnzfxjjzc_a*t
gen lnzfxjjzc_a_t2=lnzfxjjzc_a*t*t
gen lnzfxjjzc_a_t3=lnzfxjjzc_a*t*t*t

gen lnrjgdp_a_p=lnrjgdp_a*post
gen czzjl_a_p=czzjl_a*post
gen lnybyszc_a_p=lnybyszc_a*post
gen lnzfxjjzc_a_p=lnzfxjjzc_a*post

xi:areg y1 did lnrjgdp_a_t lnrjgdp_a_t2 lnrjgdp_a_t3 czzjl_a_t czzjl_a_t2 czzjl_a_t3 lnybyszc_a_t lnybyszc_a_t2 lnybyszc_a_t3 lnzfxjjzc_a_t lnzfxjjzc_a_t2 lnzfxjjzc_a_t3 i.year,absorb(id) cluster(id)
outreg2 using 附表8.word,replace

xi:areg y1 did lnrjgdp_a_p czzjl_a_p lnybyszc_a_p lnzfxjjzc_a_p i.year,absorb(id) cluster(id)
outreg2 using 附表8.word,append

xi:areg y1 did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表8.word,append

*  Col. 4
clear
use "土地市场降温与地方政府债务风险-数据.dta"

gen ybyssr0=ybyssr
gen zyxsr0=zyxsr
gen zfxjjsr0=zfxjjsr
gen gyzbsr0=gyzbsr

replace ybyssr0=. if ybyssr<0
replace zyxsr0=. if zyxsr<0
replace zfxjjsr0=. if zfxjjsr<0
replace gyzbsr0=. if gyzbsr<0

replace ybyssr=0 if ybyssr==.
replace zyxsr=0 if zyxsr==.
replace zfxjjsr=0 if zfxjjsr==.
replace gyzbsr=0 if gyzbsr==.

replace landsr=. if landsr<0
replace zyxsr=. if zyxsr<0
replace gyzbsr=. if gyzbsr<0

gen treatment=.
replace treatment=landsr/(ybyssr+zyxsr+zfxjjsr+gyzbsr)
replace treatment=. if treatment>1

bys county(year):egen treatment_a=mean(treatment) if year<2018
bys county(year):replace treatment_a=treatment_a[1]

bys county(year):egen rjgdp_a=mean(rjgdp) if year<2018
bys county(year):replace rjgdp_a=rjgdp_a[1]
gen lnrjgdp_a=ln(1+rjgdp_a)

bys county(year):egen czzjl_a=mean(czzjl) if year<2018
bys county(year):replace czzjl_a=czzjl_a[1]
replace czzjl_a=czzjl_a/100

bys county(year):egen ybyszc_a=mean(ybyszc) if year<2018
bys county(year):replace ybyszc_a=ybyszc_a[1]
gen lnybyszc_a=ln(ybyszc_a+1)

replace zfxjjzc=. if zfxjjzc<0
bys county(year):egen zfxjjzc_a=mean(zfxjjzc) if year<2018
bys county(year):replace zfxjjzc_a=zfxjjzc_a[1]
gen lnzfxjjzc_a=ln(zfxjjzc_a+1)

gen did=treatment_a*post

gen shiqu=ustrregexm(county,"区")
xi:areg y1 did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year if shiqu==0,absorb(id) cluster(id)
outreg2 using 附表8.word,append


****** 附表9 稳健性检验：排除预期效应 ******
*  Cols 1-3
clear
use "土地市场降温与地方政府债务风险-数据.dta"

gen ybyssr0=ybyssr
gen zyxsr0=zyxsr
gen zfxjjsr0=zfxjjsr
gen gyzbsr0=gyzbsr

replace ybyssr0=. if ybyssr<0
replace zyxsr0=. if zyxsr<0
replace zfxjjsr0=. if zfxjjsr<0
replace gyzbsr0=. if gyzbsr<0

replace ybyssr=0 if ybyssr==.
replace zyxsr=0 if zyxsr==.
replace zfxjjsr=0 if zfxjjsr==.
replace gyzbsr=0 if gyzbsr==.

replace landsr=. if landsr<0
replace zyxsr=. if zyxsr<0
replace gyzbsr=. if gyzbsr<0

gen treatment=.
replace treatment=landsr/(ybyssr+zyxsr+zfxjjsr+gyzbsr)
replace treatment=. if treatment>1

bys county(year):egen treatment_a=mean(treatment) if year<2016
bys county(year):replace treatment_a=treatment_a[1]

bys county(year):egen rjgdp_a=mean(rjgdp) if year<2016
bys county(year):replace rjgdp_a=rjgdp_a[1]
gen lnrjgdp_a=ln(1+rjgdp_a)

bys county(year):egen czzjl_a=mean(czzjl) if year<2016
bys county(year):replace czzjl_a=czzjl_a[1]
replace czzjl_a=czzjl_a/100

bys county(year):egen ybyszc_a=mean(ybyszc) if year<2016
bys county(year):replace ybyszc_a=ybyszc_a[1]
gen lnybyszc_a=ln(ybyszc_a+1)

replace zfxjjzc=. if zfxjjzc<0
bys county(year):egen zfxjjzc_a=mean(zfxjjzc) if year<2016
bys county(year):replace zfxjjzc_a=zfxjjzc_a[1]
gen lnzfxjjzc_a=ln(zfxjjzc_a+1)

replace post=1 if year>=2016
gen did=treatment_a*post
gen t=.
replace t=1 if year==2014
replace t=2 if year==2015
replace t=3 if year==2016
replace t=4 if year==2017
replace t=5 if year==2018
replace t=6 if year==2019
replace t=7 if year==2020
replace t=8 if year==2021

gen treat_t=treatment_a*t

gen lnrjgdp_a_t=lnrjgdp_a*t
gen lnrjgdp_a_t2=lnrjgdp_a*t*t
gen lnrjgdp_a_t3=lnrjgdp_a*t*t*t

gen czzjl_a_t=czzjl_a*t
gen czzjl_a_t2=czzjl_a*t*t
gen czzjl_a_t3=czzjl_a*t*t*t

gen lnybyszc_a_t=lnybyszc_a*t
gen lnybyszc_a_t2=lnybyszc_a*t*t
gen lnybyszc_a_t3=lnybyszc_a*t*t*t

gen lnzfxjjzc_a_t=lnzfxjjzc_a*t
gen lnzfxjjzc_a_t2=lnzfxjjzc_a*t*t
gen lnzfxjjzc_a_t3=lnzfxjjzc_a*t*t*t

gen lnrjgdp_a_p=lnrjgdp_a*post
gen czzjl_a_p=czzjl_a*post
gen lnybyszc_a_p=lnybyszc_a*post
gen lnzfxjjzc_a_p=lnzfxjjzc_a*post

xi:areg y1 did lnrjgdp_a_t lnrjgdp_a_t2 lnrjgdp_a_t3 czzjl_a_t czzjl_a_t2 czzjl_a_t3 lnybyszc_a_t lnybyszc_a_t2 lnybyszc_a_t3 lnzfxjjzc_a_t lnzfxjjzc_a_t2 lnzfxjjzc_a_t3 i.year,absorb(id) cluster(id)
outreg2 using 附表9.word,replace

xi:areg y1 did lnrjgdp_a_p czzjl_a_p lnybyszc_a_p lnzfxjjzc_a_p i.year,absorb(id) cluster(id)
outreg2 using 附表9.word,append

xi:areg y1 did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表9.word,append

*  Col. 4
clear
use "土地市场降温与地方政府债务风险-数据.dta"

gen ybyssr0=ybyssr
gen zyxsr0=zyxsr
gen zfxjjsr0=zfxjjsr
gen gyzbsr0=gyzbsr

replace ybyssr0=. if ybyssr<0
replace zyxsr0=. if zyxsr<0
replace zfxjjsr0=. if zfxjjsr<0
replace gyzbsr0=. if gyzbsr<0

replace ybyssr=0 if ybyssr==.
replace zyxsr=0 if zyxsr==.
replace zfxjjsr=0 if zfxjjsr==.
replace gyzbsr=0 if gyzbsr==.

replace landsr=. if landsr<0
replace zyxsr=. if zyxsr<0
replace gyzbsr=. if gyzbsr<0

gen treatment=.
replace treatment=landsr/(ybyssr+zyxsr+zfxjjsr+gyzbsr)
replace treatment=. if treatment>1

bys county(year):egen treatment_a=mean(treatment) if year<2018
bys county(year):replace treatment_a=treatment_a[1]

bys county(year):egen rjgdp_a=mean(rjgdp) if year<2018
bys county(year):replace rjgdp_a=rjgdp_a[1]
gen lnrjgdp_a=ln(1+rjgdp_a)

bys county(year):egen czzjl_a=mean(czzjl) if year<2018
bys county(year):replace czzjl_a=czzjl_a[1]
replace czzjl_a=czzjl_a/100

bys county(year):egen ybyszc_a=mean(ybyszc) if year<2018
bys county(year):replace ybyszc_a=ybyszc_a[1]
gen lnybyszc_a=ln(ybyszc_a+1)

replace zfxjjzc=. if zfxjjzc<0
bys county(year):egen zfxjjzc_a=mean(zfxjjzc) if year<2018
bys county(year):replace zfxjjzc_a=zfxjjzc_a[1]
gen lnzfxjjzc_a=ln(zfxjjzc_a+1)

gen did=treatment_a*post

gen post2017=(year==2017)
gen post2016=(year==2016)
gen did2017=treatment_a*post2017
gen did2016=treatment_a*post2016

xi:areg y1 did did2017 did2016 i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表9.word,append


****** 附图1 安慰剂检验 ******
forvalues i=1(1)500 {
use "土地市场降温与地方政府债务风险-数据.dta",clear

save reform_data,replace
use reform_data,clear
keep id year
drop if year==2014
save year_data,replace
use year_data,clear
bysort year: keep if _n==1
sample 1, count
keep year
save temp_year,replace

use reform_data,clear
merge m:1 year using temp_year
gen false_post=(_merge==3)
replace false_post=false_post[_n-1] if _merge==1
replace false_post=0 if false_post==.
drop _merge
save reform_data,replace

use reform_data,clear
duplicates drop id,force
keep id
save id_data,replace
use id_data,clear
gen false_treatment=runiform()
save temp_id,replace

use reform_data,clear
merge m:1 id using temp_id
drop _merge
save reform_data,replace
gen  false_did=false_post*false_treatment

bys county(year):egen rjgdp_a=mean(rjgdp) if false_post==0
bys county(year):replace rjgdp_a=rjgdp_a[1]
gen lnrjgdp_a=ln(1+rjgdp_a)

bys county(year):egen czzjl_a=mean(czzjl) if false_post==0
bys county(year):replace czzjl_a=czzjl_a[1]
replace czzjl_a=czzjl_a/100

bys county(year):egen ybyszc_a=mean(ybyszc) if false_post==0
bys county(year):replace ybyszc_a=ybyszc_a[1]
gen lnybyszc_a=ln(ybyszc_a+1)

replace zfxjjzc=. if zfxjjzc<0
bys county(year):egen zfxjjzc_a=mean(zfxjjzc) if false_post==0
bys county(year):replace zfxjjzc_a=zfxjjzc_a[1]
gen lnzfxjjzc_a=ln(zfxjjzc_a+1)

xi:areg y1 false_did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)

parmest,format (estimate min95 max95 %8.2f p %8.3f) saving("temp.dta", replace)
use "temp.dta", clear
keep if parm=="false_did"
append using "reform_simulations.dta"
save "reform_simulations.dta", replace
}
erase "temp.dta"
use "reform_simulations.dta", clear
drop if estimate==.
save "reform_simulations.dta", replace

dpplot estimate,xline(1.194) xtitle("Reform effect estimate") saving("PDF_Reform_dpplot.gph", replace)


****** 表2 土地市场降温与地方政府债务风险的具体表现 ******
clear
use "土地市场降温与地方政府债务风险-数据.dta"

gen ybyssr0=ybyssr
gen zyxsr0=zyxsr
gen zfxjjsr0=zfxjjsr
gen gyzbsr0=gyzbsr

replace ybyssr0=. if ybyssr<0
replace zyxsr0=. if zyxsr<0
replace zfxjjsr0=. if zfxjjsr<0
replace gyzbsr0=. if gyzbsr<0

replace ybyssr=0 if ybyssr==.
replace zyxsr=0 if zyxsr==.
replace zfxjjsr=0 if zfxjjsr==.
replace gyzbsr=0 if gyzbsr==.

replace landsr=. if landsr<0
replace zyxsr=. if zyxsr<0
replace gyzbsr=. if gyzbsr<0

gen treatment=.
replace treatment=landsr/(ybyssr+zyxsr+zfxjjsr+gyzbsr)
replace treatment=. if treatment>1

bys county(year):egen treatment_a=mean(treatment) if year<2018
bys county(year):replace treatment_a=treatment_a[1]

bys county(year):egen rjgdp_a=mean(rjgdp) if year<2018
bys county(year):replace rjgdp_a=rjgdp_a[1]
gen lnrjgdp_a=ln(1+rjgdp_a)

bys county(year):egen czzjl_a=mean(czzjl) if year<2018
bys county(year):replace czzjl_a=czzjl_a[1]
replace czzjl_a=czzjl_a/100

bys county(year):egen ybyszc_a=mean(ybyszc) if year<2018
bys county(year):replace ybyszc_a=ybyszc_a[1]
gen lnybyszc_a=ln(ybyszc_a+1)

replace zfxjjzc=. if zfxjjzc<0
bys county(year):egen zfxjjzc_a=mean(zfxjjzc) if year<2018
bys county(year):replace zfxjjzc_a=zfxjjzc_a[1]
gen lnzfxjjzc_a=ln(zfxjjzc_a+1)

gen did=treatment_a*post

*票面利率
xi:areg intrrate_a did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id)
outreg2 using 表2.word,replace

*专项债限额空间
replace zxzxe=. if (zxzxe!=.&zxzye==.)
replace zxzye=. if (zxzxe==.&zxzye!=.)
replace zxzye=. if (zxzxe==0&zxzye!=0)
replace zxzxe=. if (zxzxe==0&zxzye!=0)
replace zxzye=. if (zxzye>zxzxe)
replace zxzxe=. if (zxzye>zxzxe)
gen space=zxzxe-zxzye
gen lnspace=ln(space+1)
xi:areg lnspace did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表2.word,append

*固定资产投资
gen lngdzctz=ln(gdzctz+1)
xi:areg lngdzctz did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表2.word,append

*GDP增长率
xi:areg gdprate did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表2.word,append


****** 表3 机制分析 ******
gen lntdcj=ln(tdcj+1)
gen lntdlp=ln(tdlp+1)
gen lntdtc=ln(tdtc+1)
gen lnlandsr=ln(landsr+1)

xi:areg lntdcj did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表3.word,replace

xi:areg lntdlp did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表3.word,append

xi:areg lntdtc did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表3.word,append

xi:areg lnlandsr did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表3.word,append


****** 附表10 补充分析：土地市场降温对地方政府财力的影响 ******
gen lnybyssr=ln(ybyssr0+1)
gen lnzyxsr=ln(zyxsr0+1)
gen lnzfxjjsr=ln(zfxjjsr0+1)
gen lngyzbsr=ln(gyzbsr0+1)
gen lntaxsr=ln(taxsr+1)
gen lnzsr=ln(ybyssr0+zyxsr0+zfxjjsr0+gyzbsr0+1)

xi:areg lnybyssr did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表10.word,replace
xi:areg lnzyxsr did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表10.word,append
xi:areg lnzfxjjsr did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表10.word,append
xi:areg lngyzbsr did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表10.word,append
xi:areg lnzsr did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表10.word,append


****** 附表11 补充分析：土地市场降温对地方政府债务规模的影响 ******
clear
use "土地市场降温与地方政府债务风险-数据.dta"

gen ybyssr0=ybyssr
gen zyxsr0=zyxsr
gen zfxjjsr0=zfxjjsr
gen gyzbsr0=gyzbsr

replace ybyssr0=. if ybyssr<0
replace zyxsr0=. if zyxsr<0
replace zfxjjsr0=. if zfxjjsr<0
replace gyzbsr0=. if gyzbsr<0

replace ybyssr=0 if ybyssr==.
replace zyxsr=0 if zyxsr==.
replace zfxjjsr=0 if zfxjjsr==.
replace gyzbsr=0 if gyzbsr==.

replace landsr=. if landsr<0
replace zyxsr=. if zyxsr<0
replace gyzbsr=. if gyzbsr<0

gen treatment=.
replace treatment=landsr/(ybyssr+zyxsr+zfxjjsr+gyzbsr)
replace treatment=. if treatment>1

bys county(year):egen treatment_a=mean(treatment) if year<2018
bys county(year):replace treatment_a=treatment_a[1]

bys county(year):egen rjgdp_a=mean(rjgdp) if year<2018
bys county(year):replace rjgdp_a=rjgdp_a[1]
gen lnrjgdp_a=ln(1+rjgdp_a)

bys county(year):egen czzjl_a=mean(czzjl) if year<2018
bys county(year):replace czzjl_a=czzjl_a[1]
replace czzjl_a=czzjl_a/100

bys county(year):egen ybyszc_a=mean(ybyszc) if year<2018
bys county(year):replace ybyszc_a=ybyszc_a[1]
gen lnybyszc_a=ln(ybyszc_a+1)

replace zfxjjzc=. if zfxjjzc<0
bys county(year):egen zfxjjzc_a=mean(zfxjjzc) if year<2018
bys county(year):replace zfxjjzc_a=zfxjjzc_a[1]
gen lnzfxjjzc_a=ln(zfxjjzc_a+1)

gen did=treatment_a*post

gen lndfzfzwye=ln(dfzfzwye+1)
gen lnybzye=ln(ybzye+1)
gen lnzxzye=ln(zxzye+1)
gen lnrzptyxzw=ln(rzptyxzw+1)
gen lnzwze=ln(dfzfzwye+rzptyxzw+1)

xi:areg lndfzfzwye did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表11.word,replace
xi:areg lnybzye did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表11.word,append
xi:areg lnzxzye did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表11.word,append
xi:areg lnrzptyxzw did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表11.word,append
xi:areg lnzwze did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表11.word,append


****** 表4 地方政府如何应对土地市场降温带来的财政压力 ******
*债券平均期限
xi:areg term_a did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表4.word,replace

*再融资债券占比
xi:areg zrzzqzb did i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表4.word,append


****** 表5 异质性分析：城投债与专项债存量 ******
gen lnybyssr=ln(ybyssr0+1)
gen lnzyxsr=ln(zyxsr0+1)
gen lnzfxjjsr=ln(zfxjjsr0+1)
gen lngyzbsr=ln(gyzbsr0+1)
gen lntaxsr=ln(taxsr+1)
gen lnzsr=ln(ybyssr0+zyxsr0+zfxjjsr0+gyzbsr0+1)

bys county(year):egen rzptyxzw_a=mean(rzptyxzw) if year<2018
bys county(year):replace rzptyxzw_a=rzptyxzw_a[1]
gen lnrzptyxzw_a=ln(rzptyxzw_a+1)
gen did_lnrzptyxzw=did*lnrzptyxzw_a
gen treat_lnrzptyxzw=treatment_a*lnrzptyxzw_a
gen post_lnrzptyxzw=post*lnrzptyxzw_a

bys county(year):egen zxzye_a=mean(zxzye) if year<2018
bys county(year):replace zxzye_a=zxzye_a[1]
gen lnzxzye_a=ln(zxzye_a+1)
gen did_lnzxzye=did*lnzxzye_a
gen treat_lnzxzye=treatment_a*lnzxzye_a
gen post_lnzxzye=post*lnzxzye_a

xi:areg y1 did_lnrzptyxzw did treat_lnrzptyxzw post_lnrzptyxzw i.year*lnrzptyxzw_a i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表5.word,replace
xi:areg y1 did_lnzxzye did treat_lnzxzye post_lnzxzye i.year*lnzxzye_a i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表5.word,append
xi:areg lnzwze did_lnrzptyxzw did treat_lnrzptyxzw post_lnrzptyxzw i.year*lnrzptyxzw i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表5.word,append
xi:areg lnzsr did_lnrzptyxzw did treat_lnrzptyxzw post_lnrzptyxzw i.year*lnrzptyxzw i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表5.word,append
xi:areg lndfzfzwye did_lnrzptyxzw did treat_lnrzptyxzw post_lnrzptyxzw i.year*lnrzptyxzw i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表5.word,append
xi:areg lnzyxsr did_lnrzptyxzw did treat_lnrzptyxzw post_lnrzptyxzw i.year*lnrzptyxzw i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表5.word,append
xi:areg lnzfxjjsr did_lnrzptyxzw did treat_lnrzptyxzw post_lnrzptyxzw i.year*lnrzptyxzw i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表5.word,append


****** 表6 异质性分析：地方政府财政收入质量 ******
gen taxzb=.
replace taxzb=taxsr/(ybyssr+zyxsr+zfxjjsr+gyzbsr)
replace taxzb=. if taxzb>1

bys county(year):egen taxzb_a=mean(taxzb) if year<2018
bys county(year):replace taxzb_a=taxzb_a[1]

gen did_taxzba=did*taxzb_a
gen treat_taxzba=treatment_a*taxzb_a
gen post_taxzba=post*taxzb_a

xi:areg y1 did_taxzba did treat_taxzba post_taxzba i.year*taxzb_a i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表6.word,replace
xi:areg lnzwze did_taxzba did treat_taxzba post_taxzba i.year*taxzb_a i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表6.word,append
xi:areg lnzsr did_taxzba did treat_taxzba post_taxzba i.year*taxzb_a i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表6.word,append
xi:areg lnybyssr did_taxzba did treat_taxzba post_taxzba i.year*taxzb_a i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表6.word,append
xi:areg lnzyxsr did_taxzba did treat_taxzba post_taxzba i.year*taxzb_a i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表6.word,append
xi:areg lnzfxjjsr did_taxzba did treat_taxzba post_taxzba i.year*taxzb_a i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表6.word,append
xi:areg lngyzbsr did_taxzba did treat_taxzba post_taxzba i.year*taxzb_a i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表6.word,append
xi:areg lntaxsr did_taxzba did treat_taxzba post_taxzba i.year*taxzb_a i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 表6.word,append


****** 附表12 异质性分析：地理区域 ******
gen east=0
replace east=1 if province=="北京市"|province=="天津市"|province=="河北省"|province=="上海市"|province=="江苏省"|province=="浙江省"|province=="福建省"|province=="山东省"|province=="广东省"|province=="海南省"

gen central=0
replace central=1 if province=="山西省"|province=="安徽省"|province=="江西省"|province=="河南省"|province=="湖北省"|province=="湖南省"

gen west=0
replace west=1 if province=="内蒙古自治区"|province=="广西壮族自治区"|province=="重庆市"|province=="四川省"|province=="贵州省"|province=="云南省"|province=="西藏自治区"|province=="陕西省"|province=="甘肃省"|province=="青海省"|province=="宁夏回族自治区"|province=="新疆维吾尔自治区"

gen northeast=0
replace northeast=1 if province=="辽宁省"|province=="吉林省"|province=="黑龙江省"

gen did_east=did*east
gen treat_east=treatment_a*east
gen post_east=post*east

gen did_central=did*central
gen treat_central=treatment_a*central
gen post_central=post*central

gen did_west=did*west
gen treat_west=treatment_a*west
gen post_west=post*west

gen did_northeast=did*northeast
gen treat_northeast=treatment_a*northeast
gen post_northeast=post*northeast

xi:areg y1 did_east did treat_east post_east i.year*east i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表12.word,replace
xi:areg y1 did_central did treat_central post_central i.year*central i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表12.word,append
xi:areg y1 did_west did treat_west post_west i.year*west i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表12.word,append
xi:areg y1 did_northeast did treat_northeast post_northeast i.year*northeast i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表12.word,append


****** 附表13 异质性分析：地理区域的进一步解释 ******
*Panel A
xi:areg lnzwze did_east did treat_east post_east i.year*east i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表13A.word,replace
xi:areg lnzwze did_central did treat_central post_central i.year*central i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表13A.word,append
xi:areg lnzwze did_west did treat_west post_west i.year*west i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表13A.word,append
xi:areg lnzwze did_northeast did treat_northeast post_northeast i.year*northeast i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表13A.word,append
xi:areg lnzsr did_east did treat_east post_east i.year*east i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表13A.word,append
xi:areg lnzsr did_central did treat_central post_central i.year*central i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表13A.word,append
xi:areg lnzsr did_west did treat_west post_west i.year*west i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表13A.word,append
xi:areg lnzsr did_northeast did treat_northeast post_northeast i.year*northeast i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表13A.word,append

*Panel B
xi:areg lndfzfzwye did_central did treat_central post_central i.year*central i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表13B.word,replace
xi:areg lnybzye did_central did treat_central post_central i.year*central i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表13B.word,append
xi:areg lnzxzye did_central did treat_central post_central i.year*central i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表13B.word,append
xi:areg lnrzptyxzw did_central did treat_central post_central i.year*central i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表13B.word,append
xi:areg lnybyssr did_central did treat_central post_central i.year*central i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表13B.word,append
xi:areg lnzyxsr did_central did treat_central post_central i.year*central i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表13B.word,append
xi:areg lnzfxjjsr did_central did treat_central post_central i.year*central i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表13B.word,append
xi:areg lngyzbsr did_central did treat_central post_central i.year*central i.year*lnrjgdp_a i.year*czzjl_a i.year*lnybyszc_a i.year*lnzfxjjzc_a i.year,absorb(id) cluster(id)
outreg2 using 附表13B.word,append
