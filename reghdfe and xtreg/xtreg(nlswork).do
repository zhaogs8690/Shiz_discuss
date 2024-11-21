webuse nlswork, clear
 use "E:\桌面\实证方法\raw data\webuse nlswork.dta",clear

xtset idcode year //告诉stata我们使用的是面板数据
xtdes 

**change to balanced panel**
bys idcode: gen n=_N //按变量idcode进行分组，然后生成一个新变量n，其值为每组的观测数。
keep if n==15 
xtset idcode year //告诉stata我们使用的是面板数据


xtreg ln_wage hours age race grade, re r //随机效应
xtreg ln_wage hours age race grade, fe r  //固定效应


global xlist0 hours age race grade  //定义一个全局宏变量xlist0
global xlist1 hours age race grade i.year //加上时间虚拟变量i.year。

sum hours age race grade
sum $xlist0

xtreg ln_wage $xlist1  , fe r 
est store m1

xtreg ln_wage $xlist0, r
est store m2

xtreg tenure $xlist1, fe r
est store m3

xtreg tenure $xlist0, r
est store m4


esttab m1 m2 m3 m4 using xtreg_nlswork.rtf

local mlist "m1 m2 m3 m4" //定义一个局部宏变量mlist，包含m1、m2、m3和m4，代表要输出的四个回归结果。
esttab `mlist' using xtreg.rtf


reg2docx m1 m2 m3 m4 using xtreg_nlswork.docx, append keep(hours) addfe("控制变量 = Yes" "个体固定效应 = Yes" "时间固定效应 = Yes") b(%6.4f) t(%6.4f) ///
scalars(N r2_a(%6.4f)) noconstant varlabel                       ///
mtitles ("Ln(wage)" "Ln(wage)" "Tenure" "Tenure")      ///
title("Table 1: Baseline regression--TWFE analysis")






reg2docx `mlist' using stata_teaching.docx, append newsection ///
    keep(hours) ///
    addfe("控制变量 = Yes" "个体固定效应 = Yes" "时间固定效应 = Yes") ///
    b(%6.4f) t(%6.4f) ///
    scalars(N r2_a(%6.4f)) ///
    noconstant ///
    varlabel ///
    mtitles("Ln(wage)" "Ln(wage)" "Tenure" "Tenure") ///
    title("Table 1: Baseline regression--TWFE analysis")



esttab `mlist' using xtreg.rtf, replace ///
    indicate("Control = Yes" "Individual FE = Yes" "Time FE = Yes") ///
    b(%9.3f) t(%9.3f) ///
    scalars(N r2_a) ///
    noconstant