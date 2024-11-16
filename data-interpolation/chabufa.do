*display* 
webuse nlsw88 // open a dataset about individual information调用数据库
clear
use "E:\桌面\实证方法\webuse.dta



*删除缺失
nmissing //查看哪些变量有缺失值
sum
drop if missing(grade,indus,occup,union,hours,tenure) //删除缺失样本量，变量无缺失部分依然保留
sum //所有变量的样本量完全一致，当样本量本身比较少时，不太可取
*上述方法更为简单的命令
clear
use "E:\桌面\实证方法\webuse.dta
dropmiss,any obs



*单一插补法

*平均值插补,连续性变量才可以
clear
use "E:\桌面\实证方法\webuse.dta"
summarize grade
replace grade = r(mean) if grade==.
sum union
replace union = r(mean) if union==.
sum union


*回归插补，先用reg求出回归分析的系数值，再用list找到缺失系数的所在行，使用stata内置 _b进行计算
clear
use "E:\桌面\实证方法\webuse.dta
sum
reg wage grade hours
list wage hours if grade==. //查看grade有缺失的序号
replace grade =(wage[496]-_b[_cons]-_b[hours]*hours[496])/_b[grade] if (wage == wage[496]&grade ==.&hours==hours[496])
replace grade =(wage[2210]-_b[_cons]-_b[hours]* hours[2210])/_b[grade] if (wage == wage[2210]&grade==.&hours==hours[2210])
sum grade
clear


*向前向后填补
*向下填补，用上一个非缺失值填补下一个缺失值
use "E:\桌面\实证方法\webuse.dta
gen id=_n
list grade if grade==.
list grade if inlist(id,495,496,2209,2210,2211)
replace grade = grade[_n-1] if mi(grade)
*查看不连续观测值
list grade if inlist(id,495,496,497,2209,2210,2211)
sum grade 
*carryforward 相当于上面 
use "E:\桌面\实证方法\webuse.dta
gen id =_n 
list grade if inlist(id,495,496,497,2209,2210,2211)
carryforward grade,replace 
*查看不连续观测值
list grade if inlist (id ,495,496,497,2209,2210,2211)

*﹣向上填补﹣用下一个非缺失值填补上一个缺失值
use "E:\桌面\实证方法\webuse.dta
 gen id =_n 
 list grade if inlist(id,495,496,497,2209,2210,2211) 
 replace grade = grade[n+1] if mi(grade)


 
*多重插补法
 
*识别缺失数据
misstable summarize, gen (m_) 
//生成数据集里缺失值摘要统计表，生成一个新变量，变量的值为每个变量缺失值的个数，＋一个m_前缀
tab m_* 
//这是tabulate命令的简写，用于生成一个列联表。m_*：这是一个通配符，表示所有以“m_”开头的变量。这行代码的目的是生成一个关于新生成的“m_”变量的列联表，显示每个变量的缺失值个数。
misstable pattern
 //生成一个关于数据集中缺失值模式的表格，显示哪些变量有缺失值及其组合。
misstable pattern, freq 
//生成一个关于数据集中缺失值模式的表格，并显示每个模式的频率。


*多重插补法操作
*准备工作
mi set flong/mlong/wide //为 MI 声明数据结构，flong和mlong是类似的，表示插补值存储在一个扩展的数据结构中，而wide表示插补值存储在不同的变量中。
mi register imputed grade  //声明要插补的变量

*插补阶段
mi impute regress/logit        //单一插补法，regress表示使用线性回归进行插补，logit表示使用逻辑回归进行插补。
mi impute monotone/mvn/chained  //多重插补法
                             *monotone：表示使用单调插补法，适用于数据中缺失值的顺序是单调的情况。
                             *mvn：表示使用多变量正态分布进行插补。
                             *chained：表示使用链式方程进行插补。

*分析与汇集阶段
mi estimate  //分析并整合结果，分析插补后的数据，并整合插补结果。它会生成一个综合的统计估计，考虑了插补的不确定性。




*多重插补法MVN

*定义回归模型
. regress varlist //设定模型为线性回归
*查看数据缺失数量和缺失模式
. mi set flong/mlong/wide //为 MI 声明数据结构
. mi misstable summarize varlist //查看缺失数据
*指定完全变量和要插补的变量
. mi register imputed varlist  //声明要插补的变量
. mi register regular varlist  //声明不含缺失值的变量
*选择 MVN 方法进行多重插补
. mi impute mvn ivars [= indepvars] [if] [, impute_options options]  //声明使用 MVN 方法
                //ivars为需要插补的变量列表
				//[]均代表可选项
				//indepvars表示插补模型中的独立变量（预测变量）。如果省略，Stata 会自动选择所有非缺失变量作为预测变量。
				//[if]: 这是条件语句，用于指定插补操作只应用于满足特定条件的观测值
				//[, impute_options options]: 这是一些可选的选项，例如 add 指定插补的次数，burnin 指定预烧次数等。
*将回归模型拟合到插补数据中
. mi estimate: regress varlist    //以线性回归模型为例，用于在进行多重插补后估计模型。对插补的数据集进行回归分析，并整合多个插补结果，以提供更加稳健的估计。
. mi estimate, vartable nocitable //显示插补的方差信息，对插补后的回归结果进行进一步的输出和处理。nocitable代表不生成表格，以文本形式输出



*对应具体模型的命令
clear
use "E:\桌面\实证方法\webuse.dta
misstable summarize
global x "grade hours occupation industry tenure union"
mi set wide  //为 MI 声明数据结构
mi misstable summarize $x
reg wage $x
mi register imputed $x
mi register regular wage
mi impute mvn $x = wage, add(20) //插补20次



*MICE多重插补法

*定义回归模型
. regress varlist //设定模型为线性回归
*查看数据缺失数量和缺失模式
. mi set flong/mlong/wide //为 MI 声明数据结构
. mi misstable summarize varlist //查看缺失数据 
*指定完全变量和要插补的变量
. mi register imputed varlist  //声明要插补的变量
. mi register regular varlist  //声明不含缺失值的变量
*选择 MICE 方法进行多重插补
. mi impute chained (ologit) var2 (regress) var1 [= indepvars] ///
           [if] [weight] [, impute_options options] //为不同种类缺失变量指定不同的回归模型
*mi impute chained (ologit) var2 (regress) var1 [= indepvars] [if] [weight] [, impute_options options]
                   //(ologit)对var2有序逻辑回归插补
				   //(regress) var1 对变量1线性回归插补
				   //[= indepvars]: 这是可选的，表示插补模型中的独立变量（预测变量）。如果省略，Stata 会自动选择所有非缺失变量作为预测变量。
                   //[if]: 这是条件语句，用于指定插补操作只应用于满足特定条件的观测值。
                   //[weight]: 这是可选的权重变量。
                   //[, impute_options options]: 这是一些可选的选项，例如 add 指定插补的次数，burnin 指定预烧次数等
*将回归模型拟合到插补数据中
. mi estimate: regress varlist    //以线性回归模型为例
. mi estimate, vartable nocitable //显示插补的方差信息




*对应具体模型的命令
clear
use "E:\桌面\实证方法\webuse.dta
misstable summarize
global x "grade hours occupation industry tenure union"
mi set wide 
mi misstable summarize $x
reg wage $x
mi register imputed $x
mi register regular wage
mi impute chained (ologit) union (regress) grade =wage, add(20) //ologit有序逻辑回归，因变量为有序离散变量的回归估计
mi impute chained (ologit) union occupation industry (regress) grade hours tenure =wage, add(20) //这行代码跑不动，可能没有写规范
mi estimate: regress wage $x //对插补数据集执行线性回归。
mi estimate, vartable nocitable //输出插补后的回归估计结果。
misstable summarize



*模型检验，残差分析
mi estimate: regress wage $x
mi xeq: predict wage_hat 
mi xeq: gen residuals = wage - wage_hat  // 计算残差，并保留在新变量中,不太清楚这里为什么用不了
mi xeq: twoway scatter residuals grade, title("Residuals vs. Grade") //绘制残差图
mi xeq: summarize residuals //查看每个插补后的数据集的残差统计信息



mi estimate: regress wage $x  // 进行回归分析，这有点问题
mi xeq: {
    predict wage_hat,xb // 生成预测值
    gen residuals = wage - wage_hat  // 计算残差
}
mi xeq: twoway scatter residuals grade, title("Residuals vs. Grade")  // 绘制残差图
mi xeq: summarize residuals  // 汇总残差信息




*非正态分布
*画直方图
histogram grade, norm frequency
histogram wage, norm frequency //工资分布非正态，但是它没有缺失值
histogram tenure, norm frequency

mi impute pmm ivar [indepvars] [if] [weight] , knn(#) [impute_options options]//预测均值匹配方法进行插补，其中 MICE 方法可以使用 PMM
             *ivar：指的是要进行插补的变量。例如，这里是 log_tenure。
             //indepvars：自变量，即预测插补值时使用的其他变量。最好选择与 ivar 高度相关的变量以提升插补精度。
             //if 和 weight：可选的条件语句和权重。条件语句用来选择特定的观测值进行插补，权重用于加权插补。
             //knn(#)：表示 k 值，即从最相似的 k 个观测值中选择一个真实值作为插补值。k 越大，选择的观测值范围越宽泛；k 越小，则越接近真实观测值。
             //impute_options options：包括一些其他选项，如 add 指定生成的插补数据集数目，replace 表示覆盖现有的数据。
help mi impute chained

global x "grade hours occupation industry tenure union"
mi set wide 
mi misstable summarize $x
reg wage $x
mi register imputed $x
mi register regular wage
mi impute pmm tenure = grade hours, knn(10) add(10) replace
histogram tenure, norm frequency


*非线性相关，明天生成一个二次变量，尝试一下

clear
use "E:\桌面\实证方法\webuse.dta
misstable summarize
gen hour2=hour^2
global x "grade hours hour2 occupation industry tenure union"
mi set wide  //为 MI 声明数据结构
mi misstable summarize $x
reg wage $x
mi register imputed $x
mi register regular wage
mi impute pmm hour hours = wage, knn(10) add(10) replace //可以用，但不知道是否正确
mi impute chained (pmm, knn(5) include((hour2))) hour (pmm, knn(5)) $x = wage, add(10)
mi impute chained (pmm, knn(5)) hour (pmm, knn(5)) hour2 = wage $x, add(10) replace









*交互项
*MICE 方法交互项
*MICE 方法交互项
*mi impute chained (logit, include ((sbp*age))) sex (regress, include ((sbp*sex))) age = sbp



clear
use "E:\桌面\实证方法\webuse.dta
misstable summarize
gen hoursage = hours*age
global x "grade hours occupation industry tenure union"
mi set wide  //为 MI 声明数据结构
mi misstable summarize $x
reg wage $x
mi register imputed $x
mi register regular wage age race
mi impute chained (logit, include ((hours*union))) union(regress, include (( hours*wage))) hours = wage, add(10)
*受什么因素影响，需要的是有缺失的变量

twoway (scatter wage hours)(qfit wage hours if race==2)(qfit wage hours if race==1) //画散点图，hours对wage的影响是否受到race影响




*SMC FCS方法
*SMC FCS 方法由 MICE 方法演变而来，其核心思想是强制使插补模型与分析模型兼容。当样本量 n 较小时，SMC FCS 方法能快速得到结果，但当 n 扩大时，其速度逐渐下降。
*安装 SMC FCS 命令
ssc install smcfcs
*针对非线性的命令
smcfcs regress y x xsq, regress(x) passive(xsq = x^2) m(10)
*针对交互项的命令
smcfcs regress sbp age sex agesex, regress(age) logit(sex) passive(agesex = age*sex) m(10)



clear
use "E:\桌面\实证方法\webuse.dta
misstable summarize
gen hours2=hours^2
gen hoursage = hours*age
gen hoursunion = hours*union
global x "grade hours occupation industry tenure union"
mi set wide  //为 MI 声明数据结构
mi misstable summarize $x
reg wage $x
mi register imputed $x
mi register regular wage age race
*针对非线性的命令
smcfcs regress wage hours hours2, regress(hours) passive(hours2=hours^2) m(10)
smcfcs regress wage $x hours2, regress($x) passive(hours2=hours^2) m(10) //应该可以用
smcfcs regress wage grade hours occupation industry tenure union hours2, regress(grade hours occupation industry tenure union) passive(hours2=hours^2) m(10)
*针对交互项的命令
smcfcs regress wage hours union hoursunion, regress(hours) logit(union) passive(hoursunion = hours*union) m(10)



*面板数据插值法 （两个平均插值法）
*方法一
clear
input ID    year    var1    var2    var3
      1     2006     34      45      65
      1     2007     45      43      41
      1     2007      3      56      59
      1     2008     39      54      76
      1     2009     41      57      68
end
save "data00.dta", replace

list, clean 

use "data00.dta", clear
*-删除重复值  (数据有误)
. drop if year == 2007
. tsset ID year   
. tsfill  // tsfill 填充年份，让数据变成平行面板
. list, clean 

*-填充缺失值：
forv i = 1/3 {
    tssmooth ma v`i' = var`i' , w(1, 0, 1)
    replace var`i' = v`i' if var`i' == .
}
drop v?
list, clean

*方法2
clear
input id year var1 var2 var3
1 2006 34 45 65
1 2007 45 43 41
1 2007 3 56 59
1 2008 39 54 76
1 2009 41 57 68
end

*使用 duplicates tag 标记重复的观察值
. duplicates tag id year, gen(mistake)  //标记重复的观察值，并使用gen() 选项产生新变量用以记录标记情况；
. list
. bysort id year: keep if (_n == 1)  //删除重复的观察值 (其实，这两步可以合为一步)；
  foreach v of varlist var1 var2 var3 {  
     replace `v' =  (`v'[_n-1] + `v'[_n+1])/2 if mistake 
  }
