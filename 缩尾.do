
**离群值对回归结果的影响

  sysuse "auto.dta", clear
  histogram price
  count if price>13000

  *汽车大于13000美金的四辆车对回归分析产生了什么影响
  
  reg price weight length foreign
  est store r1
  reg price weight length foreign if price<13000
  est store r2
	   
  esttab r1 r2, mtitle("with" "without") nogap


 *-结论：虽然离群值只有4个，但对回归结果的影响却很大

*----------------------------------------------------------


** 离群值的处理

  *1 对数转换
	
   *-原理
  display %6.2f ln(10) 
  dis %6.2f ln(100)
  dis %6.2f ln(1000)
  dis %6.2f ln(10000)  //  %6.2f 意味着显示一个浮点数，总共占用6个字符宽度，其中小数点后有2位数字
  
  
  sysuse "nlsw88.dta", clear
  histogram wage, normal     // normal选项是在直方图上绘制出基于工资数据的正态分布曲线   
  
  gen ln_wage = ln(wage)	 
  sum wage ln_wage,detail

  histogram ln_wage, normal 
  
  histogram wage,  ylabel(, angle(0)) xtitle("wage") name(fig1, replace)
  histogram ln_wage, ylabel(, angle(0)) xtitle("ln_wage") name(fig2, replace)
graph combine fig1 fig2
  
  *-Note: 使用对数转换后，系数的含义会发生变化，请慎重解释
	    	    
*----------------------------------------------------------		
  
  *2 缩尾处理
	
 /*  原理:   默认以第1和第99百分位数值为临界点，小于第一百分位数的都统一替换成第1百分位数的值，大于第99百分位数值都替换成第99百分位数值  
  		  
	对哪些变量进行缩尾:
    对回归模型中的非虚拟变量进行缩尾，并且要按照一个标准进行缩尾*/
	
     sysuse "nlsw88.dta", clear
     histogram wage  
   
   *-winsor/winsor2 命令
   
    sysuse "nlsw88.dta", clear
    winsor wage, gen(wage_w2) p(0.025)
  // Stata官方提供的命令
   
   ssc install winsor2,replace
  
  sysuse nlsw88.dta, clear
   winsor2 wage, cut(2.5 97.5)           

  // 连玉君老师编写，支持对多个变量同时进行缩尾处理 
   
    
	*-图示
     twoway (histogram wage,color(green))      ///
            (histogram wage_w,color(yellow)), ///
            legend(label(1 "wage") label(2 "wage_winsor2")) 
  
  
     *-Note: [1] cuts(1 99)选项可以省略，这是默认值
     *       [2] 默认情况下，winsor2 自动加后缀_w生成新变量 wage_w。如果希望改变新变量的后缀，可以在 winsor2 命令行后面加 suffix()，括号内填写新变量名称所要增加的后缀。
	  
	  winsor2 wage,cuts(1 99) suffix(_tr)

	  **如果希望缩尾后直接替换掉旧变量，而不生成新变量，则加上 replace：
     
	 sysuse nlsw88.dta, clear
     winsor2 wage, cut(1 99) replace 
	 // 这是 winsor2 与 winsor 的主要区别
	 
	 
   
*-----------------
    
	*-单边缩尾  
	*winsor对最高值做缩尾处理，只需在命令行后面加上 highonly，反之则加上 lowonly。
     winsor wage, gen(wage_h) p(0.01) highonly
	 

      winsor2 wage, cut(1 0)       // 仅对左侧缩尾
	 
	  winsor2 wage, cut(0 99)      // 仅对右侧缩尾 
	 
		
*-------------------------
	
	*多变量批量缩尾
	
	*使用 Stata 的官方命令 winsor：利用循环语句
	
	sysuse nlsw88.dta, clear
foreach v of varlist wage hours race{
    winsor `v', gen(`v'_wi) p(0.025)
}
	
	
	*winsor2
	winsor2 wage hours race, cut(2.5, 97.5)
	
   
*-------------------------------
  **分组缩尾与添加标签
  *winsor2还有一个功能，就是分组缩尾，即将变量在分组内部分别进行缩尾。 例如，对变量 wage 进行分行业缩尾处理：
   
   sysuse nlsw88.dta, clear
   winsor2 wage hours race, cut(2.5, 97.5)  by(ind)
     
   *如果希望缩尾或截尾后的变量自带"缩尾"或"截尾"标签，则在命令行后面加上 label： 
	 	 
sysuse nlsw88.dta, clear
winsor2 wage hours race, cut(2.5, 97.5)  by(ind) label
 
 /// 生成的新变量则都会自带标签，例如 wage_w 的标签是"hourly wage-Winsor(p00,p97.5)"。
	 
	 
	  
*---------------------------------------------------------- 
	 
	 
  *3 删除 （截尾处理）
	 
	 *如果要进行截尾处理，只需在命令后面加上 trim
	 
    sysuse nlsw88.dta, clear
    winsor2 wage, cut(2.5 97.5) trim 
	
	///所有位于 wage最低2.5%和最高2.5%的观测值都被直接删除。  wage主要是右偏，因此我们可以对极大值进行截尾，而极小值则不截尾。截尾之后，默认生成新变量 wage_tr。

	sysuse nlsw88.dta, clear
    winsor2 wage, cut(0 97.5) trim //左端不截尾
    
	histogram wage,  ylabel(, angle(0)) xtitle("wage") name(fig1, replace)
    histogram wage_tr,  ylabel(, angle(0)) xtitle("wage_tr") name(fig2, replace)
    graph combine fig1 fig2
 
 
 *对比截尾前后回归系数的差异：
  reg wage hours
  eststo e1
  reg wage_tr hours
  eststo e2
  esttab e1 e2
 
 /// 对 wage 截尾之后，Hours 对 wage 的回归系数有所下降，表明可能工资异常高的那一小部分人，每小时能赚的工资很高，因而把这最高的 2.5% 的样本删掉之后，剩下的样本每多工作一个小时所获取的工资就相对较少了。
 
 
 
*-------------------
 *对比一下缩尾和截尾的差别：
 
 sysuse nlsw88.dta, clear

 winsor2 wage, cut(0 97.5) trim //仅对右侧截尾

 winsor2 wage, cut(0 97.5)      //仅对右侧缩尾

 histogram wage, ylabel(, angle(0)) xtitle("wage") name(fig1, replace)

 histogram wage_tr, ylabel(, angle(0)) xtitle("wage_tr") name(fig2, replace)

 histogram wage_w, ylabel(, angle(0)) xtitle("wage_w") name(fig3, replace)

 graph combine fig1 fig2 fig3
 
 /// 第一幅图是原变量 wage 的分布 第二幅图是截尾后的 wage 的分布 第三幅图是缩尾后的 wage 的分布,可以看出，右侧截尾是把右侧最高的 2.5% 的值直接截掉，而缩尾则是把这些值替换成 97.5% 分位数的值，因而在最右端多出了一个较长的柱体。
 
 
  /**总结
      在公司金融领域，缩尾被较为广泛地运用。而在时间序列分析中，则更适宜采用插值法。*/
 
 
  *4 插值
  
  *应用原有数据信息对离群值赋予一个相对合理的新值的方法。
    
  *ipolate 命令可以进行线性插值操作。 impute 命令可以进行回归赋值操作。
   
   
*注意声明为面板数据！！！
   xtset id year

  **1.简单插值
  *（1）内插法，插补中间数值
    
	ipolate × year, gen（×1）
   
   /// 创建 x1，其中包含 x 在 year 上的线性插值，用于 x的缺失值。插值要求 x是 year的函数。

   *（2）同时采用内插与外推法
    /*ipolate 只能填补数值范围内的缺漏值，无法填补数值范围外的缺漏值，即只能用于「插值」。若要进行「外推」，我们还需要在 ipolate 命令后加上 epolate 选项。*/
    
	ipolate x year,gen（×1） epolate，

	 /// 当我们仅仅采用内插法时，变量x中首尾缺失值并没有被补齐加入epolate选项同时采用内插与外推结合的方法时，x变量中的所有缺失值都被补齐。

**2.分组插值
  *（1）内插法
     by ind: ipolate x year, gen (x_1)
     
	 /// x_1 变量就储存着补缺后的完整数据：ind 为分组变量
  
  *（2）内插外推法
   by ind: ipolate x year,gen（×_1）epolate
 
 
 **3.循环插值
   foreach i
   in ×1 x2 x3｛
   by id: ipolate
  year, gen(x_ i') epolate
｝

 
  
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 


  
  
  
  
