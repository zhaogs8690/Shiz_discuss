. sysuse auto.dta, clear
. reghdfe price weight length, absorb(turn trunk) vce(cluster turn)
//表示要吸收turn和trunk这两个变量的固定效应。这意味着在回归中会控制这些固定效应，从而消除它们对回归结果的个体差异影响。
//vce(cluster turn)：指定使用聚类稳健标准误，并且聚类变量为turn
  (output omitted)
  
  
*吸收固定效应后的自由度信息
Absorbed degrees of freedom:
-----------------------------------------------------+
 Absorbed FE | Categories  - Redundant  = Num. Coefs |
-------------+---------------------------------------|
        turn |        13          13           0    *|
       trunk |        13           0          13     |
-----------------------------------------------------+
* = FE nested within cluster; treated as redundant for DoF computation
* 显示有13个类别（Categories），其中被视为冗余（Redundant）的数量也是13，最终计算系数（Num. Coefs）时有效的数量为0，并标记*表示该固定效应嵌套在聚类变量内（FE nested within cluster），在计算自由度时被当作冗余处理。


indicate("Length Controls=length") //回归结果种原本以变量名 length 出现的地方，会被替换为 “Length Controls” 这个标签

* Setup
sysuse auto, clear

* Run and store regressions
reghdfe price weight length, a(turn)
estimates store m1
reghdfe price weight length, a(turn trunk)
estimates store m2
reghdfe price weight length, a(turn foreign)
estimates store m3

* Prepare estimates for -estout-
estfe m*, labels(turn "Turn FE" trunk "Trunk FE" foreign "Foreign FE")
return list
*将 turn、trunk、foreign 等固定效应，以 Turn FE=Yes/No、Trunk FE=Yes/No、Foreign FE=Yes/No 等方式呈现在回归结果中。


* Run estout/esttab/reg2docx
// esttab m*, indicate("Length Controls=length" `r(indicate_fe)')
* Return stored estimates to their previous state
// estfe m*, restore



*结果输出：
reg2docx m1 m2 m3 using reghdfe_auto.docx, replace indicate("Length Controls=length" `r(indicate_fe)')
pwd //查询文件保存位置
reg2docx m1 m2 m3 using E:\桌面\reghdfe_auto.docx, replace indicate("Length Controls=length" `r(indicate_fe)')
esttab m1 m2 m3 using reghdfe_auto.rtf, replace indicate("Length Controls=length" `r(indicate_fe)') //其他结果输出语句


