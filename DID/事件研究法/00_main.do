*  
*  ███████╗████████╗ █████╗ ████████╗ █████╗ 
*  ██╔════╝╚══██╔══╝██╔══██╗╚══██╔══╝██╔══██╗
*  ███████╗   ██║   ███████║   ██║   ███████║
*  ╚════██║   ██║   ██╔══██║   ██║   ██╔══██║
*  ███████║   ██║   ██║  ██║   ██║   ██║  ██║
*  ╚══════╝   ╚═╝   ╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═╝
*
/*-----------------------------------------------------------------------
|                           Eventstudy                                  |
|                            July 2023                                  |
|                      Zhang ZiYao & Huang Wei                          |
|                                                                       |
| Wirtten by Zhang ZiYao                                                |
| Stata Version: 17.0                                                   |
| Required packages: reghdfe            - 多维固定效应模型估计             |
|                    hdfe               - 计算多维固定效应模型的残差        |
|                    coefplot           - 估计结果画图                    |
|                    event_plot         - 估计结果画图 (事件研究法)         |
|                    grc1leg2           - 多图合并共用图例                 |
|                    ereplace           - egen函数快速replace            |
|                    eventstudyinteract - Sun & Abraham (2021)          |
|                    csdid              - Callaway & Sant'Anna (2021)   |
|                    did_imputation     - Borusyak et al.(2023)         |
|                    did2s              - Gardner(2022)                 |
|                    fect               - Liu et al.(2022)              |
|                    stackedev          - Cengiz et al.(2019)           |
|                    _gwtmean           - fect需要                      |
| Note: 运行前请确认上述package已经全部安装                                 |
-----------------------------------------------------------------------*/

clear all

* 设定路径

gl root "$cloud\2023_事件研究法\code" //设定工作路径，建议设置为dofiles所在路径

cd $root

* 创建figure文件夹
cap mkdir figure, public

* 生成数据
do "01_DGP.do"

* 生成图1和图4
do "figure1&4.do"

* 2.传统事件研究法
do "02_Homogeneity.do"

* 3.存在组间差异化趋势的事件研究法
do "03_GroupTrend.do"  

* 4.异质性处理效应下的事件研究法
do "04_Heterogeneity.do"

* 5.异质性处理效应&组间差异化趋势下的事件研究法
do "05_Heterogeity_GroupTrend.do"

/* 最后测试时间：2023年7月19日00:00 */
