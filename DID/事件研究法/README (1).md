# 《事件研究法的实现、问题和拓展》数据与代码文件使用说明

###### 张子尧　　黄　炜

## 1.文件内容

本文件夹包含2个数据集和7个dofiles

- Data：
  - `Currie_2020.dta`：Top5论文统计数据。来自Currie et al. （2020）。
  - `cntop.dta`：2005-2022年知网检索结果。作者手工检索。
  
- Dofiles
  - `main.do`：主程序，按顺序执行全部代码。
  - `01_DGP.do`：生成4个模拟数据集。
  - `figure1&4.do`：图1和图4。
  - `02_Homogeneity.do`：图2-图6。
  - `03_GroupTrend.do`：图7-图9。
  - `04_Heterogeneity.do`：图10-图12。
  - `05_Heterogeity_GroupTrend.do`：图13-图14。

## 2.使用方法

运行前请**务必确认**以下`Stata`第三方`package`已经安装完毕：

```stata
/*-----------------------------------------------------------------------
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
| Notes: 所有package需要更新到最新版本 									|
-----------------------------------------------------------------------*/
```

安装完毕后，按以下步骤进行：

1. 打开`main.do`文件，设定工作路径，保存文件。

   ```stata
   * 设定路径
   
   gl root " " //设定工作路径，建议设置为dofiles所在路径
   
   ```

2. 运行`main.do`。

## 3.运行结果

代码顺利运行完毕后会生成14张图，生成文件自动保存在`figure`文件夹。

## 4.其他事项

1. 代码文件最后于`2023年7月19日00:00 `在`Stata 17.0`中测试通过。
2. 如有疑问，请联系`ziyao.zhang@zuel.edu.cn`。
