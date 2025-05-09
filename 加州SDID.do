
* 从github上加载数据
global link "https://raw.githubusercontent.com/synth-inference/synthdid/"

import delimited "$link/master/data/california_prop99.csv", clear delim(";")

* 数据下载失败


* SDID基本代码
sdid packspercapita state year treated, vce(placebo) seed(123) reps(50) ///

graph graph_export(sdid_, .eps) g1_opt(xtitle(""))