use  原始数据.dta, clear

global control " prir secr govr gove pden ppr mpr lfin  "

***附表1
logout, save(描述性统计) word replace: tabstat  lnrgdp  lnperrgdp  did $control saves uincome rincome empoly tempoly  NewlyEnterprises , stat(count mean sd min max) col(stat) format(%10.3f)
 
***表2
reghdfe lnrgdp did D T    , cluster(i.countycode)
est store a1 
reghdfe lnrgdp did D T $control ,  cluster(i.countycode) 
est store a2 
reghdfe lnrgdp did $control , absorb( i.countycode i.year ) cluster(i.countycode) 
est store a3 
reghdfe lnperrgdp did D T    , cluster(i.countycode)
est store a4 
reghdfe lnperrgdp did D T $control ,  cluster(i.countycode) 
est store a5 
reghdfe lnperrgdp did $control , absorb( i.countycode i.year ) cluster(i.countycode) 
est store a6 
outreg2 [a1 a2 a3 a4 a5 a6] using 基准回归.xml, replace  dec(3)

***图1
***(a)
 reghdfe lnrgdp before8  before7 before6 before5 before4 before3 before2  current after1 after2 $control , absorb(year countycode )  cluster(i.countycode)
 est store dyear
 coefplot dyear, keep(before8 before7 before6 before5 before4 before3 before2 before1 current after1 after2) vertical ///
 recast(connect) yline(0) scheme(s1mono) ciopts(recast(rline) lwidth(thin) lpattern(dash) lcolor(gs2))
***(b)
 reghdfe lnperrgdp before8 before7 before6 before5 before4 before3 before2  current after1 after2 $control   , absorb(year  countycode)  cluster(i.countycode)
 est store Dyear
 coefplot Dyear, keep(before8 before7 before6 before5 before4 before3 before2 before1 current after1 after2) vertical ///
 recast(connect) yline(0) scheme(s1mono)  ciopts(recast(rline) lwidth(thin) lpattern(dash) lcolor(gs2))
 
 ***附表2
reghdfe lnrgdp did $control if year>=2015 & year<=2019 , absorb( i.countycode i.year ) cluster(i.countycode) 
est store b1 
reghdfe lnperrgdp did $control if year>=2015 & year<=2019 , absorb( i.countycode i.year ) cluster(i.countycode) 
est store b2
reghdfe rgdpg did $control , absorb( i.countycode i.year ) cluster(i.countycode) 
est store b3
reghdfe perrgdpg did $control , absorb( i.countycode i.year ) cluster(i.countycode) 
est store b4
outreg2 [b1 b2 b3 b4 ] using 稳健性检验.xml, replace drop(i.year year#省代码 i.省代码 $control ) dec(3)

***附表3
reghdfe lnrgdp did $control  if provencycode!=13 & provencycode!=14 & provencycode!=34 & provencycode!= 62 & provencycode!= 63 & provencycode!=64 , absorb( i.countycode i.year  ) cluster(i.countycode)
est store a1
reghdfe lnperrgdp did $control  if provencycode!=13 & provencycode!=14 & provencycode!=34 & provencycode!= 62 & provencycode!= 63 & provencycode!=64 , absorb( i.countycode i.year  ) cluster(i.countycode)
est store a2
reghdfe lnrgdp did $control if 是否是第一批光伏扶贫项目2016年10月!=1, absorb( i.countycode i.year ) cluster(i.countycode)
est store a3
reghdfe lnperrgdp did $control if 是否是第一批光伏扶贫项目2016年10月!=1, absorb( i.countycode i.year ) cluster(i.countycode)
est store a4
reghdfe lnrgdp did $control if (D==1 | poor==0), absorb( i.countycode i.year ) cluster(i.countycode) 
est store a5
reghdfe lnperrgdp did $control if (D==1 | poor==0), absorb( i.countycode i.year ) cluster(i.countycode) 
est store a6
reghdfe lnrgdp did $control if (D==1 | poor==0) & provencycode!=13 & provencycode!=14 & provencycode!=34 & provencycode!= 62 & provencycode!= 63 & provencycode!=64 & 是否是第一批光伏扶贫项目2016年10月!=1, absorb( i.countycode i.year ) cluster(i.countycode)
est store a7
reghdfe lnperrgdp did $control if (D==1 | poor==0)& provencycode!=13 & provencycode!=14 & provencycode!=34 & provencycode!= 62 & provencycode!= 63 & provencycode!=64 & 是否是第一批光伏扶贫项目2016年10月!=1, absorb( i.countycode i.year ) cluster(i.countycode)
est store a8
outreg2 [a1 a2 a3 a4 a5 a6 a7 a8 ] using 剔除未试点样本.xml, replace drop(i.year year#省代码 i.省代码 $control) dec(3)


***附表4
xi:xtivreg2 lnrgdp   prir secr govr gove pden ppr mpr lfin i.year  ( did  = iv)   ,fe  r first  savefp(first)
 est store  a
xi:xtivreg2 lnperrgdp   prir secr govr gove pden ppr mpr lfin i.year ( did  = iv)   ,fe  r first  savefp(first)
 est store  b
 xi:xtivreg2 lnrgdp   prir secr govr gove pden ppr mpr lfin i.year  ( did  = iv2)   ,fe  r first  savefp(first)
 est store  c
xi:xtivreg2 lnperrgdp   prir secr govr gove pden ppr mpr lfin i.year ( did  = iv2)   ,fe  r first  savefp(first)
 est store  d
outreg2 [a  b c d] using fileiv.xml, drop(i.year )  replace  dec(3)

***表3
reghdfe lnrgdp did $control if trtd==1 & trtdp==1 , absorb( i.countycode i.year ) cluster(i.countycode) 
est store tt1
reghdfe lnrgdp did $control if trtd==0 & trtdp==1 , absorb( i.countycode i.year ) cluster(i.countycode) 
est store tt2
reghdfe lnperrgdp did $control if trtd==1 & trtdp==1 , absorb( i.countycode i.year ) cluster(i.countycode) 
est store tt3
reghdfe lnperrgdp did $control if trtd==0 & trtdp==1 , absorb( i.countycode i.year ) cluster(i.countycode) 
est store tt4
outreg2 [tt1 tt2 tt3 tt4] using 三区三州.xml, replace drop(i.year year#省代码 i.省代码 $control) dec(3)

***表4
reghdfe lnrgdp c.did##c.ln_SunshineDuration $control  , absorb( i.countycode i.year ) cluster(i.countycode) 
est store a
reghdfe lnperrgdp c.did##c.ln_SunshineDuration $control , absorb( i.countycode i.year ) cluster(i.countycode) 
est store b
outreg2 [a b] using 光照强度划分.xml, replace drop(i.year year#省代码 i.省代码 ) dec(3)

***表6
reghdfe lnrgdp ddd1 $control , absorb( i.countycode i.year ) cluster(i.countycode) 
est store int1
reghdfe lnrgdp ddd2 $control , absorb( i.countycode i.year ) cluster(i.countycode) 
est store int2
reghdfe lnrgdp ddd7 $control , absorb( i.countycode i.year ) cluster(i.countycode) 
est store int3
reghdfe lnrgdp ddd6 $control , absorb( i.countycode i.year ) cluster(i.countycode) 
est store int4
reghdfe lnperrgdp ddd1 $control , absorb( i.countycode i.year ) cluster(i.countycode) 
est store int5
reghdfe lnperrgdp ddd2 $control , absorb( i.countycode i.year ) cluster(i.countycode) 
est store int6
reghdfe lnperrgdp ddd7 $control , absorb( i.countycode i.year ) cluster(i.countycode) 
est store int7
reghdfe lnperrgdp ddd6 $control , absorb( i.countycode i.year ) cluster(i.countycode) 
est store int8
outreg2 [int1 int2 int3 int4 int5 int6 int7 int8] using 政策强度.xml, replace drop(i.year i.year#i.省代码 $control ) dec(3)

***表7
reghdfe saves did $control , absorb( i.countycode i.year ) cluster(i.countycode) 
est store inc1
reghdfe uincome did $control , absorb( i.countycode i.year ) cluster(i.countycode) 
est store inc2
reghdfe rincome did $control , absorb( i.countycode i.year ) cluster(i.countycode) 
est store inc3
outreg2 [inc1 inc2 inc3] using 机制分析-收入..xml, replace drop(i.year i.year#i.省代码 $control ) dec(3)

***表8
reghdfe empoly  did $control , absorb( i.countycode i.year ) cluster(i.countycode) 
est store a
reghdfe tempoly did $control , absorb( i.countycode i.year ) cluster(i.countycode) 
est store b
outreg2 [a b ] using 机制分析-就业.xml, replace drop(i.year i.year#i.省代码 $control ) dec(3)

***表9
reghdfe NewlyEnterprises did   $control , absorb( i.countycode i.year i.year#i.provencycode) cluster(i.countycode) 
est store a
reghdfe NewlyEnterprises_b did $control , absorb( i.countycode i.year i.year#i.provencycode)  cluster(i.countycode) 
est store b
reghdfe NewlyEnterprises_d did $control , absorb( i.countycode i.year ) cluster(i.countycode) 
est store c
reghdfe NewlyEnterprises_k did $control, absorb( i.countycode i.year ) cluster(i.countycode)  
est store d
reghdfe NewlyEnterprises_z did $control , absorb( i.countycode i.year ) cluster(i.countycode) 
est store e
outreg2 [a b c d e] using 企业进入.xml, replace drop(i.year i.year#i.省代码 $control ) dec(3)