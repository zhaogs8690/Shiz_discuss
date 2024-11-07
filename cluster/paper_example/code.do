
 use 数据.dta,clear

 //设置面板数据
 xtset city_id year 
 **对省份和地级市代码进行了脱敏处理，不影响结果复现
 
 //确定表2-表5 控制变量
 global control1    fiscal_transparency_lag1  financial_development_lag1 per_capita_GDP_lag1 second_industry_lag1 ln_LGFVdebt_lag1
**连续变量做缩尾处理，控制变量和解释变量滞后一期
**变量定义可参见附录内容 
 
 //表2 （1）-（4）列
 reghdfe ln_delta_Limit  fund_income_lag1 debt_ratio_lag1  fixed_investment_lag1 $control1 if year>2016, absorb( year province_id )  vce(cl city_id)  
 est store G1
**样本期间为2017-2022年
  reghdfe ln_delta_Limit  fund_income_lag1  $control1 if _est_G1==1, absorb( year province_id )  vce(cl city_id)
//if _est_G1==1为条件语句，只对在G1估计集中的观测值进行回归分析		
  reghdfe ln_delta_Limit   debt_ratio_lag1   $control1 if _est_G1==1, absorb( year province_id )  vce(cl city_id)  
  reghdfe ln_delta_Limit   fixed_investment_lag1 $control1 if _est_G1==1, absorb( year province_id )  vce(cl city_id)  	
  
//表3 （1）-（2）列 时间异质性
 reghdfe ln_delta_Limit  fund_income_lag1 debt_ratio_lag1  fixed_investment_lag1 $control1 if year>2016&year<2020, absorb( year province_id )  vce(cl city_id)  
**2017-2019年
  reghdfe ln_delta_Limit  fund_income_lag1 debt_ratio_lag1  fixed_investment_lag1 $control1 if year>2019, absorb( year province_id )  vce(cl city_id) 
**2020-2022年
 
//表3 （3）-（4）列 高低GDP分组
 reghdfe ln_delta_Limit  fund_income_lag1 debt_ratio_lag1  fixed_investment_lag1 $control1 if  year>2016&高GDP分组==1, absorb( year province_id )  vce(cl city_id)
 reghdfe ln_delta_Limit  fund_income_lag1 debt_ratio_lag1  fixed_investment_lag1 $control1 if  year>2016&高GDP分组==0, absorb( year province_id )  vce(cl city_id)

//表3 （5）-（6）列 高低人均GDP分组
 reghdfe ln_delta_Limit  fund_income_lag1 debt_ratio_lag1  fixed_investment_lag1 $control1 if  year>2016&高人均GDP分组==1, absorb( year province_id )  vce(cl city_id)
 reghdfe ln_delta_Limit  fund_income_lag1 debt_ratio_lag1  fixed_investment_lag1 $control1 if  year>2016&高人均GDP分组==0, absorb( year province_id )  vce(cl city_id)
	
//表4 债务管理绩效因素
//（1）-（2）列 资金使用进度
 reghdfe ln_delta_Limit  special_debt_retention_lag1  fund_income_lag1 debt_ratio_lag1  fixed_investment_lag1 $control1  if  year>2016, absorb( year province_id )  vce(cl city_id)
 reghdfe ln_delta_Limit  debt_retention_lag1  fund_income_lag1 debt_ratio_lag1  fixed_investment_lag1 $control1  if  year>2016, absorb( year province_id )  vce(cl city_id)	
**稳健性检验，采用地方债务限额留存率

//（3）-（5）列 经济绩效情况
 reghdfe ln_delta_Limit growth_goal1_lag1 fund_income_lag1 debt_ratio_lag1  fixed_investment_lag1 $control1 if year>2016, absorb( year province_id )  vce(cl city_id) 
 reghdfe ln_delta_Limit growth_goal2_lag1 fund_income_lag1 debt_ratio_lag1  fixed_investment_lag1 $control1 if year>2016, absorb( year province_id )  vce(cl city_id) 
**稳健性检验，采用GDP增速/GDP目标
 reghdfe ln_delta_Limit GDP_growth_lag1 fund_income_lag1 debt_ratio_lag1  fixed_investment_lag1 $control1 if year>2016, absorb( year province_id )  vce(cl city_id) 
**稳健性检验，采用GDP增速

//表5 地方申请因素
// （1）-（2）列 财政压力和转移支付情况	 
 reghdfe ln_delta_Limit fiscal_pressure_lag1 fund_income_lag1 debt_ratio_lag1  fixed_investment_lag1 $control1 if year>2016, absorb( year province_id )  vce(cl city_id) 
 reghdfe ln_delta_Limit fiscal_pressure_lag1 transfer_lag1 interaction_lag1 fund_income_lag1 debt_ratio_lag1  fixed_investment_lag1 $control1 if year>2016, absorb( year province_id )  vce(cl city_id) 
 **交互项为财政压力和转移支付比例分别去中心化之后的乘积			

//（3）-（5）列 项目预期收益情况
 reghdfe ln_delta_Limit coverage_lag1 fund_income_lag1 debt_ratio_lag1  fixed_investment_lag1 $control1 if year>2016, absorb( year province_id )  vce(cl city_id) 	
 reghdfe ln_delta_Limit coverage_lag1 fund_income_lag1 debt_ratio_lag1  fixed_investment_lag1 $control1 if year>2016&财政压力大==1, absorb( year province_id )  vce(cl city_id) 		
 reghdfe ln_delta_Limit coverage_lag1 fund_income_lag1 debt_ratio_lag1  fixed_investment_lag1 $control1 if year>2016&财政压力大==0, absorb( year province_id )  vce(cl city_id) 		

 //限额配置的后续影响
 //确定表6 控制变量
  global control2   tech_spend_lag1   financial_development_lag1 consumption_lag1 second_industry_lag1 trade_lag1
 **连续变量做缩尾处理，控制变量滞后一期

 //panel A
 reghdfe  major_invest ln_delta_Limit  $control2    if year>2016&分配过量==1, absorb( year province_id )  vce(cl city_id) 		
 reghdfe  major_invest ln_delta_Limit  $control2    if year>2016&分配过量==0, absorb( year province_id )  vce(cl city_id) 	
 reghdfe  ln_GDP ln_delta_Limit  $control2    if year>2016&分配过量==1, absorb( year province_id )  vce(cl city_id)	
 reghdfe  ln_GDP ln_delta_Limit  $control2    if year>2016&分配过量==0, absorb( year province_id )  vce(cl city_id) 	
 //panel B
 reghdfe ln_special_refinancing ln_delta_Limit  $control2    if year>2016&分配过量==1, absorb( year province_id )  vce(cl city_id) 		
 reghdfe ln_special_refinancing  ln_delta_Limit  $control2    if year>2016&分配过量==0, absorb( year province_id )  vce(cl city_id) 		
 reghdfe  refinancing_payment	 ln_delta_Limit  $control2    if year>2016&分配过量==1, absorb( year province_id )  vce(cl city_id) 		
 reghdfe refinancing_payment	 ln_delta_Limit  $control2    if year>2016&分配过量==0, absorb( year province_id )  vce(cl city_id) 		
	
		
		


	