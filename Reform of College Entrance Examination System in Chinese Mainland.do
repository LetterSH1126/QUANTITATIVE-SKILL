
//筛选变量
keep if (provcd == 11 | provcd == 31 | provcd == 32 | provcd == 33)

keep fid18 provcd wd503r
keep fid18 provcd finc

//横向合并家庭收入
duplicates drop fid provcd,force
merge 1:1 fid provcd using "/Users/linchenglee/Desktop/data/18/cfps2018famecon_202101"
drop _merge

//标记是否有政策
clear

sort provcd
merge m:1 provcd using "/Users/linchenglee/Desktop/data/child output/provcd12.dta"
drop _merge

//合并数据
append using "/Users/linchenglee/Desktop/2016/cfps2016总.dta"
append using cfps2016总.dta

//分析原始数据
sum wd
sum finc

//对数据取对数
gen wd21  = ln(wd)
gen finc21 = ln(finc)
sum wd21
sum finc21

//描述统计
use "/Users/linchenglee/Desktop\DID\12 14 16年.dta",clear  
asdoc sum wd finc gender age
//1.基准回归
gen time = (Year>=2014) & !missing(year) 
gen did = time*policy //产生交互项
reghdfe wd21 did,absorb(code)
est store a
reghdfe wd21 did finc21 age11 gender11,absorb(code)
est store b
outreg2 [a b] using 基准回归.doc,replace tstat bdec(5) tdec(2) addstat(F-test,`e(F)')

//2.平行趋势检验
gen event=Year-2014 //2014是政策时间
tab event
replace event=-6 if event<-6
forvalues i=6(-1)1{
  gen pre`i'=(event==-`i')
}
gen current=(event==0 )
forvalues i=1(1)6{
  gen post`i'=(event==`i')
}
drop pre1 //丢掉一期作为基准期
reghdfe wd21 pre* current post* finc21 age11 gender11,absorb(code)
*画图
ssc install coefplot
coefplot, keep(pre2 current post1 post2) vertical addplot(line @b @at) yline(0) levels(95) 

//3.安慰剂检验  
permute did beta = _b[did], reps(500) seed(123) saving("134.dta"):reghdfe wd21 did finc21 age11 gender11,absorb(code)

use "134.dta",clear

#delimit ;
dpplot beta, xline(63.46972, lc(black*0.5) lp(dash))  //63.46972是真实回归系数
             xline(0, lc(black*0.5) lp(solid))
             xtitle("系数", size(*0.8)) 
             ytitle("密度", size(*0.8)) 
             ylabel(, nogrid format(%4.1f) labsize(small)) 
             note("") caption("") 
             graphregion(fcolor(white)) ;  //26-34行全选运行

