capture log close
log using "数据分析", replace text


*** 基本系统设置 ***

version 16 // 版本选择
clear all // 清除已有记录
set more off // 一次性显示全部结果
set scheme s2color // 图像显示格式
set linesize 80 // 结果显示宽度 （屏宽）
set maxvar 20000 // 最大变量个数设置
set seed 2020 // 复制结果需要 Random-number generator（e.g., bootstrapping, multiple imputation, simulations）（种子数）
set cformat %4.3f // 小数点位数显示（coefficients, SEs, and CIs）
set pformat %4.3f // 小数点位数显示（p-values）
set sformat %4.3f // 小数点位数显示（test statistics）

  use "stata数据.dta", clear
 
hist Score0
quantile Score0
pnorm Score0 //pnorm对数据的中间范围内的非正态敏感
qnorm Score0 //qnorm根据正态分布的分位数绘制变量的分位数; 对尾部附近的非正态性很敏感
sktest Score0
//结论：虽然柱状图呈现中间高两端低的样式，但是GGGI的分布不属于正态分布。
//总体而言，GGGI的分布呈现左偏，大部分国家集中于0.65~0.8之间，0.7左右的国家最多。只有很少的国家在0.55以下。表明促进国内性别平等是世界共识

***大洲与性别平等***
tab Continent
reg Score0 Continent
estimate store model1
estimate restore model1
margins, at(Continent=(1 2 3 4 5 6 7 8)) atmeans
marginsplot, recast(bar) xtitle("地区") ytitle("预测的性别平等指数'")
bysort Continent : egen meanScore0 =mean( Score0 )
gen lowest=1 if Score0>0.4&Score0<0.5
gen lower=1 if Score0>=0.5&Score0<0.6
gen middle=1 if Score0>=0.6&Score0<0.7
gen higher=1 if Score0>=0.7&Score0<0.8
gen highest=1 if Score0>=0.8&Score0<0.9
graph bar (mean) lowest lower middle higher highest, over(Continent) stack intensity(*0.8) ///
      ytitle("所占比例") ///
      legend(order(1 "性别最不平等" 2 "性别不平等" 3 "一般" 4 "性别较平等'" 5 "性别最平等") row(2)) ///
	  title("不同地区国家的性别平等概况")
//发现性别平等指数由小到大分别是：东欧及中亚地区＜中东和北非地区＜东亚和太平洋国家＜拉美和加勒比国家＜北美＜南亚＜撒哈拉以南国家＜西欧国家。大致呈现规律的阶梯状上升
//以0.1为一档对GGGI进行平均分组之后，发现性别最不平等的国家集中于中东和南亚地区
//各地区平均GGGI:东欧及中亚地区:0.619,中东和北非地区:0.622,东亚和太平洋国家:0.689,拉美和加勒比国家:0.702,北美:0.725,南亚:0.729,撒哈拉以南国家:0.768,西欧国家:0.780
//基本符合我们的预期。值得注意的是撒哈拉以南国家的性别平等指数仅次于西欧。猜想：撒哈拉以南多是低收入或贫困国家，在基本生存线边缘，政治经济教育上反而没有强烈的性别差异。西欧多高福利国家，不论男女都能获得较好的收入以及医疗资源，受到同等的教育

  
***假设1：女性劳动人口就业率越高（失业率越低），性别平等指数越高。***
reg Score0 Unemploymentfemaleoffemal

gen Unemploymentfemale2=Unemploymentfemaleoffemal*Unemploymentfemaleoffemal //纳入变量女性失业率的平方
sum Score0 Unemploymentfemaleoffemal Unemploymentfemale2
reg Score0 Unemploymentfemaleoffemal Unemploymentfemale2,beta 
di 0.247*0.067
//aaplot Score0 Unemploymentfemaleoffemal Unemploymentfemale2, name(plot)
//发现拟合优度（R-squared）是0.0509升高，但是矫正判定系数（Adj R-squared）略有减少，但总体而言模型解释力度上升
//不论是失业率还是失业率的平方的相关系数（相关系数就是y=ax²+bx+c里面的a和b）都小于0.001，画出的是相当平滑微向下的曲线
//发现纳入二次项之后，失业率的平方每上升一个标准差，性别平等指数就减少0.247个标准差即0.017

corr Score0 Unemploymentfemaleoffemal Unemploymentfemale2//协方差相关性检验，纯数字版本的热力图
//算失业率和性别平等之间的相关系数（correlationcoefficient）越接近1越高，约接近0越低，得出性别平等和女性失业率之间的数字是-0.2134，性别平等和女性失业率的平方之间的相关系数是-0.2256
//得出结论：失业率与性别平等之间有微弱的非线性关系，虽然并没有过于明显的量化联系，但结合相关的社会学理论，我们依然能够认为女性劳动人口就业率越高的社会性别越平等，但可能它们之间的直接相关性没有想象得那么明显


***经济发展水平和性别平等（人均GDP，经济增长速度，女性劳动者失业率）***
sum Score0 GDPperson2021 GDPgrowth2021 Unemploymentfemaleoffemal
reg Score0 GDPperson2021 GDPgrowth2021 Unemploymentfemaleoffemal,beta
vif //利用vif进行多重共线性检验，发现方差膨胀因子均在10及以下，说明该模型不存在多重共线性问题，可信度高
reg Score0 GDPperson2021 GDPgrowth2021 Unemploymentfemaleoffemal Privatedebtloansanddebtsec,beta
vif //利用vif进行多重共线性检验，发现方差膨胀因子均在10及以下，说明该模型不存在多重共线性问题，可信度高
corr Score0 GDPperson2021 GDPgrowth2021 Unemploymentfemaleoffemal Privatedebtloansanddebtsec//协方差相关性检验，纯数字版本的热力图

//接下来进行因子分析和主成分分析
reg Score0 GDPperson2021 GDPgrowth2021 Unemploymentfemaleoffemal Privatedebtloansanddebtsec,beta
factor GDPperson2021 GDPgrowth2021 Unemploymentfemaleoffemal Privatedebtloansanddebtsec, pcf//发现人均GDP和GDP增长速度权重较大
factor GDPperson2021 Privatedebtloansanddebtsec, pcf
factor GDPgrowth2021 Unemploymentfemaleoffemal, pcf
//发现人均债务和人均GDP关联度高，国家GDP增长率与女性失业率关联性高，而且这两组关联呈现反比状态
//为什么人均GDP高的人均债务高？ （进行一个猜测）
//为什么国家GDP增长率和女性失业呈正比？ 回答：2021年是世界新冠疫情高峰，各国普遍增加了失业率。女性在行业中所创造的GDP价值不如男性，因此会被优先裁员
twoway (scatter Score0 GDPgrowth2021, mlabel(Country)) (lfit Score0 GDPgrowth2021), ///
       xtitle("GDP增长率") xlabel(-18(1)34) ///
	   ytitle("性别平等认同") ylabel(0.4(.1)0.9) legend(off)

***平均债务高：（2，4）***
***假设1：个人平均债务高是因为国家的社会环境比较安全；***
***假设2：个人平均债务高是高福利的表现；***
***假设3：个人平均债务高是因为男女收入水平的差异大；***
***假设4：个人平均债务高是因为人均收入高。***
//变量解释：个人平均债务：个人债务与人均GDP的相对比值，数字越高说明这个国家里的个人债务危机越严重
reg Score0 Privatedebtloansanddebtsec
gen Privateover200=Privatedebtloansanddebtsec if Privatedebtloansanddebtsec>=200
twoway (scatter Score0 Privateover200, mlabel(Country)) (lfit Score0 Privateover200), ///
       xtitle("个人平均债务（200以上）") xlabel(200(50)400) ///
	   ytitle("性别平等认同") ylabel(0.4(.05)0.9)
	   
gen Private200=Privatedebtloansanddebtsec if Privatedebtloansanddebtsec<=200
twoway (scatter Score0 Private200, mlabel(Country)) (lfit Score0 Private200), ///
       xtitle("个人平均债务(200以下)") xlabel(2(20)200) ///
	   ytitle("性别平等认同") ylabel(0.4(.05)0.9) legend(off)



log close
exit