merge m:1 city  using"D:\jd2\新质生产力对绿色全要素生产率\异质性.dta"
drop if _merge==2
drop _merge

*描述性统计
logout, save(描述性统计6) word replace: tabstat Gtfp Npro GDP  People Fin Urban Gov Ep , stats(N mean sd min p50 max) f(%10.4f) c(s)

use panel.dta,clear
encode city,gen(city_id)
xtset city_id year
gen lnGDP = log(GDP)
gen lnPeople = log(People)
ssc install corrtable,replace
pwcorr Npro lnGDP ln People Fin Urban Gov Ep
reg Gtfp Npro lnGDP lnPeople Fin Urban Gov Ep
estat vif


*基准回归
reghdfe Gtfp Npro1 ,absorb(cid year)
estadd local City "YES"
estadd local Year "YES"
est store a1
reghdfe Gtfp Npro1 GDP  People Fin Urban Gov Ep ,absorb(cid year)
estadd local City "YES"
estadd local Year "YES"
est store a2
esttab a* using 新基.rtf, b(4) se(4) nogap compress stats(City Year N r2_a, fmt(%12.0f %12.0f %12.0f %9.3f)) star(* 0.1 ** 0.05 *** 0.01) replace
*稳健性5个
reghdfe CCR Npro1 GDP  People Fin Urban Gov Ep ,absorb(cid year)
estadd local city "YES"
estadd local year "YES"
est store b1
reghdfe Gtfp Npros GDP  People Fin Urban Gov Ep ,absorb(cid year)
estadd local city "YES"
estadd local year "YES"
est store b2
encode city,gen(city_id)
xtset cid year
reghdfe f.Gtfp Npro1 GDP  People Fin Urban Gov Ep ,absorb(cid year)
estadd local city "YES"
estadd local year "YES"
est store b3
esttab b* using 新稳.rtf, b(4) se(4) nogap compress stats(City Year N r2_a, fmt(%12.0f %12.0f %12.0f %9.3f)) star(* 0.1 ** 0.05 *** 0.01) replace

use panel.dta, clear
egen npro_mean = mean(Npro)
egen npro_sd   = sd(Npro)
keep if abs(Npro - npro_mean) < 3 * npro_sd  
count 
reghdfe Gtfp Npro $controls, absorb(city year) vce(cluster city)
est store R6

use panel.dta, clear
encode 省份, gen(prov)  
reghdfe Gtfp Npro $controls, absorb(city prov#year) vce(cluster city)
est store R7

estout b1 b2 b3 R6 R7 using "新稳.rtf", replace

*内生性1个
xtset cid year
gen IV=l.Npro1
ivreghdfe Gtfp  GDP  People Fin Urban Gov Ep (Npro1=IV),absorb(cid year) cluster(cid year) first savefirst savefprefix(f)
eststo second
est restore fNpro1
esttab fNpro1 using 新稳1.rtf, b(4) se(4) nogap compress stats(City Year N r2_a, fmt(%12.0f %12.0f %12.0f %9.3f)) star(* 0.1 ** 0.05 *** 0.01) replace
est restore second
esttab second using 新稳2.rtf, b(4) se(4) nogap compress stats(City Year N r2_a, fmt(%12.0f %12.0f %12.0f %9.3f)) star(* 0.1 ** 0.05 *** 0.01) replace
*机制（绿色创新，产业结构升级，资源配置）
reghdfe Indu Npro1 GDP  People Fin Urban Gov Ep ,absorb(cid year)
estadd local City "YES"
estadd local Year "YES"
est store c1
reghdfe lnInpatent Npro1 GDP  People Fin Urban Gov Ep ,absorb(cid year)
estadd local City "YES"
estadd local Year "YES"
est store c2
reghdfe Kl Npro1 GDP  People Fin Urban Gov Ep ,absorb(cid year)
estadd local City "YES"
estadd local Year "YES"
est store c3
reghdfe Ll Npro1 GDP  People Fin Urban Gov Ep ,absorb(cid year)
estadd local City "YES"
estadd local Year "YES"
est store c4
reghdfe Sl Npro1 GDP  People Fin Urban Gov Ep ,absorb(cid year)
estadd local City "YES"
estadd local Year "YES"
est store c5
esttab c* using 新机2.rtf, b(4) se(4) nogap compress stats(City Year N r2_a, fmt(%12.0f %12.0f %12.0f %9.3f)) star(* 0.1 ** 0.05 *** 0.01) replace
*异质性（资源城市和东中西）

use panel.dta,clear
gen ne = 0
replace ne = 1 if ustrregexm(city, "沈阳|大连|鞍山|抚顺|本溪|丹东|锦州|营口|阜新|辽阳|盘锦|铁岭|朝阳|葫芦岛")
replace ne = 1 if ustrregexm(city, "长春|吉林|四平|辽源|通化|白山|松原|白城|延边")
replace ne = 1 if ustrregexm(city, "哈尔滨|齐齐哈尔|鸡西|鹤岗|双鸭山|大庆|伊春|佳木斯|七台河|牡丹江|黑河|绥化|大兴安岭")
gen region = .
replace region = 1 if ustrregexm(city, "北京|天津|河北|上海|江苏|浙江|福建|山东|广东|海南")
replace region = 2 if ustrregexm(city, "山西|安徽|江西|河南|湖北|湖南")
replace region = 3 if ustrregexm(city, "内蒙古|广西|重庆|四川|贵州|云南|西藏|陕西|甘肃|青海|宁夏|新疆")
keep if region == 1 | ne == 1
global ctrl "GDP People Fin Urban Gov Ep"
reghdfe Gtfp c.Npro##i.ne $ctrl, absorb(city year) vce(cluster city)
test 1.ne#c.Npro = 0
local p_int = r(p)

reghdfe Gtfp Npro1 GDP  People Fin Urban Gov Ep if V==1,absorb(cid year) 
estadd local City "YES"
estadd local Year "YES"
est store d1
reghdfe Gtfp Npro1 GDP  People Fin Urban Gov Ep if V==0,absorb(cid year) 
estadd local City "YES"
estadd local Year "YES"
est store d2
reghdfe Gtfp Npro1 GDP  People Fin Urban Gov Ep if V==1,absorb(cid year) 
estadd local City "YES"
estadd local Year "YES"
est store d3
reghdfe Gtfp Npro1 GDP  People Fin Urban Gov Ep if 所属地域=="东部",absorb(cid year) cluster(cid)
estadd local City "YES"
estadd local Year "YES"
est store d4
reghdfe Gtfp Npro1 GDP  People Fin Urban Gov Ep if 所属地域=="中部",absorb(cid year) cluster(cid) 
estadd local City "YES"
estadd local Year "YES"
est store d5
reghdfe Gtfp Npro1 GDP  People Fin Urban Gov Ep if 所属地域=="西部",absorb(cid year) cluster(cid) 
estadd local City "YES"
estadd local Year "YES"
est store d6
esttab d* using 新异1.rtf, b(4) se(4) nogap compress stats(City Year N r2_a, fmt(%12.0f %12.0f %12.0f %9.3f)) star(* 0.1 ** 0.05 *** 0.01) replace
drop if abs(Npro - r(mean)) > 3*r(sd)
reghdfe Gtfp Npro $controls, absorb(city year) vce(cluster city)



**门槛
xthreg Gtfp  GDP  People Fin Urban Gov Ep , rx(Npro1) qx(IV) thnum(1) bs(300 )   trim(0.01) grid(100) r

**did
reghdfe Gtfp did GDP  People Fin Urban Gov Ep ,absorb(cid year)
est store r1
reg2docx  r1 using myfile.docx, replace b(%9.3f) t(%9.3f) scalars(N 个体固定 行业固定 r2 F) title(表1回归表1) note(注：1.括号内为T值；2. *代表p<0.10,**代表p<0.05,***代表p<0.01。)

**parallel trend
gen policy = year - 2019
replace policy = -5 if policy < -5
replace policy = 4 if policy > 4
forvalues i = 5(-1)1 {
    gen pre_`i' = (policy == -`i')
}
gen current = (policy == 0)
forvalues j = 1(1)4 {
    gen post_`j' = (policy == `j')
}
drop pre_1
reghdfe Gtfp pre_5 pre_4 pre_3 pre_2 current post_1 post_2 post_3 post_4 GDP  People Fin Urban Gov Ep 
coefplot, baselevels vertical keep(pre_* current post_*) omitted order( pre_3 pre_2 pre_1 current post_1 post_2 post_3 post_4 post_5 post_6) level(95) yline(0,lcolor(edkblue*0.8)) xline(3, lwidth(vthin) lpattern(dash) lcolor(teal))ylabel(,labsize(*0.75)) xlabel(,labsize(*0.75)) ytitle("policy", size(small)) xtitle("policytime", size(small)) addplot(line @b @at) ciopts(lpattern(dash) recast(rcap) msize(medium)) msymbol(circle_hollow) 

**placebo
cap erase "simulations.dta"
permute did beta = _b[did] se = _se[did] df = e(df_r), reps(500) seed(123) saving("simulations.dta"): reghdfe Gtfp did
use "simulations.dta", clear
gen t_value = beta / se
gen p_value = 2 * ttail(df, abs(beta/se))
dpplot beta, xline(0.4040, lc(black*0.5) lp(dash)) xline(0, lc(black*0.5) lp(solid))xtitle("Estimator", size(*0.8)) ytitle("Density", size(*0.8)) 
dpplot t_value, xtitle("T-value", size(*0.8)) ytitle("Density", size(*0.8))
twoway (scatter p_value beta)(kdensity beta,yaxis(2)),xline(0.4040) yline(0.1,lpattern(dash))

**异质性
reghdfe Gtfp did GDP  People Fin Urban Gov Ep if 所属地域=="东部",absorb(cid year)
estadd local City "YES"
estadd local Year "YES"
est store d3
reghdfe Gtfp did GDP  People Fin Urban Gov Ep if 所属地域=="中部",absorb(cid year)
estadd local City "YES"
estadd local Year "YES"
est store d4
reghdfe Gtfp did GDP  People Fin Urban Gov Ep if 所属地域=="西部",absorb(cid year)
estadd local City "YES"
estadd local Year "YES"
est store d5
esttab d3 d4 d5 using 新异1.rtf, b(4) se(4) nogap compress stats(City Year N r2_a, fmt(%12.0f %12.0f %12.0f %9.3f)) star(* 0.1 ** 0.05 *** 0.01) replace

reghdfe Gtfp did GDP  People Fin Urban Gov Ep if V==1,absorb(cid year) 
estadd local City "YES"
estadd local Year "YES"
est store d7
reghdfe Gtfp did GDP  People Fin Urban Gov Ep if V==0,absorb(cid year) 
estadd local City "YES"
estadd local Year "YES"
est store d8
esttab d7 d8 using 新异1.rtf, b(4) se(4) nogap compress stats(City Year N r2_a, fmt(%12.0f %12.0f %12.0f %9.3f)) star(* 0.1 ** 0.05 *** 0.01) replace

* 0. 保险头
clear all
program drop _all
use panel.dta, clear
gen nproXfin = Npro*Fin
drop if missing(Npro, Fin, Sl, Gtfp, nproXfin)
ssc install estout,replace

encode city, gen(city_id)
eststo MY1: reg Gtfp Indu Npro Fin i.city_id i.year
eststo MY2: reg Gtfp lnInpatent Npro Fin i.city_id i.year
eststo MY3: reg Gtfp Sl Npro Fin i.city_id i.year
reg Gtfp Npro $controls i.city_id i.year, cluster(city)
* 1. 先把控制变量" partial out "
reg Gtfp i.city_id i.year
predict r_Gtfp, resid

reg Npro i.city_id i.year
predict r_Npro, resid

reg Indu i.city_id i.year
predict r_Indu, resid

reg lnInpatent i.city_id i.year
predict r_lnInpatent, resid

reg Kl i.city_id i.year
predict r_Kl, resid

reg Ll i.city_id i.year
predict r_Ll, resid

rename r_Gtfp sh_Gtfp
rename r_Npro sh_Npro
rename r_Indu sh_Indu
rename r_lnInpatent sh_InPat
rename r_Kl sh_Kl
rename r_Ll sh_Ll

describe sh_Gtfp sh_Npro sh_Indu sh_InPat sh_Kl sh_Ll

* 然后继续 domin 分析
ssc install domin, replace
domin sh_Gtfp sh_Npro sh_Indu sh_InPat sh_Kl sh_Ll


esttab MY1 MY2 MY3, b(%7.4f) se starlevels(* 0.1 ** 0.05 *** 0.01) ///
                   title("表 5-4  M→Y 回归：中介变量对绿色TFP的影响")

* 2. Monte Carlo 间接效应 CI（以 Sl 渠道为例）----------
program define simMed, rclass
    bsample _N
    reg Sl Npro Fin nproXfin          // X→M
    scalar a1 = _b[Npro]
    scalar a3 = _b[nproXfin]
    reg Gtfp Npro Sl Fin nproXfin     // M→Y
    scalar b  = _b[Sl]
    sum Fin, d
    scalar lo = r(mean) - r(sd)
    scalar hi = r(mean) + r(sd)
    return scalar IndLow = (a1 + a3*lo)*b
    return scalar IndHigh= (a1 + a3*hi)*b
    return scalar IMM    = a3*b
end

simulate IndLow=r(IndLow) IndHigh=r(IndHigh) IMM=r(IMM), ///
         reps(5000) seed(12345): simMed

* 3. 95% CI（偏差校正）
_pctile IndLow, p(2.5 97.5)
local mL = r(mean)
local lL = r(r1)
local uL = r(r2)
_pctile IndHigh, p(2.5 97.5)
local mH = r(mean)
local lH = r(r1)
local uH = r(r2)
_pctile IMM, p(2.5 97.5)
local mI = r(mean)
local lI = r(r1)
local uI = r(r2)

* 4. 结果
di _n(2) "=== 表 5-5  Monte Carlo 间接效应 95% CI ==="
di "渠道        点估计     95% 偏差校正 CI        显著"
di "IndLow     " %7.4f `mL' "   [" %7.4f `lL' " , " %7.4f `uL' " ]  " ///
   cond(`lL'<0 & `uL'>0, "否", "是")
di "IndHigh    " %7.4f `mH' "   [" %7.4f `lH' " , " %7.4f `uH' " ]  " ///
   cond(`lH'<0 & `uH'>0, "否", "是")
di "IMM        " %7.4f `mI' "   [" %7.4f `lI' " , " %7.4f `uI' " ]  " ///
   cond(`lI'<0 & `uI'>0, "否", "是")
   
   

