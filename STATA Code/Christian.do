*****ECON UOA group assignment*****
clear
set more off

*****data set up*****

global dir  "\\stf8\stf8\users\9\em17839\ECON UOA\"

cd "${dir}GASS1\"

import excel "Christian.xlsx", sheet("Input") firstrow clear
gen date=ym(year,month)
tsset date, monthly
//gen usd = 1/nzd				//generates a varaible equal to the cost of 1 nzd in usd

gen rj_minus_r_1 = 12*(r1y_nz - r1y_us)/1200
gen rj_minus_r_2 = 24*(r2y_nz - r2y_us)/1200
gen rj_minus_r_5 = 60*(r5y_nz - r5y_us)/1200
gen rj_minus_r_10 = 120*(r10y_nz - r10y_us	)/1200
*****Equation 1*****
gen e = ln(usd)		//generates the c varaible used in equation 1
foreach num of numlist 1, 2, 5, 10 {
	local n = (`num'*12)
	gen ek_minus_e_`num' =(f`n'.e-e)
}
capture matrix drop result
foreach num of numlist 1, 2, 5, 10 {

qui newey ek_minus_e_`num' rj_minus_r_`num', lag(2)
local obs=e(N)
local m=round(1.3*(`obs'^(1/2)))
 newey ek_minus_e_`num' rj_minus_r_`num', lag(`m') level(90)
mat a = r(table)
mat temp = (a[1,1],a[5,1],a[6,1])
//mat b = (1,b)	
//mat temp=temp,r(mean)
matrix result=nullmat(result)\temp
mat drop temp

}
	
mat l result
preserve
drop _all
svmat result
gen Horizon = .
replace Horizon = 6 in 1
replace Horizon = 12 in 2
replace Horizon = 60 in 3
replace Horizon = 120 in 4
rename result1 UIP
rename result2 CI_lower_bound
rename result3 CI_upper_bound
twoway rcap CI_lower_bound CI_upper_bound Horizon || connected UIP Horizon, ti("Figure 1: Estimated coefficients from canonical""UIP regression at different horizons")
graph export First_regression_graph.png, replace
export excel using "\\stf8\stf8\users\9\em17839\ECON UOA\GASS1\Regression.xls", replace
restore

gen sus = r10y_us-r3m_us
gen snz = r10y_nz-r3m_nz
gen sj_minus_s = (snz-sus	)/100	
	
	
capture matrix drop result2
foreach num of numlist 1, 2, 5, 10 {

qui newey ek_minus_e_`num' rj_minus_r_`num' sj_minus_s, lag(2)
local obs=e(N)
local m=round(1.3*(`obs'^(1/2)))
 newey ek_minus_e_`num' rj_minus_r_`num' sj_minus_s, lag(`m') level(90)
mat a = r(table)

mat temp = (a[1,2],a[5,2],a[6,2])
//mat b = (1,b)	
//mat temp=temp,r(mean)
matrix result2=nullmat(result2)\temp
mat drop temp

}
	
mat l result2
preserve
drop _all
svmat result2
gen Horizon = .
replace Horizon = 6 in 1
replace Horizon = 12 in 2
replace Horizon = 60 in 3
replace Horizon = 120 in 4
rename result21 Slope
rename result22 CI_lower_bound
rename result23 CI_upper_bound
twoway rcap CI_lower_bound CI_upper_bound Horizon || connected Slope Horizon, ti("Figure 2: Estimated relative slope" "coefficients from augmented UIP regression ")
graph export Altered_regression_graph.png, replace
export excel using  "\\stf8\stf8\users\9\em17839\ECON UOA\GASS1\Altered_regression.xls", replace
restore


preserve
newey ek_minus_e_1 rj_minus_r_1, lag(27) level(90)	//runs the regression
predict ek_minus_e_1p if e(sample) 
corr ek_minus_e_1 ek_minus_e_1p if e(sample)
local r2 = r(rho)^2 
local a_r2 = (1-((1-`r2')*((434-1)/(434-1-1))))
mat d = (`a_r2')
newey ek_minus_e_2 rj_minus_r_2, lag(27) level(90)	//runs the regression
predict ek_minus_e_2p if e(sample) 
corr ek_minus_e_2 ek_minus_e_2p if e(sample)
local r2 = r(rho)^2
local a_r2 = (1-((1-`r2')*((434-1)/(434-1-1))))
mat a = (`a_r2')
mat d = (d \ (a[1,1]))
newey ek_minus_e_5 rj_minus_r_5, lag(27) level(90)	//runs the regression
predict ek_minus_e_5p if e(sample) 
corr ek_minus_e_5 ek_minus_e_5p if e(sample)
local r2 = r(rho)^2 
local a_r2 = (1-((1-`r2')*((434-1)/(434-1-1))))
mat a = (`a_r2')
mat d = (d \ (a[1,1]))
newey ek_minus_e_10 rj_minus_r_10, lag(27) level(90)	//runs the regression
predict ek_minus_e_10p if e(sample) 
corr ek_minus_e_10 ek_minus_e_10p if e(sample)
local r2 = r(rho)^2 
local a_r2 = (1-((1-`r2')*((434-1)/(434-1-1))))
mat a = (`a_r2')
mat d = (d \ (a[1,1]))

restore
preserve

newey ek_minus_e_1 rj_minus_r_1 sj_minus_s, lag(27) level(90)	//runs the regression
predict ek_minus_e_1p if e(sample) 
corr ek_minus_e_1 ek_minus_e_1p if e(sample)
local r2 = r(rho)^2 
local a_r2 = (1-((1-`r2')*((434-1)/(434-2-1))))
mat e = (`a_r2')
newey ek_minus_e_2 rj_minus_r_2 sj_minus_s, lag(27) level(90)	//runs the regression
predict ek_minus_e_2p if e(sample) 
corr ek_minus_e_2 ek_minus_e_2p if e(sample)
local r2 = r(rho)^2 
local a_r2 = (1-((1-`r2')*((434-1)/(434-2-1))))
mat a = (`a_r2')
mat e = (e \ (a[1,1]))
newey ek_minus_e_5 rj_minus_r_5 sj_minus_s, lag(27) level(90)	//runs the regression
predict ek_minus_e_5p if e(sample) 
corr ek_minus_e_5 ek_minus_e_5p if e(sample)
local r2 = r(rho)^2 
local a_r2 = (1-((1-`r2')*((434-1)/(434-2-1))))
mat a = (`a_r2')
mat e = (e \ (a[1,1]))
newey ek_minus_e_10 rj_minus_r_10 sj_minus_s, lag(27) level(90)	//runs the regression
predict ek_minus_e_10p if e(sample) 
corr ek_minus_e_10 ek_minus_e_10p if e(sample)
local r2 = r(rho)^2 
local a_r2 = (1-((1-`r2')*((434-1)/(434-2-1))))
mat a = (`a_r2')
mat e = (e \ (a[1,1]))

clear 
svmat d
svmat e
gen Horizon = .
replace Horizon = 6 in 1
replace Horizon = 12 in 2
replace Horizon = 60 in 3
replace Horizon = 120 in 4
rename d1 adjusted_r2
rename e1 adjusted_r2_with_Slope
twoway connected adjusted_r2 Horizon || connected adjusted_r2_with_Slope Horizon, ti("Figure 3: Explanatory power of UIP regression augmented""with relative yield curve slope at different horizons")
graph export Adjusted-R2.png, replace
export excel using "\\stf8\stf8\users\9\em17839\ECON UOA\GASS1\Ajusted_r2.xls", replace
restore

