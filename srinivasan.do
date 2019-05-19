*==============================================================
*This code is for econometrics -2 PS7 

*Date: 08/may/2019
*Author: Krishna Srinivasan 
*krishna.srinivasan@econ.uzh.ch

*==============================================================

***************************************************************
*                      Initialize 
***************************************************************
version 14
set more off
cd "C:/Users/ksrini/OneDrive/Zurich/Courses/Trix_2/Exercises/PS7/analysis"

***************************************************************
*                    7.1
***************************************************************
use "runandjump_sample1500g", clear


***************** 
*a)
 
g bwtcent = bweight -1500

***************** 
* b)
gen bin1 = bwtcent + 0.5 

gen bin2 = .
forvalues i = -140(10)150 {
replace bin2 = `i' if bwtcent < `i' & bin2 == .
}

gen bin3 = .
forvalues i = -125(25)150 {
replace bin3 = `i' if bwtcent < `i' & bin3 == .
}
*

replace bin2 = bin2 -5 
replace bin3 = bin3 -12.5 

hist bin1, scheme(s1mono) bin(300) xline(0) freq
graph export "./graphs/hist1.png", replace 
hist bin2, scheme(s1mono) bin(30) xline(0) freq
graph export "./graphs/hist2.png", replace 
hist bin3, scheme(s1mono) bin(12) xline(0) freq
graph export "./graphs/hist3.png", replace 


***************** 
*d)

*Create bandwidths 
gen band1 = cond(bwtcent <= 150 & bwtcent >= -150, 1,0)  
gen band2 = cond(bwtcent <= 100 & bwtcent >= -100, 1,0)  
gen band3 = cond(bwtcent <= 50 & bwtcent >= -50, 1,0)  


forvalues j = 1(1)3 {
	preserve
	collapse (count) y = bwtcent (mean) bwtcent (min) band1 band2 band3, by(bin`j')
	egen tot = total(y)
	replace y = y/tot
	gen dummy = bin`j' > 0
	
	forvalues i = 1(1)3 {
		
	reg y c.bin`j'##dummy if band`i' == 1, vce(robust)
				 
// 	gen bin`j'_2 = bin`j'*bin`j'
// 	reg y c.bin`j'##dummy c.bin`j'_2##dummy if band`i' == 1

	graph twoway (lfitci y bin`j' if band`i' == 1 & bin`j'<=0) ///
				 (lfitci y bin`j' if band`i' == 1 & bin`j'>0) ///
				 scatter y bin`j' if band`i' == 1, xline(0) 
	graph export "./graphs/linear_bin`j'_band`i'.png", replace
			 
	graph twoway (lowess y bin`j' if band`i' == 1 & bin`j'<=0, bwidth(1)) ///
				 (lowess y bin`j' if band`i' == 1 & bin`j'>0, bwidth(1)) ///
				 (scatter y bin`j' if band`i' == 1), xline(0) 
	graph export "./graphs/lowess_bin`j'_band`i'.png", replace
	
	graph twoway (qfitci y bin`j' if band`i' == 1 & bin`j'<=0) ///
				 (qfitci y bin`j' if band`i' == 1 & bin`j'>0) ///
				  scatter y bin`j' if band`i' == 1, xline(0) 
    graph export "./graphs/pol_bin`j'_band`i'.png", replace
	}
	
	restore
}


***************** 
* e) 
gen band4 = cond(bwtcent <= 90 & bwtcent >= -90, 1,0)  
gen band5 = cond(bwtcent <= 60 & bwtcent >= -60, 1,0)  
gen band6 = cond(bwtcent <= 30 & bwtcent >= -30, 1,0)  

gen mom_dum = mom_race == 1
gen dummy = bwtcent > 0

forvalues i = 4(1)6 {
reg mom_dum c.bwtcent##dummy if band`i' == 1, vce(cluster bwtcent)
estimates store dum_`i'
reg mom_ed1 c.bwtcent##dummy if band`i' == 1, vce(cluster bwtcent)
estimates store edu_`i'
}
*

esttab dum_4 dum_5 dum_6 using "./tables/table1e1.tex", ///
cells(b(star fmt(3)) se(fmt(3) par)) ///
mtitle("band = 90" "band = 60" "band = 30") ///
starlevels( ^{*} 0.10 ^{**} 0.05 ^{***} 0.010) substitute (# \#) ///
title(Paramater estimates from OLS, race \label{tab:1e1}) ///
alignment(@{}l*{8}{D{.}{.}{3}}@{}) booktabs replace

esttab edu_4 edu_5 edu_6 using "./tables/table1e2.tex", ///
cells(b(star fmt(3)) se(fmt(3) par)) ///
mtitle("band = 90" "band = 60" "band = 30") ///
starlevels( ^{*} 0.10 ^{**} 0.05 ^{***} 0.010) substitute (# \#)  ///
title(Paramater estimates from OLS, education \label{tab:1e2}) ///
alignment(@{}l*{8}{D{.}{.}{3}}@{}) booktabs replace

***************** 
* g)
forvalues i = 4(1)6 {
reg agedth5 c.bwtcent##dummy if band`i' == 1, vce(cluster bwtcent)
estimates store cluster_`i'
reg agedth5 c.bwtcent##dummy if band`i' == 1, vce(robust)
estimates store robust_`i'
}
*

esttab cluster_4 cluster_5 cluster_6 using "./tables/table1g1.tex", ///
keep(bwtcent 1.dumm*) mtitle("band = 90" "band = 60" "band = 30") ///
cells(b(star fmt(3)) se(fmt(3) par)) ///
starlevels( ^{*} 0.10 ^{**} 0.05 ^{***} 0.010) ///
title(Paramater estimates from OLS, cluter se \label{tab:1g1}) ///
alignment(@{}l*{8}{D{.}{.}{3}}@{}) booktabs replace

esttab robust_4 robust_5 robust_6 using "./tables/table1g2.tex", ///
keep(bwtcent 1.dumm*) mtitle("band = 90" "band = 60" "band = 30") ///
cells(b(star fmt(3)) se(fmt(3) par)) ///
starlevels( ^{*} 0.10 ^{**} 0.05 ^{***} 0.010) ///
title(Paramater estimates from OLS, robust se \label{tab:1g2}) ///
alignment(@{}l*{8}{D{.}{.}{3}}@{}) booktabs replace

