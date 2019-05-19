***********************************************
* Problem set 7, Econometrics
***********************************************

clear
clear all
cd "/Users/giacomomangiante/Desktop/Zurich/Semester II/Econometrics/part 2/PS 3/"

***********************************************
* 7.1
***********************************************
use "./runandjump_sample1500g.dta", clear
set more off

* a)
gen bwtcent = bweight - 1500

* b)
gen bin1 = bwtcent +0.5

gen bin2 = .
forvalues i = -140(10)150 {
replace bin2 = `i' if bwtcent < `i' & bin2 == .
}
*

gen bin3 = .
forvalues i = -125(25)150 {
replace bin3 = `i' if bwtcent < `i' & bin3 == .
}
*

replace bin2 = bin2 -5 
replace bin3 = bin3 -12.5 

hist bin1, bin(300) xline(0) freq
graph export "./graphs/hist1.png", replace 
hist bin2, bin(30) xline(0) freq
graph export "./graphs/hist2.png", replace 
hist bin3, bin(12) xline(0) freq
graph export "./graphs/hist3.png", replace 

/*
cmogram agedth1 bwtcent, cut(0) histopts(bin(150))
cmogram agedth1 bwtcent, cut(0) count histopts(bin(15))
cmogram agedth1 bwtcent, cut(0) count histopts(bin(6))
*/

* d)
gen band1 = bwtcent <= 150 
replace band1 = 0 if bwtcent < - 150
gen band2 = bwtcent <= 100 
replace band2 = 0 if bwtcent < - 100
gen band3 = bwtcent <= 50 
replace band3 = 0 if bwtcent < - 50

forvalues i = 1(1)3 {
preserve
collapse (count) y = bwtcent (mean) bwtcent (min) band1 band2 band3, by(bin1)
egen tot = total(y)
replace y = y/tot
gen dummy = bin1 > 0
reg y c.bin1##dummy if band`i' == 1, vce(robust)
			 
gen bin1_2 = bin1*bin1
reg y c.bin1##dummy c.bin1_2##dummy if band`i' == 1
/*
graph twoway (lfitci y bin1 if band`i' == 1 & bin1<=0) ///
			 (lfitci y bin1 if band`i' == 1 & bin1>0) ///
			 scatter y bin1 if band`i' == 1, xline(0)
graph export "./graphs/linear_bin1_band`i'.png", replace 
		 
graph twoway (lowess y bin1 if band`i' == 1 & bin1<=0, bwidth(1)) ///
			 (lowess y bin1 if band`i' == 1 & bin1>0, bwidth(1)) ///
			 (scatter y bin1 if band`i' == 1), xline(0)
graph export "./graphs/lowess_bin1_band`i'.png", replace 			 
graph twoway (qfitci y bin1 if band`i' == 1 & bin1<=0) ///
			 (qfitci y bin1 if band`i' == 1 & bin1>0) ///
			  scatter y bin1 if band`i' == 1, xline(0)
graph export "./graphs/pol_bin1_band`i'.png", replace 
*/				 
drop dummy tot bin1_2
restore
}
*


forvalues i = 1(1)3 {
preserve
collapse (count) y = bwtcent (mean) bwtcent (min) band1 band2 band3, by(bin2)
egen tot = total(y)
replace y = y/tot
gen dummy = bin2 > 0
reg y c.bin2##dummy if band`i' == 1, vce(robust)
			 
gen bin2_2 = bin2*bin2
reg y c.bin2##dummy c.bin2_2##dummy if band`i' == 1

graph twoway (lfitci y bin2 if band`i' == 1 & bin2<=0) ///
			 (lfitci y bin2 if band`i' == 1 & bin2>0) ///
			 scatter y bin2 if band`i' == 1, xline(0) title("Linear graph, bin 10 grams, bandwidths `i'")
graph export "./graphs/linear_bin2_band`i'.png", replace 
		 
graph twoway (lowess y bin2 if band`i' == 1 & bin2<=0, bwidth(1)) ///
			 (lowess y bin2 if band`i' == 1 & bin2>0, bwidth(1)) ///
			 (scatter y bin2 if band`i' == 1), xline(0) title("Lowess graph, bin 10 grams, bandwidths `i'")
graph export "./graphs/lowess_bin2_band`i'.png", replace 			 
graph twoway (qfitci y bin2 if band`i' == 1 & bin2<=0) ///
			 (qfitci y bin2 if band`i' == 1 & bin2>0) ///
			  scatter y bin2 if band`i' == 1, xline(0) title("Quadratic graph, bin 10 grams, bandwidths `i'")
graph export "./graphs/pol_bin2_band`i'.png", replace 				 
drop dummy tot bin2_2
restore
}
*

forvalues i = 1(1)3 {
preserve
collapse (count) y = bwtcent (mean) bwtcent (min) band1 band2 band3, by(bin3)
egen tot = total(y)
replace y = y/tot
gen dummy = bin3 > 0
reg y c.bin3##dummy if band`i' == 1, vce(robust)
			 
gen bin3_2 = bin3*bin3
reg y c.bin3##dummy c.bin3_2##dummy if band`i' == 1
/*
graph twoway (lfitci y bin3 if band`i' == 1 & bin3<=0) ///
			 (lfitci y bin3 if band`i' == 1 & bin3>0) ///
			 scatter y bin3 if band`i' == 1, xline(0)
graph export "./graphs/linear_bin3_band`i'.png", replace 
		 
graph twoway (lowess y bin3 if band`i' == 1 & bin3<=0, bwidth(1)) ///
			 (lowess y bin3 if band`i' == 1 & bin3>0, bwidth(1)) ///
			 (scatter y bin3 if band`i' == 1), xline(0)
graph export "./graphs/lowess_bin3_band`i'.png", replace 			 
graph twoway (qfitci y bin3 if band`i' == 1 & bin3<=0) ///
			 (qfitci y bin3 if band`i' == 1 & bin3>0) ///
			  scatter y bin3 if band`i' == 1, xline(0)
graph export "./graphs/pol_bin3_band`i'.png", replace 
*/				 
drop dummy tot bin3_2
restore
}
*

* e) 
gen band4 = bwtcent <= 90
replace band4 = 0 if bwtcent < - 90
gen band5 = bwtcent <= 60
replace band5 = 0 if bwtcent < - 60
gen band6 = bwtcent <= 30 
replace band6 = 0 if bwtcent < - 30

gen mom_dum = mom_race == 1
gen dummy = bwtcent > 0

forvalues i = 4(1)6 {
reg mom_dum c.bwtcent##dummy if band`i' == 1, vce(cluster bwtcent)
estimates store dum_`i'
reg mom_ed1 c.bwtcent##dummy if band`i' == 1, vce(cluster bwtcent)
estimates store edu_`i'
}
*

esttab dum_4 dum_5 dum_6 using "./tables/mom_race.tex", ///
keep(bwtcent 1.dumm*) mtitle("OLS, band = 90" "OLS, band = 60" "OLS, band = 30") ///
starlevels( ^{*} 0.10 ^{**} 0.05 ^{***} 0.010) ///
title(Paramater estimates from OLS, race, cluter se \label{tab:1a}) ///
alignment(@{}l*{8}{D{.}{.}{3}}@{}) booktabs replace

esttab edu_4 edu_5 edu_6 using "./tables/mom_edu.tex", ///
keep(bwtcent 1.dumm*) mtitle("OLS, band = 90" "OLS, band = 60" "OLS, band = 30") ///
starlevels( ^{*} 0.10 ^{**} 0.05 ^{***} 0.010) ///
title(Paramater estimates from OLS, education, cluter se \label{tab:2a}) ///
alignment(@{}l*{8}{D{.}{.}{3}}@{}) booktabs replace

/*
forvalues i = 1(1)3 {
preserve
collapse (mean) bwtcent (min) band4 band5 band6 (mean) mom_ed1, by(bin`i')
bs, reps(100) cluster(bin`i') : rd_obs mom_ed1 bin`i' if band6 == 1, bwidth(30) kernel(rec) z0(0) 
bs, reps(100) cluster(bin`i') : rd_obs mom_ed1 bin`i' if band5 == 1, bwidth(60) kernel(rec) z0(0) 
bs, reps(100) cluster(bin`i') : rd_obs mom_ed1 bin`i' if band4 == 1, bwidth(90) kernel(rec) z0(0)  
restore
}
*/

* f)


* g)
forvalues i = 4(1)6 {
reg agedth5 c.bwtcent##dummy if band`i' == 1, vce(cluster bwtcent)
estimates store cluster_`i'
reg agedth5 c.bwtcent##dummy if band`i' == 1, vce(robust)
estimates store robust_`i'
}
*

esttab cluster_4 cluster_5 cluster_6 using "./tables/agedth5_cluster.tex", ///
keep(bwtcent 1.dumm*) mtitle("OLS, band = 90" "OLS, band = 60" "OLS, band = 30") ///
starlevels( ^{*} 0.10 ^{**} 0.05 ^{***} 0.010) ///
title(Paramater estimates from OLS, cluter se \label{tab:3a}) ///
alignment(@{}l*{8}{D{.}{.}{3}}@{}) booktabs replace

esttab robust_4 robust_5 robust_6 using "./tables/agedth5_robust.tex", ///
keep(bwtcent 1.dumm*) mtitle("OLS, band = 90" "OLS, band = 60" "OLS, band = 30") ///
starlevels( ^{*} 0.10 ^{**} 0.05 ^{***} 0.010) ///
title(Paramater estimates from OLS, robust se \label{tab:4a}) ///
alignment(@{}l*{8}{D{.}{.}{3}}@{}) booktabs replace

* h)
preserve
drop if bwtcent == 0

forvalues i = 4(1)6 {
reg agedth5 c.bwtcent##dummy if band`i' == 1, vce(cluster bwtcent)
estimates store cluster_drop_`i'
reg agedth5 c.bwtcent##dummy if band`i' == 1, vce(robust)
*estimates store robust_drop_`i'
}
*

esttab cluster_drop_4 cluster_drop_5 cluster_drop_6 using "./tables/agedth5_drop_cluster.tex", ///
keep(bwtcent 1.dumm*) mtitle("OLS, band = 90" "OLS, band = 60" "OLS, band = 30") ///
starlevels( ^{*} 0.10 ^{**} 0.05 ^{***} 0.010) ///
title(Paramater estimates from OLS, with drop, cluter se \label{tab:5a}) ///
alignment(@{}l*{8}{D{.}{.}{3}}@{}) booktabs replace
restore

* i)
preserve
collapse (count) y = bwtcent (mean) agedth5 bwtcent (min) band4 band5 band6, by(bin1)
graph twoway (lfitci agedth5 bwtcent if band5 == 1 & bwtcent<=0) ///
			 (lfitci agedth5 bwtcent if band5 == 1 & bwtcent>0) ///
			 scatter agedth5 bwtcent if band5 == 1, xline(0) title("Linear graph, bandwidths 60")
restore
graph export "./graphs/linear_final.png", replace 

