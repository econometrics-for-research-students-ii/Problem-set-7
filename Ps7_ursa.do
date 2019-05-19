clear all
cd  "/Users/luigi/Library/Mobile Documents/com~apple~CloudDocs/Econometrics 2/Part 2/Problem set/PS7"

use "runandjump_sample1500g.dta", clear

	*Questions for emilio: can you use command hist directly
	

	*a
	count if bweight == .
	gen bwtcent = bweight-1500
	sum  bweight bwtcent

	
	
	*b
	
	/*not sure what the issue with using command hist is
	histogram bwtcent, width(1) frequency ///
	ytitle("Freq") ///
			xtitle("Birth weight (centered on 1500g)",) ///
			title("Birth weights: Bandwidth of `i'g", color(black)) ///
			graphregion(lcolor(white) fcolor(white))	
	graph save "hist_b1_a", replace*/
	
		for num 1 10 25: gen binX = X*(floor((bwtcent)/X)+.5)

		foreach i of numlist 1 10 25 {
		preserve
		collapse (count) bwtcent, by(bin`i')
		graph twoway (bar bwtcent bin`i', barwidth(`i')), ///
			ytitle("Freq") ///
			xtitle("Birth weight (centered on 1500g)",) ///
			title("Baby Weights w Bandwidth of `i'g") ///
		graph save "graph_proper_bin`i'", replace
		restore
	}
	/*graphs are not smooth at all*/
	
	*c 
	/*if some people self select just below the cutoff, ie you have bunching
	just bellow the cutoff that's a problem because people who have an incentive
	to report lower wage of their children might be the ones who would benefit
	from all the health care services more. In simple words babies below
	and above the cutoff are no longer comparable*/
	
	
	
	*d

		
	foreach i of numlist 1 10 25 {
		
		preserve
		collapse (count) bwtcent, by(bin`i')	
		g neg_b = bin`i'<0							
		gen neg_b_bin = neg_b*bin`i'				
			*testing
			reg bwtcent bin`i' neg_b neg_b_bin if inrange(bin`i',-50,50+ `i'), vce(robust)	// run regressions
			reg bwtcent bin`i' neg_b neg_b_bin if inrange(bin`i',-100,100+ `i'), vce(robust)	// run regressions
			reg bwtcent bin`i' neg_b neg_b_bin if inrange(bin`i',-150,150+ `i'), vce(robust)	// run regressions

			test neg_b neg_b_bin			
			

		*Polynomial
	rdplot bwtcent bin`i' if bin`i'>-50 & bin`i'<(50+ `i'), ci(1500) shade bwtcent ci(95) p(2) 
	rdplot bwtcent bin`i'  if bin`i'>-100 & bin`i'<(100+ `i'), ci(1500) shade bwtcent ci(95) p(2) 
	rdplot bwtcent bin`i' bin`i'>-150 & bin`i'<(150+ `i'), ci(1500) shade bwtcent ci(95) p(2) 

	*linear
	rdplot bwtcent bin`i' if bin`i'>-50 & bin`i'<(50+ `i'), ci(1500) shade bwtcent ci(95) p(1) 
	rdplot bwtcent bin`i'  if bin`i'>-100 & bin`i'<(100+ `i'), ci(1500) shade bwtcent ci(95) p() 
	rdplot bwtcent bin`i' if bin`i'>-150 & bin`i'<(150+ `i'), ci(1500) shade bwtcent ci(95) p(1) 

	
	graph twoway (lowess frequency bin`i' if abs(bin`i')<=50 & bin`i'<=0, bwidth(1)) ///
					 (lowess frequency bin`i' if abs(bin`i')<=50 & bin`i'>0, bwidth(1)) ///
					 (scatter frequency bin`i' if abs(bin`i')<=50), xline(0)
					 
	graph twoway (lowess frequency bin`i' if abs(bin`i')<=100 & bin`i'<=0, bwidth(1)) ///
					 (lowess frequency bin`i' if abs(bin`i')<=100 & bin`i'>0, bwidth(1)) ///
					 (scatter frequency bin`i' if abs(bin`i')<=100), xline(0)
					 
	graph twoway (lowess frequency bin`i' if abs(bin`i')<=150 & bin`i'<=0, bwidth(1)) ///
					 (lowess frequency bin`i' if abs(bin`i')<=150 & bin`i'>0, bwidth(1)) ///
					 (scatter frequency bin`i' if abs(bin`i')<=150), xline(0)
	
		restore
	}
	

*e

	tab mom_white,m 
	gen mom_white = mom_race == 1 //dummy for mother's race

	tab mom_ed1, missing 				
	


	gen LBW=bwtcent<0 //dummy for low bw
	replace LBW = . if bwtcent == .

	gen bwtcent_LBW = bwtcent*LBW //interactions

* RD ESTIMATES *
	estimates clear
	foreach var of varlist mom_white mom_ed1 {
	* Rectangular Kernel
		estimates store rd_rec_`var'

	*Triangular
		qui rd `var' bwtcent, z0(0) bwidth(30) mbw(100 200 300) cl(bwtcent)				// mbw 100 = bw of 30, mbw 200 = bw of 60, mbw 300 = bw of 90
		estimates store rd_tri_`var'
		
	}
	
* Tables *

	esttab rd_rec_mom_white rd_rec_mom_ed1
	esttab rd_tri_mom_white rd_tri_mom_ed1
	

*Q2


/*
*a) outcome: assimilation of immigrants using gap in fertility rate as a proxy
	treatment:policy reform
	being  an immigrant (1st/2nd generation) or having a spouse who is an immigrant

b) culture of having more kids are the unobservables

c) the fertility trends across different grouos are parallel before the reform

d) it seems to be internally valid, externally valid perhaps only for countries w similar trends



*/
