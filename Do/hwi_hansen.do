clear

use "C:\Users\Bernardo\Documents\GitHub\Hansen-DUI\Data\hansen_dwi.dta"

outreg2 using test.doc, replace

gen dui = 0
replace dui = 1 if bac1 >= 0.08

gen age= aged/100

qui reg acc bac1 dui bac_dui if bac1>=.03 & 0.13>=bac1, robust
estimates store Acc
qui reg male bac1 dui bac_dui if bac1>=.03 & 0.13>=bac1, robust
estimates store Male
qui reg aged bac1 dui bac_dui if bac1>=.03 & 0.13>=bac1, robust
estimates store Age
qui reg white bac1 dui bac_dui if bac1>=.03 & 0.13>=bac1, robust
estimates store White

estimates table Acc Male Age White, se(%7.2f)

keep if bac1 >= 0.03 & bac1 <= 0.13
qui rdrobust acc bac1, c(0.08) h(0.05) kernel(uni) vce(hc0)
estimates store Acc
qui rdrobust male bac1, c(0.08) h(0.05) kernel(uni) vce(hc0)
estimates store Male
qui rdrobust aged bac1, c(0.08) h(0.05) kernel(uni) vce(hc0)
estimates store Age
qui rdrobust white bac1, c(0.08) h(0.05) kernel(uni) vce(hc0)
estimates store White


asdoc esttab Acc Male Age White, scalars(tau_cl_l)

cmogram acc bac1, cut(0.08) cutright scatter l(0.08) lfitci h(width(0.002)) g(xlabel(0.03(0.01)0.13) title("Panel A. Accident at scene") xtitle("BAC") ytitle(""))
cmogram male bac1, cut(0.08) cutright scatter l(0.08) lfitci h(width(0.002)) g(xlabel(0.03(0.01)0.13) title("Panel B. Male") xtitle("BAC") ytitle(""))
cmogram age bac1, cut(0.08) cutright scatter l(0.08) lfitci h(width(0.002)) g(xlabel(0.03(0.01)0.13) title("Panel C. Age") xtitle("BAC") ytitle(""))
cmogram white bac1, cut(0.08) cutright scatter l(0.08) lfitci h(width(0.002)) g(xlabel(0.03(0.01)0.13) title("Panel D. White") xtitle("BAC") ytitle(""))

cmogram acc bac1, cut(0.08) cutright scatter l(0.08) qfitci h(width(0.002)) g(xlabel(0.03(0.01)0.13) title("Panel A. Accident at scene") xtitle("BAC") ytitle(""))
cmogram male bac1, cut(0.08) cutright scatter l(0.08) qfitci h(width(0.002)) g(xlabel(0.03(0.01)0.13) title("Panel B. Male") xtitle("BAC") ytitle(""))
cmogram age bac1, cut(0.08) cutright scatter l(0.08) qfitci h(width(0.002)) g(xlabel(0.03(0.01)0.13) title("Panel C. Age") xtitle("BAC") ytitle(""))
cmogram white bac1, cut(0.08) cutright scatter l(0.08) qfitci h(width(0.002)) g(xlabel(0.03(0.01)0.13) title("Panel D. White") xtitle("BAC") ytitle(""))

graph save fig1.gph,replace
gr export temp.png, height(270) width(360) replace
png2rtf using ch1.doc, g(temp.png) replace

graph save fig2.gph,replace
gr export temp.png, height(270) width(360) replace
png2rtf using ch1.doc, g(temp.png) a

gen bac_dui = bac1*dui
gen bacsq_dui = (bac1^2)*dui

qui reg recidivism bac1 dui acc male aged white, robust
estimates store Linear
qui reg recidivism bac1 dui bac_dui acc male aged white, robust
estimates store Interaction
qui reg recidivism bac1 dui bac_dui bacsq_dui acc male aged white, robust
estimates store Interaction_sq

estimates table Linear Interaction Interaction_sq, stats(r2) star

qui rdrobust recidivism bac1, p(0) covs(acc male aged white) c(0.08) h(0.05) kernel(uni) vce(hc0)
estimates store Linear
qui rdrobust recidivism bac1, covs(acc male aged white) c(0.08) h(0.05) kernel(uni) vce(hc0)
estimates store Interaction
qui rdrobust recidivism bac1, p(2) covs(acc male aged white) c(0.08) h(0.05) kernel(uni) vce(hc0)
estimates store Interaction_sq

estimates table Linear Interaction Interaction_sq, stats(se_tau_rb tau_cl_l N) star

keep if bac1 >= 0.055 & bac1 <= 0.105

qui reg recidivism bac1 dui acc male aged white, robust
estimates store Linear
qui reg recidivism bac1 dui bac_dui acc male aged white, robust
estimates store Interaction
qui reg recidivism bac1 dui bac_dui bacsq_dui acc male aged white, robust
estimates store Interaction_sq

estimates table Linear Interaction Interaction_sq, stats(r2) star

qui rdrobust recidivism bac1, p(0) covs(acc male aged white) c(0.08) h(0.025) kernel(uni) vce(hc0)
estimates store Linear
qui rdrobust recidivism bac1, covs(acc male aged white) c(0.08) h(0.025) kernel(uni) vce(hc0)
estimates store Interaction
qui rdrobust recidivism bac1, p(2) covs(acc male aged white) c(0.08) h(0.025) kernel(uni) vce(hc0)
estimates store Interaction_sq

estimates table Linear Interaction Interaction_sq, stats(se_tau_rb tau_cl_l N) star

keep if bac1 >= 0.03 & bac1 < 0.15

cmogram recidivism bac1, con(male white aged acc) cut(0.08) cutright scatter l(0.08) lfit h(width(0.002)) g(xlabel(0.03(0.01)0.15) title("Panel A. All offenders") xtitle("BAC") ytitle(""))

cmogram recidivism bac1, con(male white aged acc) cut(0.08) cutright scatter l(0.08) qfit h(width(0.002)) g(xlabel(0.03(0.01)0.15) title("Panel A. All offenders") xtitle("BAC") ytitle(""))

putdocx begin
putdocx table Table1=etable
putdocx table Table2=etable
putdocx save example5

graph save fig1.gph,replace
gr export temp.png, height(270) width(360) replace
png2rtf using ch1.doc, g(temp.png) replace

graph save fig2.gph,replace
gr export temp.png, height(270) width(360) replace
png2rtf using ch1.doc, g(temp.png) a
