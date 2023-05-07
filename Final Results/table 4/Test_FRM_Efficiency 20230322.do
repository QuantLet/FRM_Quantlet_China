
clear
global date = "20230322"
global root = "/Users/ruting/Documents/macbook/PcBack/FRM_Quantlet/FRM_All/"

*use $root/Output/Asia/Stata_FRM_20220523.dta, clear
*use $root/Output/Asia/Stata_FRM_20230321.dta, clear
use $root/Output/Asia/Stata_FRM_20230409.dta, clear
encode Stkcd, gen(Stkcd_n)
gen Date_n = date(Date,"YMD")
drop Stkcd 
xtset Date_n Stkcd_n   
gen log_SRisk = log(SRisk+1)

winsor2 Vola VaR_5P DeltaVaR SRisk log_SRisk Leverage maturity MB_Rank Size, replace cuts (1 99)


asdoc sum FRM_L5 FRM_L21 FRM_L5 Vola VaR_5P DeltaVaR SRisk log_SRisk Leverage maturity MB_Rank Size , detail ///
			  , save($root/Output/summary_FRMCausal.doc) replace

global control = "Leverage maturity MB_Rank Size"

					
// prediction test
reghdfe Vola FRM_L5 Date_n $control, absorb(Stkcd sector) cluster(Stkcd)
outreg2 using $root/Output/FRM_lag5_Causality.xls, ///
			stat(coef se) bdec(4) sdec(3)title("FRM") replace  drop(_I** _est* o.**)  ///
			addtext(Controls, Yes, Firm FE, YES, Industry FE, YES)
			
reghdfe VaR_5P FRM_L5 Date_n $control, absorb(Stkcd sector) cluster(Stkcd)
outreg2 using $root/Output/FRM_lag5_Causality.xls, ///
			stat(coef se) bdec(4) sdec(3)title("FRM") append  drop(_I** _est* o.**)  ///
			addtext(Controls, Yes, Firm FE, YES, Industry FE, YES)		
					
reghdfe DeltaVaR FRM_L5 Date_n $control, absorb(Stkcd sector) cluster(Stkcd)
outreg2 using $root/Output/FRM_lag5_Causality.xls, ///
			stat(coef se) bdec(4) sdec(3)title("FRM") append  drop(_I** _est* o.**)  ///
			addtext(Controls, Yes, Firm FE, YES, Industry FE, YES)
			
reghdfe log_SRisk FRM_L5 Date_n $control, absorb(Stkcd sector) cluster(Stkcd)
outreg2 using $root/Output/FRM_lag5_Causality.xls, ///
			stat(coef se) bdec(4) sdec(3)title("FRM") append  drop(_I** _est* o.**)  ///
			addtext(Controls, Yes, Firm FE, YES, Industry FE, YES)

reghdfe Vola FRM_L21 Date_n $control, absorb(Stkcd sector) cluster(Stkcd)
outreg2 using $root/Output/FRM_lag21_Causality.xls, ///
			stat(coef se) bdec(4) sdec(3)title("FRM") replace  drop(_I** _est* o.**)  ///
			addtext(Controls, Yes, Firm FE, YES, Industry FE, YES)
						
reghdfe VaR_5P FRM_L21 Date_n $control, absorb(Stkcd sector) cluster(Stkcd)
outreg2 using $root/Output/FRM_lag21_Causality.xls, ///
			stat(coef se) bdec(4) sdec(3)title("FRM") append  drop(_I** _est* o.**)  ///
			addtext(Controls, Yes, Firm FE, YES, Industry FE, YES)
			
reghdfe DeltaVaR FRM_L21 Date_n $control, absorb(Stkcd sector) cluster(Stkcd)
outreg2 using $root/Output/FRM_lag21_Causality.xls, ///
			stat(coef se) bdec(4) sdec(3)title("FRM") append  drop(_I** _est* o.**)  ///
			addtext(Controls, Yes, Firm FE, YES, Industry FE, YES)
			
reghdfe log_SRisk FRM_L21 Date_n $control, absorb(Stkcd sector) cluster(Stkcd)
outreg2 using $root/Output/FRM_lag21_Causality.xls, ///
			stat(coef se) bdec(4) sdec(3)title("FRM") append  drop(_I** _est* o.**)  ///
			addtext(Controls, Yes, Firm FE, YES, Industry FE, YES)

			





						