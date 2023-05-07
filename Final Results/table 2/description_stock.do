
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

			





						