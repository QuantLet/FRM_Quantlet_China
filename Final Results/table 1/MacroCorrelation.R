## Select Macro Variables

rm(list = ls(all = TRUE))

libraries = c("iml","ggplot2", "data.table", "igraph","timeDate", "stringr", "graphics","magick", "scales", "tidyr", "zoo", "foreach", "doParallel",
              "xgboost","shapr","randomForest", "rpart", "quantreg", "readxl","dplyr", "xlsx", "psych","qgraph", "gganimate","av",
              "gifski", "strex","matrixStats","tools")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#----------------------------------------START UPDATE----------------------------------------

wdir = "/Users/ruting/Documents/macbook/PcBack/FRM_Quantlet/FRM_All/"
setwd(wdir)
  
#Choose between "Americas", "Europe", "Crypto", "SP500", "ER", "Asia", "EM"
channel = "Asia"

#Data source
date_end_source = 20230306
#Index output, varying companies
date_start = 20190417
date_end = 20230228

#Network output, fixed companies
date_start_fixed = 20191210
date_end_fixed = 20230228
quantiles = c(0.99, 0.95, 0.90, 0.85, 0.80, 0.75, 0.70, 0.65, 0.60, 0.55, 0.50, 0.25)

#Estimation window size (63)
s = 63 
#Tail risk level (0.05) 0.1 0.25 0.5
tau = 0.05
if (channel == "ER") tau = 1 - tau
#Number of iterations (25)
I = 25   
#CoStress top and bottom L (5)
L = 5

#Number of largest companies, highlighted node for network graph,
#plot parameter defined based on the outliers
if (channel == "Americas") {
  date_start_source = 20190603
  stock_main = "??"
  J = 100} else 
if (channel == "Europe") {
  date_start_source = 20191203
  stock_main = "??"
  J = 100} else     
if (channel == "Crypto") {
  date_start_source = 20141128
  lambda_cutoff = 0.1359 
  stock_main = "BTC"
  J = 15} else 
if (channel == "ER") {
  date_start_source = 20180102
  stock_main = "??"
  J = 11} else 
if (channel == "Asia") {
  date_start_source = 20190102
  stock_main = "X601628.CH.EQUITY"
  lambda_cutoff = 0.4 
  J = 50} else
if (channel == "EM") {
  date_start_source = 20190101
  lambda_cutoff = 0.4 
  stock_main = "ELEKTRA..MF.Equity"
  J = 25} else
if (channel == "SP500") {
  date_start_source = 20190103
  stock_main = "TFC.UN.EQUITY"
  J = 100}
#-----------------------------------------END UPDATE-----------------------------------------

input_path = paste0("Input/", channel, "/", date_start_source, "-", date_end_source)
if (tau == 0.05) output_path = paste0("Output/", channel) else 
  output_path = paste0("Output/", channel, "/Sensitivity/tau=", 100*tau, "/s=", s)

#TODO choose between quantile and expectile in the header
source("FRM_Statistics_Algorithm.R")

## 1. Data Preprocess
mktcap =  read.csv(file = paste0(input_path, "/", channel, "_Mktcap_", 
                                 date_end_source, ".csv"), header = TRUE) %>% as.matrix()
mktcap = mktcap[,-(colnames(mktcap) %in% "X")]
mktcap = data.frame(mktcap)
mktcap[, -1] = sapply(mktcap[, -1], as.numeric)

mktcap_weight = mktcap
mktcap_weight[,-1] = 0
mktcap_weight[,-1] = mktcap[,-1]/rowSums(mktcap[, -1],na.rm = TRUE)
mktcap_weight = mktcap_weight[-1,]

stock_prices =  read.csv(file = paste0(input_path, "/", channel, "_Price_", 
                                       date_end_source, ".csv"), header = TRUE) %>% as.matrix()
stock_prices = stock_prices[,-(colnames(stock_prices) %in% "X")]
stock_prices = data.frame(stock_prices)

Name_Stock = names(stock_prices)
Name_Stock = Name_Stock[-1]

# calculate weighted index
all_prices = stock_prices
all_prices[, -1] = sapply(all_prices[, -1], as.numeric)
all_return = all_prices[-1, ]
all_return[,-1] = 0

all_return[,which(names(all_return)%in%Name_Stock)] = 
  as.data.frame(diff(log(as.matrix(all_prices[, which(names(all_prices)%in%Name_Stock)])))) 
all_return[is.na(all_return)] = 0

# # describe for return
# des_return = all_return[,2]
# for (i in c(3:189)){
#   des_return = c(des_return, all_return[,i])
# }
# des_return = des_return[des_return!=0]
# length(des_return)

all_return_temp = all_return
all_return_temp[,-1]= all_return_temp[,-1]*mktcap_weight[,-1]
all_return_temp[is.na(all_return_temp)] = 0
Fin_Index = data.frame(Date = all_return_temp$Date, Return = rowSums(all_return_temp[, -1]))
# Fin_Index$ticker = as.Date(Fin_Index$ticker)

# Realestate Infor
RealEstate = read_excel(paste0(input_path, "/Realestate.xlsx"))
colnames(RealEstate) = c('Date','Index')
RealEstate$RealEstateReturn = c(0,diff(log(RealEstate$Index)))

RealEstate$Date = paste0(substring(RealEstate$Date,1,4),'-', substring(RealEstate$Date,5,6),'-',substring(RealEstate$Date,7,8))
Fin_Index = merge(Fin_Index, RealEstate, by = "Date", all.x = TRUE)
Fin_Index$RealEstateDiff = Fin_Index$RealEstateReturn- Fin_Index$Return


# Bond Infor
BondYield = read_excel(paste0(input_path, "/BondYield.xlsx"))
BondYield = data.frame(BondYield)
colnames(BondYield) = c("Date","Bond_3M","Bond_10Y","Bond_2Y","Shibor_3M","AAA_10Y")

BondYield$Date = as.character( BondYield$Date)
BondYield$CN3M = c(0, BondYield$Bond_3M[c(2:nrow(BondYield))] - BondYield$Bond_3M[c(1:(nrow(BondYield)-1)) ])
BondYield$CN310SLOPE = BondYield$Bond_10Y - BondYield$Bond_3M
BondYield$TED = BondYield$Shibor_3M  - BondYield$Bond_3M
BondYield$CreSP =   BondYield$AAA_10Y  - BondYield$Bond_10Y
BondYield = merge(BondYield, Fin_Index, by = "Date", all.x = TRUE )

# first delete na, then log return!
VIX = read_excel(paste0(input_path, "/vix.xlsx"))
VIX$Date = as.character( VIX$Date)
colnames(VIX) = c('Date','FXI.US.EQUITY','VXFXI')
VIX[,-1]= sapply(VIX[, -1], as.numeric)
VIX = VIX[!is.na(VIX$FXI.US.EQUITY),]

VIX$MKRturn = c(0,diff(log(VIX$FXI.US.EQUITY)))

BondYield = merge(VIX, BondYield, by = "Date", all.x = TRUE )
BondYield = BondYield[!is.na(BondYield$RealEstateDiff) & !is.na(BondYield$MKRturn) ,]

Index = BondYield[,c('Date','MKRturn','CN3M','CN310SLOPE','TED','CreSP','RealEstateDiff')]

write.csv(Index,paste0(input_path,"/MacroIndex.csv"),quote = FALSE)

corr = rcorr( as.matrix(Index[-1,-1]),type = c("pearson","spearman"))
corr

corr = rcorr( as.matrix(Index[-1,c('MKRturn','TED','RealEstateDiff')]),type = c("pearson","spearman"))
corr

write.csv(corr$r,paste0(input_path,"/MacroCorr_0320.csv"),quote = FALSE)
write.csv(corr$P,paste0(input_path,"/MacroCorrP_0320.csv"),quote = FALSE)





