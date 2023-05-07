# data description


rm(list = ls(all = TRUE))

libraries = c("iml","ggplot2", "data.table", "igraph","timeDate", "stringr", "graphics","magick", "scales", "tidyr", "zoo", "foreach", "doParallel",
              "xgboost","shapr","randomForest", "rpart", "quantreg", "readxl","dplyr", "xlsx", "psych","qgraph", "gganimate","av",
              "gifski", "strex","matrixStats","tools","Hmisc","vars","aTSA","quantreg","rapport","sjmisc","haven","foreign")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

wdir = "/Users/ruting/Documents/macbook/PcBack/FRM_Quantlet/FRM_All/"
setwd(wdir)
channel = "Asia"
date_start_source = 20190102
date_end_source =  20230306
save_date = 20230321

input_path = paste0("Input/", channel, "/", date_start_source, "-", date_end_source)

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

if (!all(sort(colnames(mktcap)) == sort(colnames(stock_prices)))) 
  stop("columns do not match")

M_stock = ncol(mktcap)-1

# WRT change 
Name_Stock = names(all_return)
Name_Stock = Name_Stock[-1]

Macro_Total = c('MKRturn','TED','RealEstateDiff')
macro = Index[,c('Date', Macro_Total)]
colnames(macro)[1] = "ticker"

# change column positions of price and market value

fun_loc = function(x){
  loc = which(f$StockID_Raw == x)
  return(loc)
}


f = read_excel(paste0(input_path, "/","Name_Stock.xlsx"),sheet = 'Add Short Name')
f = data.frame(f)

Firms = data.frame(StockID_Raw = Name_Stock, sector = NA, ShortName = NA)
Firms[,c("sector", "ShortName")] = f[unlist(lapply(Firms$StockID_Raw, fun_loc)),c("GICS_INDUSTRY_GROUP_NAME", "ShortName")]
Firms$sector[!Firms$sector %in% c("Banks", "Diversified Financials", "Insurance")] = "Others"
Firms$Place = NA
Firms$Place[which(grepl("*CH.EQUITY",Firms$StockID_Raw) | grepl("*CH.Equity",Firms$StockID_Raw))] = '1_ML'
Firms$Place[which(grepl("*HK.EQUITY",Firms$StockID_Raw))] = '2_HK'
Firms$Place[which(grepl("*TT.EQUITY",Firms$StockID_Raw))] = '3_TW'
Firms = Firms[order(Firms$sector, Firms$Place),]


reorder = NA
for (i in Firms$StockID_Raw){
  reorder = rbind(reorder, which(names(all_return) == i))
}
reorder = reorder[-1]
all_return = all_return[, c(1,reorder)]
mktcap = mktcap[, c(1,reorder)]

if (!all(!is.na(macro))) stop("missing macros")


#Note: if missing market caps are kept NA, the column will be excluded 
#from top J  => do not fill up NA in mktcap
mktcap[is.na(mktcap)] = 0

colnames(mktcap)[1] = "ticker"
colnames(all_return)[1] = "ticker"

#Load the stock prices and macro-prudential data matrix
all_return = merge(all_return, macro, by = "ticker", all.x = TRUE)
keep = which(!is.na(all_return$MKRturn))
all_return = all_return[keep,]

ticker_str = as.data.frame(all_return$ticker)
colnames(ticker_str) = "ticker"
ticker = as.numeric(gsub("-", "", ticker_str$ticker))

N = nrow(ticker_str)

#Align mktcap rows to all_return
mktcap = merge(ticker_str, mktcap, by = "ticker", all.x = TRUE)
mktcap[, -1] = sapply(mktcap[, -1], as.numeric)

if (!all(mktcap$ticker == ticker_str$ticker)) stop("dates do not match")

#Calculate the daily return and differences matrix of all selected financial 
#companies and macro-prudential variables; use exponential function for selected
#macro-prudential variables that are expressed in first order differences

stock_return = all_return[, 2 : (M_stock+1)]
macro_return = all_return[, (M_stock+2):ncol(all_return)]

#Sorting the market capitalization data
FRM_sort = function(data) {sort(as.numeric(data), decreasing = TRUE, index.return = TRUE)}
#Determining the index number of each company
#according to decreasing markefggt capitalization
mktcap_index = matrix(0, N, M_stock)
mktcap_sort = apply(mktcap[, -1], 1, FRM_sort)
for (t in 1:N) mktcap_index[t,] = mktcap_sort[[t]]$ix


# colnames(mktcap_index)[-1] =c(1:(ncol(mktcap_index)-1))
# colnames(stock_return) =c(1:ncol(stock_return))

# mktcap_index = data.frame(mktcap_index)
# stock_return = data.frame(stock_return)

stock_return = as.matrix(stock_return)
mktcap = as.matrix(mktcap[,-1])
mktcap[mktcap<0] = NA
mktcap[mktcap==0] = NA
sta_stock = c(nrow(stock_return),mean(stock_return,na.rm = TRUE), sd(stock_return), median(stock_return), min(stock_return), max(stock_return))
sta_mktcap = c(nrow(mktcap),mean(mktcap, na.rm = TRUE), sd(mktcap, na.rm = TRUE), median(mktcap, na.rm = TRUE), min(mktcap, na.rm = TRUE), max(mktcap, na.rm = TRUE))
sta = rbind(sta_stock,sta_mktcap)

for (i in Macro_Total){
  sta = rbind(sta,c(nrow(mktcap_index),mean(macro_return[, i],na.rm= TRUE), sd(macro_return[, i]), median(macro_return[, i]), min(macro_return[, i]), max(macro_return[, i])))
}

  
# read FRM
select_mac = 'MKRturn'
FRM_5P  = data.frame(read.csv(paste0(wdir,'Output/Asia/20230228/',select_mac,'/Adj_Matrices_ShapleyCom/Comb_2_plus/1/Lambda_DynamicFactor/FRM_Asia_index.csv')))
colnames(FRM_5P)[2] = 'FRM_5P'
sta = rbind(sta,c(nrow(FRM_5P),mean(FRM_5P[, 2],na.rm= TRUE), sd(FRM_5P[, 2]), median(FRM_5P[, 2]), min(FRM_5P[, 2]), max(FRM_5P[, 2])))

FRM_1P  = data.frame(read.csv(paste0(wdir,'Output/Asia/20230228/Sensitivity/tau=1/s=63/20230228/',select_mac,'/Adj_Matrices_ShapleyCom/Comb_2_plus/1/Lambda_DynamicFactor/FRM_Asia_index.csv')))
colnames(FRM_1P)[2] = 'FRM_1P'
sta = rbind(sta,c(nrow(FRM_1P),mean(FRM_1P[, 2],na.rm= TRUE), sd(FRM_1P[, 2]), median(FRM_1P[, 2]), min(FRM_1P[, 2]), max(FRM_1P[, 2])))


FRM_10P  = data.frame(read.csv(paste0(wdir,'Output/Asia/20230228/Sensitivity/tau=10/s=63/20230228/',select_mac,'/Adj_Matrices_ShapleyCom/Comb_2_plus/1/Lambda_DynamicFactor/FRM_Asia_index.csv')))
colnames(FRM_10P)[2] = 'FRM_10P'
sta = rbind(sta,c(nrow(FRM_10P),mean(FRM_10P[, 2],na.rm= TRUE), sd(FRM_10P[, 2]), median(FRM_10P[, 2]), min(FRM_10P[, 2]), max(FRM_10P[, 2])))

# VaR 
VaR_5p =  data.frame(read.csv(file = paste0(wdir,"VaR_5P_Index_0.05_20230228.csv"), header = TRUE) %>% as.matrix())
VaR_5p = VaR_5p[,-1]
VaR_5p[,-1] = as.numeric(VaR_5p[,-1])
sta = rbind(sta,c(nrow(VaR_5p),mean(VaR_5p[, 2],na.rm= TRUE), sd(VaR_5p[, 2]), median(VaR_5p[, 2]), min(VaR_5p[, 2]), max(VaR_5p[, 2])))

# read Covar
stock_prices = read.csv(file = paste0(wdir,input_path, "/", channel, "_Price_", 
                                      date_end_source, ".csv"), header = TRUE)
stock_prices = stock_prices[,-(colnames(stock_prices) %in% "X")]
stock_prices = data.frame(stock_prices)

# calculate weighted index
all_prices = stock_prices
all_prices[, -1] = sapply(all_prices[, -1], as.numeric)
all_return = all_prices[-1, ]
all_return[,-1] = 0

all_return[,which(names(all_return)%in%Name_Stock)] = 
  as.data.frame(diff(log(as.matrix(all_prices[, which(names(all_prices)%in%Name_Stock)])))) 
all_return[is.na(all_return)] = 0

col = colnames(all_return)
VaR = read.csv(file = paste0(wdir,"/VaR_movingwindows_0.05_20230228.csv"), header = TRUE)
VaR = VaR[,-1]
for  (iStock in c(2:ncol(VaR))){
  Temp = data.frame("Date" = VaR$ticker, "ID" = colnames(VaR)[iStock], "VaR_5P" = VaR[,iStock])
  if (iStock == 2){
    Stata_VaR = Temp
  }else {
    Stata_VaR = rbind(Stata_VaR, Temp)
  }
}
Stata_VaR_5P  = Stata_VaR[grepl("*CH.EQUITY",Stata_VaR$ID) | grepl("*CH.Equity",Stata_VaR$ID), ]


date = VaR$ticker[-c(1:62)]
omit = c(70, 75, 184, 185, 186, 187, 188)
seq = seq(2,length(col),1)
seq = seq[!seq %in% omit]
for (iStock in seq){
  CoVaR  = data.frame(read.csv(paste0(wdir, "CoVaR/CoVaR_L", iStock, "_20230228.csv")))
  CoVaR = data.frame("Date" = date, "ID" = col[iStock], "CoVaR" = CoVaR[,2])  
  if (iStock == 2){
    Stata_CoVaR = CoVaR
  }else{
    Stata_CoVaR = rbind(Stata_CoVaR, CoVaR)
  }
}
Stata_CoVaR  = Stata_CoVaR[grepl("*CH.EQUITY",Stata_CoVaR$ID) | grepl("*CH.Equity",Stata_CoVaR$ID), ]

# read Covar 50p
col = colnames(all_return)
date = VaR$ticker[-c(1:62)]
for (iStock in seq){
  CoVaR  = data.frame(read.csv(paste0(wdir, "CoVaR_50p/CoVaR_L", iStock, "_20230228.csv")))
  CoVaR = data.frame("Date" = date, "ID" = col[iStock], "CoVaR_50p" = CoVaR[,2])  
  if (iStock == 2){
    Stata_CoVaR_50p = CoVaR
  }else{
    Stata_CoVaR_50p = rbind(Stata_CoVaR_50p, CoVaR)
  }
}
Stata_CoVaR_50p  = Stata_CoVaR_50p[grepl("*CH.EQUITY",Stata_CoVaR_50p$ID) | grepl("*CH.Equity",Stata_CoVaR_50p$ID), ]

Stata_CoVaR = merge(Stata_CoVaR, Stata_CoVaR_50p, all.x = TRUE)
Stata_CoVaR$DeltaVaR = Stata_CoVaR$CoVaR - Stata_CoVaR$CoVaR_50p

CoVaR_Index = data.frame(Date = date, DeltaCoVaR = 0)
for (i in CoVaR_Index$Date){
  temp = Stata_CoVaR[Stata_CoVaR$Date == i,]
  CoVaR_Index$DeltaCoVaR[CoVaR_Index$Date == i] = mean(temp$DeltaVaR, na.rm = TRUE)
}


sta = rbind(sta,c(nrow(CoVaR_Index),mean(CoVaR_Index[, 2],na.rm= TRUE), sd(CoVaR_Index[, 2]), 
                  median(CoVaR_Index[, 2]), min(CoVaR_Index[, 2]), max(CoVaR_Index[, 2])))

# read BGVAR
BGVAR  = data.frame(read_excel(paste0(wdir,'MethodAdd/BGVAR_20230306.xlsx')))
colnames(BGVAR)[1] = colnames(FRM_5P)[1]

sta = rbind(sta,c(nrow(BGVAR),mean(BGVAR[, 2],na.rm= TRUE), sd(BGVAR[, 2]), 
                  median(BGVAR[, 2]), min(BGVAR[, 2]), max(BGVAR[, 2])))

# read GDC
GDC  = data.frame(read_excel(paste0(wdir,'MethodAdd/rolling_GDC_total_20230306.xlsx')))
colnames(GDC)[1] = colnames(FRM_5P)[1]
colnames(GDC)[2] = 'GDC'
GDC$date = paste0(substr(GDC$date,1,4), substr(GDC$date,6,7), substr(GDC$date,9,10))
GDC$date = as.numeric(GDC$date)

sta = rbind(sta,c(nrow(GDC),mean(GDC[, 2],na.rm= TRUE), sd(GDC[, 2]), 
                  median(GDC[, 2]), min(GDC[, 2]), max(GDC[, 2])))


# read PCA
PCA  = data.frame(read_excel(paste0(wdir,'MethodAdd/PCA_20230306.xlsx')))
PCA = PCA[,-1]
colnames(PCA)[1] = colnames(FRM_5P)[1]
PCA$date = paste0(substr(PCA$date,1,4), substr(PCA$date,6,7), substr(PCA$date,9,10))
PCA$date = as.numeric(PCA$date)

sta = rbind(sta,c(nrow(PCA),mean(PCA[, 2],na.rm= TRUE), sd(PCA[, 2]), 
                  median(PCA[, 2]), min(PCA[, 2]), max(PCA[, 2])))

# read DY
DY  = data.frame(read_excel(paste0(wdir,'MethodAdd/DY_TCI.xlsx')))
DY = DY[,-1]
colnames(DY)[1] = colnames(FRM_5P)[1]

sta = rbind(sta,c(nrow(DY),mean(DY[, 2],na.rm= TRUE), sd(DY[, 2]), 
                  median(DY[, 2]), min(DY[, 2]), max(DY[, 2])))


# cal market return

Fun_Vola = function (data){
  
  for (iStock in c(1:ncol(data))){
    vola = NA
    for (i in c(63 : nrow(data)) ){
      vola = c(vola, sd(data[(i-62) : i, iStock]))
    }
    vola = vola[-1]
    
    if (iStock == 1){
      vola_T = vola
    }else {
      vola_T = cbind(vola_T, vola)
    }
  }
  
  
  return(vola_T)
}

winsize    = 63
Macro  = data.frame(read.csv(paste0(wdir,input_path,'/MacroIndex.csv')))
Macro = Macro[,-1]
Macro$date = as.numeric(gsub("-", "", Macro$Date))
Macro$MKVola = NA
Macro$MKVola[winsize:nrow(Macro)]= Fun_Vola(data.frame(Macro$MKRturn))
Macro = Macro[-c(1:winsize),]
sta = rbind(sta,c(nrow(Macro),mean(Macro$MKVola,na.rm= TRUE), sd(Macro$MKVola), 
                  median(Macro$MKVola), min(Macro$MKVola), max(Macro$MKVola)))


FRM_list = c('FRM_5P','FRM_1P','FRM_10P','VaR_5P','DeltaCoVaR', 'DAG','GDC', 'PCA_1','TCI','MKVola')

colnames(sta) = c('n','mean','sd','median','min','max')
rownames(sta) =c('StockReturn','Mktcap',Macro_Total,FRM_list) 

write.csv(sta, paste0(wdir, "/Output/Asia/StaSummary_",save_date,".csv"), quote = FALSE)



for  (iStock in c(2:ncol(all_return))){
  Temp = data.frame("Date" = all_return$Date, "ID" = colnames(all_return)[iStock], "return" = all_return[,iStock],'mktcap'= mktcap[-1,iStock])
  if (iStock == 2){
    Stata = Temp
  }else {
    Stata = rbind(Stata, Temp)
  }
}
write_dta(Stata, paste0(wdir, "/Output/Asia/Stata_Stock_",save_date,".dta"), version = 14)  






