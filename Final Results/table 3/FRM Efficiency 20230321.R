
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

# gen big data: frm, covar, var, financial report information
# FI characteristics
# leverage: asset/ equity
# maturity: book asset / (short term debt - short-term investments - cash)
# size: log(total market value / cross-sectional average of market value)
# boom:  decile of market-to-book ratio
Path_FReport = paste0(wdir,"Input/Asia/Financial Report")

Fun_Read = function (File){
  Raw.all <-read_excel(paste0(Path_FReport,'/',File,'.xlsx'));
  Raw.all = Raw.all[-c(1,2),]
  # Raw.all = do.call(data.frame,lapply(Raw.all, function(x) replace(x, is.na(x),0)))
  Raw.all = do.call(data.frame, Raw.all)
  if (File == 'Balance Sheet'){ Raw.all = Raw.all[which(Raw.all$Typrep == "A"),]}
  for (i in c(3:ncol(Raw.all))){
    Raw.all[,i] = as.numeric(Raw.all[,i])
  }
  
  Raw.all$Year = as.numeric(substring(Raw.all$Accper,1,4))
  Raw.all$Month = as.numeric(substring(Raw.all$Accper,6,7))
  Raw.all = Raw.all[which(Raw.all$Month == 12),]
  
  return(Raw.all)

}

BS <- Fun_Read('Balance Sheet')
FIdi<- Fun_Read('Financial Indicator')
Stata_FI = merge(BS, FIdi, by = intersect(names(BS), names(FIdi)),all = FALSE,sort = TRUE)

Stata_FI[is.na(Stata_FI)] = 0
Stata_FI$Leverage = Stata_FI$Asset / Stata_FI$Equity
Stata_FI$maturity = Stata_FI$Asset / (Stata_FI$ShortTermDebt-Stata_FI$ShortTermInvest-Stata_FI$Cash)
Stata_FI$maturity[Stata_FI$ShortTermDebt == 0] = 0
Stata_FI$MB = 1/Stata_FI$BM
Stata_FI = Stata_FI[Stata_FI$Year >=2018,]


fun_loc = function(x){
  loc = which(f$Stkcd == x)
  return(loc)
}


f = read_excel(paste0(input_path, "/","Name_Stock.xlsx"),sheet = 'Add Short Name')
f = data.frame(f)
f  = f[grepl("*CH.EQUITY",f$StockID_Raw) | grepl("*CH.Equity",f$StockID_Raw), ]
f$Stkcd = substring(f$StockID,1,6)
Stata_FI = Stata_FI[Stata_FI$Stkcd %in% f$Stkcd,]

Stata_FI[,c("sector","ID")] = f[unlist(lapply(Stata_FI$Stkcd, fun_loc)),c("GICS_INDUSTRY_GROUP_NAME","StockID_Raw")]
# Stata_FI$sector[!Stata_FI$sector %in% c("Banks", "Diversified Financials", "Insurance")] = "Others"

# SectorType = c("Banks", "Diversified Financials", "Insurance", "Others")
SectorType = unique(Stata_FI$sector)
Year = unique(Stata_FI$Year)
Stata_FI[,c("MV_Sector")] = NA
Stata_FI[,c("MB_Rank")] = NA
for (iYear in Year){
  for (iSec in SectorType){
    Temp = Stata_FI$MV[Stata_FI$Year == iYear & Stata_FI$sector == iSec]
    Stata_FI$MV_Sector[Stata_FI$Year == iYear & Stata_FI$sector == iSec] = mean(Temp, na.rm = TRUE)
  }
}
Stata_FI$Size = log(Stata_FI$MV) / log(Stata_FI$MV_Sector)
stocklist = unique(Stata_FI$Stkcd)
for (iYear in Year){
  Temp = Stata_FI$MB[Stata_FI$Year == iYear] 
  Rank = quantile(Temp, seq(0.1,1,0.1))
  for (iStock in stocklist){
    MB = Stata_FI$MB[Stata_FI$Stkcd == iStock & Stata_FI$Year == iYear]
    # Temp_order = c(MB,Rank)
    Temp_order = order(c(MB,Rank))
    score = which(Temp_order == 1)
    
    Stata_FI$MB_Rank[Stata_FI$Stkcd == iStock & Stata_FI$Year == iYear] = score
  }
}

# volatility 63 days
stock_prices = read.csv(file = paste0(wdir,input_path, "/", channel, "_Price_", 
                                      date_end_source, ".csv"), header = TRUE)
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


all_vola = all_return[-(1:62),]
all_vola[,-1] = 0

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

all_vola[, -1] = Fun_Vola(all_return[,-1])

for  (iStock in c(2:ncol(all_vola))){
  Temp = data.frame("Date" = all_vola$Date, "ID" = colnames(all_vola)[iStock], "Vola" = all_vola[,iStock])
  if (iStock == 2){
    Stata = Temp
  }else {
    Stata = rbind(Stata, Temp)
  }
}
Stata  = Stata[grepl("*CH.EQUITY",Stata$ID) | grepl("*CH.Equity",Stata$ID), ]
colnames(Stata)[1] = 'date'

# read FRM
select_mac = 'MKRturn'
FRM_5P  = data.frame(read.csv(paste0(wdir,'Output/Asia/20230228/',select_mac,'/Adj_Matrices_ShapleyCom/Comb_2_plus/1/Lambda_DynamicFactor/FRM_Asia_index.csv')))
colnames(FRM_5P)[2] = 'FRM_5P'

# read BGVAR
BGVAR  = data.frame(read_excel(paste0(wdir,'MethodAdd/BGVAR_20230306.xlsx')))
colnames(BGVAR)[1] = colnames(FRM_5P)[1]

# read GDC
GDC  = data.frame(read_excel(paste0(wdir,'MethodAdd/rolling_GDC_total_20230306.xlsx')))
colnames(GDC)[1] = colnames(FRM_5P)[1]
colnames(GDC)[2] = 'GDC'
GDC$date = paste0(substr(GDC$date,1,4), substr(GDC$date,6,7), substr(GDC$date,9,10))
GDC$date = as.numeric(GDC$date)

# read PCA
PCA  = data.frame(read_excel(paste0(wdir,'MethodAdd/PCA_20230306.xlsx')))
PCA = PCA[,-1]
colnames(PCA)[1] = colnames(FRM_5P)[1]
PCA$date = paste0(substr(PCA$date,1,4), substr(PCA$date,6,7), substr(PCA$date,9,10))
PCA$date = as.numeric(PCA$date)

# read DY
DY  = data.frame(read_excel(paste0(wdir,'MethodAdd/DY_TCI.xlsx')))
DY = DY[,-1]
colnames(DY)[1] = colnames(FRM_5P)[1]

# cal market return
winsize    = 63
Macro  = data.frame(read.csv(paste0(wdir,input_path,'/MacroIndex.csv')))
Macro = Macro[,-1]
Macro$date = as.numeric(gsub("-", "", Macro$Date))
Macro$MKVola = NA
Macro$MKVola[winsize:nrow(Macro)]= Fun_Vola(data.frame(Macro$MKRturn))

# VaR
for (i in winsize:nrow(Macro)) {
  ycut   = Macro[(i-winsize+1):i, 'MKRturn']
  Macro[i, 'VaR'] = qnorm(0.05, mean(ycut), sd(ycut))
}

Macro = Macro[-c(1:62),]


Index = merge(FRM_5P, BGVAR, by = intersect(names(FRM_5P), names(BGVAR)),all = FALSE,sort = TRUE)
Index = merge(Index, GDC, by = intersect(names(Index), names(GDC)),all = FALSE,sort = TRUE)
Index = merge(Index, PCA, by = intersect(names(Index), names(PCA)),all = FALSE,sort = TRUE)
Index = merge(Index, DY, by = intersect(names(Index), names(DY)),all = FALSE,sort = TRUE)
Index = merge(Index, Macro, by = intersect(names(Index), names(Macro)),all = FALSE,sort = TRUE)
write_dta(Index, paste0(wdir, "/Output/Asia/RiskIndex",save_date,".dta"), version = 14)  
write.csv(Index, paste0(wdir, "/Output/Asia/RiskIndex",save_date,".csv"), quote = FALSE)


Index =  data.frame(read.csv(file = paste0(wdir, "/Output/Asia/RiskIndex",save_date,".csv"), header = TRUE) %>% as.matrix())
Index = Index[,-1]
Index[,! colnames(Index) %in%c('Date')]=sapply(Index[,! colnames(Index) %in%c('Date')], as.numeric)

Index[,c('FRM_L5','DAG_L5','GDC_L5','PCA_L5','TCI_L5')] = NA

Index[6:nrow(Index), c('FRM_L5','DAG_L5','GDC_L5','PCA_L5','TCI_L5')] = Index[1:(nrow(Index)-5), c('FRM_5P','DAG','GDC','PCA_1','TCI')]

Index[22:nrow(Index), c('FRM_L21','DAG_L21','GDC_L21','PCA_L21','TCI_L21')] = Index[1:(nrow(Index)-21), c('FRM_5P','DAG','GDC','PCA_1','TCI')]
# cal OOR
pre_col = c('FRM_L5','DAG_L5','GDC_L5','PCA_L5','TCI_L5','FRM_L21','DAG_L21','GDC_L21','PCA_L21','TCI_L21')


# recession prediction monthly 
# Index$Month = floor(Index$date/100)
# Monthlist = unique(floor(Index$date/100))
# Index_Month = data.frame(Month = Monthlist, FRM =NA, DAG = NA, PCA = NA, TCI = NA)
# 
# for (i in Monthlist){
#   Temp = Index[Index$Month == i, c('FRM_5P','DAG','GDC','PCA_1','TCI')]
#   Index_Month[Index_Month$Month == i, -1] = colMeans(Temp)
# }
# Index_Month$Recession = 0
# Index_Month$Recession[Index_Month$Month >= 201901 & Index_Month$Month <= 202002] = 1
# Index_Month$Recession[Index_Month$Month >= 202102] = 1
# Index_Month = Index_Month[Index_Month$Month<= 202209,]

Index$Month = floor(Index$date/100)
Index$Recession = 0
Index$Recession[Index$Month >= 201901 & Index$Month <= 202002] = 1
Index$Recession[Index$Month >= 202102] = 1
Index = Index[Index$Month<= 202209,]


Index[64:nrow(Index), c('FRM_L63','DAG_L63','GDC_L63','PCA_L63','TCI_L63')] = Index[1:(nrow(Index)-63), c('FRM_5P','DAG','GDC','PCA_1','TCI')]
pre_col = c('FRM_L21','DAG_L21','GDC_L21','PCA_L21','TCI_L21','FRM_L63','DAG_L63','GDC_L63','PCA_L63','TCI_L63')

R_Recession = data.frame(matrix(0,3,length(pre_col)))
colnames(R_Recession) = pre_col
for (j in pre_col){
  temp_reg = Index[,c('Recession',j)]
  temp_reg = temp_reg[!is.na(temp_reg[,j]),]
  colnames(temp_reg) = c('y','x')
  
  
  mod <- glm(y~x,family="binomial", data = temp_reg)
  nullmod <- glm(y~1,family="binomial", data = temp_reg)
  
  R_Recession[1,j] = 1-logLik(mod)/logLik(nullmod)
  
  report = summary(mod)
  R_Recession[1,j] = 1-logLik(mod)/logLik(nullmod)
  R_Recession[2,j] = report$coefficients[2,1]
  R_Recession[3,j] = report$coefficients[2,4]
}
write.csv(R_Recession, paste0(wdir, "/Output/Asia/Prediction_Recession_R2_",save_date,".csv"), quote = FALSE)




R_oos = data.frame(matrix(0,3,length(pre_col)))
colnames(R_oos) = pre_col
for (j in pre_col){
  temp_reg = Index[,c('MKVola',j)]
  temp_reg = temp_reg[!is.na(temp_reg[,j]),]
  colnames(temp_reg) = c('y','x')
  for (i in c(63:(nrow(temp_reg)-1))){
    
    fit = lm(y ~ x, temp_reg[(i-62):(i-1), ])
    
    temp_reg[i, paste0('p_vol_',j)] = fit$coefficients[1] + fit$coefficients[2]*temp_reg$x[i]
    temp_reg[i, paste0('diff_s_',j)] = (temp_reg$y[i]-temp_reg[i,paste0('p_vol_',j)])^2
    # temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y))^2
    temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y[63:nrow(temp_reg)]))^2
  }
  
  R_oos[1,j] = 1 - sum(temp_reg[,paste0('diff_s_',j)], na.rm = TRUE)/sum(temp_reg[,paste0('diff_m_',j)], na.rm = TRUE)
  R_oos[2,j] =sum(temp_reg[,paste0('diff_s_',j)], na.rm = TRUE)
  R_oos[3,j] =sum(temp_reg[,paste0('diff_m_',j)], na.rm = TRUE)
}
write.csv(R_oos, paste0(wdir, "/Output/Asia/Prediction_vol_ROO_",save_date,".csv"), quote = FALSE)


R_2 = data.frame(matrix(0,3,length(pre_col)))
colnames(R_2) = pre_col
for (j in pre_col){
  temp_reg = Index[,c('MKVola',j)]
  temp_reg = temp_reg[!is.na(temp_reg[,j]),]
  colnames(temp_reg) = c('y','x')
  for (i in c(63:(nrow(temp_reg)-1))){
    
    fit = lm(y ~ x, temp_reg[(i-61):i, ])
    
    temp_reg[i, paste0('p_vol_',j)] = fit$coefficients[1] + fit$coefficients[2]*temp_reg$x[i]
    temp_reg[i, paste0('diff_s_',j)] = (temp_reg$y[i]-temp_reg[i,paste0('p_vol_',j)])^2
    temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y[63:nrow(temp_reg)]))^2
    # temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y[i:nrow(temp_reg)]))^2
  }
  
  R_2[1,j] = 1 - sum(temp_reg[,paste0('diff_s_',j)], na.rm = TRUE)/sum(temp_reg[,paste0('diff_m_',j)], na.rm = TRUE)
  R_2[2,j] =sum(temp_reg[,paste0('diff_s_',j)], na.rm = TRUE)
  R_2[3,j] =sum(temp_reg[,paste0('diff_m_',j)], na.rm = TRUE)
}
write.csv(R_2, paste0(wdir, "/Output/Asia/Prediction_vol_R2_",save_date,".csv"), quote = FALSE)


VaR_5p =  data.frame(read.csv(file = paste0(wdir,"VaR_5P_Index_0.05_20230228.csv"), header = TRUE) %>% as.matrix())
VaR_5p = VaR_5p[,-1]
VaR_5p[,-1] = as.numeric(VaR_5p[,-1])
Index = merge(Index, VaR_5p, by = intersect(names(Index), names(VaR_5p)),all = FALSE,sort = TRUE)

R_oos = data.frame(matrix(0,3,length(pre_col)))
colnames(R_oos) = pre_col
for (j in pre_col){
  temp_reg = Index[,c('VaR_5P',j)]
  temp_reg = temp_reg[!is.na(temp_reg[,j]),]
  colnames(temp_reg) = c('y','x')
  for (i in c(63:(nrow(temp_reg)-1))){
    
    fit = lm(y ~ x, temp_reg[(i-62):(i-1), ])
    
    temp_reg[i, paste0('p_vol_',j)] = fit$coefficients[1] + fit$coefficients[2]*temp_reg$x[i]
    temp_reg[i, paste0('diff_s_',j)] = (temp_reg$y[i]-temp_reg[i,paste0('p_vol_',j)])^2
    # temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y))^2
    temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y[63:nrow(temp_reg)]))^2
  }
  
  R_oos[1,j] = 1 - sum(temp_reg[,paste0('diff_s_',j)], na.rm = TRUE)/sum(temp_reg[,paste0('diff_m_',j)], na.rm = TRUE)
  R_oos[2,j] =sum(temp_reg[,paste0('diff_s_',j)], na.rm = TRUE)
  R_oos[3,j] =sum(temp_reg[,paste0('diff_m_',j)], na.rm = TRUE)
}
write.csv(R_oos, paste0(wdir, "/Output/Asia/Prediction_VaR_ROO_",save_date,".csv"), quote = FALSE)


R_2 = data.frame(matrix(0,3,length(pre_col)))
colnames(R_2) = pre_col
for (j in pre_col){
  temp_reg = Index[,c('VaR_5P',j)]
  temp_reg = temp_reg[!is.na(temp_reg[,j]),]
  colnames(temp_reg) = c('y','x')
  for (i in c(63:(nrow(temp_reg)-1))){
    
    fit = lm(y ~ x, temp_reg[(i-61):i, ])
    
    temp_reg[i, paste0('p_vol_',j)] = fit$coefficients[1] + fit$coefficients[2]*temp_reg$x[i]
    temp_reg[i, paste0('diff_s_',j)] = (temp_reg$y[i]-temp_reg[i,paste0('p_vol_',j)])^2
    temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y[63:nrow(temp_reg)]))^2
    # temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y[i:nrow(temp_reg)]))^2
  }
  
  R_2[1,j] = 1 - sum(temp_reg[,paste0('diff_s_',j)], na.rm = TRUE)/sum(temp_reg[,paste0('diff_m_',j)], na.rm = TRUE)
  R_2[2,j] =sum(temp_reg[,paste0('diff_s_',j)], na.rm = TRUE)
  R_2[3,j] =sum(temp_reg[,paste0('diff_m_',j)], na.rm = TRUE)
}
write.csv(R_2, paste0(wdir, "/Output/Asia/Prediction_VaR_R2_",save_date,".csv"), quote = FALSE)

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

Index = merge(Index, CoVaR_Index, by = intersect(names(Index), names(CoVaR_Index)),all = FALSE,sort = TRUE)


R_oos = data.frame(matrix(0,3,length(pre_col)))
colnames(R_oos) = pre_col
for (j in pre_col){
  temp_reg = Index[,c('DeltaCoVaR',j)]
  temp_reg = temp_reg[!is.na(temp_reg[,j]),]
  colnames(temp_reg) = c('y','x')
  for (i in c(63:(nrow(temp_reg)-1))){
    
    fit = lm(y ~ x, temp_reg[(i-62):(i-1), ])
    
    temp_reg[i, paste0('p_vol_',j)] = fit$coefficients[1] + fit$coefficients[2]*temp_reg$x[i]
    temp_reg[i, paste0('diff_s_',j)] = (temp_reg$y[i]-temp_reg[i,paste0('p_vol_',j)])^2
    # temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y))^2
    temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y[63:nrow(temp_reg)]))^2
  }
  
  R_oos[1,j] = 1 - sum(temp_reg[,paste0('diff_s_',j)], na.rm = TRUE)/sum(temp_reg[,paste0('diff_m_',j)], na.rm = TRUE)
  R_oos[2,j] =sum(temp_reg[,paste0('diff_s_',j)], na.rm = TRUE)
  R_oos[3,j] =sum(temp_reg[,paste0('diff_m_',j)], na.rm = TRUE)
}
write.csv(R_oos, paste0(wdir, "/Output/Asia/Prediction_DeltaCoVaR_ROO_",save_date,".csv"), quote = FALSE)


R_2 = data.frame(matrix(0,3,length(pre_col)))
colnames(R_2) = pre_col
for (j in pre_col){
  temp_reg = Index[,c('DeltaCoVaR',j)]
  temp_reg = temp_reg[!is.na(temp_reg[,j]),]
  colnames(temp_reg) = c('y','x')
  for (i in c(63:(nrow(temp_reg)-1))){
    
    fit = lm(y ~ x, temp_reg[(i-61):i, ])
    
    temp_reg[i, paste0('p_vol_',j)] = fit$coefficients[1] + fit$coefficients[2]*temp_reg$x[i]
    temp_reg[i, paste0('diff_s_',j)] = (temp_reg$y[i]-temp_reg[i,paste0('p_vol_',j)])^2
    temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y[63:nrow(temp_reg)]))^2
    # temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y[i:nrow(temp_reg)]))^2
  }
  
  R_2[1,j] = 1 - sum(temp_reg[,paste0('diff_s_',j)], na.rm = TRUE)/sum(temp_reg[,paste0('diff_m_',j)], na.rm = TRUE)
  R_2[2,j] =sum(temp_reg[,paste0('diff_s_',j)], na.rm = TRUE)
  R_2[3,j] =sum(temp_reg[,paste0('diff_m_',j)], na.rm = TRUE)
}
write.csv(R_2, paste0(wdir, "/Output/Asia/Prediction_DeltaCoVaR_R2_",save_date,".csv"), quote = FALSE)


# market return
R_oos = data.frame(matrix(0,3,length(pre_col)))
colnames(R_oos) = pre_col
for (j in pre_col){
  temp_reg = Index[,c('MKRturn',j)]
  temp_reg = temp_reg[!is.na(temp_reg[,j]),]
  colnames(temp_reg) = c('y','x')
  for (i in c(63:(nrow(temp_reg)-1))){
    
    fit = lm(y ~ x, temp_reg[(i-62):(i-1), ])
    
    temp_reg[i, paste0('p_vol_',j)] = fit$coefficients[1] + fit$coefficients[2]*temp_reg$x[i]
    temp_reg[i, paste0('diff_s_',j)] = (temp_reg$y[i]-temp_reg[i,paste0('p_vol_',j)])^2
    # temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y))^2
    temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y[63:nrow(temp_reg)]))^2
  }
  
  R_oos[1,j] = 1 - sum(temp_reg[,paste0('diff_s_',j)], na.rm = TRUE)/sum(temp_reg[,paste0('diff_m_',j)], na.rm = TRUE)
  R_oos[2,j] =sum(temp_reg[,paste0('diff_s_',j)], na.rm = TRUE)
  R_oos[3,j] =sum(temp_reg[,paste0('diff_m_',j)], na.rm = TRUE)
}
write.csv(R_oos, paste0(wdir, "/Output/Asia/Prediction_MKRturn_ROO_",save_date,".csv"), quote = FALSE)


R_2 = data.frame(matrix(0,3,length(pre_col)))
colnames(R_2) = pre_col
for (j in pre_col){
  temp_reg = Index[,c('MKRturn',j)]
  temp_reg = temp_reg[!is.na(temp_reg[,j]),]
  colnames(temp_reg) = c('y','x')
  for (i in c(63:(nrow(temp_reg)-1))){
    
    fit = lm(y ~ x, temp_reg[(i-61):i, ])
    
    temp_reg[i, paste0('p_vol_',j)] = fit$coefficients[1] + fit$coefficients[2]*temp_reg$x[i]
    temp_reg[i, paste0('diff_s_',j)] = (temp_reg$y[i]-temp_reg[i,paste0('p_vol_',j)])^2
    temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y[63:nrow(temp_reg)]))^2
    # temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y[i:nrow(temp_reg)]))^2
  }
  
  R_2[1,j] = 1 - sum(temp_reg[,paste0('diff_s_',j)], na.rm = TRUE)/sum(temp_reg[,paste0('diff_m_',j)], na.rm = TRUE)
  R_2[2,j] =sum(temp_reg[,paste0('diff_s_',j)], na.rm = TRUE)
  R_2[3,j] =sum(temp_reg[,paste0('diff_m_',j)], na.rm = TRUE)
}
write.csv(R_2, paste0(wdir, "/Output/Asia/Prediction_MKRturn_R2_",save_date,".csv"), quote = FALSE)

# srisk
SRisk  = data.frame(read.csv(paste0(wdir, "MethodAdd/SRISKv2/H_30/srisk_data_mean.csv")))
colnames(SRisk)[1] = 'Date'
SRisk$Date = as.Date(SRisk$Date,'%d-%b-%Y')
SRisk$Date = as.character(SRisk$Date)
Index = merge(Index, SRisk, by = intersect(names(Index), names(SRisk)),all = FALSE,sort = TRUE)

R_oos = data.frame(matrix(0,3,length(pre_col)))
colnames(R_oos) = pre_col
for (j in pre_col){
  temp_reg = Index[,c('sriskv2_mean',j)]
  temp_reg = temp_reg[!is.na(temp_reg[,j]),]
  colnames(temp_reg) = c('y','x')
  for (i in c(63:(nrow(temp_reg)-1))){
    
    fit = lm(y ~ x, temp_reg[(i-62):(i-1), ])
    
    temp_reg[i, paste0('p_vol_',j)] = fit$coefficients[1] + fit$coefficients[2]*temp_reg$x[i]
    temp_reg[i, paste0('diff_s_',j)] = (temp_reg$y[i]-temp_reg[i,paste0('p_vol_',j)])^2
    # temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y))^2
    temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y[63:nrow(temp_reg)]))^2
  }
  
  R_oos[1,j] = 1 - sum(temp_reg[,paste0('diff_s_',j)], na.rm = TRUE)/sum(temp_reg[,paste0('diff_m_',j)], na.rm = TRUE)
  R_oos[2,j] =sum(temp_reg[,paste0('diff_s_',j)], na.rm = TRUE)
  R_oos[3,j] =sum(temp_reg[,paste0('diff_m_',j)], na.rm = TRUE)
}
write.csv(R_oos, paste0(wdir, "/Output/Asia/Prediction_sriskv2_ROO_",save_date,".csv"), quote = FALSE)


R_2 = data.frame(matrix(0,3,length(pre_col)))
colnames(R_2) = pre_col
for (j in pre_col){
  temp_reg = Index[,c('sriskv2_mean',j)]
  temp_reg = temp_reg[!is.na(temp_reg[,j]),]
  colnames(temp_reg) = c('y','x')
  for (i in c(63:(nrow(temp_reg)-1))){
    
    fit = lm(y ~ x, temp_reg[(i-61):i, ])
    
    temp_reg[i, paste0('p_vol_',j)] = fit$coefficients[1] + fit$coefficients[2]*temp_reg$x[i]
    temp_reg[i, paste0('diff_s_',j)] = (temp_reg$y[i]-temp_reg[i,paste0('p_vol_',j)])^2
    temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y[63:nrow(temp_reg)]))^2
    # temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y[i:nrow(temp_reg)]))^2
  }
  
  R_2[1,j] = 1 - sum(temp_reg[,paste0('diff_s_',j)], na.rm = TRUE)/sum(temp_reg[,paste0('diff_m_',j)], na.rm = TRUE)
  R_2[2,j] =sum(temp_reg[,paste0('diff_s_',j)], na.rm = TRUE)
  R_2[3,j] =sum(temp_reg[,paste0('diff_m_',j)], na.rm = TRUE)
}
write.csv(R_2, paste0(wdir, "/Output/Asia/Prediction_sriskv2_R2_",save_date,".csv"), quote = FALSE)



# read var
Stata_Risk = merge(Stata_VaR_5P,Stata_CoVaR,by = intersect(names(Stata_VaR_5P), names(Stata_CoVaR)), all.x = TRUE)

SRisk  = data.frame(read.csv(paste0(wdir, "MethodAdd/SRISKv2/H_30/srisk_data_detail.csv")))
colnames(SRisk)[1] = 'Date'
SRisk$Date = as.Date(SRisk$Date,'%d-%b-%Y')
SRisk$Date = as.character(SRisk$Date)
SRisk = SRisk[,c(1:174)]

SRisk_name  = data.frame(read.csv(paste0(wdir, "MethodAdd/SRISKv2/SRISK_Data.csv")))
SRisk_name = colnames(SRisk_name)
select = seq(4,length(SRisk_name),4)
SRisk_name = SRisk_name[select]
SRisk_name = substr(SRisk_name, 4, nchar(SRisk_name))
colnames(SRisk)[-1] = SRisk_name

for (i in c(2:ncol(SRisk))){
  Temp = data.frame(Date = SRisk$Date, ID = colnames(SRisk)[i], SRisk = SRisk[, i])
  if (i == 2){
    SRisk_n = Temp
  }else{
    SRisk_n = rbind(SRisk_n, Temp)
  }
}

Stata_Risk = merge(Stata_Risk,SRisk_n,by = intersect(names(Stata_Risk), names(SRisk_n)), all.x = TRUE)

Index = Index[,c('Date', pre_col)]
Stata_Risk = merge(Stata_Risk,Index,by = intersect(names(Stata_Risk), names(Index)), all.x = TRUE)


# volatility 63 days
stock_prices = read.csv(file = paste0(input_path, "/", channel, "_Price_", 
                                      date_end_source, ".csv"), header = TRUE)
stock_prices = stock_prices[,-1]
Name_Stock = names(stock_prices)
Name_Stock = Name_Stock[-1]

all_prices = stock_prices
all_prices[, -1] = sapply(all_prices[, -1], as.numeric)
all_return = all_prices[-1, ]
all_return[,-1] = 0

all_return[,which(names(all_return)%in%Name_Stock)] = 
  as.data.frame(diff(log(as.matrix(all_prices[, which(names(all_prices)%in%Name_Stock)])))) 
all_return[is.na(all_return)] = 0

mktcap =  read.csv(file = paste0(input_path, "/", channel, "_Mktcap_", 
                                 date_end_source, ".csv"), header = TRUE) %>% as.matrix()
mktcap = mktcap[,-(colnames(mktcap) %in% "X")]
mktcap = data.frame(mktcap)
mktcap[, -1] = sapply(mktcap[, -1], as.numeric)


all_vola = all_return[-(1:62),]
all_vola[,-1] = 0

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

all_vola[, -1] = Fun_Vola(all_return[,-1])

for  (iStock in c(2:ncol(all_vola))){
  Temp = data.frame("Date" = all_vola$Date, "ID" = colnames(all_vola)[iStock], "Vola" = all_vola[,iStock], "return" = all_return[-c(1:62),iStock],'mktcap'= mktcap[-c(1:63),iStock])
  if (iStock == 2){
    Stata = Temp
  }else {
    Stata = rbind(Stata, Temp)
  }
}
Stata  = Stata[grepl("*CH.EQUITY",Stata$ID) | grepl("*CH.Equity",Stata$ID), ]
Stata_Risk = merge(Stata_Risk,Stata,by = intersect(names(Stata_Risk), names(Stata)), all.x = TRUE)
Stata_Risk$Year = as.numeric(substr(Stata_Risk$Date, 1,4))

Path_FReport = paste0(wdir,"Input/Asia/Financial Report")

Fun_Read = function (File){
  Raw.all <-read_excel(paste0(Path_FReport,'/',File,'.xlsx'));
  Raw.all = Raw.all[-c(1,2),]
  # Raw.all = do.call(data.frame,lapply(Raw.all, function(x) replace(x, is.na(x),0)))
  Raw.all = do.call(data.frame, Raw.all)
  if (File == 'Balance Sheet'){ Raw.all = Raw.all[which(Raw.all$Typrep == "A"),]}
  for (i in c(3:ncol(Raw.all))){
    Raw.all[,i] = as.numeric(Raw.all[,i])
  }
  
  Raw.all$Year = as.numeric(substring(Raw.all$Accper,1,4))
  Raw.all$Month = as.numeric(substring(Raw.all$Accper,6,7))
  Raw.all = Raw.all[which(Raw.all$Month == 12),]
  
  return(Raw.all)
  
}

BS <- Fun_Read('Balance Sheet')
FIdi<- Fun_Read('Financial Indicator')
Stata_FI = merge(BS, FIdi, by = intersect(names(BS), names(FIdi)),all = FALSE,sort = TRUE)

Stata_FI[is.na(Stata_FI)] = 0
Stata_FI$Leverage = Stata_FI$Asset / Stata_FI$Equity
Stata_FI$maturity = Stata_FI$Asset / (Stata_FI$ShortTermDebt-Stata_FI$ShortTermInvest-Stata_FI$Cash)
Stata_FI$maturity[Stata_FI$ShortTermDebt == 0] = 0
Stata_FI$MB = 1/Stata_FI$BM
Stata_FI = Stata_FI[Stata_FI$Year >=2018,]


fun_loc = function(x){
  loc = which(f$Stkcd == x)
  return(loc)
}


f = read_excel(paste0(input_path, "/","Name_Stock.xlsx"),sheet = 'Add Short Name')
f = data.frame(f)
f  = f[grepl("*CH.EQUITY",f$StockID_Raw) | grepl("*CH.Equity",f$StockID_Raw), ]
f$Stkcd = substring(f$StockID,1,6)
Stata_FI = Stata_FI[Stata_FI$Stkcd %in% f$Stkcd,]

Stata_FI[,c("sector","ID")] = f[unlist(lapply(Stata_FI$Stkcd, fun_loc)),c("GICS_INDUSTRY_GROUP_NAME","StockID_Raw")]
# Stata_FI$sector[!Stata_FI$sector %in% c("Banks", "Diversified Financials", "Insurance")] = "Others"

# SectorType = c("Banks", "Diversified Financials", "Insurance", "Others")
SectorType = unique(Stata_FI$sector)
Year = unique(Stata_FI$Year)
Stata_FI[,c("MV_Sector")] = NA
Stata_FI[,c("MB_Rank")] = NA
for (iYear in Year){
  for (iSec in SectorType){
    Temp = Stata_FI$MV[Stata_FI$Year == iYear & Stata_FI$sector == iSec]
    Stata_FI$MV_Sector[Stata_FI$Year == iYear & Stata_FI$sector == iSec] = mean(Temp, na.rm = TRUE)
  }
}
Stata_FI$Size = log(Stata_FI$MV) / log(Stata_FI$MV_Sector)
stocklist = unique(Stata_FI$Stkcd)
for (iYear in Year){
  Temp = Stata_FI$MB[Stata_FI$Year == iYear] 
  Rank = quantile(Temp, seq(0.1,1,0.1))
  for (iStock in stocklist){
    MB = Stata_FI$MB[Stata_FI$Stkcd == iStock & Stata_FI$Year == iYear]
    # Temp_order = c(MB,Rank)
    Temp_order = order(c(MB,Rank))
    score = which(Temp_order == 1)
    
    Stata_FI$MB_Rank[Stata_FI$Stkcd == iStock & Stata_FI$Year == iYear] = score
  }
}
Stata_FI$Year_announce = Stata_FI$Year
Stata_FI$Year = Stata_FI$Year + 1
Stata_Risk = merge(Stata_Risk,Stata_FI,by = intersect(names(Stata_Risk), names(Stata_FI)), all.x = TRUE)

# unique_id = unique(Stata_Risk$ID)
# for (i in unique_id){
#   temp = Stata_Risk$Date[Stata_Risk$ID == i]
#   unique_t = unique(temp)
#   if (length(temp) != length(unique_t)){print(i)}
#   
# }

save_date = 20230415
write_dta(Stata_Risk, paste0(wdir, "/Output/Asia/Stata_FRM_",save_date,".dta"), version = 14)  
write.csv(Stata_Risk, paste0(wdir, "/Output/Asia/Stata_FRM_",save_date,".csv"), quote = FALSE)



