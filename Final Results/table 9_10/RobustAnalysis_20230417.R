
rm(list = ls(all = TRUE))
libraries = c("iml","ggplot2", "data.table", "igraph","timeDate", "stringr", "graphics","magick", "scales", "tidyr", "zoo", "foreach", "doParallel",
              "xgboost","shapr","randomForest", "rpart", "quantreg", "readxl","dplyr", "xlsx", "psych","qgraph", "gganimate","av",
              "gifski", "strex","matrixStats","tools","Hmisc","vars","aTSA","quantreg","rapport","sjmisc","haven","foreign")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#----------------------------------------START UPDATE----------------------------------------

wdir = "/Users/ruting/Documents/macbook/PcBack/FRM_Quantlet/FRM_All/"
setwd(wdir)
channel = "Asia"
date_start_source = 20190102
date_end_source =  20230306
save_date = 20230321

input_path = paste0("Input/", channel, "/", date_start_source, "-", date_end_source)

# compare FRM of different tau

FRM_5P  = data.frame(read.csv(paste0(wdir,'Output/Asia/20230228/MKRturn/Adj_Matrices_ShapleyCom/Comb_2_plus/1/Lambda_DynamicFactor/FRM_Asia_index.csv')))
FRM_1P  = data.frame(read.csv(paste0(wdir,'Output/Asia/20230228/Sensitivity/tau=1/s=63/MKRturn/Adj_Matrices_ShapleyCom/Comb_2_plus/1/Lambda_DynamicFactor/FRM_Asia_index.csv')))
FRM_10P  = data.frame(read.csv(paste0(wdir,'Output/Asia/20230228/Sensitivity/tau=10/s=63/MKRturn/Adj_Matrices_ShapleyCom/Comb_2_plus/1/Lambda_DynamicFactor/FRM_Asia_index.csv')))

FRM_total = cbind(FRM_5P, FRM_1P[,2],FRM_10P[,2])
FRM_total = data.frame(FRM_total)
colnames(FRM_total) = c('date','FRM_5P','FRM_1P','FRM_10P')
FRM_total$FRM_5P = (FRM_total$FRM_5P-mean(FRM_total$FRM_5P))/sd(FRM_total$FRM_5P)
FRM_total$FRM_1P = (FRM_total$FRM_1P-mean(FRM_total$FRM_1P))/sd(FRM_total$FRM_1P)
FRM_total$FRM_10P = (FRM_total$FRM_10P-mean(FRM_total$FRM_10P))/sd(FRM_total$FRM_10P)


FRM_total$date = as.Date(paste0(floor(FRM_total$date / 10000),"-",
                                floor(FRM_total$date / 100) -floor(FRM_total$date / 10000)*100,"-",
                                FRM_total$date - floor(FRM_total$date/ 100) * 100))

datebreaks <- seq(FRM_total$date[1], FRM_total$date[length(FRM_total$date)],
                  by = "3 month")

png(paste0(wdir, "Output/Asia/20230228/FRM_Tau.png"), width = 900, height = 600, bg = "transparent")
print(ggplot()+geom_line(data = FRM_total,aes(x = date,y = FRM_5P),colour = "red",size=1)+
        geom_line(data = FRM_total,aes(x = date,y = FRM_1P),colour = "#14B294",size=1)+
        geom_line(data = FRM_total,aes(x = date,y = FRM_10P),colour = "orange",size=1)+
        xlab("Date")+ylab("z score")+
        scale_x_date(breaks = datebreaks) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1,size = 15), 
              axis.title = element_text(size = 20,face = "bold"),
              axis.text.y = element_text(size = 15), 
              panel.grid.major =element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black"),legend.position="none")
)
dev.off()


# gen big data: frm, covar, var, financial report information
# FI characteristics
# leverage: asset/ equity
# maturity: book asset / (short term debt - short-term investments - cash)
# size: log(total market value / cross-sectional average of market value)
# boom:  decile of market-to-book ratio
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

colnames(FRM_1P)[2] = 'FRM_1P'
colnames(FRM_10P)[2] = 'FRM_10P'


Index = merge(FRM_1P, FRM_10P, by = intersect(names(FRM_1P), names(FRM_10P)),all = FALSE,sort = TRUE)
Index = merge(Index, GDC, by = intersect(names(Index), names(GDC)),all = FALSE,sort = TRUE)
Index = merge(Index, PCA, by = intersect(names(Index), names(PCA)),all = FALSE,sort = TRUE)
Index = merge(Index, DY, by = intersect(names(Index), names(DY)),all = FALSE,sort = TRUE)
Index = merge(Index, Macro, by = intersect(names(Index), names(Macro)),all = FALSE,sort = TRUE)

Index[,! colnames(Index) %in%c('Date')]=sapply(Index[,! colnames(Index) %in%c('Date')], as.numeric)

Index[,c('FRM_1P_L5','FRM_10P_L5', 'FRM_1P_L21','FRM_10P_L21')] = NA

Index[6:nrow(Index), c('FRM_1P_L5', 'FRM_10P_L5')] = Index[1:(nrow(Index)-5), c('FRM_1P', 'FRM_10P')]

Index[22:nrow(Index), c('FRM_1P_L21','FRM_10P_L21')] = Index[1:(nrow(Index)-21), c('FRM_1P', 'FRM_10P')]

# cal OOR
pre_col = c('FRM_1P_L5','FRM_10P_L5', 'FRM_1P_L21','FRM_10P_L21')


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
write.csv(R_oos, paste0(wdir, "/Output/Asia/Robust_Prediction_vol_ROO_",save_date,".csv"), quote = FALSE)


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
write.csv(R_2, paste0(wdir, "/Output/Asia/Robust_Prediction_vol_R2_",save_date,".csv"), quote = FALSE)


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
write.csv(R_oos, paste0(wdir, "/Output/Asia/Robust_Prediction_VaR_ROO_",save_date,".csv"), quote = FALSE)


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
write.csv(R_2, paste0(wdir, "/Output/Asia/Robust_Prediction_VaR_R2_",save_date,".csv"), quote = FALSE)

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
write.csv(R_oos, paste0(wdir, "/Output/Asia/Robust_Prediction_DeltaCoVaR_ROO_",save_date,".csv"), quote = FALSE)


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
write.csv(R_2, paste0(wdir, "/Output/Asia/Robust_Prediction_DeltaCoVaR_R2_",save_date,".csv"), quote = FALSE)



# read var
Stata_Risk  = data.frame(read_dta(paste0(wdir,"/Output/Asia/Stata_FRM_20230415.dta")))
Stata_Risk = Stata_Risk[,-1]


Index =  data.frame(read.csv(file = paste0(wdir, "/Output/Asia/RiskIndex",save_date,".csv"), header = TRUE) %>% as.matrix())
Index = Index[,-1]
Index[,! colnames(Index) %in%c('Date')]=sapply(Index[,! colnames(Index) %in%c('Date')], as.numeric)

colnames(FRM_1P)[2] = 'FRM_1P'
colnames(FRM_10P)[2] = 'FRM_10P'


Index_new = merge(FRM_1P, FRM_10P, by = intersect(names(FRM_1P), names(FRM_10P)),all = FALSE,sort = TRUE)
Index = merge(Index, Index_new, by = intersect(names(Index), names(Index_new)),all = FALSE,sort = TRUE)

Index[,c('FRM_1P_L5','FRM_10P_L5', 'FRM_1P_L21','FRM_10P_L21')] = NA
Index[6:nrow(Index), c('FRM_1P_L5', 'FRM_10P_L5')] = Index[1:(nrow(Index)-5), c('FRM_1P', 'FRM_10P')]
Index[22:nrow(Index), c('FRM_1P_L21','FRM_10P_L21')] = Index[1:(nrow(Index)-21), c('FRM_1P', 'FRM_10P')]

Stata_Risk = merge(Stata_Risk, Index, by = intersect(names(Stata_Risk), names(Index)),all = FALSE,sort = TRUE)
write_dta(Stata_Risk, paste0(wdir, "/Output/Asia/Robust_Stata_FRM_20230417.dta"), version = 14)  

