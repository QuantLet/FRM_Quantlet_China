## FRM calculation

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


Macro_Total = c('MKRturn','TED','RealEstateDiff')
macro = Index[,c('Date', Macro_Total)]
colnames(macro)[1] = "ticker"

if (!all(sort(colnames(mktcap)) == sort(colnames(stock_prices)))) 
  stop("columns do not match")

M_stock = ncol(mktcap)-1

# WRT change 
Name_Stock = names(all_return)
Name_Stock = Name_Stock[-1]

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

#If too few operations for a pre-processing script
if (channel == "EM") {
  stock_prices = na.locf(stock_prices, na.rm = FALSE)
  macro = na.locf(macro)
}

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
mktcap_index = cbind(ticker, mktcap_index)

# statistical description N mean std Min Median Max
# table 1 Statistical Description
# macro_return_use = macro_return[, which(colnames(macro_return) %in%Macro_Total)]
# sta_macro_return = describe(macro_return_use)
# sta_Stock_return = describe(c(stock_return))
# mktcap_sta = as.numeric(unlist(mktcap[,-1]))
# mktcap_sta = mktcap_sta [which(mktcap_sta > 0)]
# sta_mktcap = describe(c(mktcap_sta))
# sta = rbind(sta_macro_return, sta_Stock_return, sta_mktcap)
# 
# write.csv(
#   sta,
#   paste0(
#     output_path,
#     "/statistical description.csv"
#   ),
#   quote = FALSE
# )
# write.xlsx(sta, paste0(output_path,"/statistical description..xlsx"), sheetName="Sheet1",
#            col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)
# 
# sta_macro = describe(macro[2:nrow(macro), which(colnames(macro_return) %in%Macro_Total)])
# write.xlsx(sta_macro, paste0(output_path,"/statistical description_macro_ori.xlsx"), sheetName="Sheet1",
#            col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)

## 2. Estimation

#Row index corresponding to date_start and date_end
N0 = which(ticker == date_start)
N1 = which(ticker == date_end)

N0_fixed = which(ticker == date_start_fixed)
N1_fixed = which(ticker == date_end_fixed)

N_upd = N1-N0+1
J_dynamic = matrix(0, 1, N_upd)
FRM_individ_i = vector(mode = "list")

N_fixed = N1_fixed-N0_fixed+1
output_path = paste0(output_path,'/',date_end)
dir.create(output_path)

FRM_Algorithm <- function(combination,J_dynamic,N0,N1,mktcap_index,macro_return,FRM_individ, wird_comb){
  for (iColumn in c(1 : ncol(combination))){
    setwd(wird_comb)
    MacroName = combination[, iColumn]
    # create macro dir
    wdir_comebine = as.character(iColumn)
    dir.create(wdir_comebine)
    setwd(wdir_comebine)
    
    dir.create("Adj_Matrices")
    dir.create(paste0("Lambda_DynamicFactor"))
    dir.create(paste0("Lambda_DynamicFactor/Quantiles"))
    dir.create(paste0("Lambda_DynamicFactor/Fixed"))
    for (t in N0:N1) { 
      timestart<-Sys.time()
      
      #Biggest companies at each time point
      biggest_index = as.matrix(mktcap_index[t, 2:(J + 1)])
      biggest_index = sort(biggest_index)
      macro_return_add = macro_return[(t - s):(t - 1), which(colnames(macro_return) %in%
                                                               MacroName)]
      data = stock_return[(t - s + 1):t, biggest_index]
      
      name_temp = colnames(data)
      
      data = cbind(data,macro_return_add)
      data = data[, colSums(data != 0) > 0]
      
      # WRT change standard
      data = scale(data)
      
      M_t = ncol(data)
      # WRT change macro column
      J_t = max(which((grepl(
        "EQUITY|Equity", colnames(data)
      ) == TRUE) &(grepl(
        "FXI", colnames(data)
      ) == FALSE) 
      ))
      
      J_dynamic[t-N0+1] = J_t
      
      #Initialize adjacency matrix
      adj_matix = matrix(0, M_t, M_t) 
      est_lambda_t = vector()
      #FRM quantile regression
      for (k in 1:M_t) { 
        est = FRM_Quantile_Regression(as.matrix(data), k, tau, I)
        est_lambda = abs(data.matrix(est$lambda[which(est$Cgacv == min(est$Cgacv))]))
        est_beta = t(as.matrix(est$beta[which(est$Cgacv == min(est$Cgacv)),]))
        adj_matix[k, -k] = est_beta
        est_lambda_t = c(est_lambda_t, est_lambda)
      }
      #List of vectors of different size with different column names
      est_lambda_t = t(data.frame(est_lambda_t[1:J_t]))
      colnames(est_lambda_t) = colnames(data)[1:J_t]
      FRM_individ[[t-N0+1]] = est_lambda_t
      #Save adjacency matrix
      colnames(adj_matix) = colnames(data)
      rownames(adj_matix) = colnames(data)
      write.csv(adj_matix, paste0("Adj_Matrices/adj_matix_", 
                                  ticker[t], ".csv"), quote = FALSE)
      # WRT change
      timeend<-Sys.time()
      runningtime<-timeend-timestart
      print(paste0(timeend, " runing ", runningtime," varying companies: finish ", as.character(t), "/",  as.character(N1)))
    }
    
    ## 3. Updated FRM index
    
    names(FRM_individ) = ticker[N0:N1]
    
    #Append R dataset to the historical file
    {
      if (file.exists(paste0("Lambda_DynamicFactor/FRM_", channel, ".rds"))) {
        FRM_history_prev = readRDS(paste0("Lambda_DynamicFactor/FRM_", channel, ".rds"))
        FRM_history = c(FRM_history_prev, FRM_individ)
      }
      else
        FRM_history = FRM_individ
    }
    
    #Note: be careful with different values for the same day
    FRM_history = FRM_history[order(unique(names(FRM_history)))]
    N_h = length(FRM_history)
    saveRDS(FRM_history, paste0("Lambda_DynamicFactor/FRM_", channel, ".rds"))
    
    #Transform the list of lambdas into a wide dataset
    stock_names = vector()
    for (t in 1:N_h) stock_names = c(stock_names, attributes(FRM_history[[t]])$dimnames[[2]])
    stock_names = unique(stock_names)
    N_names = length(stock_names)
    lambdas_wide = matrix(0, N_h, N_names+1)
    lambdas_wide[, 1] = names(FRM_history)
    for (k in 1:N_names)
      for (t in 1:N_h)
        if (stock_names[k] %in% attributes(FRM_history[[t]])$dimnames[[2]])
          lambdas_wide[t, k + 1] = FRM_history[[t]][, stock_names[k]]
    colnames(lambdas_wide) = c("date", stock_names)
    
    write.csv(
      lambdas_wide,
      paste0("Lambda_DynamicFactor/lambdas_wide.csv"),
      row.names = FALSE,
      quote = FALSE
    )
    
    #Calculate FRM index as the average
    FRM_index = sapply(1:N_h, function(i) mean(FRM_history[[i]]))
    FRM_index = data.frame(date = names(FRM_history), FRM = FRM_index)
    write.csv(FRM_index, paste0("Lambda_DynamicFactor/FRM_", channel, "_index.csv"),
              row.names = FALSE, quote = FALSE)
    
    #Daily maximum
    FRM_max = sapply(1:N_h, function(i) max(FRM_history[[i]]))
    name_max = sapply(1:N_h, function(i) 
      attributes(FRM_history[[i]])$dimnames[[2]][which(FRM_history[[i]] == FRM_max[i])[1]])
    FRM_max = data.frame(date = names(FRM_history), name = name_max, lambda = FRM_max)
    write.csv(FRM_max, paste0( "Lambda_DynamicFactor/max_lambda.csv"), 
              row.names = FALSE, quote = FALSE)
    
    #Daily minimum
    FRM_min = sapply(1:N_h, function(i) min(FRM_history[[i]]))
    name_min = sapply(1:N_h, function(i) 
      attributes(FRM_history[[i]])$dimnames[[2]][which(FRM_history[[i]] == FRM_min[i])])
    FRM_min = data.frame(date = names(FRM_history), name = name_min, lambda = FRM_min)
    write.csv(FRM_min, paste0("Lambda_DynamicFactor/min_lambda.csv"), 
              row.names = FALSE, quote = FALSE)
    
    #Quantiles
    for (q in quantiles) {
      FRM_q = sapply(1:N_h, function(i) quantile(FRM_history[[i]], q))
      FRM_q = data.frame(date = names(FRM_history), quantile = FRM_q)
      write.csv(FRM_q, paste0("Lambda_DynamicFactor/Quantiles/q", q*100, "_lambda.csv"), 
                row.names = FALSE, quote = FALSE)
    }
    
    ## 5.1 Boxplot
    dir.create(paste0("Boxplot_DynamicFactor"))
    png(paste0("Boxplot_DynamicFactor/Boxplot_", date_start, "_", date_end, "_", 
               channel, ".png"), width = 900, height = 600, bg = "transparent")
    
    outliers = which(FRM_max$lambda > lambda_cutoff)
    
    boxplot(FRM_individ, col = "white", xaxt = "n")
    lines(tail(FRM_index$FRM, N_upd), col = "blue", lwd = 2)
    lines(tail(FRM_max$lambda, N_upd), col = "red", lwd = 2)
    
    div = floor(nrow(FRM_index) / 4)
    plot_labels_m = c(FRM_index$date[div], 
                      FRM_index$date[div*2], 
                      FRM_index$date[div*3])
    
    ll = which(names(FRM_individ) %in% plot_labels_m)
    axis(1, at = ll, labels = plot_labels_m)
    
    dev.off()
    
  }
  
}

# shapley combination
for (iFac_Shap in Macro_Total){
  combinate_0 = data.frame(iFac_Shap)
  Macro_left = Macro_Total[-which(Macro_Total == iFac_Shap)]
  
  combinate_1_plus = data.frame(rbind(rep(iFac_Shap, ncol(combn(Macro_left,1))), combn(Macro_left,1)))
  combinate_1 = combinate_1_plus[2:nrow(combinate_1_plus),]
  
  combinate_2_plus = data.frame(rbind(rep(iFac_Shap, ncol(combn(Macro_left,2))), combn(Macro_left,2)))
  colnames(combinate_2_plus) = "X"
  combinate_2 = data.frame(combinate_2_plus[2:nrow(combinate_2_plus),])
  colnames(combinate_2) = "X"
  
  # Calculate Adj_Matrices
  sFold = paste0(wdir, output_path, "/",iFac_Shap)
  dir.create(sFold)
  dir.create(paste0(wdir, output_path, "/",iFac_Shap,"/Adj_Matrices_ShapleyCom"))
  
  wdir_0 = paste0(wdir, output_path, "/",iFac_Shap,"/Adj_Matrices_ShapleyCom/Comb_0")
  dir.create(wdir_0)
  
  wdir_1_plus = paste0(wdir, output_path, "/",iFac_Shap, "/Adj_Matrices_ShapleyCom/Comb_1_plus")
  dir.create(wdir_1_plus)
  
  wdir_1 = paste0(wdir, output_path, "/",iFac_Shap, "/Adj_Matrices_ShapleyCom/Comb_1")
  dir.create(wdir_1)
  
  wdir_2_plus = paste0(wdir, output_path, "/",iFac_Shap, "/Adj_Matrices_ShapleyCom/Comb_2_plus")
  dir.create(wdir_2_plus)
  
  wdir_2 = paste0(wdir, output_path, "/",iFac_Shap, "/Adj_Matrices_ShapleyCom/Comb_2")
  dir.create(wdir_2)
  
  
  setwd(wdir_0)
  FRM_Algorithm(combinate_0,J_dynamic,N0,N1,mktcap_index,macro_return,FRM_individ_i,wdir_0)
  
  setwd(wdir_1_plus)
  FRM_Algorithm(combinate_1_plus,J_dynamic,N0,N1,mktcap_index,macro_return,FRM_individ_i,wdir_1_plus)
  
  setwd(wdir_1)
  FRM_Algorithm(combinate_1,J_dynamic,N0,N1,mktcap_index,macro_return,FRM_individ_i,wdir_1)
  
  setwd(wdir_2_plus)
  FRM_Algorithm(combinate_2_plus,J_dynamic,N0,N1,mktcap_index,macro_return,FRM_individ_i,wdir_2_plus)
  
  setwd(wdir_2)
  FRM_Algorithm(combinate_2,J_dynamic,N0,N1,mktcap_index,macro_return,FRM_individ_i,wdir_2)
  
}

# Calculate Adj_Matrices
sFold = paste0(wdir, output_path,"/Empty")
dir.create(sFold)
dir.create(paste0(output_path, "/Empty/Adj_Matrices_ShapleyCom"))
wdir_Empty = paste0(wdir, output_path, "/Empty/Adj_Matrices_ShapleyCom")
setwd(wdir_Empty)
FRM_Algorithm_Empty <- function(J_dynamic,N0,N1,mktcap_index,macro_return,FRM_individ, wird_comb){
  setwd(wird_comb)
  
  # create macro dir
  dir.create("Adj_Matrices")
  dir.create(paste0("Lambda_DynamicFactor"))
  dir.create(paste0("Lambda_DynamicFactor/Quantiles"))
  dir.create(paste0("Lambda_DynamicFactor/Fixed"))
  for (t in N0:N1) { 
    timestart<-Sys.time()
    
    #Biggest companies at each time point
    biggest_index = as.matrix(mktcap_index[t, 2:(J + 1)])
    name_temp = colnames(stock_return)[biggest_index]
    data = stock_return[(t - s + 1):t, biggest_index]
    data = data[, colSums(data != 0) > 0]
    
    # WRT change standard
    data = scale(data)
    
    M_t = ncol(data)
    # WRT change macro column
    J_t = max(which((grepl(
      "EQUITY|Equity", colnames(data)
    ) == TRUE) &(grepl(
      "FXI", colnames(data)
    ) == FALSE) 
    ))
    
    J_dynamic[t-N0+1] = J_t
    
    #Initialize adjacency matrix
    adj_matix = matrix(0, M_t, M_t) 
    est_lambda_t = vector()
    #FRM quantile regression
    for (k in 1:M_t) { 
      est = FRM_Quantile_Regression(as.matrix(data), k, tau, I)
      est_lambda = abs(data.matrix(est$lambda[which(est$Cgacv == min(est$Cgacv))]))
      est_beta = t(as.matrix(est$beta[which(est$Cgacv == min(est$Cgacv)),]))
      adj_matix[k, -k] = est_beta
      est_lambda_t = c(est_lambda_t, est_lambda)
    }
    #List of vectors of different size with different column names
    est_lambda_t = t(data.frame(est_lambda_t[1:J_t]))
    colnames(est_lambda_t) = colnames(data)[1:J_t]
    FRM_individ[[t-N0+1]] = est_lambda_t
    #Save adjacency matrix
    colnames(adj_matix) = colnames(data)
    rownames(adj_matix) = colnames(data)
    write.csv(adj_matix, paste0("Adj_Matrices/adj_matix_", 
                                ticker[t], ".csv"), quote = FALSE)
    # WRT change
    timeend<-Sys.time()
    runningtime<-timeend-timestart
    print(paste0(timeend, " runing ", runningtime," varying companies: finish ", as.character(t), "/",  as.character(N1)))
  }
  
  ## 3. Updated FRM index
  names(FRM_individ) = ticker[N0:N1]
  #Append R dataset to the historical file
  {
    if (file.exists(paste0("Lambda_DynamicFactor/FRM_", channel, ".rds"))) {
      FRM_history_prev = readRDS(paste0("Lambda_DynamicFactor/FRM_", channel, ".rds"))
      FRM_history = c(FRM_history_prev, FRM_individ)
    }
    else
      FRM_history = FRM_individ
  }
  
  #Note: be careful with different values for the same day
  FRM_history = FRM_history[order(unique(names(FRM_history)))]
  N_h = length(FRM_history)
  saveRDS(FRM_history, paste0("Lambda_DynamicFactor/FRM_", channel, ".rds"))
  
  #Transform the list of lambdas into a wide dataset
  stock_names = vector()
  for (t in 1:N_h) stock_names = c(stock_names, attributes(FRM_history[[t]])$dimnames[[2]])
  stock_names = unique(stock_names)
  N_names = length(stock_names)
  lambdas_wide = matrix(0, N_h, N_names+1)
  lambdas_wide[, 1] = names(FRM_history)
  for (k in 1:N_names)
    for (t in 1:N_h)
      if (stock_names[k] %in% attributes(FRM_history[[t]])$dimnames[[2]])
        lambdas_wide[t, k + 1] = FRM_history[[t]][, stock_names[k]]
  colnames(lambdas_wide) = c("date", stock_names)
  
  write.csv(
    lambdas_wide,
    paste0("Lambda_DynamicFactor/lambdas_wide.csv"),
    row.names = FALSE,
    quote = FALSE
  )
  
  #Calculate FRM index as the average
  FRM_index = sapply(1:N_h, function(i) mean(FRM_history[[i]]))
  FRM_index = data.frame(date = names(FRM_history), FRM = FRM_index)
  write.csv(FRM_index, paste0("Lambda_DynamicFactor/FRM_", channel, "_index.csv"),
            row.names = FALSE, quote = FALSE)
  
  #Daily maximum
  FRM_max = sapply(1:N_h, function(i) max(FRM_history[[i]]))
  name_max = sapply(1:N_h, function(i) 
    attributes(FRM_history[[i]])$dimnames[[2]][which(FRM_history[[i]] == FRM_max[i])[1]])
  FRM_max = data.frame(date = names(FRM_history), name = name_max, lambda = FRM_max)
  write.csv(FRM_max, paste0( "Lambda_DynamicFactor/max_lambda.csv"), 
            row.names = FALSE, quote = FALSE)
  
  #Daily minimum
  FRM_min = sapply(1:N_h, function(i) min(FRM_history[[i]]))
  name_min = sapply(1:N_h, function(i) 
    attributes(FRM_history[[i]])$dimnames[[2]][which(FRM_history[[i]] == FRM_min[i])])
  FRM_min = data.frame(date = names(FRM_history), name = name_min, lambda = FRM_min)
  write.csv(FRM_min, paste0("Lambda_DynamicFactor/min_lambda.csv"), 
            row.names = FALSE, quote = FALSE)
  
  #Quantiles
  for (q in quantiles) {
    FRM_q = sapply(1:N_h, function(i) quantile(FRM_history[[i]], q))
    FRM_q = data.frame(date = names(FRM_history), quantile = FRM_q)
    write.csv(FRM_q, paste0("Lambda_DynamicFactor/Quantiles/q", q*100, "_lambda.csv"), 
              row.names = FALSE, quote = FALSE)
  }
  
  ## 5.1 Boxplot
  dir.create(paste0("Boxplot_DynamicFactor"))
  png(paste0("Boxplot_DynamicFactor/Boxplot_", date_start, "_", date_end, "_", 
             channel, ".png"), width = 900, height = 600, bg = "transparent")
  
  outliers = which(FRM_max$lambda > lambda_cutoff)
  
  boxplot(FRM_individ, col = "white", xaxt = "n")
  lines(tail(FRM_index$FRM, N_upd), col = "blue", lwd = 2)
  lines(tail(FRM_max$lambda, N_upd), col = "red", lwd = 2)
  
  div = floor(nrow(FRM_index) / 4)
  plot_labels_m = c(FRM_index$date[div], 
                    FRM_index$date[div*2], 
                    FRM_index$date[div*3])
  
  ll = which(names(FRM_individ) %in% plot_labels_m)
  axis(1, at = ll, labels = plot_labels_m)
  
  dev.off()
  
}
FRM_Algorithm_Empty(J_dynamic,N0,N1,mktcap_index,macro_return,FRM_individ_i,wdir_Empty)




