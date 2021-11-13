## 0. Preparation

rm(list = ls(all = TRUE))

libraries = c("iml","ggplot2", "data.table", "igraph","timeDate", "stringr", "graphics","magick", "scales", "tidyr", "zoo", "foreach", "doParallel",
              "xgboost","shapr","randomForest", "rpart", "quantreg", "readxl","dplyr", "xlsx", "psych","qgraph", "gganimate","av",
              "gifski", "strex","matrixStats","tools")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#----------------------------------------START UPDATE----------------------------------------

wdir = "/Users/wkhaerdle/Documents/Project/FRM-master_Modify_WRT/FRM_Quantlet/FRM_All/"

#Choose between "Americas", "Europe", "Crypto", "SP500", "ER", "Asia", "EM"
channel = "Asia"

#Data source
date_end_source = 20210210
#Index output, varying companies
date_start = 20190417
date_end = 20210210

#Network output, fixed companies
date_start_fixed = 20191210
date_end_fixed = 20200526
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

setwd(wdir)

input_path = paste0("Input/", channel, "/", date_start_source, "-", date_end_source)
if (tau == 0.05) output_path = paste0("Output/", channel) else 
  output_path = paste0("Output/", channel, "/Sensitivity/tau=", 100*tau, "/s=", s)

#TODO choose between quantile and expectile in the header
source("FRM_Statistics_Algorithm.R")

## 1. Data Preprocess

mktcap = read.csv(file = paste0(input_path, "/", channel, "_Mktcap_", 
                  date_end_source, ".csv"), header = TRUE) %>% as.matrix()
stock_prices = read.csv(file = paste0(input_path, "/", channel, "_Price_", 
                        date_end_source, ".csv"), header = TRUE)
macro = read.csv(file = paste0(input_path, "/", channel, "_Macro_20210322.csv"), header = TRUE)
macro[is.na(macro)] = 0

# replace zero with the last data
Name_Macro = names(macro)
Macro_Total = Name_Macro[grepl("FXI|CN2YR|CN210SLOPE", Name_Macro)]
for (iM in Macro_Total){
  loc = which(Name_Macro == iM)
  Temp = macro[, loc]
  loc_0 = which(Temp == 0)
 
  for (iloc in loc_0){
    if (iloc == 1){
      Temp[iloc] = Temp[iloc+1]
    }
    else{
      Temp[iloc] = Temp[iloc-1]
    }
  }
  macro[, loc] = Temp
}
Date_Macro = as.Date(macro$Date)


if (!all(sort(colnames(mktcap)) == sort(colnames(stock_prices)))) 
  stop("columns do not match")

M_stock = ncol(mktcap)-1

# WRT change 
Name_Stock = names(stock_prices)
Name_Stock = Name_Stock[-1]
# write.xlsx(Name_Stock, paste0(output_path,"/Name_Stock.xlsx"), sheetName="Sheet1",
#            col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)

Macro_Spread = which(grepl("Spread", Name_Macro))
Name_Spread = Name_Macro[Macro_Spread]
macro[, Macro_Spread] = macro[, Macro_Spread]/100

Name_BondFactor = union(Name_Macro[grepl("CCB", Name_Macro)], Name_Macro[grepl("Treasury_InterBank", Name_Macro)])
M_Macro_BondFactor = length(Name_BondFactor)
M_macro = ncol(macro)-1

colnames(mktcap)[1] = "ticker"
colnames(stock_prices)[1] = "ticker"
colnames(macro)[1] = "ticker"

#If too few operations for a pre-processing script
if (channel == "EM") {
  stock_prices = na.locf(stock_prices, na.rm = FALSE)
  macro = na.locf(macro)
}

if (!all(!is.na(macro))) stop("missing macros")

#Note: if missing market caps are kept NA, the column will be excluded 
#from top J  => do not fill up NA in mktcap
mktcap[is.na(mktcap)] = 0

#Load the stock prices and macro-prudential data matrix
all_prices = merge(stock_prices, macro, by = "ticker", all.x = TRUE)

ticker_str = as.data.frame(all_prices$ticker[-1])
colnames(ticker_str) = "ticker"
ticker = as.numeric(gsub("-", "", ticker_str$ticker))

N = nrow(ticker_str)

#Align mktcap rows to all_return
mktcap = merge(ticker_str, mktcap, by = "ticker", all.x = TRUE)

if (!all(mktcap$ticker == ticker_str$ticker)) stop("dates do not match")

if (channel %in% c("Americas", "SP500")) {
  all_prices$USGG3M10YR = exp(all_prices$USGG3M10YR)
  all_prices$USGG3M.INDEX = exp(all_prices$USGG3M.INDEX)
  all_prices$MOODCBAA10YRSPD = exp(all_prices$MOODCBAA10YRSPD)} else
if (channel == "Asia") {
  #all_prices$CN2YR = exp(all_prices$CN2YR)
  #all_prices$CN210SLOPE = exp(all_prices$CN210SLOPE)
  #all_prices$CN10YR = exp(all_prices$CN10YR)
  } else
if (channel == "Europe") {
  all_prices$BTPSDBR10 = exp(all_prices$BTPSDBR10)
  all_prices$EUAggCorp10 = exp(all_prices$EUAggCorp10)
  all_prices$DBR0110 = exp(all_prices$DBR0110)} else
if (channel == "Crypto") {
  all_prices$BV010082.INDEX = exp(all_prices$BV010082.INDEX)} else
if (channel == "ER") {
  all_prices$SLOPE_DBR = exp(all_prices$SLOPE_DBR)
  all_prices$LIQ = exp(all_prices$LIQ)
  all_prices$GE2 = exp(all_prices$GE2)
  all_prices$BE = exp(all_prices$BE)
  all_prices$SP = exp(all_prices$SP)
  all_prices$IT = exp(all_prices$IT)
  all_prices$AT = exp(all_prices$AT)
  all_prices$GE = exp(all_prices$GE)
  all_prices$FR = exp(all_prices$FR)
  all_prices$PO = exp(all_prices$PO)
  all_prices$IR = exp(all_prices$IR)
  all_prices$FI = exp(all_prices$FI)
  all_prices$NE = exp(all_prices$NE)
  all_prices$GR = exp(all_prices$GR)} else
if (channel == "EM") {
  all_prices$USGG3M10YR = exp(all_prices$USGG3M10YR)
  all_prices$USGG3M.INDEX = exp(all_prices$USGG3M.INDEX)
  all_prices$MOODCBAA10YRSPD = exp(all_prices$MOODCBAA10YRSPD)
  all_prices$JPEGSOSD.Index = exp(all_prices$JPEGSOSD.Index)
  all_prices$JPEGSOSD.Index = all_prices$JPEGSOSD.Index / 100
}

#Calculate the daily return and differences matrix of all selected financial 
#companies and macro-prudential variables; use exponential function for selected
#macro-prudential variables that are expressed in first order differences
all_prices[, -1] = sapply(all_prices[, -1], as.numeric)

# WRT change
# all_return = diff(log(as.matrix(all_prices[, -1])))
all_return = all_prices[-1, -1]
all_return[,] = 0

# log difference of stock price, FXI.US.EQUITY, VXFXI.INDEX
Column_logdiff = c(Name_Stock, "FXI.US.EQUITY", "VXFXI.INDEX")
all_return[,which(names(all_return)%in%Column_logdiff)] = 
  as.data.frame(diff(log(as.matrix(all_prices[, which(names(all_prices)%in%Column_logdiff)])))) 

#  difference of macro
Column_diff = c("CN2YR", "CN210SLOPE", "TED", Name_Spread, Name_BondFactor)
all_return[,which(names(all_return)%in%Column_diff)] = 
  as.data.frame(diff(as.matrix(all_prices[, which(names(all_prices)%in%Column_diff)]))) 

# RealEstateDiff
all_return$RealEstateDiff = all_prices$RealEstateDiff[-1]

all_return = as.matrix(all_return) 
all_return[is.na(all_return)] = 0
stock_return = all_return[, 1 : M_stock]
macro_return = all_return[, (M_stock+1):ncol(all_return)]

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
macro_return_use = macro_return[, which(colnames(macro_return) %in%Macro_Total)]
sta_macro_return = describe(macro_return_use)
sta_Stock_return = describe(c(stock_return))
mktcap_sta = as.numeric(unlist(mktcap[,-1]))
mktcap_sta = mktcap_sta [which(mktcap_sta > 0)]
sta_mktcap = describe(c(mktcap_sta))
sta = rbind(sta_macro_return, sta_Stock_return, sta_mktcap)

write.csv(
  sta,
  paste0(
    output_path,
    "/statistical description.csv"
  ),
  quote = FALSE
)
write.xlsx(sta, paste0(output_path,"/statistical description..xlsx"), sheetName="Sheet1",
           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)

sta_macro = describe(macro[2:nrow(macro), which(colnames(macro_return) %in%Macro_Total)])
write.xlsx(sta_macro, paste0(output_path,"/statistical description_macro_ori.xlsx"), sheetName="Sheet1",
           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)

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
      macro_return_add = macro_return[(t - s):(t - 1), which(colnames(macro_return) %in%
                                                               MacroName)]
      name_temp = colnames(stock_return)[biggest_index]
      data = cbind(stock_return[(t - s + 1):t, biggest_index],
                   macro_return_add)
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
  combinate_2 = combinate_2_plus[2:nrow(combinate_2_plus),]
  
  combinate_3_plus = data.frame(rbind(rep(iFac_Shap, ncol(combn(Macro_left,3))), combn(Macro_left,3)))
  colnames(combinate_3_plus) = "X"
  combinate_3 = data.frame(combinate_3_plus[2:nrow(combinate_3_plus),])
  colnames(combinate_3) = "X"
  
  # Calculate Adj_Matrices
  sFold = paste0(wdir, output_path, "/",iFac_Shap)
  dir.create(sFold)
  dir.create(paste0(output_path, "/",iFac_Shap,"/Adj_Matrices_ShapleyCom"))
  
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
  
  wdir_3_plus = paste0(wdir, output_path, "/",iFac_Shap, "/Adj_Matrices_ShapleyCom/Comb_3_plus")
  dir.create(wdir_3_plus)
  
  wdir_3 = paste0(wdir, output_path, "/",iFac_Shap, "/Adj_Matrices_ShapleyCom/Comb_3")
  dir.create(wdir_3)
  
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
  
  setwd(wdir_3_plus)
  FRM_Algorithm(combinate_3_plus,J_dynamic,N0,N1,mktcap_index,macro_return,FRM_individ_i,wdir_3_plus)
  
  setwd(wdir_3)
  FRM_Algorithm(combinate_3,J_dynamic,N0,N1,mktcap_index,macro_return,FRM_individ_i,wdir_3)
  
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

# Calculate Shapley
output_path = paste0(wdir,'Output 20210727/Output/Asia')
address_Empty = paste0(output_path,'/Empty/Adj_Matrices_ShapleyCom/Lambda_DynamicFactor')
FRM_Cal_Empty =  read.csv(file = paste0(address_Empty, "/FRM_Asia_index.csv"), header = TRUE)

# Calculate Shapley
Shapley_Group <- function(combination, address){
  for (iCombine in c(1 : ncol(combination))){
    FRM_T = read.csv(file = paste0(address, "/", iCombine, "/Lambda_DynamicFactor/FRM_Asia_index.csv"), header = TRUE)
    
    if (iCombine == 1){
      FRM_Cal = FRM_T
    }else{
      FRM_Cal = cbind(FRM_Cal, FRM_Cal[, 2])
    }
  }
  return(FRM_Cal)
}

weight_0 = factorial(0)*factorial(4-0-1) / factorial(4)
weight_1 = factorial(1)*factorial(4-1-1) / factorial(4)
weight_2 = factorial(2)*factorial(4-2-1) / factorial(4)
weight_3 = factorial(3)*factorial(4-3-1) / factorial(4)

for (iFac in c(1:length(Macro_Total))){
  iFac_Shap = Macro_Total[iFac]
  
  combinate_0 = data.frame(iFac_Shap)
  address = paste0(output_path, "/",iFac_Shap,"/Adj_Matrices_ShapleyCom/Comb_0")
  FRM_Cal_0 = Shapley_Group(combinate_0, address) 
  Shapley_0 = abs(weight_0*(FRM_Cal_0[,2]-FRM_Cal_Empty[,2]))
    
  Macro_left = Macro_Total[-which(Macro_Total == iFac_Shap)]
  
  combinate_1_plus = data.frame(rbind(rep(iFac_Shap, ncol(combn(Macro_left,1))), combn(Macro_left,1)))
  combinate_1 = combinate_1_plus[2:nrow(combinate_1_plus),]
  
  combinate_2_plus = data.frame(rbind(rep(iFac_Shap, ncol(combn(Macro_left,2))), combn(Macro_left,2)))
  combinate_2 = combinate_2_plus[2:nrow(combinate_2_plus),]
  
  combinate_3_plus = data.frame(rbind(rep(iFac_Shap, ncol(combn(Macro_left,3))), combn(Macro_left,3)))
  colnames(combinate_3_plus) = "X"
  combinate_3 = data.frame(combinate_3_plus[2:nrow(combinate_3_plus),])
  colnames(combinate_3) = "X"
  
  address = paste0(output_path, "/",iFac_Shap,"/Adj_Matrices_ShapleyCom/Comb_1_plus")
  FRM_Cal_1_plus = Shapley_Group(combinate_1_plus, address) 
  Shapley_1_plus = rowSums(FRM_Cal_1_plus[, c(2:ncol(FRM_Cal_1_plus))])
  
  address = paste0(output_path, "/",iFac_Shap,"/Adj_Matrices_ShapleyCom/Comb_1")
  FRM_Cal_1 = Shapley_Group(combinate_1, address) 
  Shapley_1 = rowSums(FRM_Cal_1[, c(2:ncol(FRM_Cal_1))])
  Shapley_Com_1 = (weight_1*(Shapley_1_plus-Shapley_1))
  
  address = paste0(output_path, "/",iFac_Shap,"/Adj_Matrices_ShapleyCom/Comb_2_plus")
  FRM_Cal_2_plus = Shapley_Group(combinate_2_plus, address) 
  Shapley_2_plus = rowSums(FRM_Cal_2_plus[, c(2:ncol(FRM_Cal_2_plus))])
  
  address = paste0(output_path, "/",iFac_Shap,"/Adj_Matrices_ShapleyCom/Comb_2")
  FRM_Cal_2 = Shapley_Group(combinate_2, address) 
  Shapley_2 = rowSums(FRM_Cal_2[, c(2:ncol(FRM_Cal_2))])
  Shapley_Com_2 = (weight_2*(Shapley_2_plus-Shapley_2))
  
  address = paste0(output_path, "/",iFac_Shap,"/Adj_Matrices_ShapleyCom/Comb_3_plus")
  FRM_Cal_3_plus = Shapley_Group(combinate_3_plus, address) 
  Shapley_3_plus = FRM_Cal_3_plus[, 2]
  
  address = paste0(output_path, "/",iFac_Shap,"/Adj_Matrices_ShapleyCom/Comb_3")
  FRM_Cal_3 = Shapley_Group(combinate_3, address) 
  Shapley_3 = FRM_Cal_3[, 2]
  Shapley_Com_3 = (weight_3*(Shapley_3_plus-Shapley_3))
  
  Shapley_Com = data.frame("date" = FRM_Cal_3[, 1], 
                           "Shapley" = Shapley_0 + Shapley_Com_1 + Shapley_Com_2 + Shapley_Com_3)
  if (iFac == 1){
    Shapley = Shapley_Com
  }else{
    Shapley = cbind(Shapley, Shapley_Com[, 2])
  }
}
colnames(Shapley) = c("Date", Macro_Total)

Shapley$Date = as.Date(paste0(floor(Shapley$Date / 10000),"-",
                             floor(Shapley$Date / 100) -floor(Shapley$Date / 10000)*100,"-",
                      Shapley$Date - floor(Shapley$Date / 100) * 100))

Shapley_Mean = colMeans(Shapley[, c(2:ncol(Shapley))])
Shapley_Mean = data.frame("Macro" = Macro_Total, "Shapley_Mean" = Shapley_Mean)
Shapley_Mean$Shapley_Mean = round(Shapley_Mean$Shapley_Mean,5)

png(paste0(output_path, "/Shapley_Mean_n2.png"), width = 900, height = 600, bg = "transparent")
print(ggplot(Shapley_Mean,aes(x= reorder(Macro, Shapley_Mean),y=Shapley_Mean, fill = Macro))+geom_bar(stat="identity", width=0.5)
      +scale_fill_manual(values = c("FXI.US.EQUITY" ="red", "VXFXI.INDEX"="grey", "CN2YR" = "orange","CN210SLOPE" = '#14B294'))
      +coord_flip()
      +geom_text(aes(label=Shapley_Mean), vjust=-5, size=5)
      +labs(y = "Shapley Values", size = 12)+labs(x = "Macro Features", size = 12)+
        theme(axis.text.x = element_text(size =20, angle = 30, hjust = 1), 
              axis.text.y = element_text(size = 20), 
              axis.title =  element_text(size=20,face = "bold"),
              panel.grid.major =element_blank(), 
              panel.grid.minor = element_blank(),
              plot.title = element_text(hjust = 0.5,size = 35, face = "bold"),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black"),legend.position="none")
      )
dev.off()

Shapley_before = Shapley[Shapley$Date <= "2020-01-20", ]
Shapley_Mean_before = colMeans(Shapley_before[, c(2:ncol(Shapley))])
Shapley_Mean_before = data.frame("Macro" = Macro_Total, "Shapley_Mean" = Shapley_Mean_before)
Shapley_Mean_before$Shapley_Mean = round(Shapley_Mean_before$Shapley_Mean,5)

Shapley_after = Shapley[Shapley$Date > "2020-01-20", ]
Shapley_Mean_after = colMeans(Shapley_after[, c(2:ncol(Shapley))])
Shapley_Mean_after = data.frame("Macro" = Macro_Total, "Shapley_Mean" = Shapley_Mean_after)
Shapley_Mean_after$Shapley_Mean = round(Shapley_Mean_after$Shapley_Mean,5)

Shapley_Mean_before$Macro =  c("FXI.US.EQUITY_Before", " VXFXI.INDEX_Before", "CN2YR_Before", "CN210SLOPE_Before")
Shapley_Mean_before = Shapley_Mean_before[order(-Shapley_Mean_before$Shapley_Mean),]

Shapley_Mean_after$Macro =  c("FXI.US.EQUITY_After", " VXFXI.INDEX_After", "CN2YR_After", "CN210SLOPE_After")
Shapley_Mean_after  = Shapley_Mean_after[order(-Shapley_Mean_after$Shapley_Mean),]
Shapley_Mean_Compare = data.frame(Macro = c(Shapley_Mean_before$Macro,Shapley_Mean_after$Macro), 
                                  Shapley_Mean = c(Shapley_Mean_before$Shapley_Mean,Shapley_Mean_after$Shapley_Mean))
Shapley_Mean_Compare$Period = c(length(Shapley_Mean_Compare$Macro):1)
png(paste0(output_path, "/Shapley_Mean_Compare.png"), width = 900, height = 600, bg = "transparent")
print(ggplot(Shapley_Mean_Compare,aes(x= reorder(Macro, Period),y=Shapley_Mean, fill = Macro))+geom_bar(stat="identity", width=0.5)
      +scale_fill_manual(values = c("FXI.US.EQUITY_Before" ="red", "VXFXI.INDEX_Before"="grey", "CN2YR_Before" = "orange","CN210SLOPE_Before" = '#14B294',
                                    "FXI.US.EQUITY_After" ="red", "VXFXI.INDEX_After"="grey", "CN2YR_After" = "orange","CN210SLOPE_After" = '#14B294'
                                    ))
      +coord_flip()
      +geom_text(aes(label=Shapley_Mean), vjust=-2, size=5)
      +labs(y = "Shapley Values", size = 12)+labs(x = "Macro Features", size = 12)+
        theme(axis.text.x = element_text(size =20, angle = 30, hjust = 1), 
              axis.text.y = element_text(size = 20), 
              axis.title =  element_text(size=20,face = "bold"),
              panel.grid.major =element_blank(), 
              panel.grid.minor = element_blank(),
              plot.title = element_text(hjust = 0.5,size = 35, face = "bold"),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black"),legend.position="none")
)
dev.off()

Shapley_R_Mean = data.frame("Date" = Shapley$Date, "Shapley_Mean" = rowMeans(Shapley[, c(2:ncol(Shapley))]))
Shapley_Win_Mean = data.frame("Date" = Shapley$Date, "Shapley_Win" = NA)
Freq = 63
for (iDate in c(Freq:length(Shapley_R_Mean$Date))){
  Temp_p = Shapley_R_Mean$Shapley_Mean[(iDate-Freq+1):iDate]
  Shapley_Win_Mean$Shapley_Win[iDate] = mean(Temp_p)
}
Shapley_Win_Mean = Shapley_Win_Mean[-c(1:(Freq - 1)), ]
Shapley_R_Mean = Shapley_R_Mean[-c(1:(Freq - 1)), ]

datebreaks <- seq(Shapley_Win_Mean$Date[1], Shapley_Win_Mean$Date[length(Shapley_Win_Mean$Date)],
                  by = "3 month")
png(paste0(output_path, "/Shapley_Win_Mean.png"), width = 900, height = 600, bg = "transparent")

print(ggplot()+geom_line(data = Shapley_Win_Mean,aes(x = Date,y = Shapley_Win),colour = "red",size=1.5)+
        geom_line(data = Shapley_R_Mean,aes(x = Date,y = Shapley_Mean),colour = "black",size=0.5)+
        xlab("Date")+ylab("Shapley Values")+
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

# Factor Ranking
ShapleyRanking = data.frame(Date = Shapley$Date, Top1 = NA, Top2 = NA, Top3 = NA, Top4 = NA)
ShapleyRolling = data.frame(Date = Shapley$Date, FXI.US.EQUITY = NA, VXFXI.INDEX = NA, CN2YR = NA, CN210SLOPE = NA)

Freq = 63
for (iDate in c(Freq:length(Shapley$Date))){
  Temp_p = Shapley[(iDate-Freq+1):iDate, ]
  Temp = colMeans(Temp_p[, -1])
  sort = order(Temp, decreasing = TRUE)
  ShapleyRanking[iDate, c(2 : ncol(ShapleyRanking))] = Macro_Total[sort]
  ShapleyRolling[iDate, c(2 : ncol(ShapleyRanking))] = Temp
}
ShapleyRanking=ShapleyRanking[-c(1:(Freq - 1)), ]
ShapleyRolling=ShapleyRolling[-c(1:(Freq - 1)), ]

write.csv(ShapleyRanking, paste0(output_path, "/ShapleyRanking.csv"), quote = FALSE)

datebreaks <- seq(ShapleyRolling$Date[1], ShapleyRolling$Date[length(ShapleyRolling$Date)],
                  by = "3 month")
png(paste0(output_path, "/Shapley_rolling.png"), width = 900, height = 600, bg = "transparent")

print(ggplot()+geom_line(data = ShapleyRolling,aes(x = Date,y = FXI.US.EQUITY),colour = "red",size=1)+
        geom_line(data = ShapleyRolling,aes(x = Date,y = VXFXI.INDEX),colour = "gray",size=1)+
        geom_line(data = ShapleyRolling,aes(x = Date,y = CN2YR),colour = "orange",size=1)+
        geom_line(data = ShapleyRolling,aes(x = Date,y = CN210SLOPE),colour = '#14B294',size=1)+
        xlab("Date")+ylab("Shapley Values")+
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

sta_Shapley = data.frame(Macro = c(Macro_Total, "FRM"), mean = NA, max = NA, min = NA, sd = NA)

for (iFac in Macro_Total){
  sta = describe(Shapley[,iFac])
  sta_Shapley[which(sta_Shapley$Macro == iFac), c(2 : ncol(sta_Shapley))] = c(sta$mean, sta$max, sta$min, sta$sd)
}

address = paste0(output_path, "/FXI.US.EQUITY/Adj_Matrices_ShapleyCom/Comb_3_plus")
FRM_T = read.csv(file = paste0(address, "/1/Lambda_DynamicFactor/FRM_Asia_index.csv"), header = TRUE)
sta = describe(FRM_T[,2])
sta_Shapley[which(sta_Shapley$Macro == "FRM"), c(2 : ncol(sta_Shapley))] = c(sta$mean, sta$max, sta$min, sta$sd)
write.csv(sta_Shapley, paste0(output_path, "/Description of Shapley.csv"), quote = FALSE)

## 2.2 Fixed companies or coins 
#Make companies constant, select the biggest companies 
wdir_fix = paste0(output_path, "/Fixed")
dir.create(wdir_fix)

biggest_index_fixed = as.matrix(mktcap_index[N0_fixed, 2:(J+1)])

#Note: dependent variable cannot be all 0
M_J = J+4

FRM_individ_fixed = matrix(0, N_fixed, J+1)
FRM_individ_fixed[, 1] = ticker[N0_fixed:N1_fixed]

for (t in N0_fixed:N1_fixed) { 
  timestart<-Sys.time()
  macro_return_add = macro_return[(t - s):(t - 1), which(colnames(macro_return) %in%
                                                           Macro_Total)]
  data_fixed = cbind(stock_return[(t - s + 1):t, biggest_index_fixed],
               macro_return_add)
  
  adj_matix_fixed = matrix(0, M_J, M_J) 
  if(all(colSums(data_fixed != 0) > 0)) {
    # WRT change standard
    data_fixed = scale(data_fixed)
    #FRM quantile regression
    for (k in 1:M_J) { 
      est_fixed = FRM_Quantile_Regression(as.matrix(data_fixed), k, tau, I)
      est_lambda_fixed = abs(data.matrix(est_fixed$lambda[
        which(est_fixed$Cgacv == min(est_fixed$Cgacv))]))
      est_beta_fixed = t(as.matrix(est_fixed$beta[
        which(est_fixed$Cgacv == min(est_fixed$Cgacv)),]))
      adj_matix_fixed[k, -k] = est_beta_fixed
      if (k <= J) FRM_individ_fixed[t-N0_fixed+1, k+1] = est_lambda_fixed  
    }
    # Save adjacency matrix
      colnames(adj_matix_fixed) = colnames(data_fixed)
      rownames(adj_matix_fixed) = colnames(data_fixed)
      write.csv(adj_matix_fixed, paste0(wdir_fix, "/adj_matix_",
                                        ticker[t], ".csv"), quote = FALSE)
  } else {
    warning("column with 0 return, check input correctness")
  }
  # WRT change
  timeend<-Sys.time()
  runningtime<-timeend-timestart
  print(paste0(timeend, " runing ", runningtime," Fixed companies: finish ", as.character(t), "/",  as.character(N1)))
}

#Saved fixed lambdas for the specified period
wdir_fix_Risk = paste0(wdir_fix, "/", "Risk")
dir.create(wdir_fix_Risk)
colnames(FRM_individ_fixed) = c("date", colnames(data_fixed)[1:J])
write.csv(FRM_individ_fixed, paste0(wdir_fix_Risk, "/lambdas_fixed_", 
                                    date_start_fixed, "_", date_end_fixed, ".csv"),
          row.names = FALSE, quote = FALSE)

# Highest Co-stress FIs
Total_Cor = read_excel(paste0(input_path, "/","Name_Stock.xlsx"),sheet = 'Add Short Name')
Total_Cor = data.frame(Total_Cor)

RiskCenter = matrix(data=NA, nrow = length(FRM_individ_fixed[,1]), ncol = 13)
RiskCenter[, 1]= FRM_individ_fixed[,1]
colnames(RiskCenter) = c("date", "Top1_ID", "Top1_Name", "Top1_Sector", "Top1_FRM", 
                         "Top2_ID", "Top2_Name", "Top2_Sector", "Top2_FRM", 
                         "Top3_ID", "Top3_Name", "Top3_Sector", "Top3_FRM")
RiskCenter = data.frame(RiskCenter)

fun_loc = function(x){
  loc = which(Total_Cor$StockID_Raw == x)
  name = Total_Cor$Name[loc]
  sector = Total_Cor$GICS_INDUSTRY_GROUP_NAME[loc]
  infor = data.frame(name = name, sector = sector)
  return(infor)
}

N0_fixed_net = which(FRM_individ_fixed[,'date'] == date_start_fixed)
N1_fixed_net = which(FRM_individ_fixed[,'date'] == date_end_fixed)
for (day in N0_fixed_net:N1_fixed_net){
  Judge = FRM_individ_fixed[day-N0_fixed_net+1,-1]

  FRM_Order = Judge[order(Judge, decreasing = TRUE)]
  FRM_Order = FRM_Order[1:3]
  id = names(FRM_Order) [1:3]
  RiskCenter[day-N0_fixed_net+1, c(2, 6, 10)] = id
  
  # find posi in Total_Cor 
  infor = lapply(id, fun_loc)
  RiskCenter[day-N0_fixed_net+1, 3:4] = data.frame(infor[[1]])
  RiskCenter[day-N0_fixed_net+1, 7:8] = data.frame(infor[[2]])
  RiskCenter[day-N0_fixed_net+1, 11:12] = data.frame(infor[[3]])
  
  RiskCenter[day-N0_fixed_net+1,c(5, 9, 13)] = round(as.numeric(FRM_Order), digits=2)
  
}

write.csv(RiskCenter, paste0(wdir_fix_Risk, "/RiskCenter_20210802_", date_end, "_", channel, ".csv"))
write.xlsx(RiskCenter, paste0(wdir_fix_Risk, "/RiskCenter_20210802_", date_end, "_", channel, ".xlsx"))

## 6. Network
dir.create(paste0(wdir_fix_Risk, "/Network"))
dir_T = paste0(output_path, "/CN2YR/Adj_Matrices_ShapleyCom/Comb_3_plus/1/")
FRM_history = readRDS(paste0(dir_T,
  "/Lambda_DynamicFactor/FRM_", channel, ".rds"))
FRM_individ_fixed = read.csv(paste0(wdir_fix_Risk, "/lambdas_fixed_", 
                                    date_start_fixed, "_", date_end_fixed, ".csv"), header = TRUE) %>% as.matrix()
FRM_index = read.csv(paste0(dir_T, "/Lambda_DynamicFactor/FRM_", channel, "_index.csv"), header = TRUE)
N0_fixed_net = which(names(FRM_history) == date_start_fixed)
N1_fixed_net = which(names(FRM_history) == date_end_fixed)

stock_main = "X600030.CH.EQUITY" # "X600030.CH.EQUITY" "X2881.TT.EQUITY" "X5.HK.EQUITY"
fig = image_graph(width = 1000, height = 1000, res = 96, bg = "transparent")
Total_Cor = read_excel(paste0(input_path, "/","Name_Stock.xlsx"),sheet = 'Add Short Name')

dir.create(paste0(wdir_fix_Risk, "/",stock_main))
for (t in N0_fixed_net:N1_fixed_net) {
  # png(paste0(wdir_fix_Risk, "/",stock_main, "/network_",names(FRM_history)[t],"_", stock_main,".png"), width = 900, height = 600, bg = "transparent")
  adj0 = read.csv(file=paste0(wdir_fix, "/Adj_Matix_", 
                              names(FRM_history)[t], ".csv"), 
                  header = TRUE, sep = "," , row.names = 1)
  adj0 = as.matrix(adj0)[1:J, 1:J] 
  names_Cir = colnames(adj0) #leftjoin
  names_Cir = data.frame(StockID_Raw = names_Cir)
  
  for (iStock in names_Cir$StockID_Raw){
    loc = which(Total_Cor$StockID_Raw == iStock)
    names_Cir$ShortName[names_Cir$StockID_Raw == iStock] = Total_Cor$ShortName[loc] 
  }

  adj0 = apply(adj0, 2, as.numeric)
  netw1 = graph_from_adjacency_matrix(adj0, mode = "directed", weighted = T)
  V(netw1)$color = ifelse(V(netw1)$name == stock_main, "orange", "lightgrey")
  colors = rep("Gray", alpha.f = .8, length(E(netw1)))
  colors = ifelse(head_of(netw1, E(netw1))$name == stock_main, 'blue', colors) #inflow
  colors = ifelse(tail_of(netw1, E(netw1))$name == stock_main, 'orange', colors) #outflow
  
  plot(netw1, layout = layout_in_circle, vertex.label = names_Cir$ShortName, edge.width = 0.8, 
       edge.color = colors, edge.arrow.size = 0.9, edge.arrow.width = 1, 
       vertex.size = 10*FRM_individ_fixed[t-N0_fixed_net+1, -1])
  title(xlab = paste0(FRM_index$date[t], "\n FRM: ", round(FRM_index$FRM[t], 5)), 
        cex.lab = 1.15, font.lab = 2, line = -0.5)
  # dev.off() #together with png
}

dev.off()

animation <- image_animate(fig, fps = 5)
image_write(animation, paste0(wdir_fix_Risk, "/", stock_main, "/Network_",stock_main,"_", date_start_fixed, "_", 
                              date_end_fixed, "_", channel, ".gif"))

# who transfer to it and which it spill out?
RiskDire_Out= matrix(data=NA, nrow = length(FRM_individ_fixed[,1]), ncol = J+1)
RiskDire_Out[,1] = FRM_individ_fixed[,1]
name = c(1:J)
name = paste0('Top_',name)
colnames(RiskDire_Out) = c('Date', name)
RiskDire_In = RiskDire_Out
for (t in N0_fixed_net:N1_fixed_net) {
  adj0 = read.csv(file=paste0(wdir_fix, "/Adj_Matix_", 
                              names(FRM_history)[t], ".csv"), 
                  header = TRUE, sep = "," , row.names = 1)
  adj0 = abs(as.matrix(adj0)[1:J, 1:J])
  
  Out = adj0[,which(colnames(adj0) == stock_main)]
  Out = Out[which(Out!=0)]
  if (length(Out) > 0 ){
    Out_Name = names(Out)[order(Out, decreasing = TRUE)]
    for (iStock in Out_Name){
      loc = which(Total_Cor$StockID_Raw == iStock)
      Out_Name[Out_Name == iStock] = Total_Cor$Short_N_Sector[loc] 
    }
    # Out_Name = as.character(lapply(Out_Name, fun_StockName))
    RiskDire_Out[t - N0_fixed_net + 1,c(2:(length(Out_Name)+1))] = Out_Name
  }
  
  In = adj0[which(colnames(adj0) == stock_main),]
  In = In[which(In!=0)]
  if (length(In) > 0 ){
    In_Name = names(In)[order(In, decreasing = TRUE)]
    for (iStock in In_Name){
      loc = which(Total_Cor$StockID_Raw == iStock)
      In_Name[In_Name == iStock] = Total_Cor$Short_N_Sector[loc] 
    }
    # In_Name = as.character(lapply(In_Name, fun_StockName))
    RiskDire_In[t - N0_fixed_net + 1,c(2:(length(In_Name)+1))] = In_Name
  }
}
write.xlsx(RiskDire_In, paste0(wdir_fix_Risk, "/RiskDire_In_Name_20210802_", stock_main,"_", date_end, "_", channel, ".xlsx"))
write.xlsx(RiskDire_Out, paste0(wdir_fix_Risk, "/RiskDire_Out_Name_20210802_", stock_main,"_", date_end, "_", channel, ".xlsx"))

RiskDire_Out= matrix(data=NA, nrow = length(FRM_individ_fixed[,1]), ncol = 5)
RiskDire_Out[,1] = FRM_individ_fixed[,1]
name = c(1:J)
name = paste0('Top_',name)
colnames(RiskDire_Out) = c('Date', 'Top1 Name','Top1 Sector','Top1 Beta', 'Num of FIs')
RiskDire_In = RiskDire_Out
for (t in N0_fixed_net:N1_fixed_net) {
  adj0 = read.csv(file=paste0(wdir_fix, "/Adj_Matix_", 
                              names(FRM_history)[t], ".csv"), 
                  header = TRUE, sep = "," , row.names = 1)
  adj0 = abs(as.matrix(adj0)[1:J, 1:J])
  
  Out = adj0[,which(colnames(adj0) == stock_main)]
  Out = Out[which(Out!=0)]
  names_Out = names(Out)
  names_Sector_Out = names_Out
  for (iStock in names_Out){
    loc = which(Total_Cor$StockID_Raw == iStock)
    names_Out[names_Out == iStock] = Total_Cor$Name[loc] 
    names_Sector_Out[names_Sector_Out == iStock] = Total_Cor$GICS_INDUSTRY_GROUP_NAME[loc] 
  }
  
  if (length(Out) > 0 ){
    order = order(Out, decreasing = TRUE)
    # Out_Name = as.character(lapply(Out_Name, fun_StockName))
    RiskDire_Out[t - N0_fixed_net + 1,2:ncol(RiskDire_Out)] = cbind(names_Out[order[1]],
                                                                    names_Sector_Out[order[1]], round(Out[order[1]],digits = 2), length(Out))
  }
  
  In = adj0[which(colnames(adj0) == stock_main),]
  In = In[which(In!=0)]
  names_In = names(In)
  names_Sector_In = names_In
  for (iStock in names_In){
    loc = which(Total_Cor$StockID_Raw == iStock)
    names_In[names_In == iStock] = Total_Cor$Name[loc] 
    names_Sector_In[names_Sector_In == iStock] = Total_Cor$GICS_INDUSTRY_GROUP_NAME[loc] 
  }
  
  if (length(In) > 0 ){
    order = order(In, decreasing = TRUE)
    # In_Name = as.character(lapply(In_Name, fun_StockName))
    RiskDire_In[t - N0_fixed_net + 1,2:ncol(RiskDire_In)] = cbind(names_In[order[1]],names_Sector_In[order[1]], 
                                                                  round(In[order[1]],digits=2), length(In))
  }
}
write.xlsx(RiskDire_In, paste0(wdir_fix_Risk, "/RiskDire_In_Infor_20210802_", stock_main,"_", date_end, "_", channel, ".xlsx"))
write.xlsx(RiskDire_Out, paste0(wdir_fix_Risk, "/RiskDire_Out_Infor_20210802_", stock_main,"_", date_end, "_", channel, ".xlsx"))

# historical index
filename = paste0(wdir,input_path,"/FRM Asia time series/FRM_CHHKTW_Time_Series_FRM_2006-20200325.csv")
FRM_index1= read.csv2(filename, header=F, encoding = "uft-8",sep = " ")
colnames(FRM_index1) = c("Date", "FRM")
FRM_index1$Date = gsub('[-]', '', FRM_index1$Date)
FRM_index1$Date = as.numeric(FRM_index1$Date)

filename2 = paste0(wdir,input_path,"/FRM Asia time series/FRM_CHHKTW_Time_Series_FRM_20201030_20200201.csv")
FRM_index2 = read.csv(filename2, header=F, encoding = "uft-8",sep = " ")
colnames(FRM_index2) = c("Date", "FRM")
FRM_index = rbind(FRM_index1, FRM_index2[which(FRM_index2$Date >= FRM_index1$Date[length(FRM_index1$Date)]), ])
colnames(FRM_index) = c("DateNum", "FRM")
VIX = read_excel(paste0(wdir,input_path,"/VIX VXFXI.xlsx"))
VIX = VIX[order(VIX$Date, decreasing = F),]
VIX$DateNum = as.numeric(gsub('[-]', '', VIX$Date))
VIX$`VXFXI Index`[which(VIX$DateNum <= 20110315)] = VIX$`VIX Index`[which(VIX$DateNum <= 20110315)]

FRM_index = left_join(FRM_index, VIX, by = c("DateNum"))
nLoc_nan = which(is.na(FRM_index$`VXFXI Index`))
FRM_index = FRM_index[-nLoc_nan,]
FRM_index[, 2] = as.numeric(FRM_index[, 2])
FRM_index[, c(2, 5)]= scale(FRM_index[, c(2, 5)])
FRM_index$Date = as.Date(FRM_index$Date)
datebreaks <- seq(FRM_index$Date[1], FRM_index$Date[length(FRM_index$Date)],
                  by = "24 month")
png(paste0(wdir_fix_Risk, "/RiskMeasure_history.png"), width = 900, height = 600, bg = "transparent")

print(ggplot()+geom_line(data = FRM_index[which(FRM_index$DateNum<=20110315),],aes(x = Date,y = `VXFXI Index`), colour = "blue", size=1)
      +geom_line(data = FRM_index[which(FRM_index$DateNum>20110315),],aes(x = Date,y = `VXFXI Index`), colour = "black", size=1)+
        geom_line(data = FRM_index,aes(x = Date,y = FRM),colour = "red",size=1)+
        scale_x_date(breaks = datebreaks)+labs(y = "z-score") +
        theme(axis.text.x = element_text(angle = 30, hjust = 1,size = 15), 
              axis.title = element_text(size = 20),
              axis.text.y = element_text(size = 15), 
              panel.grid.major =element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black"), legend.position="none") #
)

dev.off()


# Compare
FRM_index = read.csv(paste0(dir_T, "/Lambda_DynamicFactor/FRM_", channel, "_index.csv"), header = TRUE)
FRM_index$VIX = all_prices$VXFXI.INDEX[65:nrow(all_prices)]
FRM_index$FRM = FRM_index$FRM

library(psych)
sta_FRM = describe(as.matrix(FRM_index[,c(2,3)]))
write.xlsx(sta_FRM, paste0(wdir_fix_Risk, "/CompareFRM.xlsx"))

FRM_index[, 2:3] = scale(FRM_index[, 2:3])
FRM_index$Date =  all_prices$ticker[65:nrow(all_prices)]
FRM_index$Date = as.Date(FRM_index$Date)

datebreaks <- seq(FRM_index$Date[1], FRM_index$Date[length(FRM_index$Date)],
                  by = "3 month")
png(paste0(wdir_fix_Risk, "/RiskMeasure.png"), width = 900, height = 600, bg = "transparent")
print(ggplot()+geom_line(data = FRM_index,aes(x = Date,y = VIX),colour = "black",size=1)+
        geom_line(data = FRM_index,aes(x = Date,y = FRM),colour = "red", size=1)+
        scale_x_date(breaks = datebreaks) +labs(y = "z-score")+ 
        theme(axis.text.x = element_text(angle = 30, hjust = 1,size = 15), 
              axis.title = element_text(size = 20),
              axis.text.y = element_text(size = 15), 
              panel.grid.major =element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black"), legend.position="none") #
)

dev.off()

#Set ggplot2 theme
FRM_index = read.csv(paste0(dir_T, "/Lambda_DynamicFactor/FRM_", channel, "_index.csv"), header = TRUE)
theme_set(theme_classic())

#List of centrality types and their numbers
centralitylist = list("OutDegree" = 1, "InDegree" = 2, "Closeness" = 3, 
                      "Betweenness" = 4, "InInfluence" = 5, "OutInfluence" = 6)

#Create a list of files in the folder and extract dates from the names
file_list = list.files(path = paste0(dir_T, "Adj_Matrices"))
file_list = file_list[file_list!="Fixed"]
dates = as.character(str_first_number(file_list), format = "%Y%m%d")
dates = as.Date(dates, format = "%Y%m%d")
N = length(file_list)

#Create a list of all network graphs
allgraphs = lapply(1:N, function(i) {
  data = read.csv(paste0(dir_T, "/Adj_Matrices/", file_list[i]), row.names = 1)
  M_stock = ncol(data)-M_macro
  adj_matrix = data.matrix(data[1:M_stock, 1:M_stock])
  q = qgraph(adj_matrix, layout = "circle", details = TRUE, 
             vsize = c(5,15), DoNotPlot = TRUE)
  return(q)
})

allcentralities = centrality(allgraphs)
eigencentrality = lapply(1:N, function(i) eigen_centrality(as.igraph(allgraphs[[i]]))$vector)

#Calculate averages
FRM_index$outdegree_avg = sapply(1:N, function(i) mean(allcentralities[[i]]$OutDegree))
FRM_index$indegree_avg = sapply(1:N, function(i) mean(allcentralities[[i]]$InDegree))
FRM_index$closeness_avg = sapply(1:N, function(i) mean(allcentralities[[i]]$Closeness))
FRM_index$betweenness_avg = sapply(1:N, function(i) mean(allcentralities[[i]]$Betweenness))
FRM_index$eigenvector_avg = sapply(1:N, function(i) mean(eigencentrality[[i]]))
FRM_index$Date =  all_prices$ticker[65:nrow(all_prices)]
FRM_index$Date = as.Date(FRM_index$Date)

datebreaks <- seq(FRM_index$Date[1], FRM_index$Date[length(FRM_index$Date)],
                  by = "3 month")
plot_labels = c(20190923, 20200318, 20200831)

png(paste0(wdir,"Output 20210727/Centrality/FRM_outdegree_avg_2.png"),
    width = 900, height = 600, bg = "transparent")
par(mar = c(5, 4, 4, 4) + 0.3)
plot(FRM_index$FRM, type = "l", col = "blue", xlab = "Date", 
     ylab = "FRM index", xaxt = "n", lwd = 2,cex.axis=1.5,cex.lab=1.5)
par(new = TRUE)
plot(FRM_index$outdegree_avg, type = "l", col = "red", axes = FALSE, 
     xlab = "", ylab = "", xaxt = "n")
axis(side = 4, at = pretty(range(FRM_index$outdegree_avg)),cex.axis=1.5,cex.lab=1.5)
ll = which(FRM_index$date%in% plot_labels)
axis(1, at = ll, labels = plot_labels, cex.axis = 1.5)
mtext("Outdegree centrality", 
      side = 4, line = 3,cex=1.5)
dev.off()

png(paste0(wdir,"Output 20210727/Centrality/FRM_indegree_avg_2.png"),
    width = 900, height = 600, bg = "transparent")
par(mar = c(5, 4, 4, 4) + 0.3)
plot(FRM_index$FRM, type = "l", col = "blue", xlab = "Date", 
     ylab = "FRM index", xaxt = "n", lwd = 2,cex.axis=1.5,cex.lab=1.5)
par(new = TRUE)
plot(FRM_index$indegree_avg, type = "l", col = "red", axes = FALSE, 
     xlab = "", ylab = "", xaxt = "n")
mtext("Indegree centrality", 
      side = 4, line = 3,cex=1.5)
axis(side = 4, at = pretty(range(FRM_index$indegree_avg)),cex.axis=1.5,cex.lab=1.5)
ll = which(FRM_index$date%in% plot_labels)
axis(1, at = ll, labels = plot_labels, cex.axis = 1.5)

dev.off()




