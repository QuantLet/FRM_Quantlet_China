# Deal Data for srisk

# output: market return stock return  market value of equity the book value of equity book value of assets

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

date_start_source = 20190102
date_end_source = 20230306
channel = 'Asia'
input_path = paste0("Input/", channel, "/", date_start_source, "-", date_end_source)
Path_save_xls = paste0(wdir,'MethodAdd/')

# book value per share, total asset, share
BV_SH  = data.frame(read_excel(paste0(input_path,'/BOOK_VAL_PER_SH.xlsx')))
TotalAsset  = data.frame(read_excel(paste0(input_path,'/BS_TOT_ASSET.xlsx')))
BS_SH_OUT  = data.frame(read_excel(paste0(input_path,'/BS_SH_OUT.xlsx')))

Date = BV_SH$Date
for (i in Date){
  temp_season = substr(i,1,3)
  temp_Y =  substr(i,6,9)
  if (temp_season == 'FQ1'){
    temp_date = paste0(temp_Y,'0331')
  }else if(temp_season == 'FQ2'){
    temp_date = paste0(temp_Y,'0630')
  }else if(temp_season == 'FQ3'){
    temp_date = paste0(temp_Y,'0930')  
  }else{temp_date = paste0(temp_Y,'1231') }
  
  Date[Date == i] = temp_date
}

# macro data
Macro = data.frame(read.csv(file = paste0(input_path,"/MacroIndex.csv"), header = TRUE))
Macro = Macro[,-1]

Macro_Total = c('MKRturn')
macro = Macro[,c('Date', Macro_Total)]
colnames(macro)[1] = "ticker"


# calculate weighted index
stock_prices =  read.csv(file = paste0(input_path, "/", channel, "_Price_", 
                                       date_end_source, ".csv"), header = TRUE) %>% as.matrix()
stock_prices = stock_prices[,-(colnames(stock_prices) %in% "X")]
stock_prices = data.frame(stock_prices)

Name_Stock = names(stock_prices)
Name_Stock = Name_Stock[-1]

all_prices = stock_prices
all_prices[, -1] = sapply(all_prices[, -1], as.numeric)
all_return = all_prices[-1, ]
all_return[,-1] = 0

all_return[,which(names(all_return)%in%Name_Stock)] = 
  as.data.frame(diff(log(as.matrix(all_prices[, which(names(all_prices)%in%Name_Stock)])))) 
all_return[is.na(all_return)] = 0

# market value
mktcap =  read.csv(file = paste0(input_path, "/", channel, "_Mktcap_", 
                                 date_end_source, ".csv"), header = TRUE) %>% as.matrix()
mktcap = mktcap[,-(colnames(mktcap) %in% "X")]
mktcap = data.frame(mktcap)
mktcap[, -1] = sapply(mktcap[, -1], as.numeric)

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

Output = data.frame('date' = ticker_str,'rm' = all_return$MKRturn)
for (iStock in Name_Stock) {
  Temp = data.frame(cbind(ticker, all_return[,colnames(all_return) %in% iStock], mktcap[,colnames(mktcap) %in% iStock]))
  colnames(Temp) = c('ticker','rt','mktcap')
  Temp$Month = floor(Temp$ticker / 100)
  Monthlist = data.frame(Month = unique(Temp$Month))
  Monthlist$Type = Monthlist$Month -floor(Monthlist$Month / 100)*100
  Monthlist$Type[Monthlist$Type %in% c(1,2,3)] = 3
  Monthlist$Type[Monthlist$Type %in% c(4,5,6)] = 6
  Monthlist$Type[Monthlist$Type %in% c(7,8,9)] = 9
  Monthlist$Type[Monthlist$Type %in% c(10,11,12)] = 12
  Monthlist$Type = floor(Monthlist$Month/100)*100+Monthlist$Type
  Temp =  merge(Temp, Monthlist, by = "Month", all.x = TRUE)
    
  # the book value of equity  book value of assets
  Temp_FI = data.frame(ticker = Date, BV_SH = BV_SH[,colnames(BV_SH) == iStock],
                       TotalAsset= TotalAsset[,colnames(TotalAsset) == iStock], BS_SH_OUT = BS_SH_OUT[,colnames(BS_SH_OUT) == iStock])
  
  for (i in c(1:nrow(Temp_FI))){
    for (j in c(2:ncol(Temp_FI))){
      replace = as.numeric(Temp_FI[i, j])
      if (length(replace) == 0){
        Temp_FI[i, j] = NA
      }else{
        Temp_FI[i, j] = replace
      }
      
    }
  }
  
  if (sum(is.na(Temp_FI)) < 51){
    for (j in c(1:ncol(Temp_FI))){
      Temp_FI[,j] = as.numeric( Temp_FI[,j])
      nloc = which(is.na(Temp_FI[,j]))
      if (length(nloc)>0){
        nloc = nloc[nloc>1]
        Temp_FI[nloc,j]=Temp_FI[nloc-1,j]
      }
    }
    Temp_FI$BV_Eq = Temp_FI$BV_SH * Temp_FI$BS_SH_OUT
    Temp_FI$Type = floor(Temp_FI$ticker / 100)
    Temp_FI = Temp_FI[,-1]
    Temp = merge(Temp, Temp_FI, by = "Type", all.x = TRUE)
    
    Temp = Temp[,c('rt','mktcap','BV_Eq','TotalAsset')]
    colnames(Temp) = paste0(colnames(Temp), '_',iStock)
    
    Output = cbind(Output, Temp)
  }
  
}

write.csv(Output,paste0(Path_save_xls,"/SRISK_Data.csv"),quote = FALSE)
