
rm(list = ls(all = TRUE))

libraries = c("iml","ggplot2", "data.table", "igraph","timeDate", "stringr", "graphics","magick", "scales", "tidyr", "zoo", "foreach", "doParallel",
              "xgboost","shapr","randomForest", "rpart", "quantreg", "readxl","dplyr", "xlsx", "psych","qgraph", "gganimate","av",
              "gifski", "strex","matrixStats","tools","usethis","Matrix", "ConnectednessApproach")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)



wdir = "/Users/ruting/Documents/macbook/PcBack/FRM_Quantlet/FRM_All/"
Path_save_xls = paste0(wdir,'MethodAdd/')
#Choose between "Americas", "Europe", "Crypto", "SP500", "ER", "Asia", "EM"
channel = "Asia"

#Data source
# date_end_source = 20210210
date_end_source = 20230306

#Index output, varying companies
date_start = 20190417
date_end = 20210210

#Network output, fixed companies
date_start_fixed = 20191210
date_end_fixed = 20200526
setwd(wdir)

date_start_source = 20190102
J = 50

input_path = paste0("Input/", channel, "/", date_start_source, "-", date_end_source)

## 1. Data Preprocess
mktcap = read.csv(file = paste0(input_path, "/", channel, "_Mktcap_", 
                                date_end_source, ".csv"), header = TRUE) %>% as.matrix()
mktcap = mktcap[,-(colnames(mktcap) %in% "X")]


stock_prices = read.csv(file = paste0(input_path, "/", channel, "_Price_", 
                                      date_end_source, ".csv"), header = TRUE)
stock_prices = stock_prices[,-(colnames(stock_prices) %in% "X")]


M_stock = ncol(mktcap)-1

# WRT change 
Name_Stock = names(stock_prices)
Name_Stock = Name_Stock[-1]

#Note: if missing market caps are kept NA, the column will be excluded 
#from top J  => do not fill up NA in mktcap
mktcap[is.na(mktcap)] = 0

colnames(mktcap)[1] = "ticker"
colnames(stock_prices)[1] = "ticker"

ticker_str = as.data.frame(stock_prices$ticker[-1])
colnames(ticker_str) = "ticker"
ticker = as.numeric(gsub("-", "", ticker_str$ticker))

N = nrow(ticker_str)

#Align mktcap rows to all_return
mktcap = merge(ticker_str, mktcap, by = "ticker", all.x = TRUE)
mktcap[, -1] = sapply(mktcap[, -1], as.numeric)

if (!all(mktcap$ticker == ticker_str$ticker)) stop("dates do not match")

# all_return = diff(log(as.matrix(all_prices[, -1])))
all_return = stock_prices[-1, -1]
all_return[,] = 0

all_return = 
  as.data.frame(diff(log(as.matrix(stock_prices[, -1])))) 


# log difference of stock price, FXI.US.EQUITY
Column_logdiff = c(Name_Stock, "FXI.US.EQUITY")
all_return[,which(names(all_return)%in%Column_logdiff)] = 
  as.data.frame(diff(log(as.matrix(all_prices[, which(names(all_prices)%in%Column_logdiff)])))) 

#  difference of macro
Column_diff = c("CN2YR")
all_return[,which(names(all_return)%in%Column_diff)] = 
  as.data.frame(diff(as.matrix(all_prices[, which(names(all_prices)%in%Column_diff)]))) 

all_return = as.matrix(all_return) 
all_return[is.na(all_return)] = 0

M_stock = ncol(all_return)
# RealEstateDiff
#Sorting the market capitalization data
FRM_sort = function(data) {sort(as.numeric(data), decreasing = TRUE, index.return = TRUE)}
#Determining the index number of each company
#according to decreasing markefggt capitalization
mktcap_index = matrix(0, N, M_stock)
mktcap_sort = apply(mktcap[, -1], 1, FRM_sort)
for (t in 1:N) mktcap_index[t,] = mktcap_sort[[t]]$ix
mktcap_index = cbind(ticker, mktcap_index)


N0_fixed = which(ticker == date_start_fixed)
N1_fixed = which(ticker == date_end_fixed)
N_fixed = N1_fixed-N0_fixed+1

biggest_index_fixed = as.matrix(mktcap_index[N0_fixed, 2:(J+1)])
data_fixed = cbind(ticker,all_return[, biggest_index_fixed])

write.csv(data_fixed,file = paste0(Path_save_xls, "/Return_",date_end_source,".csv"))

# calculate dy2012

library(zoo)
a = data.frame(data_fixed)
a = (a^2*0.361*365)^0.5*100

time = as.Date('2019-01-03')+c(0:(nrow(data_fixed)-1))
data_test = as.zoo(a, time)

# dca = ConnectednessApproach(dy2012, 
#                             nlag=4, 
#                             nfore=10,
#                             model="VAR",
#                             connectedness="Time",
#                             Connectedness_config=list(TimeConnectedness=list(generalized=TRUE)))



drop_sum = colSums(a[c(1 : 100),])
temp = data_test[, -which(drop_sum==0)]  

dca = ConnectednessApproach(temp, 
                            nlag=2, 
                            nfore=10,
                            window.size=200,
                            model="VAR",
                            connectedness="Time",
                            Connectedness_config=list(TimeConnectedness=list(generalized=TRUE)))
table(dca$TABLE)
output = cbind(data_fixed[(nrow(data_fixed)-nrow(dca$TCI)+1):nrow(data_fixed),1], dca$TCI)
colnames(output)[1] = 'Date'

save(dca, file = paste0(Path_save_xls, "/DYResult_",date_end_source,".rds"))

write.xlsx(output, paste0(Path_save_xls,"/DY_TCI.xlsx"), sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)
