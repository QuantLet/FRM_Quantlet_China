

getwd()
setwd("/Users/annshchekina/Desktop/FRM_Crypto")

rm(list = ls(all = TRUE))
graphics.off()

install.packages("crypto")

library(crypto) 

list = crypto_list(coin = NULL, start_date = NULL, end_date = NULL,
                   coin_list = NULL)

#Scrape the largest 50 currencies
data = list()
for (i in 1:50){
  data[[i]] = crypto_history(coin = list$name[i], limit = 1, start_date = '20191001',end_date = NULL, coin_list = NULL, sleep = NULL) }

