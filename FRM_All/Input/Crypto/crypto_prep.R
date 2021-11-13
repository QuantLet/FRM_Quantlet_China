#Note 1: Replace , with . in the macro file
#Note 2: Export the macro file to csv
#Note 3: Set end date as last with all variables available in the macro file
#Note 4: Rename files with the new last date

rm(list = ls(all = TRUE))

library(zoo)
library(dplyr)

wdir = "/Users/annshchekina/Desktop/Kod/FRM_All"
setwd(wdir)

date_end_old_data = 20200924
date_end_old_data_str = "2020-09-24"

date_end_new_data = 20201231
date_end_new_data_str = "12/31/2020"

input_path_new = paste0("Input/Crypto/", date_end_new_data)
input_path = paste0("Input/Crypto/20141128-", date_end_new_data)

Mktcap_prev = read.csv(file = paste0(input_path, "/Crypto_Mktcap_", date_end_old_data, ".csv"), header = TRUE)
Stock_Prices_prev = read.csv(file = paste0(input_path, "/Crypto_Price_", date_end_old_data, ".csv"), header = TRUE)
Macro_prev = read.csv(file = paste0(input_path, "/Crypto_Macro_", date_end_old_data, ".csv"), header = TRUE)

Mktcap_new = read.csv(file = paste0(input_path_new, "/FRM_Crypto_Market_", date_end_new_data, ".csv"), header = TRUE)
colnames(Mktcap_new) = colnames(Mktcap_new) %>% toupper()
colnames(Mktcap_new)[1] = "date"
N1_m = which(Mktcap_new$date==date_end_old_data_str)
Mktcap_new = Mktcap_new[-c(1:N1_m), -which(names(Mktcap_new) %in% c("USDT", "USDC", "BUSD"))]

Stock_Prices_new = read.csv(file = paste0(input_path_new, "/FRM_Crypto_Price_", date_end_new_data, ".csv"), header = TRUE)
colnames(Stock_Prices_new) = colnames(Stock_Prices_new) %>% toupper()
colnames(Stock_Prices_new)[1] = "date"
N1_s = which(Stock_Prices_new$date==date_end_old_data_str)
Stock_Prices_new = Stock_Prices_new[-c(1:N1_s), -which(names(Stock_Prices_new) %in% c("USDT", "USDC", "BUSD"))]

Macro = read.csv(file = paste0(input_path_new, "/FRM_Crypto_Macro_Anna.csv"), header = TRUE)
N1_bv = which(Macro$BV010082.INDEX==date_end_new_data_str)
bv = Macro[1:N1_bv, 2:3]
colnames(bv) = c("date", colnames(bv)[1])
N1_cvix = which(Macro$CVIX.INDEX==date_end_new_data_str)
cvix = Macro[1:N1_cvix, 4:5]
colnames(cvix) = c("date", colnames(cvix)[1])
N1_dxy = which(Macro$DXY.INDEX==date_end_new_data_str)
dxy = Macro[1:N1_dxy, 6:7]
colnames(dxy) = c("date", colnames(dxy)[1])
N1_spx = which(Macro$SPX.INDEX==date_end_new_data_str)
spx = Macro[1:N1_spx, 8:9]
colnames(spx) = c("date", colnames(spx)[1])
N1_vix = which(Macro$VIX.INDEX==date_end_new_data_str)
vix = Macro[1:N1_vix, 10:11]
colnames(vix) = c("date", colnames(vix)[1])
Macro_new = merge(bv, cvix, by = "date", all = TRUE, sort = FALSE)
Macro_new = merge(Macro_new, dxy, by = "date", all = TRUE, sort = FALSE)
Macro_new = merge(Macro_new, spx, by = "date", all = TRUE, sort = FALSE)
Macro_new = merge(Macro_new, vix, by = "date", all = TRUE, sort = FALSE)
Macro_new$date = as.character(as.Date(Macro_new$date, "%m/%d/%Y"))
Macro_new = Macro_new[order(Macro_new$date), ]
Date_str = as.data.frame(Mktcap_new$date)
colnames(Date_str) = "date"
Macro_new = merge(Macro_new, Date_str, by = "date", all.y = TRUE)
if (!all(!is.na(Macro_new[1,-1]))) Macro_new[1,-1] = Macro_new[2,-1]
Macro_new = na.locf(Macro_new)

Macro = rbind(Macro_prev, Macro_new)
Mktcap = dplyr::bind_rows(Mktcap_prev, Mktcap_new)
Stock_Prices = dplyr::bind_rows(Stock_Prices_prev, Stock_Prices_new)

write.csv(Macro, paste0(input_path, "/Crypto_Macro_", date_end_new_data, ".csv"), row.names = FALSE, quote = FALSE)
write.csv(Mktcap, paste0(input_path, "/Crypto_Mktcap_", date_end_new_data, ".csv"), row.names = FALSE, quote = FALSE)
write.csv(Stock_Prices, paste0(input_path, "/Crypto_Price_", date_end_new_data, ".csv"), row.names = FALSE, quote = FALSE)
