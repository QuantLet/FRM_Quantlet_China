library(readxl)
channel = "Asia"
date_start_source = 20190102
date_end_source = 20210210
input_path = paste0("Input/", channel, "/", date_start_source, "-", date_end_source)
Total_Cor = read_excel(paste0(input_path, "/","Name_Stock.xlsx"))
Total_Cor = data.frame(Total_Cor)
Name <- strsplit(Total_Cor$Name," ")
Sector <- strsplit(Total_Cor$GICS_INDUSTRY_GROUP_NAME," ")
Total_Cor$ShortName = NaN
Total_Cor$ShortSector = NaN
for (i in c(1:length(Total_Cor$Name))){
  sName = unlist(Name[i])
  sName = substr(sName, start = 1, stop = 1)
  sName = gsub('[-]', '', sName)
  sName = gsub('[&]', '', sName)
  sName = paste(sName, collapse = "")
  
  sSector= unlist(Sector[i])
  sSector = substr(sSector, start = 1, stop = 1)
  sSector = gsub('[-]', '', sSector)
  sSector = gsub('[&]', '', sSector)
  sSector = paste(sSector, collapse = "")
  
  Total_Cor$ShortName[i] = sName
  Total_Cor$ShortSector[i] = sSector
}
Total_Cor$Short_N_Sector = paste0(Total_Cor$ShortName, '(', Total_Cor$ShortSector, ')')
UniName = unique(Total_Cor$Short_N_Sector)
for (iName in UniName){
  loc = which(Total_Cor$Short_N_Sector == iName)
  if (length(loc) >= 2){
    for (iLoc in loc[2:length(loc)]){
      Total_Cor$Short_N_Sector[iLoc] = paste0(substr(Total_Cor$Name[iLoc], 1,4), '(', Total_Cor$ShortSector[iLoc], ')')
    }
    print(iName)
  }
}
library(xlsx) 
write.xlsx(Total_Cor, paste0(output_path, "/Name_Stock_20210529.xlsx"))

