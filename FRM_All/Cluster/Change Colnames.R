libraries = c("iml","ggplot2", "data.table", "igraph","timeDate", "stringr", "graphics","magick", "scales", "tidyr", "zoo", "foreach", "doParallel",
              "xgboost","shapr","randomForest", "rpart", "quantreg", "readxl","dplyr", "xlsx")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

wdir = "/Users/wkhaerdle/Documents/Project/FRM-master_Modify_WRT/FRM_Quantlet/FRM_All/Cluster"
setwd(wdir)

Total_Cor = read_excel(paste0(input_path, "/","Name_Stock.xlsx"),sheet = 'Add Short Name')
J = 50
adj0 = read.csv("Adj_Matix_20200120.csv", 
                header = TRUE, sep = "," , row.names = 1)
names_Cir = colnames(adj0) #leftjoin
names_Cir = data.frame(StockID_Raw = names_Cir)

for (iStock in names_Cir$StockID_Raw[1:J]){
  loc = which(Total_Cor$StockID_Raw == iStock)
  names_Cir$Name[names_Cir$StockID_Raw == iStock] = Total_Cor$Name[loc] 
}

colnames(adj0)[1:J] = c(names_Cir$Name)[1:J]
rownames(adj0)[1:J] = c(names_Cir$Name)[1:J]

write.csv(adj0, "Adj_Matix_20200120_addname.csv", quote = FALSE)

