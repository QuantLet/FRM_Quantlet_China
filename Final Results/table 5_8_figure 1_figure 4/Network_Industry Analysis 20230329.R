## 0. Preparation

rm(list = ls(all = TRUE))

libraries = c("iml","ggplot2", "data.table", "igraph","timeDate", "stringr", "graphics","magick", "scales", "tidyr", "zoo", "foreach", "doParallel",
              "xgboost","shapr","randomForest", "rpart", "quantreg", "readxl","dplyr", "xlsx", "psych","qgraph", "gganimate","av",
              "gifski", "strex","matrixStats","tools","qgraph")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#----------------------------------------START UPDATE----------------------------------------
wdir = "/Users/ruting/Documents/macbook/PcBack/FRM_Quantlet/FRM_All/"

#Choose between "Americas", "Europe", "Crypto", "SP500", "ER", "Asia", "EM"
channel = "Asia"

#Data source
date_start_source= 20190102
date_end_source = 20230306

## 6. Network
dir_T = paste0(wdir, "Output/Asia/20230228/MKRturn/Adj_Matrices_ShapleyCom/Comb_2_plus/1")
FRM_history = readRDS(paste0(dir_T,
                             "/Lambda_DynamicFactor/FRM_", channel, ".rds"))
N0_fixed_net = 1
N1_fixed_net = length(names(FRM_history))

input_path = paste0(wdir,"Input/", channel, "/", date_start_source, "-", date_end_source)
f = read_excel(paste0(input_path, "/","Name_Stock.xlsx"),sheet = 'Add Short Name')
f = data.frame(f)

N = N1_fixed_net
file_list = list.files(path = paste0(dir_T, "/Adj_Matrices"))

#Create a list of all network graphs
allgraphs = lapply(1:N, function(i) {
  data = read.csv(paste0(dir_T, "/Adj_Matrices/", file_list[i]), row.names = 1)
  data = as.matrix(data)
  M_stock = which(colnames(data) == "MKRturn")-1
  adj_matrix = data[1:M_stock, 1:M_stock]
  
  drop = unique(c(which(rowSums(adj_matrix)==0),which(colSums(adj_matrix)==0)))
  if (length(drop) > 0){
    adj_matrix = adj_matrix[-drop,-drop]
  }
  
  q = qgraph(adj_matrix, layout = "circle", details = TRUE, 
             vsize = c(5,15), DoNotPlot = TRUE)
  return(q)
})


# 
# i=83
# cen = centrality(q)
# cen$Closeness
# cen$Betweenness
# 

allcentralities = centrality(allgraphs)
eigencentrality = lapply(1:N, function(i) eigen_centrality(as.igraph(allgraphs[[i]]))$vector)

#Calculate averages
outdegree_avg = sapply(1:N, function(i) mean(allcentralities[[i]]$OutDegree))
indegree_avg = sapply(1:N, function(i) mean(allcentralities[[i]]$InDegree))
closeness_avg = sapply(1:N, function(i) mean(allcentralities[[i]]$Closeness))

# FRM_index = read.csv(paste0(dir_T, "/Lambda_DynamicFactor/FRM_Asia_index.csv"))
# closeness = data.frame(close = closeness_avg, date = FRM_index$date)
# closeness$date[closeness$close<0.0005]
# FRM_index$date[FRM_index$FRM == max(FRM_index$FRM)]
# a = allcentralities[[83]]$Closeness
# b = allcentralities[[84]]$Closeness

betweenness_avg = sapply(1:N, function(i) mean(allcentralities[[i]]$Betweenness))
eigenvector_avg = sapply(1:N, function(i) mean(eigencentrality[[i]]))

allgraphs_nodir = lapply(1:N, function(i) {
  data = read.csv(paste0(dir_T, "/Adj_Matrices/", file_list[i]), row.names = 1)
  data = as.matrix(data)
  M_stock = which(colnames(data) == "MKRturn")-1
  adj_matrix = data[1:M_stock, 1:M_stock]
  
  drop = unique(c(which(rowSums(adj_matrix)==0),which(colSums(adj_matrix)==0)))
  if (length(drop) > 0){
    adj_matrix = adj_matrix[-drop,-drop]
  }
  
  q = qgraph(adj_matrix, layout = "circle", details = TRUE, 
             vsize = c(5,15), DoNotPlot = TRUE, directed = FALSE, weighted = FALSE)
  return(q)
})
centralities_nodire = centrality(allgraphs_nodir)
degree_avg = sapply(1:N, function(i) mean(centralities_nodire[[i]]$OutDegree))
#Restructure list into individual node centralities
output_path = paste0(dir_T,'/Centrality')
dir.create(output_path)
# outliers = which(FRM_index$FRM > lambda_cutoff)


## Plot FRM index vs all centralities
FRM_index = read.csv(paste0(dir_T, "/Lambda_DynamicFactor/FRM_Asia_index.csv"))
FRM_index$date = paste0(floor(FRM_index$date/10000),'-',floor(FRM_index$date/100)-floor(FRM_index$date/10000)*100,
                        '-',FRM_index$date-floor(FRM_index$date/100)*100)
FRM_index$date = as.Date(FRM_index$date)
# plot_labels <- seq(FRM_index$date[1], FRM_index$date[length(FRM_index$date)],
#                   by = "3 month")
# n_break = floor(N1_fixed_net / (21*3))

cent_plot = function(cent_type, lambda) {
  cent_string = deparse(substitute(cent_type))
  png(paste0(output_path, "/FRM_", cent_string,".png"), 
      width = 900, height = 600, bg = "transparent")
  par(mar = c(5, 4, 4, 4) + 0.3)
  # plot(lambda[-outliers], type = "l", col = "blue", xlab = "", 
  #      ylab = "FRM index", xaxt = "n", lwd = 2)
  plot(lambda, type = "l", col = "blue", xlab = "", 
       ylab = "FRM index", xaxt = "n", lwd = 2)
  par(new = TRUE)
  # plot(cent_type[-outliers], type = "l", col = "red", axes = FALSE, 
  #      xlab = "", ylab = "", xaxt = "n")
  plot(cent_type, type = "l", col = "red", axes = FALSE, 
       xlab = "", ylab = "", xaxt = "n")
  # axis(side = 4, at = pretty(range(cent_type[-outliers])))
  axis(side = 4, at = pretty(range(cent_type)))
  
  # ll = which(FRM_index$date[-outliers] %in% plot_labels)
  # ll = which(FRM_index$date %in% plot_labels)
  ll = seq(63, N1_fixed_net, 63)
  axis(1, at = ll, labels = FRM_index$date[ll])
  mtext(paste0(gsub("_.*", "", cent_string) %>% toTitleCase, " centrality"), 
        side = 4, line = 3)
  dev.off()
}

cent_plot(outdegree_avg, FRM_index$FRM)
cent_plot(indegree_avg, FRM_index$FRM)
cent_plot(betweenness_avg, FRM_index$FRM)
cent_plot(closeness_avg, FRM_index$FRM)
cent_plot(eigenvector_avg, FRM_index$FRM)
cent_plot(degree_avg, FRM_index$FRM)

# corr 
cormatrix = cbind(FRM_index$FRM, degree_avg, closeness_avg, eigenvector_avg)
colnames(cormatrix)[1] = 'FRM'
corr = rcorr(cormatrix, type = c("pearson","spearman"))
corr

write.csv(corr$r,paste0(output_path,"/Centralilty_FRM_Corr.csv"),quote = FALSE)
write.csv(corr$P,paste0(output_path,"/Centralilty_FRM_CorrP.csv"),quote = FALSE)


# 1. average connections firm connections directional connectedness (sna description)
fun_loc = function(x){
  loc = which(f$StockID_Raw == x)
  return(loc)
}


for (t in 1:N) {
  # png(paste0(wdir_fix_Risk, "/",stock_main, "/network_",names(FRM_history)[t],"_", stock_main,".png"), width = 900, height = 600, bg = "transparent")
  Closeness =  allcentralities[[t]]$Closeness
  eigen =  eigencentrality[[t]]
  Degree = centralities_nodire[[t]]$OutDegree
  
  
  data =  read.csv(file=paste0(dir_T, "/Adj_Matrices/Adj_Matix_", names(FRM_history)[t], ".csv"), header = TRUE, sep = "," , row.names = 1)
  data = as.matrix(data)
  M_stock = which(colnames(data) == "MKRturn")-1
  adj_matrix = data[1:M_stock, 1:M_stock]
  
  drop = unique(c(which(rowSums(adj_matrix)==0),which(colSums(adj_matrix)==0)))
  if (length(drop) > 0){
    adj_matrix = adj_matrix[-drop,-drop]
  }

  Firms = data.frame(StockID_Raw = colnames(adj_matrix), sector = NA, ShortName = NA)
  Firms[,c("sector", "ShortName","Name")] = f[unlist(lapply(Firms$StockID_Raw, fun_loc)),c("GICS_INDUSTRY_GROUP_NAME", "ShortName","Name")]
  Firms$sector[!Firms$sector %in% c("Banks", "Diversified Financials", "Insurance")] = "Others"
  
  Firms$Closeness = Closeness
  Firms$eigen = eigen
  Firms$Degree = Degree
  
  Firms$Date = names(FRM_history)[t]
  
  if (t == N0_fixed_net){
    FI_link = Firms
  }else{
    FI_link = rbind(FI_link, Firms)
  }

}


unique_Firm = unique(FI_link[,1:4])
unique_Firm$Closeness_Mean = NA
unique_Firm$eigen_Mean = NA
unique_Firm$Degree_Mean = NA

for (iF in c(1:nrow(unique_Firm))){
  Temp = FI_link[FI_link$StockID_Raw == unique_Firm$StockID_Raw[iF],]
  unique_Firm$Closeness_Mean[iF] = mean(Temp$Closeness,na.rm = TRUE)
  unique_Firm$eigen_Mean[iF] = mean(Temp$eigen,na.rm = TRUE)
  unique_Firm$Degree_Mean[iF] = mean(Temp$Degree,na.rm = TRUE)

}

unique_Firm = unique_Firm[unique_Firm$sector == "Banks",]
write.xlsx(unique_Firm, paste0(wdir, "Output/Asia/20230228/Bank_Centrality_",date_end_source,".xlsx"))



                       
for (t in N0_fixed_net:N1_fixed_net) {
  # png(paste0(wdir_fix_Risk, "/",stock_main, "/network_",names(FRM_history)[t],"_", stock_main,".png"), width = 900, height = 600, bg = "transparent")
  con = read.csv(file=paste0(dir_T, "/Adj_Matrices/Adj_Matix_", names(FRM_history)[t], ".csv"), header = TRUE, sep = "," , row.names = 1)
  loc_Macro = which(colnames(con) == "MKRturn")
  con = con[c(1:(loc_Macro-1)),c(1:(loc_Macro-1))]
  con = as.matrix(con)
  con = apply(con, 2, as.numeric)
  
  con = abs(con)
  con[con!=0] = 1
  
  Firms = data.frame(StockID_Raw = colnames(con), sector = NA, ShortName = NA)
  Firms[,c("sector", "ShortName","Name")] = f[unlist(lapply(Firms$StockID_Raw, fun_loc)),c("GICS_INDUSTRY_GROUP_NAME", "ShortName","Name")]
  Firms$sector[!Firms$sector %in% c("Banks", "Diversified Financials", "Insurance")] = "Others"
  
  Firms$In = NA
  Firms$Out = NA
  Firms$Total = NA
  
  for (iF in c(1:nrow(Firms))){
    Firms$In[iF] = sum(con[iF,]) / (loc_Macro-2)
    Firms$Out[iF] = sum(con[,iF]) / (loc_Macro-2)
    Firms$Total[iF] = ( sum(con[iF,])+sum(con[,iF]))/(2* (loc_Macro-2))
  }
  
  Firms$Date = names(FRM_history)[t]
  
  if (t == N0_fixed_net){
    FI_link = Firms
  }else{
    FI_link = rbind(FI_link, Firms)
  }
  
  NumConnection_Firm = rbind(NumConnection_Firm,  as.numeric(c(names(FRM_history)[t],mean(Firms$In, na.rm = TRUE),  
                             mean(Firms$Out, na.rm = TRUE), mean(Firms$Total, na.rm = TRUE))))
}

NumConnection_Firm = NumConnection_Firm[-1,]
NumConnection_Firm$date = paste0(floor(NumConnection_Firm$date/10000),'-',floor(NumConnection_Firm$date/100)-floor(NumConnection_Firm$date/10000)*100,
                        '-',NumConnection_Firm$date-floor(NumConnection_Firm$date/100)*100)
NumConnection_Firm$date = as.Date(NumConnection_Firm$date)

# datebreaks <- seq(NumConnection_Firm$date[1], NumConnection_Firm$date[length(NumConnection_Firm$date)],
#                   by = "3 month")

ll = seq(63, N1_fixed_net, 63)
datebreaks = NumConnection_Firm$date[ll]
png(paste0(wdir, "/Output/Asia/20230228/Connection_Firm.png"), width = 900, height = 600, bg = "transparent")
print(ggplot()+geom_line(data = NumConnection_Firm,aes(x = date,y = InOut_Firm),colour = "red",size=1)+
        xlab("Date")+ylab("Connection")+
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

unique_Firm = unique(FI_link[,1:4])
unique_Firm$In_Mean = NA
unique_Firm$Out_Mean = NA
unique_Firm$In_Mean_Before = NA
unique_Firm$Out_Mean_Before = NA
unique_Firm$In_Mean_After = NA
unique_Firm$Out_Mean_After = NA

for (iF in c(1:nrow(unique_Firm))){
  Temp = FI_link[FI_link$StockID_Raw == unique_Firm$StockID_Raw[iF],]
  unique_Firm$In_Mean[iF] = mean(Temp$In,na.rm = TRUE)
  unique_Firm$Out_Mean[iF] = mean(Temp$Out,na.rm = TRUE)
  
  unique_Firm$In_Mean_Before[iF] = mean(Temp$In[Temp$Date<= 20200120],na.rm = TRUE)
  unique_Firm$Out_Mean_Before[iF] = mean(Temp$Out[Temp$Date<= 20200120],na.rm = TRUE)
  
  
  unique_Firm$In_Mean_After[iF] = mean(Temp$In[Temp$Date > 20200120],na.rm = TRUE)
  unique_Firm$Out_Mean_After[iF] = mean(Temp$Out[Temp$Date > 20200120],na.rm = TRUE)
  
}

unique_Firm = unique_Firm[unique_Firm$sector == "Banks",]
write.xlsx(unique_Firm, paste0(wdir, "Output/Asia/20230228/Bank_link_",date_end_source,".xlsx"))

## 2. industry Network connections

Type = c("Banks", "Diversified Financials", "Insurance","Others")

Industry_network = data.frame("date" = NA, "Bank_Out_Other" = NA, "Bank_In_Other" = NA, "DF_Out_Other" = NA,"DF_In_Other" = NA,
                              "Insu_Out_Other" = NA, "Insu_In_Other" = NA, "Other_Out_Other" = NA, "Other_In_Other" = NA)


for (t in N0_fixed_net:N1_fixed_net) {
  # png(paste0(wdir_fix_Risk, "/",stock_main, "/network_",names(FRM_history)[t],"_", stock_main,".png"), width = 900, height = 600, bg = "transparent")
  con = read.csv(file=paste0(dir_T, "/Adj_Matrices/Adj_Matix_", 
                              names(FRM_history)[t], ".csv"), 
                  header = TRUE, sep = "," , row.names = 1)
  loc_Macro = which(colnames(con) == "MKRturn")
 
  con = con[c(1:(loc_Macro-1)),c(1:(loc_Macro-1))]
  con = as.matrix(con)
  con = apply(con, 2, as.numeric)
  
  con = abs(con)
  con[con!=0] = 1
  
  Firms = data.frame(StockID_Raw = colnames(con), sector = NA, ShortName = NA)
  Firms[,c("sector", "ShortName")] = f[unlist(lapply(Firms$StockID_Raw, fun_loc)),c("GICS_INDUSTRY_GROUP_NAME", "ShortName")]
  Firms$sector[!Firms$sector %in% c("Banks", "Diversified Financials", "Insurance")] = "Others"
  
  loc_Bank = which(Firms$sector == "Banks")
  loc_Security = which(Firms$sector == "Diversified Financials")
  loc_Insurance = which(Firms$sector == "Insurance")
  loc_Others = which(Firms$sector == "Others")
  
  
  for (iType in Type){
   if (iType == "Banks"){
     # out
     Bank_Out_Other = sum(con[c(loc_Security,loc_Insurance,loc_Others),loc_Bank])/(3*(loc_Macro-1)/4)
     
     # in
     Bank_In_Other = sum(con[loc_Bank,c(loc_Security,loc_Insurance,loc_Others)])/(3*(loc_Macro-1)/4)

   }
   
    if (iType == "Diversified Financials"){
      # out
      DF_Out_Other = sum(con[c(loc_Bank,loc_Insurance,loc_Others),loc_Security])/(3*(loc_Macro-1)/4)
      
      # in
      DF_In_Other = sum(con[loc_Security,c(loc_Bank,loc_Insurance,loc_Others)])/(3*(loc_Macro-1)/4)
    } 
    
    if (iType == "Insurance"){
      # out
      Insu_Out_Other = sum(con[c(loc_Bank,loc_Security,loc_Others),loc_Insurance])/(3*(loc_Macro-1)/4)
      
      # in
      Insu_In_Other = sum(con[loc_Insurance,c(loc_Bank,loc_Security,loc_Others)])/(3*(loc_Macro-1)/4)
    } 
    
    if (iType == "Others"){
      # out
      Other_Out_Other = sum(con[c(loc_Bank,loc_Security,loc_Insurance),loc_Others])/(3*(loc_Macro-1)/4)
      
      # in
      Other_In_Other = sum(con[loc_Others,c(loc_Bank,loc_Security,loc_Insurance)])/(3*(loc_Macro-1)/4)
    }

  }
  Industry_network = rbind(Industry_network, as.numeric(c(names(FRM_history)[t], Bank_Out_Other, Bank_In_Other,
                                                          DF_Out_Other, DF_In_Other, Insu_Out_Other, Insu_In_Other, Other_Out_Other, Other_In_Other)) )
}

Industry_network = Industry_network[-1,]
Industry_network$date = as.Date(paste0(floor(Industry_network$date / 10000),"-",
                              floor(Industry_network$date / 100) -floor(Industry_network$date / 10000)*100,"-",
                              Industry_network$date - floor(Industry_network$date/ 100) * 100))
write.xlsx(Industry_network, paste0(wdir, "Output/Asia/20230228/Industry_network_",date_end_source,".xlsx"))

ll = seq(63, N1_fixed_net, 63)
datebreaks = Industry_network$date[ll]

png(paste0(wdir, "/Output/Asia/20230228/Out_Industry.png"), width = 900, height = 600, bg = "transparent")
print(ggplot()+geom_line(data = Industry_network,aes(x = date,y = Bank_Out_Other),colour = "red",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = DF_Out_Other),colour = "gray",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = Insu_Out_Other),colour = "orange",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = Other_In_Other),colour = '#14B294',size=1)+
        xlab("Date")+ylab("Normalized Number of connection")+
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

png(paste0(wdir, "/Output/Asia/20230228/In_Industry.png"), width = 900, height = 600, bg = "transparent")
print(ggplot()+geom_line(data = Industry_network,aes(x = date,y = Bank_In_Other),colour = "red",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = DF_In_Other),colour = "gray",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = Insu_In_Other),colour = "orange",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = Other_In_Other),colour = '#14B294',size=1)+
        xlab("Date")+ylab("Normalized Number of connection")+
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


Type = c("Banks", "Diversified Financials", "Insurance","Others")

Industry_network = data.frame("date" = NA, "Out_BB" = NA, "Out_BS" = NA, "Out_BI" = NA,"Out_BO" = NA,
                              "In_BB" = NA, "In_BS" = NA, "In_BI" = NA, "In_BO" = NA,
                              "Out_SB" = NA, "Out_SS" = NA, "Out_SI" = NA, "Out_SO" = NA,
                              "In_SB" = NA, "In_SS" = NA, "In_SI" = NA, "In_SO" = NA,
                              "Out_IB" = NA, "Out_IS" = NA, "Out_II" = NA, "Out_IO" = NA,
                              "In_IB" = NA, "In_IS" = NA, "In_II" = NA, "In_IO" = NA,
                              "Out_OB" = NA, "Out_OS" = NA, "Out_OI" = NA, "Out_OO" = NA,
                              "In_OB" = NA, "In_OS" = NA, "In_OI" = NA, "In_OO" = NA)

for (t in N0_fixed_net:N1_fixed_net) {
  # png(paste0(wdir_fix_Risk, "/",stock_main, "/network_",names(FRM_history)[t],"_", stock_main,".png"), width = 900, height = 600, bg = "transparent")
  con = read.csv(file=paste0(dir_T, "/Adj_Matrices/Adj_Matix_", 
                             names(FRM_history)[t], ".csv"), 
                 header = TRUE, sep = "," , row.names = 1)
  loc_Macro = which(colnames(con) == "MKRturn")
  con = con[c(1:(loc_Macro-1)),c(1:(loc_Macro-1))]
  con = abs(con)
  
  Firms = data.frame(StockID_Raw = colnames(con), sector = NA, ShortName = NA)
  Firms[,c("sector", "ShortName")] = f[unlist(lapply(Firms$StockID_Raw, fun_loc)),c("GICS_INDUSTRY_GROUP_NAME", "ShortName")]
  Firms$sector[!Firms$sector %in% c("Banks", "Diversified Financials", "Insurance")] = "Others"
  
  loc_Bank = which(Firms$sector == "Banks")
  loc_Security = which(Firms$sector == "Diversified Financials")
  loc_Insurance = which(Firms$sector == "Insurance")
  loc_Others = which(Firms$sector == "Others")
  
  
  for (iType in Type){
    if (iType == "Banks"){
      # out
      dTemp_out = con[, loc_Bank]
      Out_BB = sum(con[loc_Bank,loc_Bank])/sum(dTemp_out)
      Out_BS = sum(con[loc_Security,loc_Bank])/sum(dTemp_out)
      Out_BI = sum(con[loc_Insurance,loc_Bank])/sum(dTemp_out)
      Out_BO = sum(con[loc_Others,loc_Bank])/sum(dTemp_out)
      
      # in
      dTemp_in = con[loc_Bank,]
      In_BB = sum(con[loc_Bank,loc_Bank])/sum(dTemp_in)
      In_BS = sum(con[loc_Bank,loc_Security])/sum(dTemp_in)
      In_BI = sum(con[loc_Bank,loc_Insurance])/sum(dTemp_in)
      In_BO = sum(con[loc_Bank,loc_Others])/sum(dTemp_in)
      
      
    }
    
    if (iType == "Diversified Financials"){
      # out
      dTemp_out = con[, loc_Security]
      Out_SB = sum(con[loc_Bank,loc_Security])/sum(dTemp_out)
      Out_SS = sum(con[loc_Security,loc_Security])/sum(dTemp_out)
      Out_SI = sum(con[loc_Insurance,loc_Security])/sum(dTemp_out)
      Out_SO = sum(con[loc_Others,loc_Security])/sum(dTemp_out)
      # in
      dTemp_in = con[loc_Security,]
      In_SB = sum(con[loc_Security,loc_Bank])/sum(dTemp_in)
      In_SS = sum(con[loc_Security,loc_Security])/sum(dTemp_in)
      In_SI = sum(con[loc_Security,loc_Insurance])/sum(dTemp_in)
      In_SO = sum(con[loc_Security,loc_Others])/sum(dTemp_in)
    } 
    
    if (iType == "Insurance"){
      # out
      dTemp_out = con[, loc_Insurance]
      Out_IB = sum(con[loc_Bank,loc_Insurance])/sum(dTemp_out)
      Out_IS = sum(con[loc_Security,loc_Insurance])/sum(dTemp_out)
      Out_II = sum(con[loc_Insurance,loc_Insurance])/sum(dTemp_out)
      Out_IO = sum(con[loc_Others,loc_Insurance])/sum(dTemp_out)
      # in
      dTemp_in = con[loc_Insurance,]
      In_IB = sum(con[loc_Insurance,loc_Bank])/sum(dTemp_in)
      In_IS = sum(con[loc_Insurance,loc_Security])/sum(dTemp_in)
      In_II = sum(con[loc_Insurance,loc_Insurance])/sum(dTemp_in)
      In_IO = sum(con[loc_Insurance,loc_Others])/sum(dTemp_in)
    } 
    
    if (iType == "Others"){
      # out
      dTemp_out = con[, loc_Others]
      Out_OB = sum(con[loc_Bank,loc_Others])/sum(dTemp_out)
      Out_OS = sum(con[loc_Security,loc_Others])/sum(dTemp_out)
      Out_OI = sum(con[loc_Insurance,loc_Others])/sum(dTemp_out)
      Out_OO = sum(con[loc_Others,loc_Others])/sum(dTemp_out)
      
      # in
      dTemp_in = con[loc_Others,]
      In_OB = sum(con[loc_Others,loc_Bank])/sum(dTemp_in)
      In_OS = sum(con[loc_Others,loc_Security])/sum(dTemp_in)
      In_OI = sum(con[loc_Others,loc_Insurance])/sum(dTemp_in)
      In_OO = sum(con[loc_Others,loc_Others])/sum(dTemp_in)
    }
    
  }
  Industry_network = rbind(Industry_network, as.numeric(c(names(FRM_history)[t], Out_BB, Out_BS, Out_BI,Out_BO,
                                                          In_BB, In_BS, In_BI, In_BO,Out_SB, Out_SS, Out_SI, Out_SO,
                                                          In_SB, In_SS, In_SI, In_SO,Out_IB, Out_IS, Out_II, Out_IO,
                                                          In_IB, In_IS, In_II, In_IO,Out_OB, Out_OS, Out_OI, Out_OO,
                                                          In_OB, In_OS, In_OI, In_OO)) )
}

Industry_network = Industry_network[-1,]
Industry_network$date = as.Date(paste0(floor(Industry_network$date / 10000),"-",
                                       floor(Industry_network$date / 100) -floor(Industry_network$date / 10000)*100,"-",
                                       Industry_network$date - floor(Industry_network$date/ 100) * 100))

ll = seq(63, N1_fixed_net, 63)
datebreaks = Industry_network$date[ll]


png(paste0(wdir, "/Output/Asia/20230228/Out_Bank.png"), width = 900, height = 600, bg = "transparent")
print(ggplot()+geom_line(data = Industry_network,aes(x = date,y = Out_BB),colour = "red",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = Out_BS),colour = "gray",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = Out_BI),colour = "orange",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = Out_BO),colour = '#14B294',size=1)+
        xlab("Date")+ylab("Outcoming links")+
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

png(paste0(wdir, "/Output/Asia/20230228/In_Bank.png"), width = 900, height = 600, bg = "transparent")
print(ggplot()+geom_line(data = Industry_network,aes(x = date,y = In_BB),colour = "red",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = In_BS),colour = "gray",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = In_BI),colour = "orange",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = In_BO),colour = '#14B294',size=1)+
        xlab("Date")+ylab("Incoming links")+
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


png(paste0(wdir, "/Output/Asia/20230228/Out_Security.png"), width = 900, height = 600, bg = "transparent")
print(ggplot()+geom_line(data = Industry_network,aes(x = date,y = Out_SB),colour = "red",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = Out_SS),colour = "gray",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = Out_SI),colour = "orange",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = Out_SO),colour = '#14B294',size=1)+
        xlab("Date")+ylab("Outcoming links")+
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

png(paste0(wdir, "/Output/Asia/20230228/In_Security.png"), width = 900, height = 600, bg = "transparent")
print(ggplot()+geom_line(data = Industry_network,aes(x = date,y = In_SB),colour = "red",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = In_SS),colour = "gray",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = In_SI),colour = "orange",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = In_SO),colour = '#14B294',size=1)+
        xlab("Date")+ylab("Incoming links")+
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

png(paste0(wdir, "/Output/Asia/20230228/Out_Insurance.png"), width = 900, height = 600, bg = "transparent")
print(ggplot()+geom_line(data = Industry_network,aes(x = date,y = Out_IB),colour = "red",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = Out_IS),colour = "gray",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = Out_II),colour = "orange",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = Out_IO),colour = '#14B294',size=1)+
        xlab("Date")+ylab("Outcoming links")+
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

png(paste0(wdir, "/Output/Asia/20230228/In_Insurance.png"), width = 900, height = 600, bg = "transparent")
print(ggplot()+geom_line(data = Industry_network,aes(x = date,y = In_IB),colour = "red",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = In_IS),colour = "gray",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = In_II),colour = "orange",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = In_IO),colour = '#14B294',size=1)+
        xlab("Date")+ylab("Incoming links")+
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

png(paste0(wdir, "/Output/Asia/20230228/Out_Others.png"), width = 900, height = 600, bg = "transparent")
print(ggplot()+geom_line(data = Industry_network,aes(x = date,y = Out_OB),colour = "red",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = Out_OS),colour = "gray",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = Out_OI),colour = "orange",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = Out_OO),colour = '#14B294',size=1)+
        xlab("Date")+ylab("Outcoming links")+
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

png(paste0(wdir, "/Output/Asia/20230228/In_Others.png"), width = 900, height = 600, bg = "transparent")
print(ggplot()+geom_line(data = Industry_network,aes(x = date,y = In_OB),colour = "red",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = In_OS),colour = "gray",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = In_OI),colour = "orange",size=1)+
        geom_line(data = Industry_network,aes(x = date,y = In_OO),colour = '#14B294',size=1)+
        xlab("Date")+ylab("Incoming links")+
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


# Industry interaction
ShowDate = c(20191017,20200120,20200203,20220228,20221123)
fun_loc = function(x){
  loc = which(f$StockID_Raw == x)
  return(loc)
}
for (file_date in ShowDate){
  con =  as.matrix(read.csv(file=paste0(dir_T, "/Adj_Matrices/Adj_Matix_", 
                             file_date, ".csv"), 
                 header = TRUE, sep = "," , row.names = 1))
  
  loc_Macro = which(colnames(con) == "MKRturn")
  con = con[c(1:(loc_Macro-1)),c(1:(loc_Macro-1))]

  Firms = data.frame(StockID_Raw = colnames(con), sector = NA, ShortName = NA)
  Firms[,c("sector", "ShortName")] = f[unlist(lapply(Firms$StockID_Raw, fun_loc)),c("GICS_INDUSTRY_GROUP_NAME", "ShortName")]
  Firms$sector[!Firms$sector %in% c("Banks", "Diversified Financials", "Insurance")] = "Others"
  
  # extract the date
  # dt       = as.Date(f[, 1], format = "%d/%m/%Y")[49:314]
  
  # read the firms' names
  names.fi = Firms$ShortName
  
  # divide the firms into four groups: Depositories, Insurers, Broker-Dealers,
  # Others.
  groups   = list(which(Firms$sector == "Banks"),which(Firms$sector == "Diversified Financials"),
                  which(Firms$sector == "Insurance"), which(Firms$sector == "Others"))
  col      = c(rep("red", length(which(Firms$sector == "Banks"))),
               rep("gray",  length(which(Firms$sector == "Diversified Financials"))), 
               rep("orange", length(which(Firms$sector == "Insurance"))), 
               rep("#14B294", length(which(Firms$sector == "Others"))))
  
  
  # plot a network based on the adjacency matrix 'con' before thresholding, so that
  # we can see the directional connection caused by spillover effects among 100
  # financial institutions.
  
  png(paste0(wdir,"/Output/Asia/20230228/Network_", file_date, "_all.png"), width = 900, height = 600, bg = "transparent")
  
  plot_g = qgraph(con, groups = groups, layout = "groups", layoutScale = c(1.2, 1.2), 
                  label.font = 2, label.cex = 2, shape = "circle", labels = names.fi, esize = 5, 
                  maximum = max(con), color = c("white", "white","white","white"), node.width = 0.8, label.cex = 1.8, label.color = col, 
                  edge.color = col, curve = 1, border.width = 1.2, border.color = col, asize = 2.5,trans = TRUE)
  # legend(0.82, 1.1, box.lwd = 2, box.col = "white", bg = "white", c("Banks", "Diversified Financials", "Insurance", "Others"), text.col = c("red", "blue", "green4", 
                                                                                                                                            # "mediumorchid4"), cex = 1.3)
  
  dev.off()
  
  
  
  # apply a hard thresholding to make the major connections more clearly
  con_High = ifelse(abs(con) >= quantile(abs(con[con!=0]),0.9), con, 0)
  
  
  # plot a network based on the adjacency matrix 'con' after thresholding, so that
  # we can see the directional connection caused by spillover effects among 100
  # financial institutions.
  png(paste0(wdir,"/Output/Asia/20230228/Network_", file_date, "_High.png"), width = 900, height = 600, bg = "transparent")
  
  plot_g = qgraph(con_High, groups = groups, layout = "groups", layoutScale = c(1.2, 1.2), 
                  label.font = 2, label.cex = 2, shape = "circle", labels = names.fi, esize = 5, 
                  maximum = max(con_High), color = c("white", "white","white","white"), node.width = 0.8, label.cex = 1.8, label.color = col, 
                  edge.color = col, curve = 1, border.width = 1.2, border.color = col, asize = 2.5)
  
  # legend(0.82, 1.1, box.lwd = 2, box.col = "white", bg = "white", c("Banks", "Diversified Financials", "Insurance", "Others"), text.col = c("red", "blue", "green4", 
                                                                                                                                            # "purple3"), cex = 1.3)
  dev.off()
  
}
