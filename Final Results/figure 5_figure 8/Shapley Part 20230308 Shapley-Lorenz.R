# add LZ shapley 20230308

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

wdir = "/Users/ruting/Documents/macbook/PcBack/FRM_Quantlet/FRM_All/"

# Calculate Shapley
output_path = paste0(wdir,'Output/Asia/20230228/For Shapley')
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

weight_0 = factorial(0)*factorial(3-0-1) / factorial(3)
weight_1 = factorial(1)*factorial(3-1-1) / factorial(3)
weight_2 = factorial(2)*factorial(3-2-1) / factorial(3)
Macro_Total = c("MKRturn","RealEstateDiff","TED")

address = paste0(output_path, "/",Macro_Total[1],"/Adj_Matrices_ShapleyCom/Comb_2_plus")
FRM_Cal = read.csv(file = paste0(address, "/1/Lambda_DynamicFactor/FRM_Asia_index.csv"), header = TRUE)

y_mean = mean(FRM_Cal[,2])
file_list = list.files(path = paste0(address, "/1/Adj_Matrices"))

for (i in c(1:nrow(FRM_Cal))) {
  
  data = read.csv(paste0(address, "/1/Adj_Matrices/", file_list[i]), row.names = 1)
  data = as.matrix(data)
  M_stock = which(colnames(data) == "MKRturn")-1
  adj_matrix = data[1:M_stock,(M_stock+1):ncol(data)]
  
  beta = colMeans(abs(adj_matrix))
  if (i == 1){
    beta_t = beta
  }else{
    beta_t = rbind(beta_t, beta)
  }
}

beta_Important = data.frame(colMeans(beta_t))
colnames(beta_Important) = 'beta'
beta_Important$factor = rownames(beta_Important)
beta_Important$beta = round(beta_Important$beta,5)

png(paste0(output_path, "/beta_macro.png"), width = 900, height = 600, bg = "transparent")
print(ggplot(beta_Important,aes(x= reorder(factor, beta),y=beta, fill = factor))+geom_bar(stat="identity", width=0.5)+
        scale_fill_manual(values = c("MKRturn"="red", "RealEstateDiff"="grey", "TED" = "orange"))+
        coord_flip()+
        geom_text(aes(label=beta), vjust=-5, size=5)+
        labs(y = "Mean beta", size = 12)+
        labs(x = "Macro Features", size = 12)+
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


LZ_Score <- function(FRM_Cal_add, FRM_Cal_ori){
  FRM_Rank_0 = sort(FRM_Cal_add[,2])
  FRM_Rank_Empty = sort(FRM_Cal_ori[,2])
  Temp_rank = cbind(FRM_Rank_0, FRM_Rank_Empty)
  Temp_rank = data.frame(Temp_rank)
  Temp_rank$diff = Temp_rank[,1] - Temp_rank[,2]
  Temp_rank$rank = c(1 : length(FRM_Rank_0))
  Temp_rank$score = Temp_rank$rank*Temp_rank$diff
  
  return(Temp_rank$score)
}

for (iFac in c(1:length(Macro_Total))){
  iFac_Shap = Macro_Total[iFac]
  
  combinate_0 = data.frame(iFac_Shap)
  address = paste0(output_path, "/",iFac_Shap,"/Adj_Matrices_ShapleyCom/Comb_0")
  FRM_Cal_0 = Shapley_Group(combinate_0, address) 
  Shapley_0 = abs(weight_0*(FRM_Cal_0[,2]-FRM_Cal_Empty[,2]))
  
  score = LZ_Score(FRM_Cal_0, FRM_Cal_Empty)
  ShapleyLZ_0 = abs(weight_0*(2/(nrow(FRM_Cal_0)^2 * y_mean) * sum(score))) 
  
  Macro_left = Macro_Total[-which(Macro_Total == iFac_Shap)]
  
  combinate_1_plus = data.frame(rbind(rep(iFac_Shap, ncol(combn(Macro_left,1))), combn(Macro_left,1)))
  combinate_1 = combinate_1_plus[2:nrow(combinate_1_plus),]
  
  combinate_2_plus = data.frame(rbind(rep(iFac_Shap, ncol(combn(Macro_left,2))), combn(Macro_left,2)))
  colnames(combinate_2_plus) = "X"
  combinate_2 = data.frame(combinate_2_plus[2:nrow(combinate_2_plus),])
  colnames(combinate_2) = "X"
  
  
  address = paste0(output_path, "/",iFac_Shap,"/Adj_Matrices_ShapleyCom/Comb_1_plus")
  FRM_Cal_1_plus = Shapley_Group(combinate_1_plus, address) 
  Shapley_1_plus = rowSums(FRM_Cal_1_plus[, c(2:ncol(FRM_Cal_1_plus))])
  
  address = paste0(output_path, "/",iFac_Shap,"/Adj_Matrices_ShapleyCom/Comb_1")
  FRM_Cal_1 = Shapley_Group(combinate_1, address) 
  Shapley_1 = rowSums(FRM_Cal_1[, c(2:ncol(FRM_Cal_1))])
  Shapley_Com_1 = abs(weight_1*(Shapley_1_plus-Shapley_1))
  
  score = LZ_Score(FRM_Cal_1_plus, FRM_Cal_1)
  ShapleyLZ_1 = abs(weight_1*(2/(nrow(FRM_Cal_0)^2 * y_mean) * sum(score))) 
  
  
  address = paste0(output_path, "/",iFac_Shap,"/Adj_Matrices_ShapleyCom/Comb_2_plus")
  FRM_Cal_2_plus = Shapley_Group(combinate_2_plus, address) 
  Shapley_2_plus = FRM_Cal_2_plus[, 2]
  
  address = paste0(output_path, "/",iFac_Shap,"/Adj_Matrices_ShapleyCom/Comb_2")
  FRM_Cal_2 = Shapley_Group(combinate_2, address) 
  Shapley_2 = FRM_Cal_2[, 2]
  Shapley_Com_2 = abs(weight_2*(Shapley_2_plus-Shapley_2))
  
  score = LZ_Score(FRM_Cal_2_plus, FRM_Cal_2)
  ShapleyLZ_2 = abs(weight_1*(2/(nrow(FRM_Cal_0)^2 * y_mean) * sum(score))) 
  
  
  Shapley_Com = data.frame("date" = FRM_Cal_2[, 1], 
                           "Shapley" = Shapley_0 + Shapley_Com_1 + Shapley_Com_2)
  ShapleyLZ_Com = data.frame("factor" = iFac_Shap, 
                             "ShapleyLZ" = ShapleyLZ_0 + ShapleyLZ_1 + ShapleyLZ_2)
  if (iFac == 1){
    Shapley = Shapley_Com
    ShapleyLZ = ShapleyLZ_Com
  }else{
    Shapley = cbind(Shapley, Shapley_Com[, 2])
    ShapleyLZ = rbind(ShapleyLZ,ShapleyLZ_Com)
  }
}
colnames(Shapley) = c("Date", Macro_Total)
ShapleyLZ = data.frame(ShapleyLZ)
ShapleyLZ$ShapleyLZ = round(ShapleyLZ$ShapleyLZ,5)

Shapley$Date = as.Date(paste0(floor(Shapley$Date / 10000),"-",
                              floor(Shapley$Date / 100) -floor(Shapley$Date / 10000)*100,"-",
                              Shapley$Date - floor(Shapley$Date / 100) * 100))

Shapley_Mean = colMeans(Shapley[, c(2:ncol(Shapley))])
Shapley_Mean = data.frame("Macro" = Macro_Total, "Shapley_Mean" = Shapley_Mean)
Shapley_Mean$Shapley_Mean = round(Shapley_Mean$Shapley_Mean,5)

png(paste0(output_path, "/ShapleyLZ.png"), width = 900, height = 600, bg = "transparent")
print(ggplot(ShapleyLZ,aes(x= reorder(factor, ShapleyLZ),y=ShapleyLZ, fill = factor))+geom_bar(stat="identity", width=0.5)+
        scale_fill_manual(values = c("MKRturn"="red", "RealEstateDiff"="grey", "TED" = "orange"))+
        coord_flip()+
        geom_text(aes(label=ShapleyLZ), vjust=-5, size=5)+
        labs(y = "Shapley-Lorenz", size = 12)+
        labs(x = "Macro Features", size = 12)+
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
# Freq = 21
for (iDate in c(Freq:length(Shapley_R_Mean$Date))){
  Temp_p = Shapley_R_Mean$Shapley_Mean[(iDate-Freq+1):iDate]
  Shapley_Win_Mean$Shapley_Win[iDate] = mean(Temp_p)
}
Shapley_Win_Mean = Shapley_Win_Mean[-c(1:(Freq - 1)), ]
Shapley_R_Mean = Shapley_R_Mean[-c(1:(Freq - 1)), ]

ll = seq(Freq, length(Shapley_Win_Mean$Date), 63)
datebreaks = Shapley_Win_Mean$Date[ll]


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
ShapleyRanking = data.frame(Date = Shapley$Date, Top1 = NA, Top2 = NA, Top3 = NA)
# ShapleyRolling = data.frame(Date = Shapley$Date, FXI.US.EQUITY = NA, RealEstateDiff = NA, CN2YR = NA)
ShapleyRolling = Shapley
ShapleyRolling[,-1] = NA
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

select_mac = 'MKRturn'
FRM_5P  = data.frame(read.csv(paste0(wdir,'Output/Asia/20230228/',select_mac,'/Adj_Matrices_ShapleyCom/Comb_2_plus/1/Lambda_DynamicFactor/FRM_Asia_index.csv')))
  
ShapleyRolling$FRM = FRM_5P$FRM[FRM_5P$date >= 20190718]
  
write.csv(ShapleyRanking, paste0(output_path, "/ShapleyRanking.csv"), quote = FALSE)

png(paste0(output_path, "/Shapley_rolling.png"), width = 900, height = 600, bg = "transparent")

# print(ggplot()+geom_line(data = ShapleyRolling,aes(x = Date,y = MKRturn),colour = "red",size=1)+
#         geom_line(data = ShapleyRolling,aes(x = Date,y = RealEstateDiff),colour = "gray",size=1)+
#         geom_line(data = ShapleyRolling,aes(x = Date,y = TED),colour = "orange",size=1)+
#         xlab("Date")+ylab("Shapley Values")+
#         scale_x_date(breaks = datebreaks) +
#         theme(axis.text.x = element_text(angle = 30, hjust = 1,size = 15), 
#               axis.title = element_text(size = 20,face = "bold"),
#               axis.text.y = element_text(size = 15), 
#               panel.grid.major =element_blank(), 
#               panel.grid.minor = element_blank(),
#               panel.background = element_rect(fill = "transparent",colour = NA),
#               plot.background = element_rect(fill = "transparent",colour = NA),
#               axis.line = element_line(colour = "black"),legend.position="none")
# )


print(ggplot()+geom_line(data = ShapleyRolling,aes(x = Date,y = MKRturn),colour = "red",size=1)+
        geom_line(data = ShapleyRolling,aes(x = Date,y = RealEstateDiff),colour = "gray",size=1)+
        geom_line(data = ShapleyRolling,aes(x = Date,y = TED),colour = "orange",size=1)+
        xlab("Date")+ylab("Shapley Values")+
        scale_x_date(breaks = datebreaks) +
        scale_y_continuous(limits = c(0.015,0.06),
                           breaks = seq(0,0.06,0.01),
                           expand = c(0,0),
                           sec.axis = sec_axis(~.*42,name = 'FRM', breaks = seq(0,2.5,0.5)))+
        geom_line(data = ShapleyRolling,aes(x = Date,y = FRM/42),colour = "blue",size=0.75,linetype = 'dashed')+
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

sta_Shapley = data.frame(Macro = c(Macro_Total), mean = NA, max = NA, min = NA, sd = NA)

for (iFac in Macro_Total){
  sta_Shapley[which(sta_Shapley$Macro == iFac), c(2 : ncol(sta_Shapley))] = c(mean(Shapley[,iFac]), max(Shapley[,iFac]), 
                                                                              min(Shapley[,iFac]), sd(Shapley[,iFac]))
}
write.csv(sta_Shapley, paste0(output_path, "/Description of Shapley.csv"), quote = FALSE)



