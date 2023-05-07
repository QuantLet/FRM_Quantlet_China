# -*- coding: utf-8 -*-
"""
Created on Mon Dec  3 12:08:40 2018

@author: rbgud
"""

import matplotlib.cm as cm
from datetime import datetime
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.tsa.stattools as ts
import statsmodels.api as sm
from statsmodels.tsa.stattools import grangercausalitytests
from arch import arch_model
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.decomposition import PCA
from sklearn.manifold import TSNE
from sklearn.preprocessing import scale
from sklearn import preprocessing
from sklearn.preprocessing import StandardScaler
from scipy import stats
import networkx as nx
import sys
import math

sns.set()

root = '/Users/ruting/Documents/macbook/PcBack/FRM_Quantlet/FRM_All/MethodAdd/'
date_end_source = '20230306'

Data_total = pd.read_csv(root+'Return_'+date_end_source+'.csv',engine='python')

Data_total = Data_total.drop(Data_total.columns[0], axis = 1)

Data_total['DATE'] = Data_total['ticker'].astype('str')
Data_total['DATE'].str.slice(0,4)+'-'+Data_total['DATE'].str.slice(5,6)+'-'+Data_total['DATE'].str.slice(7,8)
Data_total.index  = pd.to_datetime(Data_total['DATE'],format='%Y-%m-%d')
Data_total.pop('DATE')
Data_total = Data_total.drop(['ticker'], axis = 1)

dropcol = Data_total.columns[Data_total[:200].sum(axis = 0) == 0]
Data_total = Data_total.drop(dropcol, axis = 1)

#%% back check original
# =============================================================================
# root = '/Users/ruting/Documents/macbook/PcBack/FRM_Quantlet/FRM_All/Econometric-measures-of-connectedness-and-systemic-risk-in-the-finance-and-insurance-sectors-master/'
# 
# HFund = pd.read_csv(root+'hedge (1).csv',engine='python')
# HFund.index = pd.to_datetime(HFund['DATE'],format='%Y-%m-%d')
# HFund.pop('DATE')
# 
# Bank = pd.read_csv(root+'bank (1).csv',engine='python')
# Bank.index = pd.to_datetime(Bank['DATE'],format='%Y-%m-%d')
# Bank.pop('DATE')
# 
# Insurance = pd.read_csv(root+'insurance (1).csv',engine='python')
# Insurance.index = pd.to_datetime(Insurance['DATE'],format='%Y-%m-%d')
# Insurance.pop('DATE')
# 
# Bdealer = pd.read_csv(root+'dealer (1).csv',engine='python')
# Bdealer.index = pd.to_datetime(Bdealer['Date'],format='%Y-%m-%d')
# Bdealer.pop('Date')
# 
# mkc=pd.read_csv(root+'mkc.csv', index_col='DATE',engine='python')
# mkc.index=pd.to_datetime(mkc.index,format='%Y-%m-%d')
# 
# 
# #%% Set data period 
# 
# start_date = '2000-05-31'
# 
# HFund = HFund.loc[HFund.index >= start_date]
# 
# Bank = Bank.loc[Bank.index >= start_date]
# 
# Insurance = Insurance.loc[Insurance.index >= start_date]
# 
# Bdealer = Bdealer.loc[Bdealer.index >= start_date]
# 
# #%% Make Return data 
# 
# HFund_ret = HFund.pct_change().dropna(axis=0,how='all').dropna(axis=1,how='any').iloc[:,:]
# 
# Bank_ret = Bank.pct_change().dropna(axis=0,how='all').dropna(axis=1,how='any').iloc[:,:25]
# 
# Insurance_ret = Insurance.pct_change().dropna(axis=0,how='all').dropna(axis=1,how='any').iloc[:,:25]
# 
# Bdealer_ret = Bdealer.pct_change().dropna(axis=0,how='all').dropna(axis=1,how='any').iloc[:,:]
# 
# Bdealer_ret.pop('ISTN US Equity')
# Bdealer_ret.pop('NDB US Equity')
# 
# #%% Concate total data 
# 
# Data_total2 = pd.concat([Bank_ret,Bdealer_ret,HFund_ret,Insurance_ret],axis=1).dropna(axis=1,how='any')
# =============================================================================




#%% PCA Part
window = 36
def draw_scree(data,i,window):
    draw=[]
    for j in range(len(data)-window):
        pca=PCA()
        pca.fit(data[j:j+window])
        draw.append(pca.explained_variance_ratio_[0:i].sum())
        
    return np.array(draw)   
    
plt.figure(figsize = (12,7))    
a=draw_scree(Data_total, 1,window)*100
b=draw_scree(Data_total, 5,window)*100
c=draw_scree(Data_total, 10,window)*100
d=draw_scree(Data_total, 25,window)*100
# Your x and y axis
#x=Data_total.index[30:216]
x=Data_total.index[36:]
y=[ a, b-a, c-b, d-c]

data = {'Date' : x, 'PCA_1':a, 'PCA_5':b-a, 'PCA_10':c-b, 'PCA_25':d-c}
PCA = pd.DataFrame(data)

PCA.to_excel(root+'PCA_'+date_end_source+'.xlsx')

 
# use a known color palette (see..)
pal = sns.color_palette("Set1")
plt.stackplot(x,y, labels=['PC1','PC5','PC10','PC25'], colors=pal, alpha=0.4 )
plt.ylabel('Explained variance')
plt.xlabel('Date')
plt.legend(loc='upper left')
plt.title('Principal Component Analysis Explained Variance Ratio')
plt.legend(loc='upper left')
plt.show()



#%% Granger Causality Part
def Grangercausality_test(data,reverse):
    
    Check_list = []
    Save_index = []
    Score_list = []
    Pvalue_list = []
    in_stock_list = []
    out_stock_list = []
    
    for i in range(data.shape[1]):
        
        if reverse == False : # Granger cause
            
           j_range = np.arange(i+1,data.shape[1])
           
        elif reverse == None:
            
           j_range = np.arange(data.shape[1]) 
           
        else: # Reverse Granger cause  
            
           j_range = np.arange(i+1) 
          
        for j in j_range:    
           
           maxlag=3
               
           if i!=j:
            
                result_ = grangercausalitytests(data[[data.columns[i],data.columns[j]]],
                                                maxlag=maxlag,
                                                addconst=True,
                                                verbose=False)
            
                '''Null reject case : The significance level was set to 0.05 (5%), and the test found that the p-value was 0.05 Below 
                If it comes out, the null hypothesis can be rejected. The null hypothesis is "does not follow Granger Causality"'''
               
                if result_[3][0]['ssr_ftest'][1] < 0.05 : 
                    
#                   print('{} granger causes {}'.format(data.columns[j],data.columns[i]))
                    
                   Check_list.append(['{} granger causes {}'.format(data.columns[j],data.columns[i])]) 
                   
                   if reverse == False:
                   
                      tuple_list = (data.columns[i],data.columns[j])
                      
                   elif reverse == None:
                   
                      tuple_list = (data.columns[i],data.columns[j])   
                      
                   else:
                       
                      tuple_list = (data.columns[j],data.columns[i]) 
                      
                   in_stock = data.columns[j]
                   out_stock = data.columns[i]   
                   
                   score = 1
                   
                   pvalue = result_[1][0]['ssr_ftest'][1]
                   
                   Pvalue_list.append(pvalue)
                   
                   Score_list.append(score)
                   
                   Save_index.append(tuple_list)
                   
                   in_stock_list.append(in_stock)
                   out_stock_list.append(out_stock)
                   
                else :
                    
#                   print('{} does not granger cause {}'.format(data.columns[j],data.columns[i]))
                    
                   Check_list.append(['{} does not granger cause {}'.format(data.columns[j],data.columns[i])])
    
                   if reverse == False:
                   
                      tuple_list = (data.columns[i],data.columns[j])
                      
                   elif reverse == None:
                   
                      tuple_list = (data.columns[i],data.columns[j])   
                      
                   else:
                       
                      tuple_list = (data.columns[j],data.columns[i])
                      
                   in_stock = data.columns[j]
                   out_stock = data.columns[i]
                   
                   score = 0
                   
                   pvalue = result_[1][0]['ssr_ftest'][1]
                   
                   Pvalue_list.append(pvalue)
                   
                   Score_list.append(score)
                   
                   Save_index.append(tuple_list)    

                   in_stock_list.append(in_stock)
                   out_stock_list.append(out_stock)
         
    GC_indicator = pd.DataFrame(Check_list,columns=['GC result'],
                      index=pd.MultiIndex.from_tuples(Save_index))
    GC_indicator['P-value'] = Pvalue_list
    GC_indicator['GC score'] = Score_list
    GC_indicator['out stock'] = in_stock_list
    GC_indicator['in stock'] = out_stock_list
    
    return GC_indicator


def Rolling_DGC(data,window):
    
    if len(data) < window:
               
       print("Observation window must be shorter than Data period(len(data))")
         
    rolling_DGCscore = []
    for j in range(len(data)-window):
                     
        data_ = data[j:j+window]
        
        GC_result = Grangercausality_test(data_,reverse = False)
        
        GC_resultrev = Grangercausality_test(data_,reverse = True)
        
        GC_scoresum = (GC_result['GC score']+GC_resultrev['GC score']).sum(axis=0)
        
        DGC = GC_scoresum/(len(data_.columns)*(len(data_.columns)-1))
        
        rolling_DGCscore.append(round(DGC,4))
            
    rolling_DGCscore = pd.DataFrame(rolling_DGCscore).set_index(data.index[window:])
    rolling_DGCscore.columns = ['rolling score']
    
    return rolling_DGCscore        

rolling_GDC_total = Rolling_DGC(Data_total,window=36)
rolling_GDC_total.to_excel(root+'rolling_GDC_total_'+date_end_source+'.xlsx')

#rolling_DGC2 = Rolling_DGC(Data_total[4:80],window=36)
#rolling_DGC3 = Rolling_DGC(Data_total[44:120],window=36)
#rolling_DGC4 = Rolling_DGC(Data_total[84:160],window=36)
#rolling_DGC5 = Rolling_DGC(Data_total[124:220],window=36)

# =============================================================================
# 
# rolling_GDC_total = pd.concat([rolling_DGC,rolling_DGC2,rolling_DGC3,rolling_DGC3,rolling_DGC4,rolling_DGC5],axis=0)
# #%%   
# rolling_GDC_total = pd.read_csv(root+'DGC_rolling graph data.csv',engine='python')
# rolling_GDC_total.index = pd.to_datetime(rolling_GDC_total['DATE'],format='%Y-%m-%d')
# rolling_GDC_total.pop('DATE')
# 
# =============================================================================
plt.figure(figsize = (12,7)) 
# =============================================================================
# plt.plot(rolling_GDC_total.iloc[:73,:].index,100*rolling_GDC_total.iloc[:73,:],label='Rolling DGC')
# =============================================================================
plt.plot(100*rolling_GDC_total.iloc[:73,:],label='Rolling DGC')

plt.plot(100*rolling_GDC_total,label='Rolling DGC')

plt.plot(rolling_GDC_total.index,np.ones(len(rolling_GDC_total))*6,'r-')
plt.plot(rolling_GDC_total.index,np.ones(len(rolling_GDC_total))*5.5,'r--')
plt.plot(rolling_GDC_total.index,np.ones(len(rolling_GDC_total))*5,'r-')   
plt.ylabel('Rolling Degree of Granger Causality(%)')
plt.xlabel('Date')
plt.legend(loc='upper left')
plt.title('Rolling Degree of Granger Causality (window = 3yrs), Total Period')

plt.figure(figsize = (12,7)) 
plt.plot(rolling_GDC_total.index,100*rolling_GDC_total,label='Rolling DGC',linewidth=4)
plt.stackplot(x,y, labels=['PC1','PC5','PC10','PC25'], colors=pal, alpha=0.4 )
plt.ylabel('PCA and GC (%)')
plt.xlabel('Date')
plt.legend(loc='upper left')
plt.title('Compare PCA and Rolling Degree of Granger Causality (window = 3yrs), Total Period')

