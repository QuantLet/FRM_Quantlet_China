#This file implements the upHRP algorithm, the MinVAr portfolio and the InvLamda portf.
#Code for classical HRP is based on Lopez de Prado, M. (2018). Advances in Financial 
#Machine Learning. Wiley. The code has been modified to create an uplifted portfolio
#strategies based on FRM adjacency mtrices and its adapted in order to be used with
#python 3 and the data set.
 
#Souhir Ben Amor
#@date: 20201010
#"""

#[0] Import library

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import xlrd

import os
os.chdir('/Users/wkhaerdle/Documents/Project/FRM-master_Modify_WRT/FRM_Quantlet/FRM_All/Cluster')
path = os.getcwd() # Set Working directory here

#[0]Upload input data, Financial Institutions of the 6 Emerging Markets and adjacency matrix
date = "20200203"
FRM_Adjacency_matrix= pd.read_csv("adj_matix_"+date+"_Shortname.csv")
# FRM_Adjacency_matrix= pd.read_csv("adj_matix_"+date+"_addname.csv")

FRM_Adjacency_matrix  = FRM_Adjacency_matrix.drop(columns=['FXI.US.EQUITY', 'VXFXI.INDEX', 'CN2YR', 'CN210SLOPE'])
FRM_Adjacency_matrix  = FRM_Adjacency_matrix.drop(index=[50, 51, 52, 53])

FRM_Adjacency_matrix = FRM_Adjacency_matrix.rename(columns = {"date":""})
FRM_Adjacency_matrix = FRM_Adjacency_matrix.set_index("") # define Date  as index

Corr_mat=FRM_Adjacency_matrix
Cov_mat=FRM_Adjacency_matrix

# In[1]: 
# Load modules


# Import modules for Datastructuring and calc.
import pandas as pd
import numpy as np
from scipy import stats
import warnings
from tqdm import tqdm

# Modules for RHP algorithm
import matplotlib.pyplot as mpl
import scipy.cluster.hierarchy as sch

# Modules for the network plot
import networkx as nx
from networkx.convert_matrix import from_numpy_matrix

# Modules for Markowitz optimization
import cvxopt as opt
import cvxopt.solvers as optsolvers

warnings.filterwarnings("ignore") # suppress warnings in clustering

# In[2]: 
# define functions for HRP and IVP

def getIVP(cov,**kargs):
    # Compute the inverse-variance portfolio
    ivp=1./np.diag(cov)
    ivp/=ivp.sum()
    return ivp


def getClusterVar(cov, cItems):
    # Compute variance per cluster
    cov_=cov.loc[cItems, cItems] # matrix slice
    w_=getIVP(cov_).reshape(-1,1)
    cVar=np.dot(np.dot(w_.T,cov_),w_)[0,0]
    return cVar


def getQuasiDiag(link):
    # Sort clustered items by distance
    link=link.astype(int)
    sortIx=pd.Series([link[-1,0],link[-1,1]])
    numItems=link[-1,3] # number of original items
    while sortIx.max() >=numItems:
        sortIx.index=range(0,sortIx.shape[0]*2,2) # make space
        df0=sortIx[sortIx>=numItems] # find clusters
        i=df0.index;j=df0.values-numItems
        sortIx[i]=link[j,0] # item 1
        df0=pd.Series(link[j,1], index=i+1)
        sortIx=sortIx.append(df0) # item 2
        sortIx=sortIx.sort_index() # re-sort
        sortIx.index=range(sortIx.shape[0]) # re-index
    return sortIx.tolist()


def getRecBipart(cov,sortIx):
    # Compute HRP alloc
    w=pd.Series(1,index=sortIx)
    cItems=[sortIx] # initialize all items in one cluster
    while len(cItems)>0:
        cItems=[i[j:k] for i in cItems for j,k in ((0,len(i)//2),(len(i)//2,\
                len(i))) if len(i)>1] # bi-section
        for i in range(0,len(cItems),2): # parse in pairs
            cItems0=cItems[i] # cluster 1
            cItems1=cItems[i+1] # cluster 2
            cVar0=getClusterVar(cov,cItems0)
            cVar1=getClusterVar(cov,cItems1)
            alpha=1-cVar0/(cVar0+cVar1)
            w[cItems0]*=alpha # weight 1
            w[cItems1]*=1-alpha # weight 2
    return w


def correlDist(corr):
    # A distance matrix based on correlation, where 0<=d[i,j]<=1 
    # This is a proper distance metric
    dist=((1-corr)/2.)**.5 # distance matrix
    return dist


def plotCorrMatrix(path, corr, labels=None):
    # Heatmap of the correlation matrix
    if labels is None:labels=[]
    mpl.pcolor(corr)
    mpl.colorbar()
    mpl.yticks(np.arange(.5,corr.shape[0]+.5),labels)
    mpl.xticks(np.arange(.5,corr.shape[0]+.5),labels)
    mpl.savefig(path,dpi=300, transparent=True)
    mpl.clf();mpl.close() # reset pylab
    return

# In[3]: 
# define function for MinVar portfolio
    
# The MIT License (MIT)
#
# Copyright (c) 2015 Christian Zielinski
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.   

def min_var_portfolio(cov_mat, allow_short=False):
    """
    Computes the minimum variance portfolio.

    Note: As the variance is not invariant with respect
    to leverage, it is not possible to construct non-trivial
    market neutral minimum variance portfolios. This is because
    the variance approaches zero with decreasing leverage,
    i.e. the market neutral portfolio with minimum variance
    is not invested at all.
    
    Parameters
    ----------
    cov_mat: pandas.DataFrame
        Covariance matrix of asset returns.
    allow_short: bool, optional
        If 'False' construct a long-only portfolio.
        If 'True' allow shorting, i.e. negative weights.

    Returns
    -------
    weights: pandas.Series
        Optimal asset weights.
    """
    if not isinstance(cov_mat, pd.DataFrame):
        raise ValueError("Covariance matrix is not a DataFrame")

    n = len(cov_mat)
        
    P = opt.matrix(cov_mat.values)
    q = opt.matrix(0.0, (n, 1))

# Constraints Gx <= h
    if not allow_short:
    # x >= 0
       G = opt.matrix(-np.identity(n))
       h = opt.matrix(0.0, (n, 1))
    else:
        G = None
        h = None

# Constraints Ax = b
# sum(x) = 1
    A = opt.matrix(1.0, (1, n))
    b = opt.matrix(1.0)

    # Solve
    optsolvers.options['show_progress'] = False
    sol = optsolvers.qp(P, q, G, h, A, b)
        
    if sol['status'] != 'optimal':
        warnings.warn("Convergence problem")
            
 # Put weights into a labeled series
    weights = pd.Series(sol['x'], index=cov_mat.index)
    return weights  

# In[4]: 
# Define functions for network graphs
    
#Function to plot Network plots
def plotNetwork(path,corr):
    # Transform it in a links data frame
    #links=corr.stack().reset_index()
    #Build graph
    corr=Corr_mat
    adj_matrix = corr
    constits_latest = corr.index
    # remove self-loops
    adj_matrix = np.where((adj_matrix<=1.000001) & (adj_matrix>=0.99999),0,adj_matrix)
    # replace values that are below threshold
    # create undirected graph from adj_matrix
    graph = from_numpy_matrix(adj_matrix, parallel_edges=False, create_using= nx.Graph())
    # set names to crypots
    graph = nx.relabel.relabel_nodes(graph, dict(zip(range(len(constits_latest)), constits_latest)))
    pos_og =  nx.circular_layout(graph, scale=2)
    pos = nx.circular_layout(graph, scale=1.7)
    
    for p in pos:  # raise text positions
        if pos[p][1]>1:
            pos[p][1] += 0.15
        if pos[p][1]<-1:
            pos[p][1] -= 0.15
        elif pos[p][0]<0:
            pos[p][0] -= 0.3
        else:
            pos[p][0]+=0.3
    plt = mpl.figure(figsize = (5,5)) 
    nx.draw(graph, pos_og, with_labels= False)
    nx.draw_networkx_labels(graph, pos)
     
    plt.savefig(path,dpi=300 ,transparent=True)
    mpl.clf();mpl.close()
    return

## In[5]:
# Loading and structuring crypto data sets

def plotcluster(path, link, dist):
    mpl.figure(num=None, figsize=(17, 8), dpi=600, facecolor='w', edgecolor='k')
    dn = sch.dendrogram(link, labels = dist.columns)
    mpl.savefig(path,transparent=True,dpi = 600,bbox_inches ='tight')
    mpl.clf();mpl.close() # reset pylab
    return

# In[6]:
# Heatmap and network analysis of corr. matrix

# Plotting Correlation matrix heatmap

plotCorrMatrix(path+"/Adj_matrix_Heatmap_FIs_unsorted",Corr_mat)


# network plot of correlation matrix

plotNetwork(path+"/Corr_Network_FIs_unsorted.png", Corr_mat)

# Sort correlation matrix
dist=correlDist(Corr_mat)
link=sch.linkage(dist,'single')
sortIx=getQuasiDiag(link) 
sortIx=Corr_mat.index[sortIx].tolist() # recover labels 
Corr_sorted=Corr_mat.loc[sortIx,sortIx] # reorder

# Plot sorted correlation matrix
plotCorrMatrix(path+"/Adj_matrix_Heatmap_FIs_sorted",Corr_sorted)

# Plot dendogram of the constituents
#2) Cluster Data

plotcluster(path+"/Dendrogram_FIs_"+date,link,dist)


print(plotNetwork)

# In[7]:
#Function to calculate the HRP portfolio weights

def HRPportf(cov,corr):
    #1) Cluster covariance matrix
    dist=correlDist(corr)
    link=sch.linkage(dist,'single')
    sortIx=getQuasiDiag(link) 
    sortIx=corr.index[sortIx].tolist() # recover labels
    #2) Allocate capital according to HRP
    weights_hrp=getRecBipart(cov,sortIx)
    return weights_hrp




