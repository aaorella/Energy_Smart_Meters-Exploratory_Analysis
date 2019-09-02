#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np
import matplotlib.pylab as plt
from math import sqrt


# In[2]:


dir_origen_csv = "./CSV_generated/"
dir_export = "./Images_generated/"


# In[3]:


def DTWDistance(s1, s2,w):
    DTW={}
    
    w = max(w, abs(len(s1)-len(s2)))
    
    for i in range(-1,len(s1)):
        for j in range(-1,len(s2)):
            DTW[(i, j)] = float('inf')
    DTW[(-1, -1)] = 0
  
    for i in range(len(s1)):
        for j in range(max(0, i-w), min(len(s2), i+w)):
            dist= (s1[i]-s2[j])**2
            DTW[(i, j)] = dist + min(DTW[(i-1, j)],DTW[(i, j-1)], DTW[(i-1, j-1)])
    return sqrt(DTW[len(s1)-1, len(s2)-1])


# In[4]:


def LB_Keogh(s1,s2,r):
    LB_sum=0
    for ind,i in enumerate(s1):
        
        lower_bound=min(s2[(ind-r if ind-r>=0 else 0):(ind+r)])
        upper_bound=max(s2[(ind-r if ind-r>=0 else 0):(ind+r)])
        
        if i>upper_bound:
            LB_sum=LB_sum+(i-upper_bound)**2
        elif i<lower_bound:
            LB_sum=LB_sum+(i-lower_bound)**2
    
    return sqrt(LB_sum)


# In[6]:


#import pixiedust


# In[ ]:


# In[10]:


def sse(centroids, indicesSeries, timeSeries):
    sumaI = 0
    countI = 0
    for c in range(len(centroids)):
        suma = 0
        count = 0
        cent = centroids[c]
        indices = indicesSeries[c]
        for i in indices:
            s = timeSeries[i]
            suma += DTWDistance(cent,s,5)**2
            count += 1
        #suma = suma / count
        sumaI += suma
        countI +=1
    #sumaI /= countI
    return sumaI


# In[11]:


df = pd.read_csv('/CSV_generated/medidores_casi_unifores.csv', low_memory = False, sep=",")
df["fecha"] =  pd.to_datetime(df["fecha"]) #transforming "fecha" to datetime
df


# In[12]:


df.sort_values(by = "fecha", inplace = True)


# In[13]:


df.set_index("fecha", inplace = True)


# In[14]:


max(df.index.hour)


# In[15]:


min(df.index.hour)


# In[16]:


Series = []
count = 0
missing_count = 0
meters = []

for n,g in df.groupby(["medidor"]):
    #g.set_index("fecha", inplace = True)

    g = g.resample("2H").mean()
    date = (g.index.date[0])

    idx = pd.date_range(date, periods=12, freq='2H')
    s = pd.Series(np.nan, index = idx)
    s = s.loc[(s.index.hour >= 4) & (s.index.hour <= 18)]
    
    serie = g["kwh_consumido"]
    serie = serie[(serie.index.hour >= 5) & (serie.index.hour <= 17)]
    #serie = serie.add(s, fill_value = None)
    serie = serie.interpolate(method = "time", limit_direction = "both")
    #nornalizing
    #serie = (serie - serie.min()) / (serie.max() - serie.min())
    
    #print(len(serie))
    if(len(serie) == 366):
        meters.append(n)
        Series.append(serie)
    print(len(serie), count)
    #print(str(n) + ":   " + str(min(g.index.hour)) +"--" + str(max(g.index.hour)) +"  Tamaño: " + str(len(g)) + " indice: " + str(count))
    if (serie.isnull().sum() != 0):
        print(count)
        #print(serie)
    count +=1


# Meters and Series list have the same index

# ## Calculating the numeber of clusters

# In[17]:


meters[-2]


# In[18]:


df[df["medidor"] == 58706740]


# In[19]:


Series[-2]


# In[20]:


a = Series[1].copy()


# In[21]:


a.index = a.index.hour


# In[22]:


a


# In[23]:


import pixiedust


# In[ ]:



# The best number for our clusters is

# ## extracting dataframes from clusters

# In[ ]:




