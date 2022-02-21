#!/usr/bin/env python
# coding: utf-8

# In[ ]:


import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
import os 


# program to read in coverage files for each sample and get mean

# In[ ]:


path = '/Users/higginsj/OneDrive - Norwich BioScience Institutes/AABBcov/indiviual_median_cov'
table_list = []

for filename in os.listdir(path):
    if filename.endswith('.txt'):
        table_list.append(filename)
        


# In[ ]:


covall = {}


# In[ ]:


for f in table_list:
    name = pd.read_csv(f,  sep="\t", header = None)
    name.columns =['chr', 'start', 'end', 'cov']
    name["cov"] .mask(name["cov"]  == '.', '0', inplace=True)
    name["cov"] = pd.to_numeric(name["cov"])
    name_B = name[name['chr'].str.match('Bchr')]
    mB = name_B['cov'].mean()
    name_A = name[name['chr'].str.match('^chr')]
    mA = name_A['cov'].mean()
    covall.update({f: [mA, mB]})
   


# In[ ]:


covall


# In[ ]:


df_covall = pd.DataFrame.from_dict(covall, orient='index')


# In[ ]:


df_covall.reset_index(inplace=True)


# In[ ]:


df_covall.to_csv("results.csv", sep = ",")


# In[ ]:


df_covall


# In[ ]:





# In[ ]:


df_covall.columns =['sample','meanA', 'meanB']


# In[ ]:


sns.catplot(
    data = df_covall,
    y='sample',
    x='meanA',
    kind="bar"
    )


# In[ ]:




