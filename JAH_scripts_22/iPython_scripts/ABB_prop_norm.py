#!/usr/bin/env python
# coding: utf-8

# In[ ]:


import pandas as pd
import os
import seaborn as sns
import matplotlib.pyplot as plt


# input our_group data for samples and save as a dict
# 

# In[ ]:


meta = pd.read_csv("../meta_data13_new.csv")
meta_dict = meta.set_index('Taxa')['our_group'].to_dict()


# In[ ]:


read_dict = meta.set_index('Taxa')['total_reads'].to_dict()
read_dict


# In[ ]:


# this will hold our dataframes
all_dataframes = []

# Folder Path
path = '/Users/higginsj/Documents/align_stats_by_chrom/ABB_stats_by_chrom'
     
for filename in os.listdir(path):

    # only read files matching the pattern
    if filename.endswith(".txt"):
        print(f"reading {filename}...")
        df = pd.read_csv(filename,sep ="\t",index_col=False)

        # take the bit of the filename between underscore for sample name
        df["Taxa"] = filename.split("_")[3]
        all_dataframes.append(df)

# now concatenate all the dataframes in the list
big_df = pd.concat(all_dataframes)
big_df


# now add new columns from existing columns
# remove % from pp_mapped and make numeric
# make a new dataframe called abb_stats from required columns
# 

# In[ ]:



big_df['our_group']=big_df['Taxa'].map(meta_dict)
big_df


# In[ ]:


big_df["pp_mapped"] = big_df["pp_mapped"].replace('\%','', regex=True)
big_df.dtypes
big_df


# In[ ]:


big_df["pp_mapped"] = pd.to_numeric(big_df["pp_mapped"])         
big_df.dtypes


# In[ ]:


big_df[['stats','chrom','name','rest']] = big_df['sample'].str.split("_",expand=True,)

big_df


# In[ ]:


big_df[['genome','bam','txt']] = big_df['rest'].str.split(".",expand=True,)
big_df.to_csv("big_df.csv")
big_df


# In[ ]:





# In[ ]:


big_df["chrom"] = pd.to_numeric(big_df["chrom"])         
big_df.dtypes
big_df['chr_len']=big_df['chrom'].map(chr_dict)
big_df.dtypes
big_df


# In[ ]:


big_df['total_reads']=big_df['Taxa'].map(read_dict)
big_df
big_df.to_csv("big_df.csv")


# In[ ]:


abb_stats = big_df.filter(['Taxa','properly_paired','our_group','genome',"chrom","total_reads"], axis=1)
abb_stats.to_csv("ABB_stats.csv")
abb_stats.dtypes
#abb_stats.shape


# In[ ]:


#abb_stats
#abb_stats['all'] = abb_stats['chr_len']*abb_stats['reads']
#abb_stats


# In[ ]:


abb_stats['prop']=abb_stats['properly_paired']/abb_stats['total_reads']
abb_stats
abb_stats.to_csv("ABB_prop_stats.csv")


# In[ ]:





# plot by chromosome and group

# In[ ]:


sns.catplot(data=abb_stats, x = "our_group", y = "prop", kind = "box")


# In[ ]:


sns.catplot(data=abb_stats, x = "chrom", y = "prop", kind = "box")


# In[ ]:


sns.catplot(data=abb_stats, x = "chrom", y = "prop", kind = "swarm",hue = "our_group")



# In[ ]:


sns.catplot(data=abb_stats, x = "chrom", y = "prop", kind = "box",hue = "our_group")


# In[ ]:


sns.catplot(data=abb_stats, x = "our_group", y = "prop", kind = "box",hue = "chrom")


# https://medium.com/analytics-vidhya/how-to-summarize-data-with-pandas-2c9edffafbaf
# 
# a. min(): Return the minimum value
# 
# b. max(): Return the maximum value
# 
# c. mean(): Return the mean of the values
# 
# d. median(): Return the median of the values
# 
# 

# In[ ]:


abb_stats["our_group"].shape
abb_stats.describe
abb_stats. nunique()
abb_stats.sum()
abb_stats.count()
abb_stats["our_group"].shape


# In[ ]:


abb_stats.groupby("our_group").sum()


# In[ ]:


sucrier1 = abb_stats.loc[(abb_stats["our_group"]== "a_Sucrier_AA")]
sucrier1
suc_mean = sucrier1.groupby("chrom").mean()
suc_mean
#plt =sns.catplot(data=plantain11, x = "chrom", y = "prop", kind = "swarm")
#plt.savefig("ABB_plantain11.png")


# In[ ]:


plantain11 = abb_stats.loc[(abb_stats["our_group"]== "k_Plantain_ABB")]
plantain11

plan_mean = plantain11.groupby("chrom").mean()

plt =sns.catplot(data=plantain11, x = "chrom", y = "prop", kind = "box")
plt.savefig("ABB_plantain11.png")


# In[ ]:


GrosMichel3= abb_stats.loc[(abb_stats["our_group"]== "c_GrosMichel_AAA")]
GrosMichel3

gm3_mean = GrosMichel3.groupby("chrom").mean()


# In[ ]:


GrosMichel4= abb_stats.loc[(abb_stats["our_group"]== "d_GrosMichel-like_AAA")]
GrosMichel4

gm4_mean = GrosMichel4.groupby("chrom").mean()


# In[ ]:


cavendish2= abb_stats.loc[(abb_stats["our_group"]== "b_Cavendish_AAA")]
cavendish2

cav2_mean = cavendish2.groupby("chrom").mean()
cav2_mean


# In[ ]:


gm4_mean


# In[ ]:


groups_all = abb_stats["our_group"]
groups = groups_all.drop_duplicates(keep='first', inplace=False)


# In[ ]:


# this will hold our dataframes
mean_data = []

for gp in groups:
   print (gp)
   ind_gp = abb_stats.loc[(abb_stats["our_group"]== gp)]
   pp_mean = ind_gp.groupby("chrom").mean()
   pp_mean["our_group"] = gp
   print(pp_mean)
   
   mean_data.append(pp_mean)

# now concatenate all the dataframes in the list
mean_all = pd.concat(mean_data,axis=0)
mean_all



#mean_all  = pd.DataFrame(mean_data)
#mean_all

mean_all.to_csv("mean_ABB_all_stats.csv")


# In[ ]:


plan_like8 = abb_stats.loc[(abb_stats["our_group"]== "h_geno_unknown")]
plt =sns.catplot(data=plan_like8, x = "chrom", y = "prop", kind = "box")
plt.savefig("ABB_geno_unknown_8.png")


# In[ ]:





# In[ ]:





# In[ ]:




