#!/usr/bin/env python
# coding: utf-8

# In[1]:


import numpy as np
import pandas as pd


# In[2]:


df=pd.read_csv("bollywood.csv")
print(f"Given data is: \n{df}")


# In[3]:


idx = df[ df['imdb_votes'] < 2500 ].index
df.drop(idx , inplace = True)


# In[4]:


df['bavg_rating'] = df.apply(lambda row: row.imdb_votes*row.imdb_votes + 2500*row.imdb_rating + 2500*row.imdb_votes + 2500*5.5, axis = 1)
print(f"Updated data after adding a column of Rating based on Bayesian Average: \n{df}")


# In[5]:


df.sort_values(by = 'bavg_rating', inplace = True, ascending = False)
print(f"Top 10 movies are:\n{df.head(10)}")


# In[ ]:




