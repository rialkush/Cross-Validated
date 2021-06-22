#!/usr/bin/env python
# coding: utf-8

# In[1]:


import numpy as np
import pandas as pd
import math


# In[2]:


df=pd.read_csv("titanic.csv")
print(f"Given data is: \n {df}")


# In[3]:


indexes = ['Age', 'SibSp', 'Parch', 'Fare']
mx = dict({'Age': 0, 'SibSp': 0, 'Parch': 0, 'Fare': 0})
mn = dict({'Age': 0, 'SibSp': 0, 'Parch': 0, 'Fare': 0})


# In[4]:


for key in indexes:
    mx[key] = np.max(df[key])
    mn[key] = np.min(df[key])
print(f"Maximum element in unfit columns are: \n {mx}")
print(f"Minimum element in unfit columns are: \n {mn}")


# In[5]:


for key in indexes:
    df[key] = (df[key] - mn[key]) / (mx[key] - mn[key])
print(f"Normalized data is: \n {df}\n")


# In[6]:


target = df.Survived


# In[7]:


df.drop('Survived',axis=1,inplace=True)


# In[8]:


def stdnorm(xi, beta):
    val = math.e**((-(xi@beta)**2)/2)/math.sqrt(2*math.pi)
    return val


# In[9]:


def grad(xi, yi, beta):
    k = stdnorm(xi, beta)
    val = (xi@beta)*(xi)*(-yi+(1-yi)*(k/(1-k)))
    return val


# In[10]:


p=6
beta = np.ones(p).astype('float64')
t = 0.001
iter = 200
for j in range(iter):
    grad_sum = np.zeros(p)
    for i in range(len(df.index)):
        xi = df.loc[i].values
        yi = target[i]
        grad_sum += grad(xi, yi, beta)
    old_beta = beta
    beta += t*grad_sum
    print(f"After iteration {j+1} :")
    print(f"Updated beta is : \n {beta}")


# In[11]:


print(f"MLE of beta is: \n {beta}\n")


# In[12]:


X = df.to_numpy()


# In[13]:


sum=0
for i in range(len(X)):
    p = stdnorm(X[1], beta)
    sum+=(p>0.5)==target[i]
print(f"Accuracy on test data is: {sum/len(X)} \n")


# In[14]:


Jack = np.array([1, 1, 20, 0, 0, 7.5]).astype('float64')
Rose = np.array([1, 0, 19, 1, 1, 512]).astype('float64')
print(f"Given data of Jack: \n{Jack}\n")
print(f"Given data of Rose: \n{Rose}\n")


# In[15]:


i = 2
for key in indexes:
    Jack[i] = (Jack[i] - mn[key]) / (mx[key] - mn[key])
    Rose[i] = (Rose[i] - mn[key]) / (mx[key] - mn[key])
    i+=1
print(f"Normalized data of Jack: \n{Jack}\n")
print(f"Normalized data of Rose: \n{Rose}\n")


# In[16]:


print(f"Probability of survival of Jack is: {stdnorm(Jack, beta)}\n")
print(f"Probability of survival of Rose is: {stdnorm(Rose, beta)}\n")


# In[ ]:




