# -*- coding: utf-8 -*-
"""
Created on Sat Aug 15 11:24:38 2020

@author: rianh
"""

#%%
'''Importing the necessary packages'''
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
from mpl_toolkits.mplot3d import Axes3D
from sklearn.cluster import KMeans

#%%
'''Random concert attendance'''
np.random.seed(2)
concerts = (np.random.uniform(low = 1, high = 10, size = 60))
concerts_round = [round(i) for i in concerts]
plt.hist(concerts_round, bins = np.arange(1, 11, 1))
plt.show()
sns.kdeplot(concerts_round)
concerts_round.sort(reverse = True)
#%%
np.random.seed(56)
dummy = 3*np.random.uniform(0, 1, 60)
dummy_round = [round(i) for i in dummy]
plt.hist(dummy_round, bins = [1, 2, 3, 4])
                  
#%%
age_1 = []
age_2 = []
age_3 = []
for i in dummy_round:
    if i == 1:
        age_1.append(np.random.uniform(low = 10, high = 30))
    if i == 2:
        age_2.append(np.random.uniform(low = 25, high = 50))
    if i == 3:
        age_3.append(np.random.uniform(low = 45, high = 80))

age_rough = np.concatenate((age_1, age_2, age_3), axis = 0)
age_round = [round(i) for i in age_rough]

#%%
''''Random income'''
np.random.seed(0)
income = (np.random.uniform(low = 10, high = 91, size = 60))
income_round = [round(i) for i in income]
plt.hist(income_round, bins = np.arange(10, 91, 10))
plt.show()
sns.kdeplot(income_round)
#%%
# =============================================================================
# '''Random age'''
# np.random.seed(1)
# age = (np.random.uniform(low = 20, high  = 81, size = 60))
# age_round = [round(i) for i in age]
# plt.hist(age_round, bins = np.arange(20, 81, 5))
# plt.show()
# sns.kdeplot(age_round)
# =============================================================================
#%%
'''creating a dataframe'''
df = pd.DataFrame(list(zip(income_round, age_round, concerts_round)), columns = ['income', 'age', 'concerts'])
#%%
'''Visualising the data ensemble'''
sns.set_style("white")
fig = plt.figure(figsize=(20,10))
ax = fig.add_subplot(111, projection='3d')
ax.scatter(df['income'], df['age'], df['concerts'], c='blue', s=60)
ax.view_init(30, 185)
plt.xlabel("Annual Income (Thousands of Rands)")
plt.ylabel("Age")
ax.set_zlabel('Number of concerts attended')
plt.show()

#%%
'''Determining a suitable number of clusters'''
wcss = []
for k in range(1,11):
    kmeans = KMeans(n_clusters=k, init="k-means++")
    kmeans.fit(df.iloc[:,1:])
    wcss.append(kmeans.inertia_)
plt.figure(figsize=(12,6))    
plt.grid()
plt.plot(range(1,11),wcss, linewidth=2, color="red", marker ="8")
plt.xlabel("K Value")
plt.xticks(np.arange(1,11,1))
plt.ylabel("WCSS")
plt.show()

#%%
'''Performing and visualising K-means clustering'''
km = KMeans(n_clusters=3)
clusters = km.fit_predict(df.iloc[:,1:])
df["label"] = clusters

fig = plt.figure(figsize=(20,10))
ax = fig.add_subplot(111, projection='3d')
ax.scatter(df['income'][df.label == 0], df['age'][df.label == 0], df['concerts'][df.label == 0], c='blue', s=60)
ax.scatter(df['income'][df.label == 1], df['age'][df.label == 1], df['concerts'][df.label == 1], c='red', s=60)
ax.scatter(df['income'][df.label == 2], df['age'][df.label == 2], df['concerts'][df.label == 2], c='green', s=60)
#ax.scatter(df['age'][df.label == 3], df['income'][df.label == 3], df['concerts'][df.label == 3], c='orange', s=60)
#ax.scatter(df['age'][df.label == 4], df['income'][df.label == 4], df['concerts'][df.label == 4], c='purple', s=60)
ax.view_init(30, 185)
plt.xlabel("Annual Income (Thousands of Rands)")
plt.ylabel("Age")
ax.set_zlabel('Number of concerts attended')
plt.show()

'''Code licensed under the Apache License 2.0'''

