import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import math
from sklearn.cluster import KMeans
import plotly.express as px

#Note: This script gives a simple demonstration of how I've gone about demographic clustering in an account database.
# No real data is used or output; but this shows the general methodology to use Kmeans clustering for
# marketing segmentation purposes.

plt.interactive(False)

def floor(input):
    return math.floor(input)

#doesn't actually lead to a source, but this would be a data source with a user gender, ethnicity, and birthdate.
df = pd.read_csv("PATH_TO_DATA")

#dataframe with three columns: birthdate, race, and gender.
df_no_na = df[df[['birth', 'race', 'gender']].notnull().all(1)]

#data manipulation, making sure birthdate is a date, converting birthdate to age, and making the age an integer.
df_no_na[['creation_date','birth']] = df_no_na[['creation_date','birth']].apply(pd.to_datetime) #if conversion required
df_no_na['age'] = (df_no_na['creation_date'] - df_no_na['birth']).dt.days.astype('timedelta64[D]')
df_no_na['ageyears'] = df_no_na['age'].astype('timedelta64[D]')/365.25
df_no_na['ageyears'] = df_no_na['ageyears'].apply(floor)

df_no_na = df_no_na[['ageyears', 'gender', 'race']]

#make age a categorical variable instead of a continuous variable.
def flag_df(df):
    if (df['ageyears'] <= 18):
        return '18_under'
    elif (df['ageyears'] <= 24) & (df['ageyears']>18):
        return '19_24'
    elif (df['ageyears'] <= 34) & (df['ageyears']>24):
        return '25_34'
    elif (df['ageyears'] > 34):
        return '35_plus'

df_no_na['agegroup'] = df_no_na.apply(flag_df, axis = 1)
df_no_na.head()

df_dummies_orig = df_no_na[["agegroup"]]
df_dummies_other = df_no_na[["agegroup","gender", "race"]]

#create final df with dummy variables for each of the categorical variables above.
df_final = df_dummies_other.copy()

#Find WCSS for each version of K. Ultimately find that 3 is a good set of clusters.
wcss = []
for k in range(1,11):
    kmeans = KMeans(n_clusters=k, init="k-means++")
    kmeans.fit(df_final)
    wcss.append(kmeans.inertia_)
    print("On",str(k))
plt.figure(figsize=(12,6))
plt.grid()
plt.plot(range(1,11),wcss, linewidth=2, color="red", marker ="8")
plt.xlabel("K Value")
plt.xticks(np.arange(1,11,1))
plt.ylabel("WCSS")
plt.savefig("WCSS")

#Use a K of 3
km = KMeans(n_clusters=3)
clusters = km.fit_predict(df_final)
df_final_clusters = pd.concat([df_no_na.reset_index(drop=True), pd.DataFrame(clusters).reset_index(drop=True)], axis=1)
df_final_clusters.rename(columns={0:"category"}, inplace = True)

#Visualization
fig = px.parallel_categories(df_final_clusters, dimensions=['agegroup', 'gender', 'race'],
                color="category", color_continuous_scale=px.colors.sequential.Inferno,
                labels={'agegroup':'Age', 'gender':'Gender', 'race':'Ethnicity'})
fig.write_image("parallel_plot.png")
