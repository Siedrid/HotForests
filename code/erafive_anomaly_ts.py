# -*- coding: utf-8 -*-
"""
Created on Tue May 28 15:14:30 2024

@author: laura
"""

import os
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from datetime import datetime
import seaborn as sns
import pymannkendall as mk

os.chdir("C:/Users\\laura\\Documents\\Eagle\\CloudComputing\\")

forest_anoms = pd.read_csv("GEE_out/tables/Forest_Mean_Anomalies.csv")
forest_T = pd.read_csv("GEE_out/tables/Forest_Mean_Temperature.csv")

def make_date_column(df):
    dt = [datetime.strptime(str(df.year[i]) + '-' + str(df.month[i]).zfill(2)+ '-' + '01', "%Y-%m-%d") for i in range(len(df))]
    df['dt'] = dt

    return df

def get_era5_df(climate_param, forest_type):
    csv_name = 'Mean_' + climate_param + '_' + forest_type + '.csv'
    df = pd.read_csv("tables/" + csv_name)
    df = make_date_column(df)
    df['forest_type'] = forest_type
    return df

#%% Temperature TS

lst = ['temperature_2m', 'total_precipitation_sum', 'total_evaporation_sum']

def combine_era5_ts(climate_param):
        
    df_coniferous = get_era5_df(climate_param, 'Coniferous')
    df_broadleaf = get_era5_df(climate_param, 'Broad-leaved')
    df_mixed = get_era5_df(climate_param, 'Mixed')
    df_all = get_era5_df(climate_param, 'allForest')
    
    df_combined = pd.concat([df_coniferous, df_broadleaf, df_mixed, df_all])
    
    return df_combined

def plt_era5_ts(df_combined, climate_param):
    # plot time series for month
    # sns.set(style="whitegrid")
    fig, axes = plt.subplots(3, 4, figsize=(40, 15), sharex=True, sharey=True)
    axes = axes.flatten()
    
    if climate_param == 'total_precipitation_sum':
        for i in range(1, 13):
            month_data = df_combined[df_combined['month'] == i]
            ax = axes[i-1]
            sns.barplot(x='dt', y='forest_mean', hue = 'forest_type', data=month_data, ax=ax)
            ax.set_title(f'Month {i}')
            ax.set_xlabel('Date')
            ax.set_ylabel('Temperature')
    else:
        # Plot time series for each month
        for i in range(1, 13):
            month_data = df_combined[df_combined['month'] == i]
            ax = axes[i-1]
            sns.lineplot(x='dt', y='forest_mean', hue = 'forest_type', data=month_data, ax=ax)
            ax.set_title(f'Month {i}')
            ax.set_xlabel('Date')
            ax.set_ylabel('Temperature')
    
    # Adjust layout
    plt.tight_layout()
    plt.savefig(climate_param + "_timeseriesv2.png")
    plt.show()

def get_trend_mann_kendall(data):
    arr=np.array(data['roll_mean'].fillna(method='bfill').fillna(method='ffill'))
    rows=np.arange(len(data))
    manken=mk.original_test(arr,alpha=0.05)
    trend=manken.trend
    slope=manken.slope
    interc=manken.intercept
    if trend != 'no trend':
        data['lin_tr']=interc+slope*rows
        yr_tr=round(slope*120,4) # makes sense
    else:
        data['lin_tr']=np.nan
        yr_tr='n.s.'
        
    return data, yr_tr
#%% Anomalies TS

def calculate_anomalies(df_combined):
    # Calculate the mean temperature for each month across all years and forest types
    monthly_means = df_combined.groupby(['month'])['forest_mean'].mean().reset_index()
    monthly_means = monthly_means.rename(columns={'forest_mean': 'monthly_mean'})
    
    # Merge the monthly means with the original dataframe
    df_combined = pd.merge(df_combined, monthly_means, on=['month'])
    
    # Calculate anomalies
    df_combined['anomaly'] = df_combined['forest_mean'] - df_combined['monthly_mean']
    return df_combined

# print(df_combined.head())  # Check the resulting dataframe

# plot by forest type
forest_types = ['Coniferous', 'Broad-leaved', 'Mixed', 'allForest']
def plot_anomalies_by_forest_type(df_combined, climate_param):
    
    # units
    unit_dic = {'temperature_2m': '°C',
                'total_precipitation_sum': 'm',
                'total_evaporation_sum': 'm'}
    
    fig, axes = plt.subplots(2,2, figsize = (17,14), sharex = True, sharey = False)
    axes = axes.flatten()
    for i in range(4):
        forest_type_df = df_combined[df_combined['forest_type'] == forest_types[i]]
        forest_type_df = forest_type_df.sort_values(by = ['dt'])
        forest_type_df['roll_mean'] = forest_type_df['anomaly'].rolling(12,min_periods=1).mean()
        forest_type_df, yr_tr = get_trend_mann_kendall(forest_type_df)

        ax = axes[i-1]
        fig.set_figheight(10)
        fig.set_figwidth(30)
        
        data_pos=forest_type_df[forest_type_df['anomaly']>0]
        data_neg=forest_type_df[forest_type_df['anomaly']<0]    
        
        ax.plot(forest_type_df.dt, forest_type_df.roll_mean, color = 'black', label = '1-year moving mean')
        ax.plot(forest_type_df.dt, forest_type_df.lin_tr, color = 'black', linestyle = '--', label = 'Linear Trend')
        
        ax.bar(data_pos.dt,data_pos['anomaly'],width=30,color='red',label='Anomalies (positive)')
        ax.bar(data_neg.dt,data_neg['anomaly'],width=30,color='blue',label='Anomalies (negative)')
        
        ax.text(forest_type_df.dt.iloc[10], max(forest_type_df.anomaly), climate_param + ' ('+str(round(yr_tr,4))+unit_dic[climate_param] + '/decade)', fontsize = 10) # decode unit somewhere
        ax.set_title('Anomalies in ' + forest_types[i] + ' forest')
        ax.set_ylabel(climate_param + ' Anomalies')
        ax.grid()
        # sns.barplot(x='dt', y='anomaly', data = forest_type_df, palette = colors)
    
    fig.savefig(climate_param + "_anomaly_timeseriesv2.png") 
    return forest_type_df

def plot_anomalies(df, climate_param):
   # units
   unit_dic = {'temperature_2m': ['K', 'ERA5 Temperature', -5, 5],
               'total_precipitation_sum': ['mm', 'ERA5 Precipitation'],
               'total_evaporation_sum': ['mm', 'ERA5 Evaporation', -9, 9]}
   
   fig, ax = plt.subplots(figsize = (17,14))
   df = df.sort_values(by = ['dt'])
   df['roll_mean'] = df['anomaly'].rolling(12,min_periods=1).mean()
   df, yr_tr = get_trend_mann_kendall(df)

   fig.set_figheight(10)
   fig.set_figwidth(30)
   
   data_pos=df[df['anomaly']>0]
   data_neg=df[df['anomaly']<0]    
   
   fontsize = 40
   
   ax.plot(df.dt, df.roll_mean, color = 'black', label = '1-year moving mean')
   ax.plot(df.dt, df.lin_tr, color = 'black', linestyle = '--', label = 'Linear Trend')
   ax.set_ylim(unit_dic[climate_param][2], unit_dic[climate_param][3])
   ax.set_xlim(datetime(1950,1,1),datetime(2024,5,1))
   ax.bar(data_pos.dt,data_pos['anomaly'],width=30,color='red',alpha = 0.6, label='Anomalies (positive)')
   ax.bar(data_neg.dt,data_neg['anomaly'],width=30,color='blue', alpha = 0.6, label='Anomalies (negative)')
   
   ax.text(df.dt.iloc[10], unit_dic[climate_param][3]-2,'Linear Trend = '+str(round(yr_tr,3))+ ' ' + unit_dic[climate_param][0] + '/decade', fontsize = fontsize-5) # decode unit somewhere
   ax.tick_params(labelsize=fontsize-10)
   ax.set_title(unit_dic[climate_param][1] + ' Anomalies in Bavarian forests', fontsize = fontsize)
   ax.set_ylabel(unit_dic[climate_param][1] + ' Anomalies ' + '[' + unit_dic[climate_param][0] + ']', fontsize = fontsize-10)
   ax.grid()
   ax.legend(loc = 'lower left', markerscale = 1.2, fontsize = fontsize-10)
   fig.savefig("Plots/" + climate_param + "_anomaly_timeseries.png") 
   return df

   
#%% Anomaly time series per Growing Area

bay_wald = pd.read_csv("GEE_out/tables/Mean_temperature_2m_allForest.csv")
anoms = make_date_column(bay_wald)
anoms = calculate_anomalies(anoms)

plot_anomalies(anoms, "temperature_2m")

# Evaporation
bay_wald = pd.read_csv("GEE_out/tables/Mean_total_evaporation_sum_allForest-v2.csv")
ET_anoms = make_date_column(bay_wald)
ET_anoms['forest_mean'] = ET_anoms["forest_mean"] * 1000
ET_anoms = calculate_anomalies(ET_anoms)

plot_anomalies(ET_anoms, "total_evaporation_sum")

#%% Water Deficit TS

prec = pd.read_csv("GEE_out/tables/Mean_total_precipitation_sum_Munich.csv")
et = pd.read_csv("GEE_out/tables/Mean_total_evaporation_sum_Munich.csv")

prec['dif'] = prec.forest_mean + et.forest_mean
prec['roll_mean'] = prec['dif'].rolling(12,min_periods=1).mean()
df = make_date_column(prec)

df, yr_tr = get_trend_mann_kendall(df)
fig, ax = plt.subplots()

fig.set_figheight(10)
fig.set_figwidth(30)

data_pos=df[df['dif']>0]
data_neg=df[df['dif']<0]    

fontsize = 40

ax.plot(df.dt, df.roll_mean, color = 'black', label = '1-year moving mean')
ax.plot(df.dt, df.lin_tr, color = 'black', linestyle = '--', label = 'Linear Trend')
#ax.set_ylim(unit_dic[climate_param][2], unit_dic[climate_param][3])
ax.set_xlim(datetime(1950,1,1),datetime(2024,5,1))
ax.bar(data_pos.dt,data_pos['dif'],width=30,color='red',alpha = 0.6, label='Anomalies (positive)')
ax.bar(data_neg.dt,data_neg['dif'],width=30,color='blue', alpha = 0.6, label='Anomalies (negative)')

#ax.text(df.dt.iloc[10], max(df.anomaly), climate_param + ' ('+str(round(yr_tr,4))+unit_dic[climate_param] + '/decade)', fontsize = 10) # decode unit somewhere
ax.tick_params(labelsize=fontsize-10)
#ax.set_title(unit_dic[climate_param][1] + ' Anomalies in Bavarian forests', fontsize = fontsize)
#ax.set_ylabel(unit_dic[climate_param][1] + ' Anomalies ' + '[' + unit_dic[climate_param][0] + ']', fontsize = fontsize-10)
ax.grid()
ax.legend(loc = 'lower left', markerscale = 1.2, fontsize = fontsize-10)

#%% Iterate through climate parameters
#climate_param = 'total_precipitation_sum'
#df_combined = combine_era5_ts('total_precipitation_sum')
#plt_era5_ts(df_combined, 'total_precipitation_sum')


for param in lst:
    df_combined = combine_era5_ts(param)
    df_combined = calculate_anomalies(df_combined)
    forest_type_df = plot_anomalies_by_forest_type(df_combined, param)
    
#%% Anomalies TS from gee

forest_anoms = make_date_column(forest_anoms)

fig, ax = plt.subplots()
fig.set_figheight(10)
fig.set_figwidth(30)

data_pos = forest_anoms[forest_anoms['forest_mean_anomaly'] > 0]
data_neg = forest_anoms[forest_anoms['forest_mean_anomaly'] < 0]

ax.bar(data_pos['dt'], data_pos['forest_mean_anomaly'], width = 30, color = 'red')
ax.bar(data_neg['dt'], data_neg['forest_mean_anomaly'], width = 30, color = 'blue')