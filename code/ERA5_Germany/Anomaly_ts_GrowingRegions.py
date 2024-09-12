# -*- coding: utf-8 -*-
"""
Created on Wed Jul 31 13:13:45 2024

@author: laura
"""

# ERA5 Germany


import os
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from datetime import datetime
import seaborn as sns
import pymannkendall as mk

os.chdir("E:/EAGLE\\Forest_Project\\")


def make_date_column(df):
    dt = [datetime.strptime(str(df.year[i]) + '-' + str(df.month[i]).zfill(2)+ '-' + '01', "%Y-%m-%d") for i in range(len(df))]
    df['dt'] = dt

    return df

lst = ['temperature_2m', 'total_precipitation_sum', 'total_evaporation_sum']

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
   #ax.set_title(unit_dic[climate_param][1] + ' Anomalies in ' + np.unique(df.region)[0], fontsize = fontsize)
   ax.set_ylabel(unit_dic[climate_param][1] + ' Anomalies ' + '[' + unit_dic[climate_param][0] + ']', fontsize = fontsize-10)
   ax.grid()
   ax.legend(loc = 'lower left', markerscale = 1.2, fontsize = fontsize-10)
   fig.savefig("Maps/Timeseries/" + climate_param + "_anomaly_timeseries.png") 
   return df

#%% Plot Time Series
ET_ts = pd.read_csv("Data/tables/Mean_forest_total_evaporation_sum.csv")
T_ts = pd.read_csv("Data/tables/Mean_forest_temperature_2m.csv")

Temp_ts = make_date_column(T_ts)
Temp_ts = Temp_ts[['forest_mean', 'year', 'month', 'dt']]
Temp_ts = calculate_anomalies(Temp_ts)
plot_anomalies(Temp_ts, 'temperature_2m')

ET_ts = make_date_column(ET_ts)
ET_ts['forest_mean'] = ET_ts['forest_mean']*1000
ET_ts = calculate_anomalies(ET_ts)
plot_anomalies(ET_ts, 'total_evaporation_sum')
