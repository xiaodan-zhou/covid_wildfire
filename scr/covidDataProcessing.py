import pandas as pd
import numpy as np
import os

# url = 'https://github.com/mcooper/fires-covid/blob/master/data/moddat.csv'
# dir = "/Users/mac/Desktop/PM2.5/wildfire"

dir = os.getcwd()
df = pd.read_csv(dir + "/data/moddat.csv", sep=",")
df.sort_values(["countyFIPS", "date"], inplace=True)

df['daily_cases'] = None
df['daily_deaths'] = None

df['daily_case_rate'] = None
df['daily_death_rate'] = None

df['date_shifted_1case'] = None
df['date_shifted_100case'] = None
df['date_shifted_10kth_rate'] = None

for icounty in df.countyFIPS.unique():
    n_days = df[df.countyFIPS == icounty].shape[0]

    ## create the daily data
    df.loc[df.countyFIPS == icounty, 'daily_cases'] = [0] + np.diff(df[df.countyFIPS == icounty].cases).tolist()
    df.loc[df.countyFIPS == icounty, 'daily_deaths'] = [0] + np.diff(df[df.countyFIPS == icounty].deaths).tolist()
    df.loc[df.countyFIPS == icounty, 'daily_case_rate'] = [0] + np.diff(df[df.countyFIPS == icounty].case_rate).tolist()
    df.loc[df.countyFIPS == icounty, 'daily_death_rate'] = [0] + np.diff(df[df.countyFIPS == icounty].death_rate).tolist()

    ## find the date when the county reaches 100 cumulative cases
    day0 = np.where(df.loc[df.countyFIPS == icounty, 'cases'] >= 100)[0].tolist()
    if day0:
        df.loc[df.countyFIPS == icounty, 'date_shifted_100case'] = np.arange(n_days) - day0[0]

    ## find the date when the county get first cases
    day0 = np.where(df.loc[df.countyFIPS == icounty, 'cases'] >= 1)[0].tolist()
    if day0:
        df.loc[df.countyFIPS == icounty, 'date_shifted_1case'] = np.arange(n_days) - day0[0]

    ## find the date when the county reaches 0.0001 cumulative case rate
    day1 = np.where(df.loc[df.countyFIPS == icounty, 'case_rate'] >= 0.0001)[0].tolist()
    if day1:
        df.loc[df.countyFIPS == icounty, 'date_shifted_10kth_rate'] = np.arange(n_days) - day1[0]

df['date'] = pd.to_datetime(df.date)  
df['dayofweek'] = df['date'].dt.dayofweek
df.loc[:, "dayofweek_str"] = df.dayofweek.replace({0: "Mo", 1: "Tu", 2: "We", 3: "Th", 4: "Fr", 5: "Sa", 6: "Su"})

df.to_csv(dir + "/data/moddat2.csv")
