import pandas as pd
import numpy as np
import os

##################################### get moddat.csv ###############################
# url = 'https://github.com/mcooper/fires-covid/blob/master/data/moddat.csv'
# dir = "/Users/mac/Desktop/PM2.5/wildfire"

# os.chdir('..')
dir = os.getcwd()
df = pd.read_csv(dir + "/data/moddat_mc1.csv", sep=",")
# df['FIPS'] = df['FIPS'].astype(str)
df['date'] = pd.to_datetime(df['date'])
df.sort_values(["FIPS", "date"], inplace=True)
df.reset_index()

##################################### check ###############################
## all location have the same time period
day_count = df.groupby("FIPS").aggregate({"date": "count"})
assert np.var(np.asarray(day_count)) == 0.0


##################################### create date shift ###############################

df['daily_cases'] = None
df['daily_deaths'] = None

df['daily_case_rate'] = None
df['daily_death_rate'] = None

df['date_shifted_100case'] = None
# df['date_shifted_1case'] = None
# df['date_shifted_10kth_rate'] = None

for icounty in df.FIPS.unique():
    n_days = df[df.FIPS == icounty].shape[0]

    ## create the daily data
    df.loc[df.FIPS == icounty, 'daily_cases'] = [0] + np.diff(df[df.FIPS == icounty].cases).tolist()
    df.loc[df.FIPS == icounty, 'daily_deaths'] = [0] + np.diff(df[df.FIPS == icounty].deaths).tolist()
    df.loc[df.FIPS == icounty, 'daily_case_rate'] = [0] + np.diff(df[df.FIPS == icounty].case_rate).tolist()
    df.loc[df.FIPS == icounty, 'daily_death_rate'] = [0] + np.diff(df[df.FIPS == icounty].death_rate).tolist()

    ## find the date when the county reaches 100 cumulative cases
    day0 = np.where(df.loc[df.FIPS == icounty, 'cases'] >= 100)[0].tolist()
    if day0:
        df.loc[df.FIPS == icounty, 'date_shifted_100case'] = np.arange(n_days) - day0[0]

    ## find the date when the county get first cases
    # day0 = np.where(df.loc[df.FIPS == icounty, 'cases'] >= 1)[0].tolist()
    # if day0:
    #     df.loc[df.FIPS == icounty, 'date_shifted_1case'] = np.arange(n_days) - day0[0]

    ## find the date when the county reaches 0.0001 cumulative case rate
    # day1 = np.where(df.loc[df.FIPS == icounty, 'case_rate'] >= 0.0001)[0].tolist()
    # if day1:
    #     df.loc[df.FIPS == icounty, 'date_shifted_10kth_rate'] = np.arange(n_days) - day1[0]

df['dayofweek'] = df['date'].dt.dayofweek
df.loc[:, "dayofweek_str"] = df.dayofweek.replace({0: "Mo", 1: "Tu", 2: "We", 3: "Th", 4: "Fr", 5: "Sa", 6: "Su"})
df.to_csv(dir + "/data/moddat_mc2.csv")


##################################### merge with climate data ###############################
# https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_TERRACLIMATE#bands
# https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_GRIDMET?hl=es#bands
# Name	Units	Min	Max	Scale	Description
# pr	mm	0*	7245*    Precipitation accumulation
# srad	W/m^2	0*	5477*	Downward surface shortwave radiation
# tmmx	°C	-670*	576*	Maximum temperature
# rmax	%	1.05*	100*    Maximum relative humidity
# sph	kg/kg	0*	0.02*   Specific humidity

pr_ = pd.read_csv(dir + "/data/pr_county_2020.csv", sep=",")
tmmx_ = pd.read_csv(dir + "/data/tmmx_county_2020.csv", sep=",")
srad_ = pd.read_csv(dir + "/data/srad_county_2020.csv", sep=",")
sph_ = pd.read_csv(dir + "/data/sph_county_2020.csv", sep=",")
rmax_ = pd.read_csv(dir + "/data/rmax_county_2020.csv", sep=",")

climate = pr_
for next in [tmmx_, srad_, sph_, rmax_]:
    climate = pd.merge(climate, next, how='inner', on=["date", "fips"])

climate['date'] = pd.to_datetime(climate.date)
df = pd.merge(df, climate, how='left', left_on=["date", "FIPS"], right_on=["date", "fips"])

df.drop(columns=['Unnamed: 0_x', 'Unnamed: 0_y', 'fips'], inplace=True)
df.to_csv(dir + "/data/moddat_mc2.csv")

