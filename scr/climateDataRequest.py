## Python Version 3.5

## This script collect climate date through NOAA API.
## Sep 23, 2020
## NOAA API - https://www.ncdc.noaa.gov/cdo-web/webservices/v2
## climate station list - https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt

import os
import json
import requests
import pandas as pd
import datetime
from datetime import date, timedelta


dir = "/Users/mac/Desktop/PM2.5/wildfire"

## https://www.ncdc.noaa.gov/cdo-web/token
token = 'ZCIEEfvNRZBzqDaMaBSaCIAlUPskMnlQ'


## print all dataests available in NOAA
datasets = requests.get('https://www.ncdc.noaa.gov/cdo-web/api/v2/datasets', 
                 headers={'token': token})
d = json.loads(datasets.text)
print(d)


## collect temperature data in CA, WA and OR during 2020 Jan - 2020 Sep
states = {'CA': 'FIPS:06', 'WA': 'FIPS:53', 'OR': 'FIPS:41'}
variable = "TAVG"

## set up time range for each url requests
intervals = 1
ranges = range(258)
starts = [date(2020, 1, 1)]
ends = [date(2020, 1, intervals)]

for ii in ranges:
    starts.append(starts[-1]+timedelta(days=intervals))
    ends.append(ends[-1]+timedelta(days=intervals))
print(starts, '\n\n', ends)


## url requests
for state_abbr, state_fips in states.items():
    df = [] 

    for ii in ranges: 
    
        print("processing  ", state_fips, starts[ii], ends[ii])

        str0 = '&locationid=' + state_fips + '&startdate=' + str(starts[ii]) + '&enddate=' + str(ends[ii]) + '&datatypeid=' + variable

        r = requests.get('https://www.ncdc.noaa.gov/cdo-web/api/v2/data?datasetid=GHCND&limit=1000' + str0,
                         headers={'token': token})
        
        djson = json.loads(r.text)
        df_temp = pd.DataFrame(djson['results'])
        
        expect_count = djson['metadata']['resultset']['count']
        if expect_count > 1000:
            print("more than 1000 records, missing happens. ", expect_count)
        else:
            print(expect_count)

        if isinstance(df, pd.DataFrame): 
            df = pd.concat((df, df_temp))
        else:
            df = df_temp 
    
    df.to_csv(dir +'/data/' + state_fips + '_TAVG.csv')


## collect climate station info
stations = pd.read_csv(dir + "/data/ghcnd-stations.csv", header=None)
stations.columns = ["station_ID", "lat", "long", "elevation", "location", "flag", "FIPS"]
stations.head()


## merge climate data with climate station info
merged = None
for ifile in ["CA_TAVG", "WA_TAVG", "OR_TAVG"]:
    df = pd.read_csv(dir + "/data/" + ifile + ".csv")
    df['station_ID'] = df['station'].str.split(pat=":", expand=True)[1]
    df['date'] = [datetime.strptime(dd, "%Y-%m-%dT%H:%M:%S") for dd in df['date']]
    df['value'] = df['value']/10.0 # celcius
    df.drop(columns=['station', 'attributes'], inplace=True)
    df['state'] = ifile[:2]
    
    if isinstance(merged, pd.DataFrame):       
        merged_temp = df.merge(stations, how='inner', on = 'station_ID')
        merged = pd.concat((merged, merged_temp))
    else:
        merged = df.merge(stations, how='inner', on = 'station_ID')

merged.to_csv(dir + "/data/TAVG.csv")



