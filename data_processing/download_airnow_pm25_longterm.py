# -*- coding: utf-8 -*-
## Code to interact with the AirNow API and download a csv of all the 
## Monitors We are interested in
##
## Author: Ben Sabath
## Date: 3/31/2020 updated 4/21/2020

## Updated by Matt Cooper
## From https://github.com/NSAPH/data_requests/tree/master/request_projects/may2020_airnow_api_no2

import os
import sys
import time
from datetime import datetime, date, timedelta
from os.path import expanduser
import requests

KEYPATH = '/home/mattcoop/covid_wildfire/data_processing/airnow_key'
WRITEPATH = "/home/mattcoop/covid_wildfire/data_processing/airnow_raw/" 

with open(KEYPATH, 'r') as f:
    key = f.read().strip()

def download_daily_data(data_date):
     
    # Set API options
    ## Only grabs 12 hours at a time to reduce request size
    options = {}
    options["startdate"] = str(data_date.date())  + "t" + str(data_date.hour)	
    options["enddate"] = str((data_date + timedelta(hours = 12)).date()) + "t" + str((data_date + timedelta(hours = 12)).hour)
    ##options["enddate"] = str(datetime.now().date()) + "t00"
    options["parameters"] = "pm25"
    options["bbox"] = "-125,32.447,-114,50"
    options["datatype"] = "c"
    options["format"] = "text/csv"
    options["api_key"] = key
    options["verbose"] = 1
    
    url = "https://www.airnowapi.org/aq/data/"
    
    try:        
        # Request AirNowAPI data
        
        print("Requesting AirNowAPI data... Date = " +  str(data_date))
        # User's home directory
        download_file_name = "AirNowAPI" + datetime.now().strftime("_%Y%M%d%H%M%S." + "csv")
        download_file = download_file_name
        # Perform the AirNow API data request
        response = requests.get(url,  params = options)
        with open(WRITEPATH + str(data_date.date()) + "t" + str(data_date.hour) +"_pm25_monitor.csv", 'wb') as f:
            f.write(response.content)
    
    except Exception as e:
        print("Unable perform AirNowAPI request.", e)
        sys.exit(1)

def main():
    for y in [2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010]:
        start_date = datetime(y, 3, 1)
        date_list = [start_date + timedelta(days=x) for x in range(306)]
        
        for current_date in date_list:
            # record start time to avoid timeout
            request_start = datetime.now()
                    
            download_daily_data(current_date)
            
            # Max of 500 requests per hour, aka 1 every 7.2 seconds
            # code here checks too see how long request took, pauses for the rest of the 7.2 seconds
            elapsed = datetime.now() - request_start
            if elapsed.total_seconds() < 7.2:
                time.sleep(7.2 - elapsed.total_seconds()) # 

if __name__ == "__main__":
    main()
