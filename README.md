# Excess of COVID-19 Cases and Deaths due to PM2.5 Exposure During 2020 Wildfires in the U.S.
This is the repository for public available code and data to reproduce our analyses.

#### Key Results
![image1](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/output/pct_increases.png)
Posterior distribution of percentage increase in COVID-19 cases (left) and COVID-19 deaths (right) associated with a daily increase of 10μg/m3 in PM2.5 separately for lag 0 up to lag 14, averaged across 92 counties in California, Oregan and Washington. 

#### Data
We acquired and linked publicly available daily data on PM2.5, COVID-19 cases and deaths, and other confounding factors for 92 western U.S. counties that were affected by the 2020 wildfires. All data for analyses were cleaned and stored in [/data/moddat_Feb2021.csv](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/data/moddat_Feb2021.csv). 

### moddat_Feb2021.csv
pm25_history  the median average of PM2.5 during 2016-2019
pm25_raw      the PM2.5 for 2020 without filling missing 
pm25          the missing values in pm25_raw been filled using pm25_history
wildfire      the boolean index of wildfire days, according to the smoke phenomenon (hazardmap)
pm_wildfire   the wildfire contributed PM2.5, estimtated as (pm25 - pm25_history) during wildfire days; in non-wildfire days, the value is 0.
pm_ambient    the background PM2.5 (pm25 = pm_wildfire + pm_ambient)


#### Code
coming soon 
