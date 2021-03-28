# Excess of COVID-19 Cases and Deaths due to PM2.5 Exposure During 2020 Wildfires in the U.S.
This is the repository for public available code and data to reproduce our analyses.

#### Key Results
![image1](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/output/pct_increases.png)
Posterior distribution of percentage increase in COVID-19 cases (left) and COVID-19 deaths (right) associated with a daily increase of 10μg/m3 in PM2.5 separately for lag 0 up to lag 14, averaged across 92 counties in California, Oregan and Washington. 

#### Data
We acquired and linked publicly available daily data on PM2.5, COVID-19 cases and deaths, and other confounding factors for 92 western U.S. counties that were affected by the 2020 wildfires. All data for analyses were cleaned and stored in [/data/moddat_Feb2021.csv](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/data/moddat_Feb2021.csv). 

#### Code
We developed and implemented a Bayesian hierarchical zero-inflated negative-binomial distributed lag (BH-ZINB-DL) model to estimate the association between daily changes in PM2.5 and the percentage increase in the risk of COVID-19 cases and deaths days after exposure. The core model is in the [src/bayes](https://github.com/xiaodan-zhou/covid_wildfire/tree/master/src/bayes) folder. 

[`src/combine.R`](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/src/combine.R) pull data from all sources into [/data/moddat_Feb2021.csv](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/data/moddat_Feb2021.csv). 

[`src/SummarizeData.R`](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/src/SummarizeData.R) create all summary statistics in the main script. 

[`src/Visaulize.R`](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/src/Visaulize.R) create all summary Figures in the main script. 

[`src/Utilities.R`](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/src/Visaulize.R) utility functions to facilite the Bayes model in the [src/bayes](https://github.com/xiaodan-zhou/covid_wildfire/tree/master/src/bayes) folder. 
