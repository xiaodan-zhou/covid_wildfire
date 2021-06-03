# Excess of COVID-19 Cases and Deaths due to PM2.5 Exposure During 2020 Wildfires in the U.S.
This is the repository for public available code and data to reproduce our analyses.

#### Data
We acquired and linked publicly available daily data on PM2.5, COVID-19 cases and deaths, and other confounding factors for 92 western U.S. counties that were affected by the 2020 wildfires. All data for analyses in [/data](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/data) was merged into [/data/moddat_Feb2021.csv](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/data/moddat_Feb2021.csv). 

#### Code
We developed and implemented a Bayesian hierarchical zero-inflated negative-binomial distributed lag (BH-ZINB-DL) model to estimate the association between daily changes in PM2.5 and the percentage increase in the risk of COVID-19 cases and deaths days after exposure. 

[`src/bayes`](https://github.com/xiaodan-zhou/covid_wildfire/tree/master/src/bayes) include the BH-ZINB-DL model. 

[`src/combine.R`](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/src/combine.R) pull data from all sources into [/data/moddat_Feb2021.csv](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/data/moddat_Feb2021.csv). 

[`src/SummarizeData.R`](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/src/SummarizeData.R) create all summary statistics in the main script. 

[`src/Visaulize.R`](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/src/Visaulize.R) create all summary Figures in the main script. 

[`src/Utilities.R`](https://github.com/xiaodan-zhou/covid_wildfire/blob/master/src/Visaulize.R) utility functions to facilite the Bayes model in the [src/bayes](https://github.com/xiaodan-zhou/covid_wildfire/tree/master/src/bayes) folder. 
