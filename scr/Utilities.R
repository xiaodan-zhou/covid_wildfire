library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(pracma)
library(splines)
library(stats)
library(meta)


############################################################################
load.data = function() {
  setwd("/Users/mac/Documents/GitHub/covid_wildfire")
  in.path = "data/moddat_Feb2021.csv"
  df = read.csv(in.path)
  
  df$date_str = ymd(df$date_str)
  df$date = ymd(df$date)
  df$dayofweek = as.factor(df$dayofweek)
  df$FIPS = as.factor(as.character(df$FIPS))
  df$state = as.factor(df$state)
  df = arrange(df, date)
  return(df)
}


############################################################################
create.lag.value = function(dff, value="pm25", group="FIPS", lags=1) {
  ### return all lagged 'value' as listed in 'lags', after grouping value by 'group'
  ### assumes df is ordered in time!!! 
  ### dplyr version 0.8.5
  ### output name pm25, pm25.l1, pm25.l2
  lag.names = c()
  for (i in lags) {
    new.var = paste0(".l", i)
    lag.names = c(lag.names, new.var)
    dff = dff %>% 
      dplyr::group_by(.dots = group) %>% 
      dplyr::mutate(!!new.var := dplyr::lag(!!as.name(value), n = i, default = NA))
    dff = data.frame(dff)
  }
  return(dff[lag.names])

}

############################################################################
trans.coef = function(ls, pm.delta = 10) {
  return((exp(ls * pm.delta) - 1) * 100)
}

############################################################################
inverse.delta = function(ls, pm.delta = 10) {
  return(log(ls/100+1)/pm.delta)
}

############################################################################
list.append = function(ls, element) {
  i = length(ls)
  ls[[i+1]] = element
  return(ls)
}

############################################################################
my.acf = function(ls, lag.max=21) {
  acf.value = acf(ls, lag.max=lag.max, na.action = na.pass, plot=F)
  acf.df = data.frame(lag = acf.value$lag, value = acf.value$acf)
  return(acf.df)
}