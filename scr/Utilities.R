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
load.data.xz1 = function() {
  
  ### read data 
  setwd("/Users/mac/Documents/GitHub/covid_wildfire")
  in.path = "data/moddat_xz1_rerun.csv"
  df = read.csv(in.path)
  
  print(paste(dim(df)[1], "records in the dataset"))
   
  df$date_str = ymd(df$date_str)
  df$date = ymd(df$date)
  
  ## make variables categorical
  df$dayofweek = as.factor(df$dayofweek)
  df$FIPS = as.factor(as.character(df$FIPS))

  df = arrange(df, date)

  ### fire day should shift with lag, no need to do it here  
  return(df)
}


############################################################################
add.lag = function(dff, value="pm25", group="FIPS", lags=1) {
  ### return all lagged 'value' as listed in 'lags', after grouping value by 'group'
  ### assumes df is ordered in time!!! 
  ### dplyr version 0.8.5
  ### output name pm25, pm25.l1, pm25.l2
  lag.names = c()
  
  for (i in lags) {
    new.var = ifelse(i == 0, value, paste0(value, ".l", i))
    lag.names = c(lag.names, new.var)
    dff = dff %>% 
      dplyr::group_by(.dots = group) %>% 
      dplyr::mutate(!!new.var := dplyr::lag(!!as.name(value), n = i, default = NA))
    dff = data.frame(dff)
  }
  return(list(dff[lag.names], lag.names))

}


