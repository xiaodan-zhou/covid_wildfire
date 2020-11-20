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

############################################################################
add.smoke = function(dff, value="pm25", group="FIPS", lag=1, pm.threshold=20) {
  if (length(lag) > 1) stop("add.smoke only works for 1 lag")
  
  unique.groups = unique(as.list(dff[group])[[1]])
  ndays = dim(dff)[1] / length(unique.groups)
  
  ### transform matrix
  tx1 = diag(x = 1, nrow=ndays, ncol=ndays, names = TRUE)
  for (i in 1:(ndays-1)) tx1[i, i+1] = 1
  
  tx2 = diag(x = 1, nrow=ndays, ncol=ndays, names = TRUE)
  for (i in 2:ndays) tx2[i, i-1] = 1
  
  ### get smoke day 
  dff["fireday"] = NA
  for (ig in unique.groups) {
    values = (dff[dff[group] == ig, ][value] >= pm.threshold) * 1
    values[is.na(values)] = 0
    w1 = t(t(values) %*% tx1 >= 2) * 1
    w2 = t(t(values) %*% tx2 >= 2) * 1
    dff["fireday"][dff[group] == ig] = (w1|w2) * 1
  }
  
  ### get lagged smoke day
  new.var = ifelse(lag == 0, "fireday", paste0("fireday", ".l", lag))
  dff = dff %>%
    dplyr::group_by(.dots = group) %>%
    dplyr::mutate(!!new.var := dplyr::lag(fireday, n = lag, default = NA))
  dff = data.frame(dff)

  return(list(dff[new.var], new.var))
}

# add.smoke = function(dff, value="pm25", group="FIPS", lag=1, pm.threshold=20) {
#   
#   if (length(lag) > 1) stop("add.smoke only works for 1 lag")
#   if (lag == 0) {lags = c(0, 1)} else {lags = c(lag-1, lag, lag+1)}
#   
#   lag.names = c()
#   for (i in lags) {
#     new.var = ifelse(i == 0, value, paste0(value, ".l", i))
#     lag.names = c(lag.names, new.var)
#     dff = dff %>% 
#       dplyr::group_by(.dots = group) %>% 
#       dplyr::mutate(!!new.var := dplyr::lag(!!as.name(value), n = i, default = NA))
#     dff = data.frame(dff)
#   }
#     # print(dff[new.var])
#     # dff[new.var] = (dff[new.var] >= pm.threshold) * 1
#     # print(dff[new.var])
#   # print(pm.threshold)
#   
#   # print(dff[lag.names])
#   fireday = rowSums(dff[lag.names] == 1, na.rm=T)
#   fireday = as.factor(as.character((fireday >= 2)*1))
#   # print(fireday)
#   return(list(dff[lag.names], fireday))
# 
# }
