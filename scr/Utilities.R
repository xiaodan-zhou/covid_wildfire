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

  ## create state 
  df$state = round(as.numeric(as.character(df$FIPS))/1000, 0)
  df$state[df$state == 6] = "CA"
  df$state[df$state == 53] = "WA"
  df$state[df$state == 41] = "OR"
  df$state = as.factor(df$state)

  ## merge with hazard data 0=nosmoke, 5=(0,10)light, 16=(11-20)medium, 27=(21,32)heavy
  hms = read.csv("data/HMS_county_2020.csv")
  hms = tidyr::gather(data=hms, key="date", value="hazardmap", -"County", -"GEOID")
  hms$date = mdy("01-01-2020") + (as.numeric(substr(hms$date, 2, 5)) - 1)
  hms$hazardmap[is.na(hms$hazardmap)] = 0
  hms$GEOID = as.factor(as.character(hms$GEOID))
  df = merge(df, hms, by.x=c("date", "FIPS"), by.y=c("date", "GEOID"), all.x=T)

  ## create the pm2.5 baseline and hazardline according to hazardmap 
  df$pmbase = NA
  df$pmhazard = NA
  for (ifips in unique(df$FIPS)) {
    irow = which(df$FIPS == ifips)
    pm.splitted = split.pm(df$pm25[irow], df$hazardmap[irow])
    df$pmbase[irow] = pm.splitted[[1]]
    df$pmhazard[irow] = pm.splitted[[2]]
  }
  
  ## add mobility data 
  mb = read.csv("data/dataverse_Dec2/combined_percent_change_from_baseline_CO_westcoast.csv")
  mb$date = ymd(mb$date)
  mb$GEOID = as.factor(mb$GEOID)
  df = merge(df, mb, by.x=c("date", "FIPS"), by.y=c("date", "GEOID"), all.x=T)
  
  rm(hms, mb)
  
  ### fire day should shift with lag, no need to do it here  
  return(df)
}


############################################################################
# split the pm25 into pmbase and pmhazard according to the hazard map data 
split.pm = function(pm25, hazardmap) {
  ihazard = which(hazardmap >= 27 & !is.na(pm25))
  inothazard = which(hazardmap < 27 & !is.na(pm25))
  ina = which(is.na(pm25))

  pmbase = rep(NA,length(pm25))
  pmhazard = rep(NA,length(pm25))
  
  # if no hazard pm25 value, keep all as pmbase 
  if (length(ihazard) == 0) {
    return(list(pm25, pmhazard))
  }
  # if no non-hazard pm25 value, keep all as pmhazard 
  if (length(inothazard) == 0) {
    return(list(pmbase, pm25))
  }  

  # force split 
  pmbase[inothazard] = pm25[inothazard]
  pmhazard[ihazard] = pm25[ihazard]
  base.mean = mean(pmbase, na.rm=T)
  
  # remove base in hazard day 
  pmbase[ihazard] = base.mean
  pmhazard[ihazard] = pmhazard[ihazard] - pmbase[ihazard]
  pmhazard[inothazard] = 0
  
  # treat aloft record 
  ialoft = which(pmhazard < 0)
  if (any(ialoft)) {
    pmbase[ialoft] = pmbase[ialoft] + pmhazard[ialoft]
    pmhazard[ialoft] = 0 
  }
  
  return(list(pmbase, pmhazard))
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