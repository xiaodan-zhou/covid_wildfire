library(lubridate)

############################################################################
load.data.mc2 = function() {
  
  ### read data 
  setwd("/Users/mac/Documents/GitHub/covid_wildfire")
  in.path = "data/moddat_mc2.csv"
  df = read.csv(in.path)
  
  print(paste(dim(df)[1], "records in the dataset"))
  
  df$date_str = ymd(df$date)
  df$date = as.integer(df$date_str - min(df$date_str))

  ## make variables categorical
  df$dayofweek = as.factor(df$dayofweek)
  df$FIPS = as.factor(df$FIPS)
  
  ### replace negative number of daily cases with zero 
  if (sum(df$cases < 0) > 0) {
    print(paste(sum(df$cases < 0), "records have negative daily cases"))
    df$cases[df$cases < 0] = 0
  }
  
  ### replace negative number of daily deaths with zero 
  if (sum(df$deaths < 0) > 0) {
    print(paste(sum(df$deaths < 0), "records have negative daily deaths"))
    df$deaths[df$deaths < 0] = 0
  }
  
  # df$date = df$date_num
  # if (sum(is.na(df$date)) > 0) {
  #   df = df[!is.na(df$date), ]
  # }
  # 
  # if (!keep.missing) {
  #   ### remove data without pm record 
  #   if (sum(is.na(df$pm25)) > 0) {
  #     print(paste(sum(is.na(df$pm25)), "records have pm2.5 missing"))
  #     df = df[!is.na(df$pm25),]
  #   }
  #   
  #   ### remove data without population record
  #   if (sum(is.na(df$population)) > 0) {
  #     print(paste(sum(is.na(df$population)), "records have population missing"))
  #     df = df[!is.na(df$population),]
  #   }
  #   
  #   ### remove data without climate record
  #   if (sum(is.na(df$tmmx)) > 0) {
  #     print(paste(sum(is.na(df$tmmx)), "records have temperature missing"))
  #     df = df[!is.na(df$tmmx),]
  #   }
  #   
  #   if (sum(is.na(df$rmax)) > 0) {
  #     print(paste(sum(is.na(df$rmax)), "records have humidity missing"))
  #     df = df[!is.na(df$rmax),]
  #   }
  # }
  # 
  ### todo remove FIPS with fewer than 10 records 
  # min(data.frame(df %>% group_by(FIPS) %>% count())$n)
  
  return(df)
}




load.data.xz1 = function() {
  
  ### read data 
  setwd("/Users/mac/Documents/GitHub/covid_wildfire")
  in.path = "data/moddat_xz1.csv"
  df = read.csv(in.path)
  
  print(paste(dim(df)[1], "records in the dataset"))
   
  df$date_str = ymd(df$date_str)
  df$date = as.integer(df$date_str - min(df$date_str))

  ## make variables categorical
  df$dayofweek = as.factor(df$dayofweek)
  df$FIPS = as.factor(df$FIPS)
  
  ### replace negative number of daily cases with zero 
  if (sum(df$cases < 0) > 0) {
    print(paste(sum(df$cases < 0), "records have negative daily cases"))
    df$cases[df$cases < 0] = 0
  }
  
  ### replace negative number of daily deaths with zero 
  if (sum(df$deaths < 0) > 0) {
    print(paste(sum(df$deaths < 0), "records have negative daily deaths"))
    df$deaths[df$deaths < 0] = 0
  }
  
  # df$date = df$date_num
  # if (sum(is.na(df$date)) > 0) {
  #   df = df[!is.na(df$date), ]
  # }
  # 
  # if (!keep.missing) {
  #   ### remove data without pm record 
  #   if (sum(is.na(df$pm25)) > 0) {
  #     print(paste(sum(is.na(df$pm25)), "records have pm2.5 missing"))
  #     df = df[!is.na(df$pm25),]
  #   }
  #   
  #   ### remove data without population record
  #   if (sum(is.na(df$population)) > 0) {
  #     print(paste(sum(is.na(df$population)), "records have population missing"))
  #     df = df[!is.na(df$population),]
  #   }
  #   
  #   ### remove data without climate record
  #   if (sum(is.na(df$tmmx)) > 0) {
  #     print(paste(sum(is.na(df$tmmx)), "records have temperature missing"))
  #     df = df[!is.na(df$tmmx),]
  #   }
  #   
  #   if (sum(is.na(df$rmax)) > 0) {
  #     print(paste(sum(is.na(df$rmax)), "records have humidity missing"))
  #     df = df[!is.na(df$rmax),]
  #   }
  # }
  
  ### todo remove FIPS with fewer than 10 records 
  # min(data.frame(df %>% group_by(FIPS) %>% count())$n)
  
  return(df)
}


############################################################################
library(dplyr)

add.lag = function(dff, value, group, lags) {
  ### return all lagged 'value' as listed in 'lags', after grouping value by 'group'
  ### assumes df is ordered in time 
  ### dplyr version 0.8.5
  lag.names = c()
  
  for (i in lags) {
    new.var = ifelse(i == 0, value, paste0(value, ".l", i))
    lag.names = c(lag.names, new.var)
    dff = dff %>% 
      group_by(.dots = group) %>% 
      mutate(!!new.var := dplyr::lag(!!as.name(value), n = i, default = NA))
    dff = data.frame(dff)
  }
  return(list(dff[lag.names], lag.names))

}
