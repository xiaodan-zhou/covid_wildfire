setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
source("scr/GlobalModel.R")
dff = load.data.xz1()

pm.threshold = 20

dpm = read.csv("GlobalModel/*lag0to28.cases.df522.csv")
dsmoke = read.csv("GlobalModel/*lag0to28.cases.smokeday.df522(pm20).pdf")

 
# need to be lagged dff$fireday = (dff$pm25>pm.threshold)*1
tb1 = data.frame(dff %>% group_by(fireday) %>% summarise(cases=mean(cases,na.rm=T), pm25=mean(pm25,na.rm=T)))
tb1$pm25[1]

# pm.threshold = 20
# fireday cases   pm25
# <dbl> <dbl>  <dbl>
# 1       0 45.2    5.55
# 2       1 50.4   83.5 
# pm.threshold = 22
# fireday cases   pm25
# <dbl> <dbl>  <dbl>
# 1       0 45.4    5.63
# 2       1 48.0   87.9 


exp(0.003 * 83.5) / exp(0.003 * 5.55) - 1 # 0.263455
exp(0.002 * 83.5) / exp(0.002 * 5.55) - 1 # 0.1687093
exp(0.0014 * 83.5) / exp(0.0014 * 5.55) - 1 # 0.1153073

50.4/45.2 - 1 # 0.1150442