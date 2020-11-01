setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
df = load.data()

dt = data.frame(date = rep(df$date_str[1:20], 2), 
                value = rep(1:20, 2), 
                value2 = rep((1:20)^2, 2), 
                group = rep(c("A", "B"), each=20))

onebasis(1:5, "poly", degree=3)

lag_pollutant = crossbasis(dt$value, lag=c(0, 3), 
                           # argvar=list(fun="lin", cen=FALSE), 
                           # arglag=list(fun="integer"),
                           group=df$group)   
lag_pollutant


lag_pollutant = crossbasis(dt$value2, lag=c(0, 3), 
                           argvar=list(fun="lin", cen=FALSE), 
                           arglag=list(fun="integer"),
                           group=df$group)   



fit = glm(data=df, "cases ~ bs(date, 3) + lag(pm25, 4)", family=quasipoisson())
length(fit$fitted.values)
dim(df)


lag_pollutant = lag(df$pm25, order_by=df$FIPS, 10)







