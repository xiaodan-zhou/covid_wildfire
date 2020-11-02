setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
dff = load.data.mc2()
df.date=8
df.tmmx=3
df.rmax=3
lags=0
smooth = "ns"
cause = "cases"

gm = global.model(dff,  smooth = smooth, df.date = df.date, 
             df.tmmx = df.tmmx, df.rmax = df.rmax, lags = lags, cause = cause)


# test 
# glm("cases ~ ns(date, 4) + ns(tmmx, 1) + ns(rmax, 1) + dayofweek + log(population) + lag.data",
#     family = quasipoisson, data = dff,
#     control = control, na.action = na.exclude)