setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
source("scr/GlobalModel.R")
dff = load.data.xz1()
df.date=8
df.tmmx=3
df.rmax=3
lags=0
smooth = "ns"
cause = "cases"
pollutant="pm25"
group="FIPS"
control=glm.control(epsilon = 1e-10, maxit = 10000)
gm = global.model(dff=dff, smooth = smooth, df.date = df.date,
             df.tmmx = df.tmmx, df.rmax = df.rmax, lags = lags, cause = cause)

# ???????????? dff=dff ??????????????????bug df.name = as.name(substitute(dff))
glm("cases ~ ns(date, 4) + ns(tmmx, 1) + ns(rmax, 1) + dayofweek + log(population) + lag.data",
    family = quasipoisson, data = dff,
    control = control, na.action = na.exclude)

# setwd("/Users/mac/Documents/GitHub/covid_wildfire")
# source("scr/Utilities.R")
# # source("scr/GlobalModelInteract.R")
# dff = load.data.xz1()
# df.date=5
# df.tmmx=2
# df.rmax=2
# lags=1
# smooth = "ns"
# cause = "cases"
# pollutant="pm25"
# group="FIPS"
# control=glm.control(epsilon = 1e-10, maxit = 10000)
# 
# # stupid way 
# sum(dff$pm25,na.rm=T)
# dff$pm25 = as.matrix(add.lag(dff, lags=lags)[[1]])
# 
# # this order is so important!!!!!!
# ### fire day setup 
# dff$fireday = 0
# dff$fireday[dff$pm25 >= 20] = 1 # define those pm>=20 as smoke day
# # dff$fireday = as.factor(dff$fireday)
# 
# fit = glm(formula = deaths ~ ns(date_num, df.date) + ns(tmmx, df.tmmx) + ns(rmax, df.rmax) + FIPS + dayofweek + pm25*fireday, # FIPS   
#           family = quasipoisson, data = dff, na.action = na.exclude, 
#           control = glm.control(epsilon = 1e-10, maxit = 10000))
# fit
# print(confint(fit, "pm25"))
# # print(confint(fit, "pm25:fireday"))
# 
# 
# 
# set.seed(1)
# ng = 5
# nrep = 20
# aa = data.frame(FIPS = as.factor(rep(1:ng,each=nrep)))
# aa$POP = rep(1:ng,each=nrep) * 100
# aa$pm = runif(ng*nrep) * 10
# aa$y = aa$POP * aa$POP + aa$POP + aa$pm + runif(ng*nrep) * 3
# 
# lm(data=aa,y~POP+pm)
# lm(data=aa,y~POP+FIPS+pm)
# lm(data=aa,y~FIPS+POP+pm)
