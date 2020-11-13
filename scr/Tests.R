# ############################ check trend #########################
# setwd("/Users/mac/Documents/GitHub/covid_wildfire")
# source("scr/Utilities.R")
# source("scr/GlobalModel.R")
# dff = load.data.xz1()
# lags.to.run = 0:14
# for (ilag in lags.to.run) {
#   lag.out = add.lag(dff=dff, value="pm25", group="FIPS", lags=ilag)
#   lag.data = as.matrix(lag.out[[1]])
#   pm25 = lag.data[!is.na(lag.data)]
#   print(paste(ilag, "pm.l missing", sum(is.na(lag.data)), "fireday count", sum(lag.data>20, na.rm=T), 
#               "count pm", length(pm25), "cor", round(cor(pm25, pm25>20), 2)))
# }

# [1] "0 pm.l missing 7951 fireday count 1890 count pm 24900 cor 0.64"
# [1] "1 pm.l missing 8060 fireday count 1880 count pm 24791 cor 0.64"
# [1] "2 pm.l missing 8169 fireday count 1860 count pm 24682 cor 0.65"
# [1] "3 pm.l missing 8278 fireday count 1833 count pm 24573 cor 0.65"
# [1] "4 pm.l missing 8387 fireday count 1801 count pm 24464 cor 0.65"
# [1] "5 pm.l missing 8494 fireday count 1782 count pm 24357 cor 0.65"
# [1] "6 pm.l missing 8603 fireday count 1745 count pm 24248 cor 0.65"
# [1] "7 pm.l missing 8713 fireday count 1677 count pm 24138 cor 0.65"
# [1] "8 pm.l missing 8824 fireday count 1595 count pm 24027 cor 0.65"
# [1] "9 pm.l missing 8935 fireday count 1503 count pm 23916 cor 0.64"
# [1] "10 pm.l missing 9046 fireday count 1392 count pm 23805 cor 0.63"
# [1] "11 pm.l missing 9156 fireday count 1283 count pm 23695 cor 0.62"
# [1] "12 pm.l missing 9267 fireday count 1174 count pm 23584 cor 0.63"
# [1] "13 pm.l missing 9378 fireday count 1066 count pm 23473 cor 0.67"
# [1] "14 pm.l missing 9488 fireday count 967 count pm 23363 cor 0.69"


############################ what if spline on pm #########################
setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
source("scr/GlobalModel.R")
dff_full = load.data.xz1()
df.date=5
df.tmmx=2
df.rmax=2
df.pm=2
lags=0
lags.to.run = lags

smooth = "ns"
cause = "cases"
pollutant="pm25"
group="FIPS"
control=glm.control(epsilon = 1e-10, maxit = 10000)

pm.quantile = quantile(dff_full$pm25, c(0, .25,.5,.75,1), na.rm=T)
pm.quantile # -0.9269231   3.1576923   4.8538462   8.2230769 761.6576923
for (i in 1:4) {
  dff = load.data.xz1()
  dff$pm25[(dff$pm25 <= pm.quantile[i]) | (dff$pm25 > pm.quantile[i+1])] = NA
  print(sum(!is.na(dff$pm25),na.rm=T))
  fit = glm(formula = cases ~ FIPS + ns(date_num, df.date) + ns(tmmx, df.tmmx) + ns(rmax, df.rmax) + dayofweek + pm25,
            family = quasipoisson, data = dff, na.action = na.exclude,
            control = glm.control(epsilon = 1e-10, maxit = 10000))
  summary(fit)
  
  print(coefficients(fit)['pm25'])
  print(confint(fit, "pm25"))
}


# dff = dff_full[(dff_full$pm25 > pm.quantile[i]) & (dff_full$pm25 <= pm.quantile[i+1]), ]
# 
# fit = glm(formula = cases ~ FIPS + ns(date_num, df.date) + ns(tmmx, df.tmmx) + ns(rmax, df.rmax) + dayofweek + pm25,
#           family = quasipoisson, data = dff, na.action = na.exclude,
#           control = glm.control(epsilon = 1e-10, maxit = 10000))
# summary(fit)
# 
# coefficients(fit)
# 
# pmeffect = ns(dff$pm25, df.pm) %*%  matrix(data=tail(coefficients(fit), df.pm), ncol=1)
# c1 = confint(fit, "ns(pm25, df.pm)1")
# c2 = confint(fit, "ns(pm25, df.pm)2")
# pmeffect.low = ns(dff$pm25, df.pm) %*%  matrix(data=c(c1[1], c2[2]), ncol=1)
# pmeffect.high = ns(dff$pm25, df.pm) %*%  matrix(data=c(c1[2], c2[1]), ncol=1)
# 
# ggplot() + geom_point(aes(dff$pm25, exp(pmeffect)-1), col="black")  + 
#   geom_point(aes(dff$pm25, exp(pmeffect.low)-1), col="red") + 
#   geom_point(aes(dff$pm25, exp(pmeffect.high)-1), col="red")


  

# ############################ test the interaction model #########################
# setwd("/Users/mac/Documents/GitHub/covid_wildfire")
# source("scr/Utilities.R")
# source("scr/GlobalModel.R")
# dff = load.data.xz1()
# df.date=5
# df.tmmx=2
# df.rmax=2
# lags=0
# lags.to.run = lags
# 
# smooth = "ns"
# cause = "cases"
# pollutant="pm25"
# group="FIPS"
# control=glm.control(epsilon = 1e-10, maxit = 10000)
# 
# gm = global.model2(dff=dff, smooth = smooth, df.date = df.date,
#                    df.tmmx = df.tmmx, df.rmax = df.rmax, lags = lags, cause = cause)
# 
# 
# # dff$fireday = NA
# # dff$fireday[dff$pm25 <= 20] = "ANOFIRE"
# # dff$fireday[dff$pm25 > 20] = "FIRE"
# # dff$fireday = as.factor(dff$fireday)
# # unique(dff$fireday)
# #+ I(ns(date_num, df.date)*fireday) 
# dff$fireday = dff$pm25 > 20 * 1
# 
# fit = glm(formula = cases ~ FIPS + ns(date_num, df.date) + ns(tmmx, df.tmmx) + 
#             ns(rmax, df.rmax) + dayofweek + pm25*fireday,
#           family = quasipoisson, data = dff, na.action = na.exclude,
#           control = glm.control(epsilon = 1e-10, maxit = 10000))
# summary(fit)

# pm25                    1.769e-02  2.481e-03   7.130 1.04e-12 ***
# firedayFIRE             3.644e-01  4.561e-02   7.990 1.42e-15 ***
# pm25:firedayFIRE       -1.730e-02  2.568e-03  -6.734 1.70e-11 ***

# fit = glm(formula = cases ~ FIPS + ns(date_num, df.date) + ns(tmmx, df.tmmx) + ns(rmax, df.rmax) + dayofweek + pm25 + fireday + pm25:fireday, # 
#           family = quasipoisson, data = dff, na.action = na.exclude,
#           control = glm.control(epsilon = 1e-10, maxit = 10000))
# summary(fit)
# pm25                    1.769e-02  2.481e-03   7.130 1.04e-12 ***
# firedayFIRE             3.644e-01  4.561e-02   7.990 1.42e-15 ***
# pm25:firedayFIRE       -1.730e-02  2.568e-03  -6.734 1.70e-11 ***

# dff$fireday = dff$pm25 > 20
# fit = glm(formula = cases ~ FIPS + ns(date_num, df.date) + ns(tmmx, df.tmmx) + ns(rmax, df.rmax) + dayofweek + pm25 + fireday + pm25:fireday, # 
#           family = quasipoisson, data = dff, na.action = na.exclude,
#           control = glm.control(epsilon = 1e-10, maxit = 10000))
# summary(fit)
# pm25                    1.769e-02  2.481e-03   7.130 1.04e-12 ***
# firedayTRUE             3.644e-01  4.561e-02   7.990 1.42e-15 ***
# pm25:firedayTRUE       -1.730e-02  2.568e-03  -6.734 1.70e-11 ***

# fit = glm(formula = cases ~ FIPS + ns(date_num, df.date) + ns(tmmx, df.tmmx) + ns(rmax, df.rmax) + dayofweek + pm25*fireday, # 
#           family = quasipoisson, data = dff, na.action = na.exclude,
#           control = glm.control(epsilon = 1e-10, maxit = 10000))
# summary(fit)
# pm25                    1.769e-02  2.481e-03   7.130 1.04e-12 ***
# firedayTRUE             3.644e-01  4.561e-02   7.990 1.42e-15 ***
# pm25:firedayTRUE       -1.730e-02  2.568e-03  -6.734 1.70e-11 ***




# ############################ Test add.lag() in Utilities.R #########################
# 
# source("scr/Utilities.R")
# 
# dates = c( "2020-03-15 PDT", "2020-03-16 PDT", "2020-03-17 PDT", "2020-03-18 PDT",
#            "2020-03-19 PDT", "2020-03-20 PDT", "2020-03-21 PDT", "2020-03-22 PDT",
#            "2020-03-23 PDT", "2020-03-24 PDT")
# dt = data.frame(date = rep(dates, 2), 
#                 value = c(1:10, 101:110), 
#                 group = rep(c("A", "B"), each=10))
# 
# print(dt)
# 
# ### Test1
# nlag = 0
# dt.out = add.lag(df=dt, value="value", group="group", lags=nlag)
# print(dt.out)
# 
# ### Test2
# nlag = c(0,4)
# dt.out = add.lag(df=dt, value="value", group="group", lags=nlag)
# print(dt.out)
# 
# ### Test3
# nlag = 0:4
# dt.out = add.lag(df=dt, value="value", group="group", lags=nlag)
# print(dt.out)
# 
# 
# ### NA handling
# dt$value[3] = NA
# 
# ### Test4
# nlag = 0:4
# dt.out = add.lag(df=dt, value="value", group="group", lags=nlag)
# print(dt.out)
# 
# 
# 
######################### compare mc2 and xz1 data #########################
# setwd("/Users/mac/Documents/GitHub/covid_wildfire")
# source("scr/Utilities.R")
# dfx = load.data.xz1()
# dfm = load.moddat2()
# 
# dim(dfx)
# dim(dfm)
# 
# dfm$date = ymd(dfm$date)
# dfm = dfm[dfm$date <= "2020-09-24", ]
# 
# dfx = dfx[dfx$FIPS != 6000, ]
# 
# unique(dfx$date_str)
# unique(dfm$date)
# 
# unique(dfx$FIPS)
# unique(dfm$FIPS)
# 
# dx = data.frame(dfx %>% group_by(FIPS) %>% count())
# dm = data.frame(dfm %>% group_by(FIPS) %>% count())
# sum(dx$n != dm$n)
# 
# dx = data.frame(dfx %>% group_by(date) %>% count())
# dm = data.frame(dfm %>% group_by(date) %>% count())
# sum(dx$n != dm$n)
# 
# sum(round(dfx$pm25, 3) != round(dfm$pm25, 3), na.rm=T)
# sum(round(dfx$tmmx, 3) != round(dfm$tmmx, 3), na.rm=T)
