### show that the daily total test cases might not be correlated
### data retrived on Nov 9, 2020 https://covidtracking.com/data 

library(lubridate)
library(dplyr)
library(ggplot2)
setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
setwd("/Users/mac/Documents/GitHub/covid_wildfire/data/statetotaltests")

# ##############
# ca = read.csv("CA_tests.csv")
# ca$State = "CA"
# ca = ca[,c(1,6,7)]
# names(ca) = c("date", "totaltest", "state")
# ca$date = dmy(ca$date)
# ca$totaltest = as.numeric(gsub(",", "", ca$totaltest))
# 
# or = read.csv("OR_tests.csv")
# or$State = "OR"
# or = or[,c(1,6,7)]
# names(or) = c("date", "totaltest", "state")
# or$date = dmy(or$date)
# or$totaltest = as.numeric(gsub(",", "", or$totaltest))
# 
# wa = read.csv("WA_tests.csv")
# wa$State = "WA"
# wa = wa[,c(1,6,7)]
# names(wa) = c("date", "totaltest", "state")
# wa$date = dmy(wa$date)
# wa$totaltest = as.numeric(gsub(",", "", wa$totaltest))
# wa = wa[wa$date >= "2020-03-04", ]
# 
# ca = ca[order(ca$date),]
# wa = wa[order(wa$date),]
# or = or[order(or$date),]
# ca$order = 1:dim(ca)[1]
# or$order = 1:dim(or)[1]
# wa$order = 1:dim(wa)[1]
# 
# dt = rbind(ca,or,wa)
# rm(ca, or, wa)
# 
# 
# 
# 
# ##############
# ca = read.csv("CA_cases.csv")
# ca$State = "CA"
# ca = ca[,c(1,3,6)]
# names(ca) = c("date", "newcases", "state")
# ca$date = dmy(ca$date)
# ca$newcases = as.numeric(gsub(",", "", ca$newcases))
# 
# or = read.csv("OR_cases.csv")
# or$State = "OR"
# or = or[,c(1,3,6)]
# names(or) = c("date", "newcases", "state")
# or$date = dmy(or$date)
# or$newcases = as.numeric(gsub(",", "", or$newcases))
# 
# wa = read.csv("WA_cases.csv")
# wa$State = "WA"
# wa = wa[,c(1,3,6)]
# names(wa) = c("date", "newcases", "state")
# wa$date = dmy(wa$date)
# wa = wa[wa$date >= "2020-03-04", ]
# wa$newcases = as.numeric(gsub(",", "", wa$newcases))
# 
# dcase = rbind(ca,or,wa)
# rm(ca, or, wa)
# 
# 
# 
# ###########
# dt$state = as.factor(dt$state)
# dt = dt[dt$date >= "2020-03-15", ]
# dt = dt[dt$date <= "2020-09-24", ]
# 
# dcase$state = as.factor(dcase$state)
# dcase = dcase[dcase$date >= "2020-03-15", ]
# dcase = dcase[dcase$date <= "2020-09-24", ]
# dt = left_join(dt, dcase, by=c("date", "state"))
# rm(dcase)
# 
# dt$newcases = as.numeric(gsub(",", "", dt$newcases))
# days = dim(dt)[1]/3
# dt$dailytest = 0
# dt$dailytest[1:days] = c(dt$totaltest[1], diff(dt$totaltest[1:days])) 
# dt$dailytest[(days+1):(2*days)] = c(dt$totaltest[(days+1)], diff(dt$totaltest[(days+1):(2*days)])) 
# dt$dailytest[(2*days+1):(3*days)] = c(dt$totaltest[(2*days+1)], diff(dt$totaltest[(2*days+1):(3*days)])) 
# 
# dt$positive.rate = dt$newcases / dt$dailytest * 100
# 
# dt$dayofweek = as.factor(weekdays(as.Date(dt$date)))
# 
# write.csv(dt, "merged.csv")


### read data 
dt = read.csv("merged.csv")
dt$date = ymd(dt$date)
dt$dayofweek = as.factor(dt$dayofweek)

ggplot(data=dt) + geom_point(aes(x=date,y=dailytest, col=state)) + ylab("daily tests") + 
  geom_line(aes(x=date,y=dailytest, col=state))

ggplot(data=dt) + geom_line(aes(x=date,y=positive.rate, col=state)) + 
  ylab("positive rates (%)") + ylim(c(0,30))

### how the daily tests varies in CA 
ca = dt[dt$state=="OR",]
fit.ca = glm(data=ca, dailytest ~ ns(date, 5) + dayofweek, family=quasipoisson)
ca$fitted = fit.ca$fitted.values
ca$diff = ca$dailytest - fit.ca$fitted.values
ca$diff.ratio = ca$diff / ca$totaltest * 100
ca$resid = exp(fit.ca$residuals)

ggplot(data=ca) + geom_point(aes(x=date,y=diff)) + ggtitle(ca$state[1])

ggplot(data=ca) + geom_point(aes(x=date,y=dailytest, col=state)) + 
  geom_line(aes(x=date,y=fitted, col=state)) + ggtitle(ca$state[1])


### count high pm by days 
dff = load.data.xz1()
dff$state_num = as.factor(as.character(round(as.numeric(as.character(dff$FIPS)) / 1000, 0)))
dff$state = ""
dff$state[dff$state_num == "6"] = "CA"
dff$state[dff$state_num == "41"] = "OR"
dff$state[dff$state_num == "53"] = "WA"

pm_state = data.frame(dff %>% group_by(state, date) %>% summarise("firecount"=sum(pm25>20,na.rm=T)))
ggplot() + geom_line(data=pm_state,aes(x=date,y=firecount,col=state)) + ylab("count (FIPS where pm>20")

### newcases ~ ns(date, 5) + state + dayofweek
fit.dt = glm(data=dt, newcases ~ ns(date, 5) + state + dayofweek, family=quasipoisson)
dt$fitted = fit.dt$fitted.values
dt$diff = dt$newcases - fit.dt$fitted.values
dt$diff.ratio = dt$diff / dt$totaltest * 100
dt$resid = exp(fit.dt$residuals)

ggplot(data=dt) + geom_point(aes(x=date,y=diff, col=state))

ggplot(data=dt) + geom_point(aes(x=date,y=newcases, col=state)) + 
  geom_line(aes(x=date,y=fitted, col=state))


### newcases ~ ns(order, 9) + state + dayofweek + dailytest
fit.dt = glm(data=dt, newcases ~ ns(order, 9) + state + dayofweek + dailytest, family=quasipoisson)
dt$fitted = fit.dt$fitted.values
dt$diff = dt$newcases - fit.dt$fitted.values
dt$diff.ratio = (fit.dt$fitted.values - dt$newcases) / dt$newcases * 100
dt$resid = exp(fit.dt$residuals)

ggplot() + geom_point(data=dt, aes(x=date,y=newcases, col=state)) + 
  geom_line(data=dt, aes(x=date,y=fitted, col=state))
ggplot(data=dt) + geom_point(aes(x=date,y=diff.ratio, col=state)) + ylim(c(0, 500))




# ggplot(data=dt[dt$date >= "2020-08-15",], aes(x=date,y=diff, col=state)) + geom_point() + geom_smooth()
# ggplot(data=dt[dt$date >= "2020-08-15",], aes(x=date,y=diff.ratio, col=state)) + geom_point() + geom_smooth()
# ggplot(data=dt[dt$date >= "2020-08-15",], aes(x=date,y=resid, col=state)) + geom_point() + geom_smooth()
# 
# ggplot(data=dt, aes(x=date,y=diff, col=state)) + geom_point() + geom_smooth()
# ggplot(data=dt, aes(x=date,y=diff.ratio, col=state)) + geom_point() + geom_smooth()
# ggplot(data=dt, aes(x=date,y=resid, col=state)) + geom_point() + geom_smooth()
