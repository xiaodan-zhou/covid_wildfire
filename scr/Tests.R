### Test add.lag() in Utilities.R

source("scr/Utilities.R")

dates = c( "2020-03-15 PDT", "2020-03-16 PDT", "2020-03-17 PDT", "2020-03-18 PDT",
           "2020-03-19 PDT", "2020-03-20 PDT", "2020-03-21 PDT", "2020-03-22 PDT",
           "2020-03-23 PDT", "2020-03-24 PDT")
dt = data.frame(date = rep(dates, 2), 
                value = c(1:10, 101:110), 
                group = rep(c("A", "B"), each=10))

print(dt)

### Test1
nlag = 0
dt.out = add.lag(df=dt, value="value", group="group", lags=nlag)
print(dt.out)

### Test2
nlag = c(0,4)
dt.out = add.lag(df=dt, value="value", group="group", lags=nlag)
print(dt.out)

### Test3
nlag = 0:4
dt.out = add.lag(df=dt, value="value", group="group", lags=nlag)
print(dt.out)


### NA handling
dt$value[3] = NA

### Test4
nlag = 0:4
dt.out = add.lag(df=dt, value="value", group="group", lags=nlag)
print(dt.out)



### compare mc2 and xz1 data 
setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
dfx = load.data.xz1()
dfm = load.moddat2()

dim(dfx)
dim(dfm)

dfm$date = ymd(dfm$date)
dfm = dfm[dfm$date <= "2020-09-24", ]

dfx = dfx[dfx$FIPS != 6000, ]

unique(dfx$date_str)
unique(dfm$date)

unique(dfx$FIPS)
unique(dfm$FIPS)

dx = data.frame(dfx %>% group_by(FIPS) %>% count())
dm = data.frame(dfm %>% group_by(FIPS) %>% count())
sum(dx$n != dm$n)

dx = data.frame(dfx %>% group_by(date) %>% count())
dm = data.frame(dfm %>% group_by(date) %>% count())
sum(dx$n != dm$n)

sum(round(dfx$pm25, 3) != round(dfm$pm25, 3), na.rm=T)
sum(round(dfx$tmmx, 3) != round(dfm$tmmx, 3), na.rm=T)
