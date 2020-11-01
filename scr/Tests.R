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
