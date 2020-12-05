setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
dff = load.data.xz1()
dff = dff[dff$date >= ymd("2020-03-15"), ]
dff = dff[dff$date <= ymd("2020-09-24"), ]

length(unique(dff$FIPS)) # 133 counties
# for 194 days 2020-03-15 to 2020-09-24 

dff$state = round(as.numeric(as.character(dff$FIPS))/1000, 0)
dff$state[dff$state == 6] = "CA"
dff$state[dff$state == 53] = "WA"
dff$state[dff$state == 41] = "OR"
dff$state = as.factor(dff$state)

dff = dff[, c("state", "pm25", "cases", "deaths", "tmmx", "rmax")]
dff = dff[complete.cases(dff), ]
summary(dff)

key.var = c("pm25", "cases", "deaths", "tmmx", "rmax")
property = c("min", "qt25", "median", "mean", "qt75", "max")
d0 = summary(dff[, key.var])
d1 = summary(dff[dff$state == "WA", key.var])
d2 = summary(dff[dff$state == "OR", key.var])
d3 = summary(dff[dff$state == "CA", key.var])

d0 = t(matrix(as.numeric(sub('.*:', '', d0)), byrow=F, nrow=dim(d0)[1], ncol=dim(d0)[2]))
d1 = t(matrix(as.numeric(sub('.*:', '', d1)), byrow=F, nrow=dim(d1)[1], ncol=dim(d1)[2]))
d2 = t(matrix(as.numeric(sub('.*:', '', d2)), byrow=F, nrow=dim(d2)[1], ncol=dim(d2)[2]))
d3 = t(matrix(as.numeric(sub('.*:', '', d3)), byrow=F, nrow=dim(d3)[1], ncol=dim(d3)[2]))

d0 = data.frame(d0)
names(d0) = property
d0$var = key.var
d0$state = "ALL"

d1 = data.frame(d1)
names(d1) = property
d1$var = key.var
d1$state = "WA"

d2 = data.frame(d2)
names(d2) = property
d2$var = key.var
d2$state = "OR"

d3 = data.frame(d3)
names(d3) = property
d3$var = key.var
d3$state = "CA"

dd = rbind(d0, d1, d2, d3)
dd = dd[, c("var", "state", "mean", "min", "qt25", "median", "qt75", "max")]
dd = dd[order(dd$var, dd$state, decreasing=T),]

write.csv(dd, "ExploratoryDataAnalysis/summary.csv")

