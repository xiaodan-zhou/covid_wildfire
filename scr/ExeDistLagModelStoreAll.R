setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
source("scr/Model.R")
dff = load.data()

### set up
smooth="ns"
group="FIPS"
control=glm.control(epsilon = 1e-10, maxit = 10000)
pollutants = 1
df.date=6
df.tmmx=2
df.rmax=2

causes = c("cases", "deaths")
max.lags = c(14, 21)
mobility.options= c(T, F)

temp.name = paste0("unconstrained_df", df.date, df.tmmx, df.rmax) 
temp.name = paste0(temp.name, "[", Sys.time(), "]")
file.csv = paste0("output/", temp.name, ".csv")

### execute modelling 
result.rbind = c()
for (cause in causes) {
  for (mobility in mobility.options) {
    for (max.lag in max.lags) {
      gm = model(dff, df.date=df.date, df.tmmx=df.tmmx, df.rmax=df.rmax, lags=0:max.lag, 
                 pollutants = pollutants, cause = cause, mobility=mobility, fullDist=TRUE)
      N = length(0:max.lag)
      gm = data.frame(pm = gm[1:N], pm.low = gm[(N+1):(2*N)], pm.high = gm[(2*N+1):(3*N)], lag=0:max.lag)
      gm$max.lag = max.lag
      gm$cause = cause
      gm$mobility=1-mobility
      result.rbind = rbind(result.rbind, gm)
    }
  }
}
      
result.rbind$df.date = df.date
result.rbind$df.tmmx = df.tmmx
result.rbind$df.rmax = df.rmax
write.csv(result.rbind, file.csv, row.names=FALSE)



