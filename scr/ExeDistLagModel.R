setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
source("scr/Model.R")
dff = load.data()

## testing 
# dff = dff[dff$FIPS %in% c("6037", "6039"), ]

### parameter set up
pollutants = 2
causes = c("cases", "deaths")
max.lag = 21
mobility = NA

df.date=5
df.tmmx=2
df.rmax=2

### output file name
df.combo = paste0(df.date, df.tmmx, df.rmax)
if (pollutants == 1) temp.name = paste0("OneBand.DistLag", max.lag)
if (pollutants == 2) temp.name = paste0("TwoBand.DistLag", max.lag)
if (!is.na(mobility)) temp.name = paste0(temp.name, ".withMobility")
temp.name = paste0(temp.name, "[", Sys.time(), "]")
file.csv = paste0("output/", temp.name, ".csv")

### execute modelling 
result.rbind = c()
for (cause in causes) {
  for (ilag in 0:max.lag) {
    gm = model(dff, 
               lags=0:ilag, 
               df.date=df.date,
               df.tmmx=df.tmmx, 
               df.rmax=df.rmax,
               cause = cause,
               mobility=mobility,
               pollutants=pollutants)
    
    if (length(gm) != 1) {
      gm = c(gm, max.lag=ilag, 
             cause=cause)
      result.rbind=rbind(result.rbind, gm)
    } else {
      print("failed")
    }
  }
}

result.rbind = data.frame(result.rbind)
result.rbind$df.date = df.date
result.rbind$df.tmmx = df.tmmx
result.rbind$df.rmax = df.rmax
result.rbind$mobility=1-is.na(mobility)
write.csv(result.rbind, file.csv, row.names=FALSE)

