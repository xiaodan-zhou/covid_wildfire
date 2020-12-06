setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
source("scr/Model.R")
dff = load.data()

## testing 
# dff = dff[dff$FIPS %in% c("6037", "6039"), ]

### set up
pollutants = 2
causes = c("cases", "deaths")
max.lag = 21
mobility = NA
df.combo = list(c(3,1), c(4,1), c(5,2), c(6,2), c(7,2), 
                c(8, 2), c(9,3), c(10,3), c(11,3), c(12,3))

### output 
if (pollutants == 1) temp.name = paste0("OneBand.singleLag", max.lag)
if (pollutants == 2) temp.name = paste0("TwoBand.singleLag", max.lag)
if (!is.na(mobility)) temp.name = paste0(temp.name, ".withMobility")
temp.name = paste0(temp.name, "[", Sys.time(), "]")
file.csv = paste0("output/", temp.name, ".csv")

##################### run global model for single lags #####################
result.rbind = c()
for (cause in causes) {
  for (ilag in 0:max.lag) {
    for (idf.combo in df.combo) {
      gm = model(dff, 
                 lags=ilag, 
                 df.date=idf.combo[1], 
                 df.tmmx=idf.combo[2], 
                 df.rmax=idf.combo[2], 
                 cause = cause,
                 mobility=mobility,
                 pollutants=pollutants)
      
      if (length(gm) != 1) {
        gm = c(gm, lag=ilag, 
               df.date=idf.combo[1], 
               df.tmmx=idf.combo[2], 
               df.rmax=idf.combo[2], 
               cause=cause)
        result.rbind=rbind(result.rbind, gm)
        } else {
        print("failed")
        }
     }
  }
}
result.rbind = data.frame(result.rbind)
result.rbind$mobility=1-is.na(mobility)
write.csv(result.rbind, file.csv, row.names=FALSE)


