library(rstudioapi)
project.dir = dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(project.dir)

source("scr/Utilities.R")
source("scr/Model.R")
dff = load.data()

## special: cases ~ pmbase 
# dff = dff[dff$pmhazard == 0 | is.na(dff$pmhazard), ]
## special: by state 
# dff = dff[dff$state.x != "CA", ]
## testing 
# dff = dff[dff$FIPS %in% c("6037", "6039"), ]

pollutants = 1
causes = c("cases", "deaths")
max.lag = 21
mobility = F # NA F
df.combo = list(c(3,1), c(4,1), c(5,2), c(6,2), c(7,2),
                c(8, 2), c(9,3), c(10,3), c(11,3), c(12,3)) 

### output file name
if (pollutants == 2) stop("should not use this")
temp.name = paste0("unconstrained_model_maxlag", max.lag) 
if (!is.na(mobility)&(mobility==T)) temp.name = paste0(temp.name, "_mobility")
temp.name = paste0(temp.name, "[", Sys.time(), "]")
file.csv = paste0("output/", temp.name, ".csv")

### execute modelling 
result.rbind = c()
for (cause in causes) {
  for (ilag in 0:max.lag) {
    for (idf.combo in df.combo) {
      gm = model(dff, 
                 lags=0:ilag, 
                 df.date=idf.combo[1], 
                 df.tmmx=idf.combo[2], 
                 df.rmax=idf.combo[2], 
                 cause = cause,
                 mobility=mobility,
                 pollutants=pollutants)
      
      if (length(gm) != 1) {
        gm = c(gm, max.lag=ilag, 
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
result.rbind$mobility=1-(!is.na(mobility)&(mobility==T))
write.csv(result.rbind, file.csv, row.names=FALSE)

