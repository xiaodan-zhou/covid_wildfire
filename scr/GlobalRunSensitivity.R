
library(ggplot2)
library(gridExtra)

### read data 
setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
source("scr/GlobalModel.R")
df = load.data()


### set up 
lags.to.run = 0:3
smooth = "ns"
df.combo = list(c(2,1,1), c(3,1,1), c(4,1,1), c(5,2,2), c(6,2,2), c(7,2,2), c(8,2,2), c(9, 3, 3), c(10, 3, 3))

file.name = paste0("GlobalModel/lag", paste0(lags.to.run, collapse=""), "Sensitivity.pdf")
file.csv = paste0("GlobalModel/lag", paste0(lags.to.run, collapse=""), "Sensitivity.csv")
pdf(file.name, width = 12, height = 10)
plot.out = list()

##################### run global model for lag 0-3 separately #####################
result.rbind = c()
for (ilag in lags.to.run) {
  for (idf.combo in df.combo) {
    gm = global.model(df=df, smooth = smooth, lags=ilag, df.date=idf.combo[1], df.tmmx=idf.combo[2], df.rmax=idf.combo[3])
    fit = gm[[1]]
    fit.CI = gm[[2]]
    modelFormula.vis = gm[[3]]
    result = gm[[4]]
    if (is.null(result.rbind)) {
      result.rbind = result
    } else {
      result.rbind=rbind(result.rbind, result)
    }
  }
}

result.rbind$coef = as.numeric(result.rbind$coef)
result.rbind$ci.low = as.numeric(result.rbind$ci.low)
result.rbind$ci.high = as.numeric(result.rbind$ci.high)
result.rbind$ilag = as.numeric(result.rbind$ilag)
result.rbind$df.date = as.numeric(result.rbind$df.date)
result.rbind$df.tmmx = as.numeric(result.rbind$df.tmmx)
result.rbind$df.rmax = as.numeric(result.rbind$df.rmax)
result.rbind$df.combo = as.character(paste0(result.rbind$df.date,result.rbind$df.tmmx,result.rbind$df.rmax))
write.csv(result.rbind, file.csv)


iplot = 1 
for (ilag in lags.to.run) {
  data.vis = result.rbind[result.rbind$ilag == ilag,]
  p0 = ggplot(data=data.vis, aes(x=(1:dim(data.vis)[1]))) +
    geom_errorbar(width=.1, aes(ymin=ci.low, ymax=ci.high), colour="red") + 
    geom_point(aes(y=coef)) + 
    geom_line(aes(y=coef)) + 
    xlab("df.combo") + ylab("PM2.5 coefficients") + 
    ggtitle(paste("lag", ilag)) + 
    scale_x_continuous(breaks = 1:length(data.vis$df.combo),
                       labels = data.vis$df.combo)
  plot.out[[iplot]] = p0
  iplot = iplot + 1
}

##################### save #####################
do.call('grid.arrange',c(plot.out, ncol = 1, top = "global model"))

dev.off()
