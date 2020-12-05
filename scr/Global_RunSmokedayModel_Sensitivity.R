############################ test the model with fireday, no pm25 #########################
setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
source("scr/GlobalModel.R")
dff = load.data.xz1()

### set up
lags.to.run = 0:14
smooth="ns"
cause = "deaths"
df.combo = list(c(2,1,1), c(3,1,1), c(4,1,1),
                c(5,2,2), c(6,2,2), c(7,2,2),
                c(9,2,2), c(11,3,3), c(14,3,3))
pm.threshold = 20
extra.note = paste0("(pm", pm.threshold, ")")

### output 
temp.name = paste0(paste(lags.to.run, collapse=""), ".", cause, ".smokeday.sensitivity.", extra.note)
file.pdf = paste0("GlobalModel/TwoChannellag", temp.name, ".pdf")
file.csv = paste0("GlobalModel/TwoChannellag", temp.name, ".csv")


##################### run global model for single lags #####################
result.rbind = c()
for (ilag in lags.to.run) {
  for (idf.combo in df.combo) {
    gm = global.model3(dff, smooth = smooth, lag=ilag, 
                      df.date=idf.combo[1], df.tmmx=idf.combo[2], 
                      df.rmax=idf.combo[2], cause = cause, pm.threshold=pm.threshold)
    
    if (length(gm) != 1) {
      gm = c(gm, lag=ilag, df.date=idf.combo[1], df.tmmx=idf.combo[2], df.rmax=idf.combo[3], pm.threshold=pm.threshold)
      if (is.null(gm)) {
        result.rbind = gm
      } else {
        result.rbind = rbind(result.rbind, gm)
      }
    } else {
      print("failed")
    }
  }
}

result.rbind = as.data.frame(result.rbind)
write.csv(result.rbind, file.csv, row.names=F)

result.rbind = read.csv("GlobalModel/TwoChannellag01234567891011121314.cases.TwoChanel.sensitivity..sensitivity.csv")
##################### visualize #####################
result.rbind$df.combo = paste0(result.rbind$df.date, result.rbind$df.tmmx, result.rbind$df.rmax)

plot.out = list()
iplot = 1 
for (ilag in 0:7) {  # lags.to.run  ### todo: for death 0:4,6:9, 11:14 works only 
  data.vis = result.rbind[result.rbind$lag == ilag,]
  if (sum(is.na(data.vis)) == 0) {
    p0 = ggplot(data=data.vis, aes(x=(1:dim(data.vis)[1]))) +
      geom_errorbar(width=.1, aes(ymin=lag.base.low, ymax=lag.base.high), colour="red") + 
      geom_point(aes(y=lag.base)) + 
      geom_line(aes(y=lag.base)) + 
      theme_bw() + 
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) +
      xlab("df.combo") + ylab("base coefficients") + 
      ggtitle(paste("lag", ilag)) + 
      scale_x_continuous(breaks = 1:length(data.vis$df.combo),
                         labels = data.vis$df.combo) + 
      geom_hline(yintercept=0, linetype="dashed", color = "black", alpha=.6)
    plot.out[[iplot]] = p0
    iplot = iplot + 1
    
    p0 = ggplot(data=data.vis, aes(x=(1:dim(data.vis)[1]))) +
      geom_errorbar(width=.1, aes(ymin=lag.hazard.low, ymax=lag.hazard.high), colour="red") + 
      geom_point(aes(y=lag.hazard)) + 
      geom_line(aes(y=lag.hazard)) + 
      theme_bw() + 
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) + 
      xlab("df.combo") + ylab("hazard coefficients") + 
      ggtitle(paste("lag", ilag)) + 
      scale_x_continuous(breaks = 1:length(data.vis$df.combo),
                         labels = data.vis$df.combo) + 
      geom_hline(yintercept=0, linetype="dashed", color = "black", alpha=.6)
    plot.out[[iplot]] = p0
    iplot = iplot + 1
  }
}

pdf(file.pdf, width = 6, height = 1.5 * length(plot.out))
do.call('grid.arrange',c(plot.out, ncol = 1, top = "global model"))
dev.off()

