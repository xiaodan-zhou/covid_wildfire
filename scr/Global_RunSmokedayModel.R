############################ test the model with fireday, no pm25 #########################
setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
source("scr/GlobalModel.R")
dff = load.data.xz1()

### set up
lags.to.run = 0:28
smooth="ns"
cause = "cases"
df.date=5
df.tmmx=2
df.rmax=2
pm.threshold = 24
extra.note = paste0("(pm", pm.threshold, ")")

### output 
temp.name = paste0(paste0(lags.to.run[1], "to", tail(lags.to.run, n=1)), 
                   ".", cause, ".smokeday.", paste0("df", df.date, df.tmmx, df.rmax), extra.note)
file.pdf = paste0("GlobalModel/lag", temp.name, ".pdf")
file.csv = paste0("GlobalModel/lag", temp.name, ".csv")


##################### run global model for single lags #####################
result.rbind = c()
for (ilag in lags.to.run) {
  gm = global.model3(dff, smooth = smooth, lag=ilag, 
                     df.date=df.date, df.tmmx=df.tmmx, 
                     df.rmax=df.rmax, cause = cause, pm.threshold=pm.threshold)
  if (length(gm) != 1) {
    gm = c(gm, lag=ilag, df.date=df.date, df.tmmx=df.tmmx, df.rmax=df.rmax, pm.threshold=pm.threshold)
    if (is.null(gm)) {
      result.rbind = gm
    } else {
      result.rbind = rbind(result.rbind, gm)
    }
  } else {
    print("failed")
  }
}
result.rbind = as.data.frame(result.rbind)
write.csv(result.rbind, file.csv, row.names=F)



##################### visualize #####################
if (!isempty(result.rbind)) {
  
  plot.out = list()
  
  p1 = ggplot(result.rbind, aes(x=lag)) +
    geom_errorbar(width=.1, aes(ymin=lag.smoke.low, ymax=lag.smoke.high), colour="red") + 
    geom_point(aes(y=lag.smoke)) + 
    geom_line(aes(y=lag.smoke)) + 
    xlab("PM2.5 lag") + ylab("Smoke coefficients") + 
    geom_hline(yintercept=0, linetype="dashed", color = "blue", alpha=.6)
  plot.out[[1]] = p1
  
  pdf(file.pdf, width = 12, height = 5 * length(plot.out))
  do.call('grid.arrange',c(plot.out, ncol = 1, top = "global model"))
  dev.off()
}

