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
extra.note = "new"


### output 
temp.name = paste0(paste0("df", df.date, df.tmmx, df.rmax), ".", 
                   paste0(lags.to.run[1], "to", tail(lags.to.run, n=1)), 
                   ".", cause, extra.note)
file.pdf = paste0("GlobalModel/lag", temp.name, ".pdf")
file.csv = paste0("GlobalModel/lag", temp.name, ".csv")


##################### run global model for multiple lags separately #####################
result.rbind = c()
for (ilag in lags.to.run) {
  print(paste("==lag==", ilag))
  gm = global.model(dff, smooth=smooth, lags=ilag, cause = cause, 
                    df.date=df.date, df.tmmx=df.tmmx, df.rmax=df.rmax)
  
  if (length(gm) != 1) {
    fit = gm[[1]]
    fit.CI = gm[[2]]
    modelFormula.vis = gm[[3]]
    result = gm[[4]]
    
    if (is.null(result.rbind)) {
      result.rbind = result
    } else {
      result.rbind=rbind(result.rbind, result)
    }
  } else {
    print(paste("failed: ", gm))
  }
}

result.rbind$coef = as.numeric(result.rbind$coef)
result.rbind$ci.low = as.numeric(result.rbind$ci.low)
result.rbind$ci.high = as.numeric(result.rbind$ci.high)
result.rbind$ilag = as.numeric(result.rbind$ilag)
result.rbind$df.date = as.numeric(result.rbind$df.date)
result.rbind$df.tmmx = as.numeric(result.rbind$df.tmmx)
result.rbind$df.rmax = as.numeric(result.rbind$df.rmax)
write.csv(result.rbind, file.csv)


# result.rbind = read.csv("GlobalModel/*lag0to28.cases.df522.csv")

##################### visualize #####################
if (!isempty(result.rbind)) {
  
  plot.out = list()
  
  p1 = ggplot(result.rbind, aes(x=ilag)) +
    geom_errorbar(width=.1, aes(ymin=ci.low, ymax=ci.high), colour="red") + 
    geom_point(aes(y=coef)) + 
    geom_line(aes(y=coef)) + 
    xlab("PM2.5 lag") + ylab("PM2.5 coefficients") + 
    # ggtitle(modelFormula.vis) + 
    geom_hline(yintercept=0, linetype="dashed", color = "blue", alpha=.6)
  plot.out[[1]] = p1
  
  pdf(file.pdf, width = 12, height = 5 * length(plot.out))
  do.call('grid.arrange',c(plot.out, ncol = 1, top = "global model"))
  dev.off()
}
