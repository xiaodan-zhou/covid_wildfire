setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
source("scr/GlobalModel.R")
# dff = load.data.xz1()
dff = load.data.error()

### set up 
lags.to.run = 0:3
smooth="ns"
cause = "cases"
df.date=8
df.tmmx=3
df.rmax=3


### output 
temp.name = paste0(paste(lags.to.run, collapse=""), ".", cause, ".error") # confit
file.pdf = paste0("GlobalModel/lag", temp.name, ".pdf")
file.csv = paste0("GlobalModel/lag", temp.name, ".csv")


##################### run global model for multiple lags separately #####################
result.rbind = c()
for (ilag in lags.to.run) {
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




##################### visualize #####################
if (!isempty(result.rbind)) {
  
  plot.out = list()
  
  p1 = ggplot(result.rbind, aes(x=ilag)) +
    geom_errorbar(width=.1, aes(ymin=ci.low, ymax=ci.high), colour="red") + 
    geom_point(aes(y=coef)) + 
    geom_line(aes(y=coef)) + 
    xlab("PM2.5 lag") + ylab("PM2.5 coefficients") + 
    ggtitle(modelFormula.vis)
  plot.out[[1]] = p1
  
  pdf(file.pdf, width = 12, height = 5 * length(plot.out))
  do.call('grid.arrange',c(plot.out, ncol = 1, top = "global model"))
  dev.off()
}
# ### visualize fitted cases 
# cases.fitted = fit$fitted.values
# cases.raw = fit$data[names(fit$fitted.values),'cases']
# cases.resid = cases.fitted-cases.raw
# 
# ggplot() + geom_point(aes(x=cases.raw, y=cases.fitted), alpha=.1) +
#   xlim(c(0, 5000)) + ylim(c(0, 5000)) +
#   geom_line(aes(x=1:5000, y=1:5000), color='red') +
#   xlab("cases") + ylab("fitted") + ggtitle("Global Model Fitted")
# 
# ggplot() + geom_point(aes(x=cases.raw, y=cases.fitted), alpha=.1) + 
#   xlim(c(0, 500)) + ylim(c(0, 500)) +
#   geom_line(aes(x=1:5000, y=1:5000), color='red') + 
#   xlab("cases") + ylab("fitted") + ggtitle("Global Model Fitted")
# 
# 
# ### visualize residuals
# ggplot() + geom_point(aes(x=cases.raw, y=cases.resid), alpha=.1) + 
#   xlim(c(0, 5000)) + ylim(c(-2500, 5000)) +
#   xlab("cases") + ylab("residuals") + ggtitle("Global Model Residuals")
# 
# ggplot() + geom_point(aes(x=cases.raw, y=cases.resid), alpha=.1) + 
#   xlim(c(0, 500)) + ylim(c(-500, 500)) +
#   xlab("cases") + ylab("residuals") + ggtitle("Global Model Residuals")
