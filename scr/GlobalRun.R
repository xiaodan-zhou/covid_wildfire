
library(ggplot2)
library(gridExtra)

### read data 
setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
source("scr/GlobalModel.R")
df = load.data.xz1()
### set up 
lags.to.run = 0:3
smooth = "ns"
df.date=8
df.tmmx=3
df.rmax=3

file.name = paste0("GlobalModel/lag", paste0(lags.to.run, collapse=""), ".", smooth, "_xd1+Test.pdf")
file.csv = paste0("GlobalModel/lag", paste0(lags.to.run, collapse=""), ".", smooth, "_xd1+Test.csv")
pdf(file.name, width = 12, height = 5)
plot.out = list()

##################### run global model for lag 0-3 using natural spline separately #####################
result.rbind = c()
for (ilag in lags.to.run) {
  gm = global.model(dff=df, smooth=smooth, lags=ilag)
  
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
  }
}

result.rbind$coef = as.numeric(result.rbind$coef)
result.rbind$ci.low = as.numeric(result.rbind$ci.low)
result.rbind$ci.high = as.numeric(result.rbind$ci.high)
result.rbind$ilag = as.numeric(result.rbind$ilag)
write.csv(result.rbind, file.csv)
### visualize pm coefficients
p2 = ggplot(result.rbind, aes(x=ilag)) +
  geom_errorbar(width=.1, aes(ymin=ci.low, ymax=ci.high), colour="red") + 
  geom_point(aes(y=coef)) + 
  geom_line(aes(y=coef)) + 
  xlab("PM2.5 lag") + ylab("PM2.5 coefficients") + 
  ggtitle(modelFormula.vis)
plot.out[[1]] = p2


##################### run global model for lag 0-3 together #####################
# gm = global.model(df=df, smooth=smooth, lags=lags.to.run)
# fit = gm[[1]]
# fit.CI = gm[[2]]
# modelFormula.vis = gm[[3]]
# result = gm[[4]]
# 
# result$coef = as.numeric(result$coef)
# result$ci.low = as.numeric(result$ci.low)
# result$ci.high = as.numeric(result$ci.high)
# result$ilag = as.numeric(result$ilag)
# 
# ### visualize pm coefficients
# p1 = ggplot(result, aes(x=ilag)) +
#   geom_errorbar(width=.1, aes(ymin=ci.low, ymax=ci.high), colour="red") + 
#   geom_point(aes(y=coef)) + 
#   geom_line(aes(y=coef)) + 
#   xlab("PM2.5 lag") + ylab("PM2.5 coefficients") + 
#   ggtitle(modelFormula.vis)
# plot.out[[2]] = p1


##################### save #####################

do.call('grid.arrange',c(plot.out, ncol = 1, top = "global model"))

dev.off()

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
