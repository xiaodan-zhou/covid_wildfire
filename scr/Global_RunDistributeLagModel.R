setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
source("scr/GlobalModel.R")
dff = load.data.xz1()

### set up 
lags.to.run = 0:14
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
file.pdf = paste0("GlobalModelDistributed/lag", temp.name, ".pdf")
file.csv = paste0("GlobalModelDistributed/lag", temp.name, ".csv")

##################### run global model for multiple lags separately #####################
result.rbind = c()
gm = global.model(dff, smooth=smooth, lags=lags.to.run, cause = cause, 
                  df.date=df.date, df.tmmx=df.tmmx, df.rmax=df.rmax)

if (length(gm) != 1) {
  fit = gm[[1]]
  fit.CI = gm[[2]]
  modelFormula.vis = gm[[3]]
  result.rbind = gm[[4]]
} else {
  print(paste("failed: ", gm))
}

result.rbind$coef = as.numeric(result.rbind$coef)
result.rbind$ci.low = as.numeric(result.rbind$ci.low)
result.rbind$ci.high = as.numeric(result.rbind$ci.high)
result.rbind$ilag = as.numeric(result.rbind$ilag)
result.rbind$df.date = as.numeric(result.rbind$df.date)
result.rbind$df.tmmx = as.numeric(result.rbind$df.tmmx)
result.rbind$df.rmax = as.numeric(result.rbind$df.rmax)

mean.g = sum(result.rbind$coef)
std.g = sqrt(sum(vcov(fit)[result.rbind$name, result.rbind$name])) # or rep(1, 7) %*% vcov(fit)[varr, varr] %*% rep(1, 7)
result.rbind[nrow(result.rbind) + 1,] = list(mean.g, std.g, mean.g - 1.96 * std.g, mean.g + 1.96 * std.g, NA, NA, NA, NA, NA, NA)

write.csv(result.rbind, file.csv)



##################### visualize #####################
dt1 = read.csv("GlobalModel/*lag0to28.cases.df522.csv")
dt2 = read.csv("GlobalModelDistributed/*lagdf522.0to14.cases.csv") # result.rbind
# dt1 = read.csv("GlobalModel/*lag0to28.deaths.df522.csv")
# dt2 = read.csv("GlobalModelDistributed/*lagdf522.0to14.deaths.csv") # result.rbind
# combine
up = dim(dt2)[1]
dt0 = dt1[1:up-1,]
dt0[nrow(dt0)+1, ] = tail(dt2, 1)
dt0

exp.trans = function(ls, delta = 10) {
  return((exp(ls * delta) - 1) * 100)
}
dt0$coef = exp.trans(dt0$coef)
dt0$ci.low = exp.trans(dt0$ci.low)
dt0$ci.high = exp.trans(dt0$ci.high)

if (!isempty(dt0)) {
  
  plot.out = list()
  
  p1 = ggplot(dt0, aes(x=c(0:(up-2), up))) +
    geom_errorbar(width=.1, aes(ymin=ci.low, ymax=ci.high), colour="black") + 
    geom_point(aes(y=coef)) + 
    xlab(paste0("Single Lag Model (Lag 0 - Lag", up-2, ") and Unconstrained Distributed-lag Model")) + 
    ylab(paste0("% ", cause, " change given \n10ug/m3 increase in PM2.5")) + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
  # axis.title.x = element_text(size = 16),
  # axis.text = element_text(size = 14),
  # axis.title.y = element_text(size = 16)
    scale_x_continuous(breaks = c(0:(up-2), up), 
                       labels=c(as.character(0:(up-2)), "unconstrained\ndistributed-lag"))
  p1
  plot.out[[1]] = p1
  
  pdf(file.pdf, width = 8, height = 4 * length(plot.out))
  do.call('grid.arrange',c(plot.out, ncol = 1))
  dev.off()
}
