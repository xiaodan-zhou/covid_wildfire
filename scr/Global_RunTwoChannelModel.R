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
extra.note = "zz"

### output 
temp.name = paste0(paste0("df", df.date, df.tmmx, df.rmax), ".", 
                   paste0(lags.to.run[1], "to", tail(lags.to.run, n=1)), 
                   ".", cause, extra.note)
file.pdf = paste0("GlobalModel/TwoChannellag", temp.name, ".pdf")
file.csv = paste0("GlobalModel/TwoChannellag", temp.name, ".csv")


##################### run global model for multiple lags separately #####################
result.rbind = c()
for (ilag in lags.to.run) {
  gm = global.model4(dff=dff, smooth=smooth, lags=ilag, cause = cause, 
                     df.date=df.date, df.tmmx=df.tmmx, df.rmax=df.rmax)
  print(gm)
  if (length(gm) != 1) {
    gm = c(gm, lag=ilag, df.date=df.date, df.tmmx=df.tmmx, df.rmax=df.rmax)
    if (is.null(gm)) {
      result.rbind = gm
    } else {
      result.rbind = rbind(result.rbind, gm)
    }
  } else {
    print("failed")
  }
}
result.rbind = data.frame(result.rbind)
write.csv(result.rbind, file.csv)
# result.rbind = read.csv(file.csv)

result.rbind = read.csv("GlobalModel/TwoChannellagdf522.0to14.casesnew.csv")

##################### visualize #####################
plot.out = list()

p1 = ggplot(result.rbind, aes(x=lag)) +
  geom_errorbar(width=.1, aes(ymin=lag.base.low, ymax=lag.base.high), colour="red") + 
  geom_point(aes(y=lag.base)) + 
  geom_line(aes(y=lag.base)) + 
  xlab("PM2.5 baseline (lag)") + ylab("PM2.5 coefficients") + 
  geom_hline(yintercept=0, linetype="dashed", color = "black", alpha=.6) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


p2 = ggplot(result.rbind, aes(x=lag)) +
  geom_errorbar(width=.1, aes(ymin=lag.hazard.low, ymax=lag.hazard.high), colour="red") + 
  geom_point(aes(y=lag.hazard)) + 
  geom_line(aes(y=lag.hazard)) + 
  xlab("PM2.5 hazard contribution (lag)") + ylab("PM2.5 coefficients") + 
  geom_hline(yintercept=0, linetype="dashed", color = "black", alpha=.6) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

plot.out[[1]] = p1
plot.out[[2]] = p2

pdf(file.pdf, width = 6, height = 2 * length(plot.out))
do.call('grid.arrange',c(plot.out, ncol = 1, top = "global model"))
dev.off()

