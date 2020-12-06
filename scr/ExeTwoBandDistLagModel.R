setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
source("scr/GlobalModel.R")
dff = load.data.xz1()


### set up
lags.to.run = 0:14
smooth="ns"
cause = "cases"
df.combo = list(c(5,2,2), c(6,2,2), c(7,2,2),
                c(9,2,2), c(11,3,3), c(14,3,3))

extra.note = paste0(".sensitivity")

### output 
temp.name = paste0(paste(lags.to.run, collapse=""), ".", cause, ".TwoChanel.sensitivity.", extra.note)
file.pdf = paste0("GlobalModel/TwoChannellag", temp.name, ".pdf")
file.csv = paste0("GlobalModel/TwoChannellag", temp.name, ".csv")


##################### run global model for multiple lags separately #####################
result.rbind = c()
for (ilag in lags.to.run) {
  for (idf.combo in df.combo) {
    gm = global.model4(dff=dff, smooth=smooth, lags=ilag, cause = cause, 
                       df.date=idf.combo[1], df.tmmx=idf.combo[2], df.rmax=idf.combo[2])
    print(gm)
    if (length(gm) != 1) {
      gm = c(gm, lag=ilag, df.date=idf.combo[1], df.tmmx=idf.combo[2], df.rmax=idf.combo[3])
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

result.rbind = data.frame(result.rbind)
write.csv(result.rbind, file.csv)
# result.rbind = read.csv(file.csv)

##################### visualize #####################
plot.out = list()

p1 = ggplot(result.rbind, aes(x=lag)) +
  geom_errorbar(width=.1, aes(ymin=lag.base.low, ymax=lag.base.high), colour="red") + 
  geom_point(aes(y=lag.base)) + 
  geom_line(aes(y=lag.base)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  xlab("PM2.5 base lag") + ylab("PM2.5 coefficients") + 
  geom_hline(yintercept=0, linetype="dashed", color = "blue", alpha=.6)

p2 = ggplot(result.rbind, aes(x=lag)) +
  geom_errorbar(width=.1, aes(ymin=lag.hazard.low, ymax=lag.hazard.high), colour="red") + 
  geom_point(aes(y=lag.hazard)) + 
  geom_line(aes(y=lag.hazard)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  xlab("PM2.5 hazard lag") + ylab("PM2.5 coefficients") + 
  geom_hline(yintercept=0, linetype="dashed", color = "blue", alpha=.6)

plot.out[[1]] = p1
plot.out[[2]] = p2

pdf(file.pdf, width = 12, height = 5 * length(plot.out))
do.call('grid.arrange',c(plot.out, ncol = 1, top = "global model"))
dev.off()

