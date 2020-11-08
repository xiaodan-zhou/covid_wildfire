setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
source("/Users/mac/Desktop/GlobalModel.R")
dff = load.data.xz1()

### set up
lags.to.run = 0:28
smooth="ns"
cause = "cases"
df.date=5
df.tmmx=2
df.rmax=2
extra.note = "rerunoldGlolbaModel"


### output 
temp.name = paste0(paste0("df", df.date, df.tmmx, df.rmax), ".", 
                   paste0(lags.to.run[1], "to", tail(lags.to.run, n=1)), 
                   ".", cause, extra.note)
file.pdf = paste0("GlobalModel/Interactlag.", temp.name, ".pdf")
file.csv = paste0("GlobalModel/Interactlag.", temp.name, ".csv")


##################### run global model for multiple lags separately #####################
result.rbind = c()
for (ilag in lags.to.run) {
  gm = global.model2(dff=dff, smooth=smooth, lags=ilag, cause = cause, 
                    df.date=df.date, df.tmmx=df.tmmx, df.rmax=df.rmax)
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
write.csv(result.rbind, file.csv, row.names=F)


##################### visualize #####################
if (isempty(result.rbind)) stop("the output table is empty")
  
plot.out = list()

result.vis = result.rbind[,c(1:3, 10)]
result.vis = data.frame(result.vis)
names(result.vis) = c("coef", "coef.low", "coef.high", "lag")
p1 = ggplot(data=result.vis, aes(x=lag)) +
  geom_errorbar(width=.1, aes(ymin=coef.low, ymax=coef.high), colour="red") + 
  geom_point(aes(y=coef)) + 
  geom_line(aes(y=coef)) + ylab("PM2.5 coefficients") 
plot.out[[1]] = p1

result.vis = result.rbind[,c(7:9, 10)]
result.vis = data.frame(result.vis)
names(result.vis) = c("coef", "coef.low", "coef.high", "lag")
p2 = ggplot(data=result.vis, aes(x=lag)) +
  geom_errorbar(width=.1, aes(ymin=coef.low, ymax=coef.high), colour="red") + 
  geom_point(aes(y=coef)) + 
  geom_line(aes(y=coef)) + ylab("PM2.5:Fireday coefficients") 
plot.out[[2]] = p2

result.vis = result.rbind[,c(4:6, 10)]
result.vis = data.frame(result.vis)
names(result.vis) = c("coef", "coef.low", "coef.high", "lag")
p3 = ggplot(data=result.vis, aes(x=lag)) +
  geom_errorbar(width=.1, aes(ymin=coef.low, ymax=coef.high), colour="red") + 
  geom_point(aes(y=coef)) + 
  geom_line(aes(y=coef)) + ylab("WildFire day coefficients") 
plot.out[[3]] = p3

pdf(file.pdf, width = 12, height = 5 * length(plot.out))
do.call('grid.arrange',c(plot.out, ncol = 1, top = "global model"))
dev.off()

