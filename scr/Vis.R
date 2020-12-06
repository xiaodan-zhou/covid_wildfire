##################### visualize ExeSingleLagModel #####################

result = read.csv("")

plot.out = list()
iplot = 1

for (ilag in lags.to.run) {
  data.vis = result[result$ilag == ilag,]
  if (sum(is.na(data.vis)) == 0) {
    p0 = ggplot(data=data.vis, aes(x=(1:dim(data.vis)[1]))) +
      geom_errorbar(width=.1, aes(ymin=ci.low, ymax=ci.high), colour="red") +
      geom_point(aes(y=coef)) +
      geom_line(aes(y=coef)) +
      xlab("df.combo") + ylab("PM2.5 coefficients") +
      ggtitle(paste("lag", ilag)) +
      scale_x_continuous(breaks = 1:length(data.vis$df.combo),
                         labels = data.vis$df.combo) +
      geom_hline(yintercept=0, linetype="dashed", color = "blue", alpha=.6)
    plot.out[[iplot]] = p0
    iplot = iplot + 1
  }
}

pdf(file.pdf, width = 12, height = 3 * length(plot.out))
do.call('grid.arrange',c(plot.out, ncol = 1, top = "global model"))
dev.off()











##################### visualize ExeDistLagModel.R #####################
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