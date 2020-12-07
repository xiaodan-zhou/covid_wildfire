### single lag model up to 14/21, with the unconstrained model on the right side
setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
r.single = read.csv("output/OneBand.singleLag21.csv")
r.dist = read.csv("output/OneBand.DistLag21.csv")
pollu.var = list(c("pm", "pm.low", "pm.high"))
causes = c("cases", "deaths")
mlags = c(14, 21)
mobility.options = c(1, 0)
df.date = 5
df.tmmx = 2
df.rmax = 2


for (mlag in mlags) {
  file.pdf = paste0("output/p.", mlag, Sys.time(), ".pdf")
  plot.out = list()
  iplot = 1
  
  for (cause in causes) {
    for (mobility in mobility.options) {
      
      ### choose degree of freedom
      sub.single = r.single[(r.single$df.date == df.date)&
                              (r.single$df.tmmx == df.tmmx)&
                              (r.single$df.rmax == df.rmax),]
      sub.dist = r.dist[(r.dist$df.date == df.date)&
                          (r.dist$df.tmmx == df.tmmx)&
                          (r.dist$df.rmax == df.rmax),]
      
      ### set maximum lag
      sub.single = sub.single[sub.single$lag <= mlag,]
      sub.dist = sub.dist[sub.dist$max.lag == mlag,]
      
      ### choose cause
      sub.single = sub.single[sub.single$cause == cause,]
      sub.dist = sub.dist[sub.dist$cause == cause,]

      ### choose mobility 
      sub.single = sub.single[sub.single$mobility == mobility,]
      sub.dist = sub.dist[sub.dist$mobility == mobility,]
      
      if ((dim(sub.single)[1] == 0)|(dim(sub.dist)[1] == 0))
        next
      
      xstr = paste0("Single Lag Model (Lag 0 - Lag", mlag, ") and the corresponding Unconstrained Distributed-lag Model")
      if (mobility == 1) xstr = paste0(xstr, ", adjusted for mobility")
      ystr = paste0("% ", cause, " change given in \n10ug/m3 increase in PM2.5")
      
      p0 = ggplot() +
        geom_errorbar(data=sub.single, width=.1, 
                      aes_string(x=1:(mlag+1), ymin="pm.low", ymax="pm.high")) +
        geom_point(data=sub.single, 
                   aes_string(x=1:(mlag+1), y="pm")) +
        geom_errorbar(data=sub.dist, width=.1, 
                      aes_string(x=mlag+3, ymin="pm.low", ymax="pm.high"), group=1) +
        geom_point(data=sub.dist, 
                   aes_string(x=mlag+3, y="pm"), group=1) + 
        theme_bw() + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank()) +
        xlab(xstr) + ylab(ystr) + ggtitle("") +
        scale_x_continuous(breaks = c(1:(mlag+1), mlag+3),
                           labels = as.character(c(0:mlag, "Unconstrained \nDistributed-lag Model"))) +
        geom_hline(yintercept=0, linetype="dashed", alpha=.6)
      
      plot.out[[iplot]] = p0
      iplot = iplot + 1
      }
  }
  pdf(file.pdf, width = 8, height = 3 * length(plot.out))
  do.call('grid.arrange',c(plot.out, ncol = 1, top = ""))
  dev.off()
}












# 
# 
# 
# for (ilag in lags.to.run) {
#   r.sub1 = r.single[(r.single$lag == ilag)&(r.single$cause == "cases"),]
#   r.sub2 = r.dist[(r.dist$max.lag == ilag)&(r.dist$cause == "cases"),]
#   
#   nrows = dim(r.sub1)[1]
#   
#   p0 = ggplot(NULL) +
#     geom_errorbar(data=r.sub1, width=.1, aes(x=1:nrows, ymin=pm.low, ymax=pm.high)) +
#     geom_point(data=r.sub1, aes(x=1:nrows, y=pm)) +
#     geom_line(data=r.sub1, aes(x=1:nrows, y=pm)) + 
#     geom_errorbar(data=r.sub2, width=.1, aes(x=nrows+2, ymin=pm.low, ymax=pm.high)) +
#     geom_point(data=r.sub2, aes(x=nrows+2, y=pm)) +
#     geom_line(data=r.sub2, aes(x=nrows+2, y=pm)) + 
#     theme_bw() + 
#     theme(panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank()) +
#     xlab("df.combo") + ylab("% Changes given 10ug/m3 increase in PM2.5") +
#     ggtitle(paste("lag", ilag)) +
#     scale_x_continuous(breaks = 1:length(data.vis$df.combo),
#                        labels = data.vis$df.combo) +
#     geom_hline(yintercept=0, linetype="dashed", alpha=.6)
#   p0
#   plot.out[[iplot]] = p0
#   iplot = iplot + 1
# 
# }
# 
# pdf(file.pdf, width = 12, height = 3 * length(plot.out))
# do.call('grid.arrange',c(plot.out, ncol = 1, top = "global model"))
# dev.off()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ##################### visualize ExeDistLagModel.R #####################
# dt1 = read.csv("GlobalModel/*lag0to28.cases.df522.csv")
# dt2 = read.csv("GlobalModelDistributed/*lagdf522.0to14.cases.csv") # result.rbind
# # dt1 = read.csv("GlobalModel/*lag0to28.deaths.df522.csv")
# # dt2 = read.csv("GlobalModelDistributed/*lagdf522.0to14.deaths.csv") # result.rbind
# # combine
# up = dim(dt2)[1]
# dt0 = dt1[1:up-1,]
# dt0[nrow(dt0)+1, ] = tail(dt2, 1)
# dt0
# 
# exp.trans = function(ls, delta = 10) {
#   return((exp(ls * delta) - 1) * 100)
# }
# dt0$coef = exp.trans(dt0$coef)
# dt0$ci.low = exp.trans(dt0$ci.low)
# dt0$ci.high = exp.trans(dt0$ci.high)
# 
# if (!isempty(dt0)) {
#   
#   plot.out = list()
#   
#   p1 = ggplot(dt0, aes(x=c(0:(up-2), up))) +
#     geom_errorbar(width=.1, aes(ymin=ci.low, ymax=ci.high), colour="black") + 
#     geom_point(aes(y=coef)) + 
#     xlab(paste0("Single Lag Model (Lag 0 - Lag", up-2, ") and Unconstrained Distributed-lag Model")) + 
#     ylab(paste0("% ", cause, " change given \n10ug/m3 increase in PM2.5")) + 
#     theme_bw() + 
#     theme(panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank()) +
#     # axis.title.x = element_text(size = 16),
#     # axis.text = element_text(size = 14),
#     # axis.title.y = element_text(size = 16)
#     scale_x_continuous(breaks = c(0:(up-2), up), 
#                        labels=c(as.character(0:(up-2)), "unconstrained\ndistributed-lag"))
#   p1
#   plot.out[[1]] = p1
#   
#   pdf(file.pdf, width = 8, height = 4 * length(plot.out))
#   do.call('grid.arrange',c(plot.out, ncol = 1))
#   dev.off()
# }