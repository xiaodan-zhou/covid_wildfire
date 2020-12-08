### single lag model up to 14/21, with the unconstrained model on the right side
setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")

################## 
# r.dist = read.csv("output/OneBand.DistLag21.csv")
# r.dist$df.combo = paste0(r.dist$df.date, ",", r.dist$df.tmmx)
# pollus = list(c("pm", "pm.low", "pm.high"))
################## 

################## 
# r.dist = read.csv("output/OneBand.DistLag21.withMobility.v1.csv")
# r.dist$df.combo = paste0(r.dist$df.date, ",", r.dist$df.tmmx)
# pollus = list(c("pm", "pm.low", "pm.high"))
# mobility=T
################## 

################## 
# r.dist = read.csv("output/TwoBand.DistLag21.csv")
# r.dist$df.combo = paste0(r.dist$df.date, ",", r.dist$df.tmmx)
# pollus = list(c("base", "base.low", "base.high"), c("hazard", "hazard.low", "hazard.high"))
################## 

################## 
r.dist = read.csv("output/TwoBand.DistLag21.withMobility.v1.csv")
r.dist$df.combo = paste0(r.dist$df.date, ",", r.dist$df.tmmx)
pollus = list(c("base", "base.low", "base.high"), c("hazard", "hazard.low", "hazard.high"))
mobility=T
################## 



causes = c("cases", "deaths")
mlags = 21
mobility.options = c(1, 0)


for (mlag in mlags) {
  file.pdf = paste0("output/vis2.pollu", length(pollus), ".df.unconstrained.[", Sys.time(), "].pdf")
  if (mobility==T) file.pdf = paste0("output/vis2.pollu", length(pollus), ".df.unconstrained.wMobility[", Sys.time(), "].pdf")
  plot.out = list()
  iplot = 1
  
  for (cause in causes) {
    for (mobility in mobility.options) {
      ### set maximum lag
      sub.dist = r.dist[r.dist$max.lag == mlag,]
      
      ### choose cause
      sub.dist = sub.dist[sub.dist$cause == cause,]
      
      ### choose mobility 
      sub.dist = sub.dist[sub.dist$mobility == mobility,]
      
      nrows = dim(sub.dist)[1]
      if (nrows == 0) {
        print("next")
        next
      }
      
      for (ipollu in pollus) {
        
        xstr = paste0("Unconstrained Distributed-lag Model with different degrees of freedom")
        if (mobility == T) xstr = paste0(xstr, ", adjusted for mobility")
        ystr = paste0("% ", cause, " change given in \n10ug/m3 increase in PM2.5")
        ttstr = as.character(ipollu[[1]])
        if (mobility == T) ttstr = paste0(ttstr, ", adjusted for mobility")

        p0 = ggplot() +
          geom_errorbar(data=sub.dist, width=.1, 
                        aes_string(x=1:nrows, ymin=ipollu[2], ymax=ipollu[3]), group=1) +
          geom_point(data=sub.dist, 
                     aes_string(x=1:nrows, y=ipollu[1]), group=1) + 
          theme_bw() + 
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank()) +
          xlab(xstr) + ylab(ystr) + ggtitle(ttstr) +
          scale_x_continuous(breaks = 1:nrows, labels = sub.dist$df.combo) +
          geom_hline(yintercept=0, linetype="dashed", alpha=.6)
        p0
        plot.out[[iplot]] = p0
        iplot = iplot + 1
      }
    }
  }
  pdf(file.pdf, width = 8, height = 3 * length(plot.out))
  do.call('grid.arrange',c(plot.out, ncol = 1, top = ""))
  dev.off()
}
