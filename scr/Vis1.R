### single lag model up to 14/21, with the unconstrained model on the right side
setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")

################## 
# r.single = read.csv("output/OneBand.singleLag21[2020-12-06 13:11:57].csv")
# r.dist = read.csv("output/OneBand.DistLag21[2020-12-06 16:04:22].csv")
# pollus = list(c("pm", "pm.low", "pm.high"))
################## 

################## 
r.single = read.csv("output/TwoBand.singleLag21[2020-12-06 13:12:34].csv")
r.dist = read.csv("output/TwoBand.DistLag21[2020-12-06 13:15:23].csv")
pollus = list(c("base", "base.low", "base.high"), c("hazard", "hazard.low", "hazard.high"))
################## 


causes = c("cases", "deaths")
mlags = c(14, 21)
mobility.options = c(1, 0)
df.date = 5
df.tmmx = 2
df.rmax = 2

for (mlag in mlags) {
  file.pdf = paste0("output/vis1.pollu", length(pollus), ".lag", mlag, "[", Sys.time(), "].pdf")
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
      
      for (ipollu in pollus) {
        
        xstr = paste0("Single Lag Model (Lag 0 - Lag", mlag, ") and the corresponding Unconstrained Distributed-lag Model")
        if (mobility == 1) xstr = paste0(xstr, ", adjusted for mobility")
        ystr = paste0("% ", cause, " change given in \n10ug/m3 increase in PM2.5")
        ttstr = as.character(ipollu[[1]])
        x.loc = c(1:(mlag+1), mlag+3)
        x.lab = as.character(c(0:mlag, "Unconstrained \nDistributed-lag Model"))
        
        p0 = ggplot() +
          geom_errorbar(data=sub.single, width=.1, 
                        aes_string(x=1:(mlag+1), ymin=ipollu[2], ymax=ipollu[3])) +
          geom_point(data=sub.single, 
                     aes_string(x=1:(mlag+1), y=ipollu[1])) +
          geom_errorbar(data=sub.dist, width=.1, 
                        aes_string(x=mlag+3, ymin=ipollu[2], ymax=ipollu[3]), group=1) +
          geom_point(data=sub.dist, 
                     aes_string(x=mlag+3, y=ipollu[1]), group=1) + 
          theme_bw() + 
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank()) +
          xlab(xstr) + ylab(ystr) + ggtitle(ttstr) +
          scale_x_continuous(breaks = x.loc, labels = x.lab) +
          geom_hline(yintercept=0, linetype="dashed", alpha=.6)
        
        plot.out[[iplot]] = p0
        iplot = iplot + 1
      }
    }
  }
  pdf(file.pdf, width = 8, height = 3 * length(plot.out))
  do.call('grid.arrange',c(plot.out, ncol = 1, top = ""))
  dev.off()
}