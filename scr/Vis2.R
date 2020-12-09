### single lag model up to 14/21, with the unconstrained model on the right side
setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")

################## for testing 
# input.file = "output/OneBand.DistLag21.csv"
# input.file = "output/TwoBand.DistLag21.csv"
# input.file = "output/OneBand.DistLag21.withMobility.v1.csv"
# input.file = "output/TwoBand.DistLag21.withMobility.v1.csv"
################## 

input.file.lists = list()
input.file.lists = list.append(input.file.lists, 
                                 "output/OneBand.DistLag21.csv")
input.file.lists = list.append(input.file.lists,
                                 "output/TwoBand.DistLag21.csv")
input.file.lists = list.append(input.file.lists, 
                                 "output/OneBand.DistLag21.withMobility.v1.csv")
input.file.lists = list.append(input.file.lists, 
                                 "output/TwoBand.DistLag21.withMobility.v1.csv")


### set up 
causes = c("cases", "deaths")
mlags = c(14, 21)
mobility.options = c(1, 0)


for (i.file in input.file.lists) {
  input.file = i.file[[1]]
  
  ### read data 
  r.dist = read.csv(input.file)
  r.dist$df.combo = paste0(r.dist$df.date, ",", r.dist$df.tmmx)
  
  ### get info from file name and set up 
  split.file.name = unlist(strsplit(input.file, "[./]"))
  wmobility = ifelse("withMobility" %in% split.file.name, T, F)
  if ("TwoBand" %in% split.file.name) {
    pollus = list(c("base", "base.low", "base.high", "PM2.5 Baseline"), 
                  c("hazard", "hazard.low", "hazard.high", "PM2.5 Driven by Wildfire"))
  } else pollus = list(c("pm", "pm.low", "pm.high", "PM2.5"))
  
  
  for (mlag in mlags) {
    file.pdf = paste0("output/vis2.pollu", length(pollus), ".lag", mlag, ".df.unconstrained.pdf")
    if (wmobility==T) file.pdf = paste0("output/vis2.pollu", length(pollus), ".lag", mlag, ".df.unconstrained.wMobility.pdf")
    # file.pdf = paste0("output/vis2.pollu", length(pollus), ".lag", mlag, ".df.unconstrained.[", Sys.time(), "].pdf")
    # if (wmobility==T) file.pdf = paste0("output/vis2.pollu", length(pollus), ".lag", mlag, ".df.unconstrained.wMobility[", Sys.time(), "].pdf")
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
          
          xstr = paste0("Unconstrained Distributed-lag Model\n with different degrees of freedom")
          if (mobility == T) xstr = paste0(xstr, ", adjusted for mobility")
          ystr = paste0("% ", cause, " change given in \n10ug/m3 increase in PM2.5")
          ttstr = paste0(ipollu[[4]], " ", mlag, "lags")
  
          p0 = ggplot() +
            geom_errorbar(data=sub.dist, width=.2, 
                          aes_string(x=1:nrows, ymin=ipollu[2], ymax=ipollu[3]), group=1) +
            geom_point(data=sub.dist, 
                       aes_string(x=1:nrows, y=ipollu[1]), group=1) + 
            theme_bw() + 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank()) +
            xlab(xstr) + ylab(ystr) + ggtitle(ttstr) +
            scale_x_continuous(breaks = 1:nrows, labels = sub.dist$df.combo) +
            geom_hline(yintercept=0, linetype="dotted")
          
          plot.out[[iplot]] = p0
          iplot = iplot + 1
        }
      }
    }
    pdf(file.pdf, width = 6, height = 4 * length(plot.out))
    do.call('grid.arrange',c(plot.out, ncol = 1, top = ""))
    dev.off()
  }
}