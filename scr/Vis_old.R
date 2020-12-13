### single lag model up to 14/21, with the unconstrained model on the right side
setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")


################## for testing 
# input.file.sinle = "output/OneBand.singleLag21.csv"
# input.file.dist = "output/OneBand.DistLag21.csv"
# input.file.sinle = "output/TwoBand.singleLag21.csv"
# input.file.dist = "output/TwoBand.DistLag21.csv"
# input.file.sinle = "output/OneBand.singleLag21.withMobility.v1.csv"
# input.file.dist = "output/OneBand.DistLag21.withMobility.v1.csv"
# input.file.sinle = "output/TwoBand.singleLag21.withMobility.v1.csv"
# input.file.dist = "output/TwoBand.DistLag21.withMobility.v1.csv"
################## 

input.file.lists = list()
input.file.lists = list.append(input.file.lists, 
                               c("output/OneBand.singleLag21.PMBase.csv",
                                 "output/OneBand.DistLag21.PMBase.csv"))
# input.file.lists = list.append(input.file.lists, 
#                                c("output/TwoBand.singleLag21.csv",
#                                  "output/TwoBand.DistLag21.csv"))
# input.file.lists = list.append(input.file.lists, 
#                                c("output/OneBand.singleLag21.withMobility.v1.csv",
#                                  "output/OneBand.DistLag21.withMobility.v1.csv"))
# input.file.lists = list.append(input.file.lists, 
#                                c("output/TwoBand.singleLag21.withMobility.v1.csv",
#                                  "output/TwoBand.DistLag21.withMobility.v1.csv"))

### set up 
causes = c("cases", "deaths")
mlags = c(14, 21)
mobility.options = c(1, 0)
df.date = 6
df.tmmx = 2
df.rmax = 2

for (i.file in input.file.lists) {
  input.file.sinle = i.file[1]
  input.file.dist = i.file[2]
  
  ### read data 
  r.single = read.csv(input.file.sinle)
  r.dist = read.csv(input.file.dist)
  r.dist$df.combo = paste0(r.dist$df.date, ",", r.dist$df.tmmx)
  
    ### get info from file name and set up 
  split.file.name = unlist(strsplit(input.file.sinle, "[./]"))
  wmobility = ifelse("withMobility" %in% split.file.name, T, F)
  if ("TwoBand" %in% split.file.name) {
    pollus = list(c("base", "base.low", "base.high", "PM2.5 Baseline"), 
                  c("hazard", "hazard.low", "hazard.high", "PM2.5 Driven by Wildfire"))
  } else pollus = list(c("pm", "pm.low", "pm.high", "PM2.5"))
  
  
  for (mlag in mlags) {
    # file.pdf = paste0("output/vis1.pollu", length(pollus), ".lag", mlag, "df.", df.date, df.tmmx, ".pdf")  
    # if (wmobility==T) file.pdf = paste0("output/vis1.pollu", length(pollus), ".lag", mlag, "df.", df.date, df.tmmx, ".wMobility.pdf") 
    file.pdf = paste0("output/vis1.pollu", length(pollus), ".lag", mlag, "df.", df.date, df.tmmx, "[", Sys.time(), "].pdf")
    if (wmobility==T) file.pdf = paste0("output/vis1.pollu", length(pollus), ".lag", mlag, "df.", df.date, df.tmmx, ".wMobility[", Sys.time(), "].pdf")
    
    print(file.pdf)
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
        
        xstr = paste0("Single Lag Model (Lag 0 - Lag", mlag, ") \n and the corresponding Unconstrained Distributed-lag Model")
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
                             labels = as.character(c(0:mlag, "Distributed\nlag"))) +
          geom_hline(yintercept=0, linetype="dashed", alpha=.6)
        
        plot.out[[iplot]] = p0
        iplot = iplot + 1
        }
    }
  pdf(file.pdf, width = 6, height = 4 * length(plot.out))
  do.call('grid.arrange',c(plot.out, ncol = 1, top = ""))
  dev.off()
  }
}