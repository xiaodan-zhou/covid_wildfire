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
                               c("output/OneBand.singleLag21.csv",
                                 "output/OneBand.DistLag21.csv"))
input.file.lists = list.append(input.file.lists, 
                               c("output/TwoBand.singleLag21.csv",
                                 "output/TwoBand.DistLag21.csv"))
input.file.lists = list.append(input.file.lists, 
                               c("output/OneBand.singleLag21.withMobility.v1.csv",
                                 "output/OneBand.DistLag21.withMobility.v1.csv"))
input.file.lists = list.append(input.file.lists, 
                               c("output/TwoBand.singleLag21.withMobility.v1.csv",
                                 "output/TwoBand.DistLag21.withMobility.v1.csv"))

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
    file.pdf = paste0("output/vis1.pollu", length(pollus), ".lag", mlag, "df.", df.date, df.tmmx, ".pdf")  
    if (wmobility==T) file.pdf = paste0("output/vis1.pollu", length(pollus), ".lag", mlag, "df.", df.date, df.tmmx, ".wMobility.pdf") 
    # file.pdf = paste0("output/vis1.pollu", length(pollus), ".lag", mlag, "df.", df.date, df.tmmx, "[", Sys.time(), "].pdf")
    # if (wmobility==T) file.pdf = paste0("output/vis1.pollu", length(pollus), ".lag", mlag, "df.", df.date, df.tmmx, ".wMobility[", Sys.time(), "].pdf") 
    
    print(file.pdf)
    plot.out = list()
    iplot = 1
    
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
      # sub.single = sub.single[sub.single$cause == cause,]
      # sub.dist = sub.dist[sub.dist$cause == cause,]
      
      ### choose mobility 
      sub.single = sub.single[sub.single$mobility == mobility,]
      sub.dist = sub.dist[sub.dist$mobility == mobility,]
      
      
      if ((dim(sub.single)[1] == 0)|(dim(sub.dist)[1] == 0))
        next
      
      for (ipollu in pollus) {
        eps = 0.20
        xxm = 1:(mlag+1)-eps
        xxm2 = mlag+3-eps
        xxp = 1:(mlag+1)+eps
        xxp2 = mlag+3+eps
        x.loc = c(1:(mlag+1), mlag+3)
        x.lab = as.character(c(0:mlag, "Unconstrained \nDistributed-lag"))
        
        xstr = paste0("Single Lag Model (Lag 0 - Lag", mlag, ") \n and Unconstrained Distributed-lag Model")
        if (mobility == 1) xstr = paste0(xstr, ", adjusted for mobility")
        ystr = paste0("% change given in \n10ug/m3 increase in PM2.5")
        ttstr = as.character(ipollu[[4]])
        
        p0 = ggplot() +
          geom_errorbar(data=sub.single[sub.single$cause == "cases",], width=.2, 
                        aes_string(x=xxm, ymin=ipollu[2], ymax=ipollu[3]), col="black") +
          geom_errorbar(data=sub.dist[sub.dist$cause == "cases",], width=.2, 
                        aes_string(x=xxm2, ymin=ipollu[2], ymax=ipollu[3]), col="black", group=1) +
          geom_point(data=sub.single[sub.single$cause == "cases",], 
                     aes_string(x=xxm, y=ipollu[1], 
                                color=shQuote("incidences"))) +
          geom_point(data=sub.dist[sub.dist$cause == "cases",], 
                     aes_string(x=xxm2, y=ipollu[1], 
                                color=shQuote("incidences")), group=1) + 
          geom_errorbar(data=sub.single[sub.single$cause == "deaths",], width=.2, 
                        aes_string(x=xxp, ymin=ipollu[2], ymax=ipollu[3]), col="black") +
          geom_errorbar(data=sub.dist[sub.dist$cause == "deaths",], width=.2, 
                        aes_string(x=xxp2, ymin=ipollu[2], ymax=ipollu[3]), col="black", group=1) +
          geom_point(data=sub.single[sub.single$cause == "deaths",], 
                     aes_string(x=xxp, y=ipollu[1], 
                                fill=shQuote("mortality")), shape=21) +
          geom_point(data=sub.dist[sub.dist$cause == "deaths",], 
                     aes_string(x=xxp2, y=ipollu[1],
                                fill=shQuote("mortality")), shape=21, group=1) +
          theme_bw() + 
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                legend.title = element_blank(), 
                legend.position = c(.3, .8),
                legend.box = "horizontal", 
                legend.text = element_text(size = 12, colour = "black")) +
          xlab(xstr) + ylab(ystr) + ggtitle(ttstr) +
          scale_x_continuous(breaks = x.loc, labels = x.lab) + 
          scale_fill_manual(name="", values=c( mortality="white")) +
          scale_color_manual(name="", values=c(incidences="black")) + 
          geom_hline(yintercept=0, linetype="dotted")
        p0
        plot.out[[iplot]] = p0
        iplot = iplot + 1
      }
    } 
    pdf(file.pdf, width = 6, height = 4 * length(plot.out))
    do.call('grid.arrange',c(plot.out, ncol = 1, top = ""))
    dev.off()
  }

}
