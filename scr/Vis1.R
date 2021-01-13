### single lag model up to 14/21, with the unconstrained model on the right side
### output name example simple_n_unconstrained_lag14_df622_cases_mobility.pdf
setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
library(latex2exp)

input.file.lists = list()
input.file.lists[[1]] = c("output/OneBand.singleLag21.wMobility.csv",
                          "output/OneBand.DistLag21.wMobility.csv")

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

  for (mlag in mlags) {
    for (cause in causes) {
      for (mobility in mobility.options) {
        
        file.pdf = paste0("output/simple_n_unconstrained_lag", mlag, "_df", df.date, df.tmmx, df.rmax, "_", cause)
        if (mobility) file.pdf = paste0(file.pdf, "_mobility")
        file.pdf = paste0(file.pdf, "[", Sys.time(), "]")
        file.pdf = paste0(file.pdf, ".pdf")
        print(file.pdf)

        plot.out = list()
        iplot = 1
        # xstr = paste0("Single Lag Model (Lag 0 - Lag", mlag, ") \n and the corresponding Unconstrained Distributed-lag Model")
        # ystr = paste0("% ", cause, " change given \n10ug/m3 increase in PM2.5") 
        # ystr = TeX("\\overset{Percentage of COVID19 deaths change }{given $10 \\mu g/m^3$ increase in $PM_{2.5}$}")
        # ystr = TeX("Percentage of COVID19 cases change \newline given $10 \\mu g/m^3$ increase in $PM_{2.5}$")
        
        xstr = paste0("Single Lag Model and the Unconstrained Distributed-lag Model")
        if (mobility == 1) xstr = paste0(xstr, ", adjusted for mobility")
        if (cause == "cases") {
          ystr = "Percent of COVID19 Cases"
        } else {
          ystr = "Percent of COVID19 Deaths"
        }
        
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
        
        if ((dim(sub.single)[1] == 0)|(dim(sub.dist)[1] == 0)) {
          print("skip")
          next
        }
        
        if (cause == "cases") {
          p0 = ggplot() +
            geom_errorbar(data=sub.single, width=.1, 
                          aes_string(x=1:(mlag+1), ymin="pm.low", ymax="pm.high")) +
            geom_point(data=sub.single, 
                       aes_string(x=1:(mlag+1), y="pm"), shape=21, fill="white") +
            geom_errorbar(data=sub.dist, width=.1, 
                          aes_string(x=mlag+3, ymin="pm.low", ymax="pm.high"), group=1) +
            geom_point(data=sub.dist, 
                       aes_string(x=mlag+3, y="pm"), shape=21, fill="white", group=1)
        }
        else {
          p0 = ggplot() +
            geom_errorbar(data=sub.single, width=.1, 
                          aes_string(x=1:(mlag+1), ymin="pm.low", ymax="pm.high")) +
            geom_point(data=sub.single, 
                       aes_string(x=1:(mlag+1), y="pm")) +
            geom_errorbar(data=sub.dist, width=.1, 
                          aes_string(x=mlag+3, ymin="pm.low", ymax="pm.high"), group=1) +
            geom_point(data=sub.dist, 
                       aes_string(x=mlag+3, y="pm"), group=1)
        }
        p0 = p0 + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
          xlab(xstr) + ylab(ystr) + ggtitle("") +
          scale_x_continuous(breaks = c(1:(mlag+1), mlag+3),
                             labels = as.character(c(0:mlag, "Distributed\nlag"))) +
          geom_hline(yintercept=0, linetype="dashed", alpha=.6)

        
        plot.out[[iplot]] = p0
        iplot = iplot + 1
        print("generating...")
        pdf(file.pdf, width = 6, height = 4 * length(plot.out))
        do.call('grid.arrange',c(plot.out, ncol = 1, top = ""))
        dev.off()
      }
    }
  }
}

