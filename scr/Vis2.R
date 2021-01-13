### output name example df_sensitivity_(un)constrained_lag14_cases_mobility.pdf
setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")

input.file.lists = list()
input.file.lists[[1]] = "output/OneBand.DistLag21.wMobility.csv"

### set up 
causes = c("cases", "deaths")
mlags = c(14, 21)
mobility.options = c(1, 0)


for (i.file in input.file.lists) {
  input.file = i.file[[1]]
  
  ### read data 
  r.dist = read.csv(input.file)
  r.dist$df.combo = paste0(r.dist$df.date, ",", r.dist$df.tmmx)

  for (mlag in mlags) {
    for (cause in causes) {
      for (mobility in mobility.options) {
        
        file.pdf = paste0("output/df_sensitivity_(un)constrained_lag", mlag, "_", cause)
        if (mobility) file.pdf = paste0(file.pdf, "_mobility")
        file.pdf = paste0(file.pdf, "[", Sys.time(), "]")
        file.pdf = paste0(file.pdf, ".pdf")
        print(file.pdf)
        
        plot.out = list()
        iplot = 1
        
        # xstr = paste0("Unconstrained Distributed-lag Model\n with different degrees of freedom")
        # if (mobility == T) xstr = paste0(xstr, ", adjusted for mobility")
        xstr = paste0("Degrees of Freedom")
        
        if (cause == "cases") {
          ystr = "Percent of COVID19 Cases"
        } else {
          ystr = "Percent of COVID19 Deaths"
        }

        ### set maximum lag
        sub.dist = r.dist[r.dist$max.lag == mlag,]
        
        ### choose cause
        sub.dist = sub.dist[sub.dist$cause == cause,]
        
        ### choose mobility 
        sub.dist = sub.dist[sub.dist$mobility == mobility,]
        
        nrows = dim(sub.dist)[1]
        if (nrows == 0) {
          print("skip")
          next
        }

        if (cause == "cases") {
          p0 = ggplot() +
            geom_errorbar(data=sub.dist, width=.2, 
                          aes_string(x=1:nrows, ymin="pm.low", ymax="pm.high"), group=1) +
            geom_point(data=sub.dist, 
                       aes_string(x=1:nrows, y="pm"), shape=21, fill="white",group=1) + 
            theme_bw() + 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank()) +
            xlab(xstr) + ylab(ystr) + ggtitle("") +
            scale_x_continuous(breaks = 1:nrows, labels = sub.dist$df.combo) +
            geom_hline(yintercept=0, linetype="dashed")
        } 
        else {
          p0 = ggplot() +
            geom_errorbar(data=sub.dist, width=.2, 
                          aes_string(x=1:nrows, ymin="pm.low", ymax="pm.high"), group=1) +
            geom_point(data=sub.dist, 
                       aes_string(x=1:nrows, y="pm"), group=1) + 
            theme_bw() + 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank()) +
            xlab(xstr) + ylab(ystr) + ggtitle("") +
            scale_x_continuous(breaks = 1:nrows, labels = sub.dist$df.combo) +
            geom_hline(yintercept=0,  linetype="dashed")
        }
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

