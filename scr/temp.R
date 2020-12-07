##################### ????????? supp ????????????????????? ###################


setwd("/Users/mac/Documents/GitHub/covid_wildfire")
r.single = read.csv("output/OneBand.singleLag21[2020-12-06 13:11:57].csv")
r.dist = read.csv("output/OneBand.DistLag21[2020-12-06 13:13:50].csv")

file.pdf = "output/p1.pdf"

plot.out = list()
iplot = 1

for (ilag in lags.to.run) {
  dsub = result[r.single$lag == ilag,]
  
  nrows = dim(dsub)[1]
  
  p0 = ggplot(data=dsub, aes(x=(1:nrows))) +
    geom_errorbar(width=.1, aes(ymin=pm.low, ymax=pm.high)) +
    geom_point(aes(y=pm)) +
    geom_line(aes(y=pm)) + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    xlab("df.combo") + ylab("PM2.5 coefficients") +
    ggtitle(paste("lag", ilag)) +
    scale_x_continuous(breaks = 1:length(data.vis$df.combo),
                       labels = data.vis$df.combo) +
    geom_hline(yintercept=0, linetype="dashed", alpha=.6)
  p0
  plot.out[[iplot]] = p0
  iplot = iplot + 1
  
}

pdf(file.pdf, width = 12, height = 3 * length(plot.out))
do.call('grid.arrange',c(plot.out, ncol = 1, top = "global model"))
dev.off()








