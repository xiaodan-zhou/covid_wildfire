library(splines)
library(ggplot2)
library(gridExtra)


plot.splines = function(smooth="ns", df=1) {
  
  n.df = df
  n.sample = 100
  xx = 1:n.sample/n.sample
  
  if (smooth == "ns") {
    dt = ns(xx, df=n.df)
  } else {
    dt = bs(xx, df=n.df)
  }
  dt = data.frame(value = matrix(dt, ncol = 1, byrow = FALSE),
                  splines = as.factor(rep(1:n.df, each=n.sample)), 
                  xx = rep(xx, n.df)) 
  
  p0 = ggplot(data=dt) + 
    geom_line(aes(x=xx, y=value, color=splines))
  if (smooth == "ns") {
    p0 = p0 + ggtitle(paste("natural cubic spline-", n.df))
  } else {
    p0 = p0 + ggtitle(paste("bspline-", n.df))
  }
  return(p0)
}



setwd("/Users/mac/Documents/GitHub/covid_wildfire")

file.name = paste0("ExploratoryDataAnalysis/natural.splines.pdf")
pdf(file.name, width = 12, height = 5 * 4)

plot.out = list()
for (i in 1:5) {
  plot.out[[i]] = plot.splines(smooth="ns", df=i)
}
do.call('grid.arrange',c(plot.out, ncol = 1))
dev.off()



file.name = paste0("ExploratoryDataAnalysis/b.splines.pdf")
pdf(file.name, width = 12, height = 3 * 4)

plot.out = list()
for (i in 3:5) {
  plot.out[[i-2]] = plot.splines(smooth="bs", df=i)
}
do.call('grid.arrange',c(plot.out, ncol = 1))
dev.off()

