# Exploratory Data Analysis 
# function: visualize the PM2.5 and case count in pairs
# output file name: pm&cases.raw.pdf

library(splines)
library(ggplot2)
library(gridExtra)
library(pracma)

setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
df = load.data.xz1()
df = df[df$pm25 <= 20, ]

### set up 
n.col.grid = 8
file.name = paste0("ExploratoryDataAnalysis/pm.low20&cases.pdf")

fips.unique = unique(na.omit(df$FIPS[order(df$population, decreasing=TRUE)]))

pdf(file.name, width = 12, height = 200/n.col.grid)

plot.list = list()
for (ifips in 1:length(fips.unique)) {

  iplot = (ifips-1)*2+1
  
  df.selected = df[df$FIPS == fips.unique[ifips], ]
  print(dim(df.selected))
  if (dim(df.selected)[1] == 0) {
    next 
  }
  
  ### visualize PM2.5
  p1 = ggplot() +
    geom_point(data=df.selected, aes(x=date, y=pm25), colour="blue", size = .1) +
    theme(legend.position="none", axis.title.x=element_blank(),
          axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
          axis.text.y=element_blank(), axis.ticks.y=element_blank())# + 
    # ggtitle(as.character(fips.unique[ifips]))
  
  ### visualize number of cases
  p2 = ggplot() +
    geom_point(data=df.selected, aes(x=date, y=cases), colour="red", size = .1) +
    theme(legend.position="none", axis.title.x=element_blank(),
          axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
          axis.text.y=element_blank(), axis.ticks.y=element_blank())

  plot.list[[iplot]] = p1
  plot.list[[iplot+1]] = p2

}
do.call('grid.arrange',c(plot.list, ncol = n.col.grid, top = "Raw Value of PM2.5 case count"))
dev.off()
