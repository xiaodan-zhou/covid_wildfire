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

### manipulate the fireday 
# df$fireday[df$pm25 >= 20] = 1
# Valid California FIPS codes are 06001 to 06115.
# df = df[(as.integer(as.character(df$FIPS)) >= 6001) & (as.integer(as.character(df$FIPS)) <= 6115), ]

### set up 
n.col.grid = 8
file.name = paste0("ExploratoryDataAnalysis/pm.fireday.CA.pdf")

fips.unique = unique(df$FIPS[order(df$population, decreasing=TRUE)])

pdf(file.name, width = 12, height = 12)

plot.list = list()
iplot = 1
for (ifips in 1:length(fips.unique)) { #

  df.selected = df[df$FIPS == fips.unique[ifips], ]
  df.selected = df.selected[!is.na(df.selected$pm25),]
  if (dim(df.selected)[1] == 0) {
    next 
  }
  ### visualize PM2.5
  p1 = ggplot() +
    geom_point(data=df.selected, aes(x=date, y=pm25,col=as.factor(fireday)), size = .1) +
    theme(legend.position="none", 
          axis.title.x=element_blank(), axis.title.y=element_blank(), 
          axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
          axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
    ylim(c(0,100)) # + 
    # ggtitle(as.character(fips.unique[ifips]))
  plot.list[[iplot]] = p1
  iplot = iplot + 1

}
do.call('grid.arrange',c(plot.list, ncol = n.col.grid, top = "CA Wildfire Day")) #  or PM>=20
dev.off()

