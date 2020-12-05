# Exploratory Data Analysis 
# function: visualize the PM2.5 and case count in pairs
# output file name: pm&cases.raw.pdf

# library(splines)
# library(pracma)
library(ggplot2)
library(gridExtra)
library(grid)


setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
df = load.data.xz1()


## ?????? 
df$pm25 = df$pmhazard
########################### top counties ################################
### set up 
n.col.grid = 6
file.name = paste0("ExploratoryDataAnalysis/pm&cases_publishHAZARD.pdf")
fips.unique = unique(df$FIPS[order(df$population, decreasing=TRUE)])[1:10]

pdf(file.name, width = 12, height = 8)
plot.list = list()
iplot = 1
for (ifips in 1:length(fips.unique)) {
  # iplot = (ifips-1)*2+1
  df.selected = df[df$FIPS == fips.unique[ifips], ]
  
  ### visualize PM2.5
  p1 = ggplot() +
    geom_point(data=df.selected, aes(x=date, y=pm25, colour="PM2.5"), colour="blue", size = .1) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          #panel.background = element_blank(), 
          #axis.line = element_line(colour = "black"), 
          #axis.text = element_text(size = 10),
          axis.title.x = element_blank(), # element_text(size = 16),
          axis.title.y = element_blank(), # element_text(size = 16),
          axis.ticks.y = element_blank(), 
          plot.title = element_text(hjust = 0.5)) + 
    xlab("Date") + ggtitle(df.selected$FIPS[1]) # ylab("PM2.5 (ug/m3)") +
  plot.list[[iplot]] = p1
  iplot = iplot + 1
  
  ### visualize number of cases
  p2 = ggplot() +
    geom_point(data=df.selected, aes(x=date, y=cases, colour="Cases"), colour="red", size = .1) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),  
          axis.title.y = element_blank(),  
          plot.title = element_text(hjust = 0.5)) + 
    xlab("Date") + ggtitle("")
  plot.list[[iplot]] = p2
  iplot = iplot + 1
  
  ### visualize number of cases
  p3 = ggplot() +
    geom_point(data=df.selected, aes(x=date, y=deaths, colour="deaths"), colour="black", size = .1) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),  
          axis.title.y = element_blank(),  
          plot.title = element_text(hjust = 0.5)) + 
    xlab("Date") + ggtitle("")
  plot.list[[iplot]] = p3
  iplot = iplot + 1
}

do.call('grid.arrange',c(plot.list, ncol = n.col.grid, 
                         top = "PM2.5 (blue, ug/m3), COVID-19 incidences (red) and deaths (black) of the most populated counties (FIPS)",
                         bottom="", left=""))
dev.off()

# library(ggpubr)
# ggarrange(plotlist = plot.list, ncol=n.col.grid, nrow=2, common.legend = TRUE, legend="bottom")


########################### all counties ################################
# ### set up 
# n.col.grid = 8
# file.name = paste0("ExploratoryDataAnalysis/pm&cases_raw_moddat2.pdf")
# 
# fips.unique = unique(df$FIPS[order(df$population, decreasing=TRUE)])
# 
# pdf(file.name, width = 12, height = 200/n.col.grid)
# 
# plot.list = list()
# for (ifips in 1:length(fips.unique)) {
# 
#   iplot = (ifips-1)*2+1
#   
#   df.selected = df[df$FIPS == fips.unique[ifips], ]
#   
#   ### visualize PM2.5
#   p1 = ggplot() +
#     geom_point(data=df.selected, aes(x=date, y=pm25), colour="blue", size = .1) +
#     theme(legend.position="none", axis.title.x=element_blank(),
#           axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
#           axis.text.y=element_blank(), axis.ticks.y=element_blank())# + 
#     # ggtitle(as.character(fips.unique[ifips]))
#   
#   ### visualize number of cases
#   p2 = ggplot() +
#     geom_point(data=df.selected, aes(x=date, y=cases), colour="red", size = .1) +
#     theme(legend.position="none", axis.title.x=element_blank(),
#           axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
#           axis.text.y=element_blank(), axis.ticks.y=element_blank())
# 
#   plot.list[[iplot]] = p1
#   plot.list[[iplot+1]] = p2
# 
# }
# do.call('grid.arrange',c(plot.list, ncol = n.col.grid, top = "Raw Value of PM2.5 case count"))
# dev.off()
