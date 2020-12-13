# Exploratory Data Analysis 
# function: visualize the PM2.5 and case count and death count in pairs

setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
df = load.data()
df = df[df$date >= "2020-03-15", ]
df = df[df$date <= "2020-09-24", ]
########################### top counties ################################
### set up 
n.col.grid = 6
file.name = paste0("output/pm&cases&deaths.top10z.pdf")
fips.unique = unique(df$FIPS[order(df$population, decreasing=TRUE)])[1:10]

### TODO ???????????????????
df$start.hazard = as.Date("2020-03-15")
df$end.hazard = as.Date("2020-03-15")
df$start.hazard[!is.na(df$pmhazard)&(df$pmhazard > 0)] = df$date[!is.na(df$pmhazard)&(df$pmhazard > 0)] - 1
df$end.hazard[!is.na(df$pmhazard)&(df$pmhazard > 0)] = df$date[!is.na(df$pmhazard)&(df$pmhazard > 0)] + 1


# g_legend<-function(a.gplot){
#   tmp <- ggplot_gtable(ggplot_build(a.gplot))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   return(legend)}


plot.list = list()
iplot = 1

for (ifips in fips.unique) {
  df.selected = df[df$FIPS == ifips, ]

  ### visualize PM2.5
  p1 = ggplot() + 
    geom_rect(data=df.selected, aes(xmin = start.hazard, xmax = end.hazard,
                  ymin = -Inf, ymax = Inf), fill = "grey", colour =NA, alpha = 1) + 
    geom_point(data=df.selected, aes_string(x="date", y="pm25", color=shQuote("PM25")), size = .1) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          # axis.text = element_text(size = 11),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          plot.title = element_text(hjust = 0.5)) + 
    scale_color_manual(name="", values=c(PM25="blue")) +
    xlab("Date") + ggtitle(ifips)
 
  # if (iplot == 1) mylegend1 = g_legend(p1)
  p1 = p1 + theme(legend.position = "none")
    # p1 = p1 + theme(legend.title = element_blank(), 
    #                 legend.position = c(.3, .8),
    #                 legend.box = "horizontal")
                    # legend.text = element_text(size = 11, colour = "black"))
  
  plot.list[[iplot]] = p1
  iplot = iplot + 1
  
  ### visualize number of cases
  p2 = ggplot() +
    geom_rect(data=df.selected, aes(xmin = start.hazard, xmax = end.hazard,
                                    ymin = -Inf, ymax = Inf), fill = "grey", colour =NA, alpha = 1) + 
    geom_point(data=df.selected, aes_string(x="date", y="cases", color=shQuote("Cases")), size = .1) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          # axis.text = element_text(size = 11),
          axis.title.x = element_blank(),  
          axis.title.y = element_blank(),  
          # legend.position = "none",
          plot.title = element_text(hjust = 0.5)) + 
    scale_color_manual(name="", values=c(Cases="red")) +
    xlab("Date") + ggtitle("")
  
  # if (iplot == 2) mylegend2 = g_legend(p2)
  p2 = p2 + theme(legend.position = "none")
    # p2 = p2 + theme(legend.title = element_blank(), 
    #                 legend.position = c(.3, .8),
    #                 legend.box = "horizontal")
  # legend.text = element_text(size = 11, colour = "black"))
  
  plot.list[[iplot]] = p2
  iplot = iplot + 1
  
  ### visualize number of cases
  p3 = ggplot() +
    geom_rect(data=df.selected, aes(xmin = start.hazard, xmax = end.hazard,
                                    ymin = -Inf, ymax = Inf), fill = "grey", colour =NA, alpha = 1) + 
    geom_point(data=df.selected, aes_string(x="date", y="deaths", color=shQuote("Deaths")), size = .1) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          # axis.text = element_text(size = 11),
          # legend.position = "none",
          axis.title.x = element_blank(),  
          axis.title.y = element_blank(),  
          plot.title = element_text(hjust = 0.5)) + 
    scale_color_manual(name="", values=c(Deaths="black")) +
    xlab("Date") + ggtitle("")
  # if (iplot == 3) mylegend3 = g_legend(p3)
  p3 = p3 + theme(legend.position = "none")
    # p3 = p3 + theme(legend.title = element_blank(), 
    #                 legend.position = c(.3, .8),
    #                 legend.box = "horizontal")
  # legend.text = element_text(size = 11, colour = "black"))

  plot.list[[iplot]] = p3
  iplot = iplot + 1
}
# mylegend1, mylegend2, mylegend3
# https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots 
pdf(file.name, width = 10, height = 8)
do.call('grid.arrange',c(plot.list, ncol = n.col.grid, 
                         top = "PM2.5 (blue, ug/m3), COVID-19 incidences (red) and deaths (black) of the most populated counties (FIPS)",
                         bottom="", left=""))
dev.off()


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
