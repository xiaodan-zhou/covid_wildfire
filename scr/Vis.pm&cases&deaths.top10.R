# Exploratory Data Analysis 
# function: visualize the PM2.5 and case count and death count in pairs
# https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots 

setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
df = load.data()
## PM2.5 (blue, ug/m3), COVID-19 incidences (red) and deaths (black) of the most populated counties (FIPS)

########################### top counties ################################
### set up 
n.col.grid = 6
file.name = paste0("output/pm&cases&deaths.top10.Jan2021_.pdf")
fips.unique = unique(df$FIPS[order(df$population, decreasing=TRUE)])[1:10]

### TODO ???????????????????
df$start.hazard = as.Date("2020-03-15")
df$end.hazard = as.Date("2020-03-15")
df$start.hazard[!is.na(df$pmhazard)&(df$pmhazard > 0)] = df$date[!is.na(df$pmhazard)&(df$pmhazard > 0)] - 1
df$end.hazard[!is.na(df$pmhazard)&(df$pmhazard > 0)] = df$date[!is.na(df$pmhazard)&(df$pmhazard > 0)] + 1

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
          plot.title = element_text(hjust = 0.5, size = 8)) +  # , face = "bold" 
    scale_color_manual(name="", values=c(PM25="blue")) +
    xlab("Date") + ggtitle(paste0(df.selected$County[1], " County, ", df.selected$state[1]))
 
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
          plot.title = element_text(hjust = 0.5, size = 8)) +
    scale_color_manual(name="", values=c(Cases="red")) +
    xlab("Date") + ggtitle(" ")
  
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
          plot.title = element_text(hjust = 0.5, size = 8)) +
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
pdf(file.name, width = 10, height = 8)
do.call('grid.arrange',c(plot.list, ncol = n.col.grid, 
                         top = "",
                         bottom="", left=""))
dev.off()

