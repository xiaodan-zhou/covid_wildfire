# function: visualize the PM2.5 and case count and death count in pairs
# PM2.5 (blue, ug/m3), COVID-19 incidences (red) and deaths (black) of the most populated counties (FIPS)

library(rstudioapi)
project.dir = dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(project.dir)
source("scr/Utilities.R")
df = load.data()

# highlight where wildfire < baseline 
irow = (df$pm25<df$pm25_history)&(!is.na(df$pm25<df$pm25_history))
df$start.hazard2 = as.Date("2020-03-15")
df$end.hazard2 = as.Date("2020-03-15")
df$start.hazard2[irow] = df$date[irow] - 1
df$end.hazard2[irow] = df$date[irow] + 1

# highlight where wildfire > baseline 
irow = (df$pm25>=df$pm25_history)&(!is.na(df$pm25>=df$pm25_history))
df$start.hazard3 = as.Date("2020-03-15")
df$end.hazard3 = as.Date("2020-03-15")
df$start.hazard3[irow] = df$date[irow] - 1
df$end.hazard3[irow] = df$date[irow] + 1


########################### top counties ################################
### set up 
n.col.grid = 6
file.name = paste0("output/pm_cases_deaths___.pdf")
fips.unique = unique(df$FIPS[order(df$population, decreasing=TRUE)])# [1:6]
point.size = 0.3

plot.list = list()
iplot = 1

for (ifips in fips.unique) {
  df.selected = df[df$FIPS == ifips, ]

  ### visualize PM2.5
  p1 = ggplot() + 
    geom_point(data=df.selected, aes_string(x="date", y="pm25", color=shQuote("PM25")), size = point.size) +
    geom_rect(data=df.selected, aes(xmin = start27, xmax = end27,
                                    ymin = pm25_history, ymax = pm25), fill = "orange", colour = "orange", alpha = 1) +
    # geom_rect(data=df.selected, aes(xmin = start27, xmax = end27,
                  # ymin = -Inf, ymax = Inf), fill = "grey", colour =NA, alpha = 1) +
    # geom_rect(data=df.selected, aes(xmin = start.hazard2, xmax = end.hazard2,
    #                                 ymin = pm25, ymax = pm25_history), fill = "red", colour =NA, alpha = 1) +
    # geom_rect(data=df.selected, aes(xmin = start.hazard3, xmax = end.hazard3,
    #                                 ymin = pm25_history, ymax = pm25), fill = "orange", colour = NA, alpha = 1) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          plot.title = element_text(hjust = 0.5, size = 8)) +  
    scale_color_manual(name="", values=c(PM25="blue")) +
    xlab("Date") + ggtitle(paste0(df.selected$County[1], ", ", df.selected$State[1]))
 
  p1 = p1 + theme(legend.position = "none")
  
  plot.list[[iplot]] = p1
  iplot = iplot + 1
  
  ### visualize number of cases
  p2 = ggplot() +
    # geom_rect(data=df.selected, aes(xmin = start27, xmax = end27,
    #                                 ymin = -Inf, ymax = Inf), fill = "grey", colour =NA, alpha = 1) + 
    geom_point(data=df.selected, aes_string(x="date", y="cases", color=shQuote("Cases")), size = point.size) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),  
          axis.title.y = element_blank(),  
          plot.title = element_text(hjust = 0.5, size = 8)) +
    scale_color_manual(name="", values=c(Cases="red")) +
    xlab("Date") + ggtitle(" ")
  
  p2 = p2 + theme(legend.position = "none")
  plot.list[[iplot]] = p2
  iplot = iplot + 1
  
  ### visualize number of cases
  p3 = ggplot() +
    # geom_rect(data=df.selected, aes(xmin = start27, xmax = end27,
    #                                 ymin = -Inf, ymax = Inf), fill = "grey", colour =NA, alpha = 1) + 
    geom_point(data=df.selected, aes_string(x="date", y="deaths", color=shQuote("Deaths")), size = point.size) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),  
          axis.title.y = element_blank(),  
          plot.title = element_text(hjust = 0.5, size = 8)) +
    scale_color_manual(name="", values=c(Deaths="black")) +
    xlab("Date") + ggtitle("")
  p3 = p3 + theme(legend.position = "none")

  plot.list[[iplot]] = p3
  iplot = iplot + 1
}

pdf(file.name, width = 10, height = iplot/n.col.grid * 4/3)
do.call('grid.arrange',c(plot.list, ncol = n.col.grid))
dev.off()

