# function: visualize the PM2.5 in 2020 vs historical

library(rstudioapi)
project.dir = dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(project.dir)
source("scr/Utilities.R")
df = load.data()


file.name = paste0("output/pm_vs_historical.pdf")
fips.unique = unique(df$FIPS[order(df$population, decreasing=TRUE)])
point.size = 0.3
line.size = point.size
## LA
# fips.unique = c("6037")
# point.size = 1.5
# line.size = 0.8 

plot.list = list()
iplot = 1

for (ifips in fips.unique) {
  df.selected = df[df$FIPS == ifips, ]

    p1 = ggplot() + 
    geom_rect(data=df.selected, aes(xmin = start27, xmax = end27,
                                    ymin = -Inf, ymax = Inf), fill = "grey", colour = "grey", alpha = 1) +
    geom_rect(data=df.selected[df.selected$pm25_history < df.selected$pm25, ], aes(xmin = start27, xmax = end27,
                                    ymin = pm25_history, ymax = pm25), fill = "orange", colour = "orange", alpha = 1) +
    geom_point(data=df.selected, aes_string(x="date", y="pm25", color=shQuote("PM25")), size = point.size) +
    geom_line(data=df.selected, aes_string(x="date", y="pm25_history", color=shQuote("PMHIST")), size = line.size) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          plot.title = element_text(hjust = 0.5, size = 8)) +
    scale_color_manual(name="", values=c(PM25="blue", PMHIST="black")) +
    xlab("Date") + ggtitle(paste0(df.selected$County[1], ", ", df.selected$State[1]))
  
  p1 = p1 + theme(legend.position = "none")
  
  plot.list[[iplot]] = p1
  iplot = iplot + 1
}

n.col.grid = 4
pdf(file.name, width = 8.5, height = (iplot)/n.col.grid * 4/3)
do.call('grid.arrange',c(plot.list, ncol = n.col.grid))
dev.off()

## LA 
# pdf(file.name, width = 10, height = 4)
# do.call('grid.arrange',c(plot.list, ncol = 1))
# dev.off()
