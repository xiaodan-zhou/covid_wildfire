
library(ggplot2)
library(gridExtra)
library(meta)

setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
source("scr/GlobalModel.R")
dff = load.data.mc2()

mv = data.frame(dff %>% group_by(FIPS) %>% summarise(mean = mean(cases), variance = var(cases)))
mv = mv[order(mv$mean),]

ggplot() + geom_point(data=mv, aes(x=mean,y=variance)) + # geom_line(aes(x=1:1000,y=1:1000)) + 
  xlim(0, 300) + ylim(0, 150000) + ggtitle("each point is a county") + 
  xlab("cases mean") + ylab("cases variance")


ggplot(data=dff, aes(x=date,y=cases,color=FIPS)) + geom_line()

# create plot
ggplot( data_long, aes( x, y, color = curve ) ) +geom_line() + theme_bw()
