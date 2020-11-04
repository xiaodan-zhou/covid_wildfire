# https://www.fire.ca.gov/incidents/

library(ggplot)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(rgdal)

######################### read and filter wildfire data #########################
setwd("/Users/mac/Documents/GitHub/covid_wildfire")
fire = read.csv("data/mapdataall_Nov3.csv")

fire = dplyr::rename(fire, county = incident_county, startday = incident_dateonly_created, endday = incident_dateonly_extinguished, 
              longitude = incident_longitude, latitude = incident_latitude)

fire$startday = as.Date(fire$startday)
fire$endday = as.Date(fire$endday)

sum(fire$endday < fire$startday, na.rm = T)
fire = fire[fire$endday >= fire$startday, ]

fire = fire[(fire$endday >= "2020-03-13"), ]
fire = fire[(fire$startday <= "2020-09-24"), ]
fire = fire[!(is.na(fire$startday)|is.na(fire$endday)), ] # 169 records

# 1 acre = 0.0015625 sq mi
# most counties > 200 sq mi
fire$sqmi_burned = fire$incident_acres_burned * 0.0015625
# two extreme fires
# sum(fire$sqmi_burned > 200)
 
fire$lastdays = as.integer(fire$endday - fire$startday) + 1
cor(as.integer(fire$lastdays), fire$sqmi_burned) # 0.5473724

# project to counties ZIPS 
# https://gis.stackexchange.com/questions/133625/checking-if-points-fall-within-polygon-shapefile

cty_map = readOGR("data/cb_2018_us_county_5m", layer="cb_2018_us_county_5m")
coordinates(fire) <- ~ longitude + latitude
proj4string(fire) <- proj4string(cty_map)
fire$ZIPS = over(fire, cty_map)$GEOID

######################### select columns and save #########################
write.csv(fire, 'data/fireCA.csv')
fire = read.csv('data/fireCA.csv')
fire = fire[, c("ZIPS", "county", "startday", "endday", "longitude", "latitude", 
                "lastdays", "sqmi_burned", "incident_id", "incident_url")]
write.csv(fire, 'data/fireCA.csv')


######################### visualize wildfire #########################
fire = read.csv('data/fireCA.csv')
fire$startday = as.Date(fire$startday)
fire$endday = as.Date(fire$endday)

plot.out = list()
iplot = 1

pp = ggplot(data=fire,aes(x=startday,y=1:dim(fire)[1])) + 
  geom_linerange(aes(xmin=startday, 
                     xmax=endday)) + 
  ggtitle(paste0("fires during 03/15 and 09/24 in 2020 (", dim(fire)[1], " events)")) + 
  xlab("date") + ylab("fire event")
plot.out[[iplot]] = pp
iplot = iplot + 1

pp = ggplot(data=fire,aes(x=startday,y=order(sqmi_burned, decreasing=F))) +
  geom_linerange(aes(xmin=startday, xmax=endday)) + 
  ggtitle("fire strength") + 
  xlab("date") + ylab("burned areas ranked")
plot.out[[iplot]] = pp
iplot = iplot + 1

pp = ggplot(data=fire,aes(x=longitude,y=latitude, size=lastdays)) +
  geom_point(alpha=.6) + xlim(-125, -115) + ylim(32.5, 42.5) + 
  ggtitle(paste0("Mapping fires, happened in ", length(unique(fire$county)), " out of 58 counties")) + 
  xlab("Long") + ylab("Lat")
plot.out[[iplot]] = pp
iplot = iplot + 1

pp = ggplot(data=fire,aes(x=longitude,y=latitude, size=sqmi_burned)) +
  geom_point(alpha=.6) + xlim(-125, -115) + ylim(32.5, 42.5) + 
  ggtitle(paste0("Mapping fires, happened in ", length(unique(fire$county)), " out of 58 counties")) + 
  xlab("Long") + ylab("Lat")
plot.out[[iplot]] = pp
iplot = iplot + 1

pp = ggplot(data=fire) + geom_histogram(aes(lastdays), binwidth=1) + 
  ggtitle("how many days last") + 
  xlab("days") + ylab("count")
plot.out[[iplot]] = pp
iplot = iplot + 1 

pdf("fire.vis.pdf", width = 12, height = 12)
do.call('grid.arrange',c(plot.out, ncol = 2, top = "california fire"))
dev.off()


######################### create wildfire-day index #########################
fire = read.csv('data/fireCA.csv')
fire$startday = as.Date(fire$startday)
fire$endday = as.Date(fire$endday)

colClasses = c("Date", "integer")
col.names = c("Date", "ZIPS")
fire.out = read.table(text = "", colClasses = colClasses, col.names = col.names)

for (irow in 1:dim(fire)[1]) {
  l = as.integer(fire$endday[irow] - fire$startday[irow])+1
  temp = data.frame(date=seq(fire$startday[irow], by = "day", length.out = l), ZIPS = fire$ZIPS[irow])
  fire.out = rbind(fire.out, temp)
}
write.csv(fire.out, "data/fireCA_reshaped.csv")
