# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/1CLYWS

library(reshape2)
library(ggplot2)
library(gridExtra)
setwd("/Users/mac/Documents/GitHub/covid_wildfire")

plot.list = list()
iplot = 1
pdf.out = "state_overview.pdf"
################################ state ###############################
# groc = read.csv("grocery_and_pharmacy_percent_change_from_baseline_ST.csv")
# park = read.csv("parks_percent_change_from_baseline_ST.csv")
# resi = read.csv("residential_percent_change_from_baseline_ST.csv")
# reta = read.csv("retail_and_recreation_percent_change_from_baseline_ST.csv")
# tran = read.csv("transit_stations_percent_change_from_baseline_ST.csv")
# work = read.csv("workplaces_percent_change_from_baseline_ST.csv")
# 
# dim(groc) == dim(park)
# dim(groc) == dim(resi)
# dim(groc) == dim(reta)
# dim(groc) == dim(tran)
# dim(groc) == dim(work)
# 
# groc$type = "grocery"
# park$type = "park"
# resi$type = "residential"
# reta$type = "retail"
# tran$type = "transit"
# work$type = "work"
# 
# dt = rbind(groc, park, resi, reta, tran, work)
# dt = melt(dt, id.vars=c("X", "type", "STATE", "NAME"))
# dt$date = ymd(substr(dt$variable, 2, 20))
# dt$mobility = dt$value 
# dt = dt[, c("STATE", "NAME", "date", "mobility", "type")]
# head(dt)
# write.csv(dt, "combined_percent_change_from_baseline_ST.csv")

dt = read.csv("data/dataverse_Dec2/combined_percent_change_from_baseline_ST.csv")
dt$date = ymd(dt$date)
dt = dt[dt$date <= "2020-09-24", ]
dt = dt[dt$date >= "2020-03-15", ]
dt = dt[dt$NAME %in% c("California", "Washington", "Oregon"),]
for (istate in unique(dt$NAME)) {
  p1 = ggplot(dt[dt$NAME == istate, ]) + geom_line(aes(date,mobility,col=type)) + 
    ggtitle(istate) + theme_bw()
  plot.list[[iplot]] = p1
  iplot = iplot + 1
}

# p2 = ggplot(dt[dt$NAME == "Washington", ]) + geom_line(aes(date,mobility,col=type)) + 
#   ggtitle("Washington") + theme_bw()
# plot.list[[iplot]] = p2
# iplot = iplot + 1
# 
# p3 = ggplot(dt[dt$NAME == "Oregon", ]) + geom_line(aes(date,mobility,col=type)) + 
#   ggtitle("Oregon") + theme_bw()
# plot.list[[iplot]] = p2
# iplot = iplot + 1

############################### county ###############################
# groc = read.csv("grocery_and_pharmacy_percent_change_from_baseline_CO.csv")
# park = read.csv("parks_percent_change_from_baseline_CO.csv")
# resi = read.csv("residential_percent_change_from_baseline_CO.csv")
# reta = read.csv("retail_and_recreation_percent_change_from_baseline_CO.csv")
# tran = read.csv("transit_stations_percent_change_from_baseline_CO.csv")
# work = read.csv("workplaces_percent_change_from_baseline_CO.csv")
# 
# dim(groc) == dim(park)
# dim(groc) == dim(resi)
# dim(groc) == dim(reta)
# dim(groc) == dim(tran)
# dim(groc) == dim(work)
# 
# groc$type = "grocery"
# park$type = "park"
# resi$type = "residential"
# reta$type = "retail"
# tran$type = "transit"
# work$type = "work"
# 
# dt = rbind(groc, park, resi, reta, tran, work)
# dt = melt(dt, id.vars=c("X", "type", "COUNTY", "NAME", "addr"))
# dt$date = ymd(substr(dt$variable, 2, 20))
# dt$GEOID = dt$COUNTY
# dt$mobility = dt$value
# dt$state = round(dt$COUNTY / 1000, 0)
# dt = dt[, c("GEOID", "NAME", "date", "mobility", "type", "state")]
# head(dt)
# 
# write.csv(dt, "combined_percent_change_from_baseline_CO.csv")
co.full = read.csv("data/dataverse_Dec2/combined_percent_change_from_baseline_CO.csv")
co.full$date = ymd(co.full$date)
co = co.full[co.full$state %in% c(53), ] #CA only , WA 53, OR 41
co = co[co$date <= ymd("2020-09-24"), ]
co = co[co$date >= ymd("2020-03-15"), ]

summary(co)
# all grocery in california 
# &dt$GEOID %in% c(6037, 6073, 6059, 6065, 6071, 6085, 6001, 6067, 6013)
for (itype in unique(co$type)) {
  p4 = ggplot(co[co$type == itype,]) + 
    geom_line(aes(date, mobility, col=as.factor(GEOID))) + 
    ggtitle(paste0(itype, " in California counties")) + theme_bw() + 
    theme(legend.position = "none") 
  plot.list[[iplot]] = p4
  iplot = iplot + 1
}

pdf(pdf.out, width = 12, height = iplot * 3)
do.call('grid.arrange',c(plot.list, ncol = 1, top = "2020-03-15 to 2020-09-24"))
dev.off()




################################ county imputation ###############################