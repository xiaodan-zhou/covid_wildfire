# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/1CLYWS

library(reshape2)
library(ggplot2)
library(gridExtra)
setwd("/Users/mac/Documents/GitHub/covid_wildfire")

plot.list = list()
iplot = 1
pdf.out = "state_overview.pdf"

################################ state ###############################
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
co.full = read.csv("data/dataverse_Dec2/combined_percent_change_from_baseline_CO.csv")
co.full$date = ymd(co.full$date)
co = co.full[co.full$state %in% c(53, 31, 6), ] #CA only , WA 53, OR 41
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





############################### visualize ###############################
# pdf.out = "output/county.mobility.pdf"
# plot.list = list()
# iplot = 1
# for (ifips in unique(dff$FIPS)) {
#   dsub = dff[(dff$kp == 1)&(dff$FIPS == ifips), ]
#   if (dim(dsub)[1]!=0) {
#     p4 = ggplot(dsub) + 
#       # geom_line(aes(date, cases)) +
#       # geom_line(aes(date, deaths)) +
#       geom_line(aes(date, work)) +
#       geom_line(aes(date, retail)) + theme_bw()
#     plot.list[[iplot]] = p4
#     iplot = iplot + 1
#   }
# }
# 
# pdf(pdf.out, width = 12, height = iplot * 3)
# do.call('grid.arrange',c(plot.list, ncol = 1, top = "2020-03-15 to 2020-09-24"))
# dev.off()
