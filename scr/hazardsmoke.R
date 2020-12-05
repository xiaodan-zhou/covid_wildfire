library(lubridate)
library(tidyr)
library(ggplot2)
library(gridExtra)


setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
dff = load.data.xz1()
dff$heavy.hazard = dff$hazardmap >= 27

## visualize the boxplot for pm2.5 by group of hazard level 
pct.heavy = round(c(sum(dff$hazardmap == 0, na.rm=T), sum(dff$hazardmap == 5, na.rm=T),
                    sum(dff$hazardmap == 16, na.rm=T), sum(dff$hazardmap == 27, na.rm=T)) / sum(!is.na(dff$hazardmap)) * 100, 1)

ggplot(dff[!is.na(dff$hazardmap), ], aes(x=as.factor(hazardmap), y=pm25, fill=as.factor(hazardmap))) + 
  geom_boxplot(outlier.shape = NA) + xlab("smoke density based on satellite aerosal") + 
  ylab("surface level pm2.5") + 
  theme_bw() + 
  theme(legend.position = "none") + 
  ylim(c(0, 150)) +
  scale_x_discrete(labels=c("0" = paste0("No Smoke ", pct.heavy[1], "%"), 
                            "5" = paste0("Light ", pct.heavy[2], "%"),
                            "16" = paste0("Medium ", pct.heavy[3], "%"), 
                            "27" = paste0("Heavy ", pct.heavy[4], "%")))


# ####################################
## visualize heavy hazard by county
plot.list = list()
iplot = 1
pdf.out = "ExploratoryDataAnalysis/base.hazard.split.pdf"

for (ifips in unique(dff$FIPS)) {
  # 6037 most populated 
  dsub = dff[dff$FIPS == ifips, ]
  p1 = ggplot(dsub) + geom_line(aes(date, pm25), col="grey") + 
    geom_line(aes(date, pmhazard), col="red") +
    # geom_point(aes(date, pm25, col=heavy.hazard), size=1) + 
    ggtitle(ifips) + theme_bw() + # + ylim(c(0,100)) 
    theme(legend.position = "none", 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),  
          axis.title.y = element_blank(),  
          plot.title = element_text(hjust = 0.5)) # + scale_y_log10()
  plot.list[[iplot]] = p1
  iplot = iplot + 1
}

pdf(pdf.out, width = 12, height = 100)
do.call('grid.arrange',c(plot.list, ncol = 2, top = "heavy hazard"))
dev.off()



# ####################################
## visualize heavy hazard by county
# plot.list = list()
# iplot = 1
# pdf.out = "hazard3.pdf"
# 
# for (ifip in unique(dff$FIPS)) {
#   # 6037 most populated 
#   dsub = dff[dff$FIPS == ifip, ]
#   p1 = ggplot(dsub) + geom_line(aes(date, pm25)) + 
#     geom_point(aes(date, pm25, col=heavy.hazard), size=1) + 
#      ggtitle(ifip) + ylim(c(0,100)) + theme_bw() +
#     theme(legend.position = "none", 
#           panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank(),
#           axis.title.x = element_blank(),  
#           axis.title.y = element_blank(),  
#           plot.title = element_text(hjust = 0.5)) # + scale_y_log10()
#     
#   plot.list[[iplot]] = p1
#   iplot = iplot + 1
#   }
# 
# pdf(pdf.out, width = 12, height = 100)
# do.call('grid.arrange',c(plot.list, ncol = 2, top = "heavy hazard"))
# dev.off()



# pm.threshold = 15
# hazard.threshold = 27
# ss = data.frame(dff %>% group_by(FIPS) %>% 
#                   summarise(heavy.hms = sum(hazardmap >= hazard.threshold, na.rm=T),
#                             high.pm = sum(pm25 >= pm.threshold, na.rm=T),
#                             state = state[1]))
# ggplot(ss) + geom_point(aes(x=high.pm,y=heavy.hms,col=state)) + 
#   geom_line(aes(x=c(1:66,1:67), y=c(1:66,1:67))) + 
#   ggtitle(paste0("Smoke Days by County (cor = ", round(cor(ss$heavy.hms, ss$high.pm), 2), ")")) + 
#   xlab(paste0("PM2.5 >= ", pm.threshold)) +
#   ylab(paste0("Heavy Smoke (", hazard.threshold, ")")) + 
#   theme_bw()











# ####################################
# dff$state = round(as.numeric(as.character(dff$FIPS))/1000, 0)
# dff$state[dff$state == 6] = "CA"
# dff$state[dff$state == 53] = "WA"
# dff$state[dff$state == 41] = "OR"
# dff$state = as.factor(dff$state)
# summary(dff$state)
# 
# hms = read.csv("data/HMS_county_2020.csv")
# hms = tidyr::gather(data=hms, key="date_num", value="hazardmap", -"County", -"GEOID")
# 
# hms$date_num = as.numeric(substr(hms$date_num, 2, 5))
# hms$date = mdy("01-01-2020") + (hms$date_num - 1)
# # filter date
# hms = hms[(hms$date >= mdy("03-15-2020")) & (hms$date <= mdy("09-24-2020")), ]
# hms$hazardmap[is.na(hms$hazardmap)] = 0
# 
# hms$GEOID = as.factor(as.character(hms$GEOID))
# 
# summary(hms)

# n = sum(!is.na(dff$hazardmap))
# for (i in unique(dff$hazardmap)) {
#   print(sum(dff$hazardmap == i, na.rm=T)/n*100)
# }

# 
# hist(dff$hazardmap)
# class(hms$date)
# class(dff$date)
# class(hms$GEOID)
# class(dff$FIPS)
# dff.merge = merge(dff, hms, by.x=c("date", "FIPS"), by.y=c("date", "GEOID"), all.x=T)
# summary(dff.merge)
# 
# print(sum(is.na(dff.merge$hazardmap)))
# unique(dff.merge$date[is.na(dff.merge$hazardmap)])