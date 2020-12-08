# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/1CLYWS

############################### clean and save ###############################
# mb = read.csv("data/dataverse_Dec2/combined_percent_change_from_baseline_CO.csv")
# mb = mb[mb$state %in% c(53, 41, 6), ] #CA 6, WA 53, OR 41
# mb$date = ymd(mb$date)
# mb$GEOID = as.factor(mb$GEOID)
# mb = mb[mb$date <= ymd("2020-09-24"), ]
# mb = mb[mb$date >= ymd("2020-03-15"), ]
# mb = mb[,(names(mb) != "X")]
# mb = mb[,(names(mb) != "X.1")]
# mb = tidyr::spread(mb, type, mobility)
# write.csv(mb, "data/dataverse_Dec2/combined_percent_change_from_baseline_CO_westcoast.csv")


setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
dff = load.data()

############################### select counties that has less missing ###############################
mb.missing.fips = data.frame(
  dff %>% group_by(FIPS) %>% summarise(grocery = sum(is.na(grocery)) / length(grocery) * 100,
                                       work = sum(is.na(work)) / length(work) * 100,
                                       transit = sum(is.na(transit)) / length(transit) * 100,
                                       retail = sum(is.na(retail)) / length(retail) * 100,
                                       residential = sum(is.na(residential)) / length(residential) * 100,
                                       park = sum(is.na(park)) / length(park) * 100))

round(colSums(mb.missing.fips[, c(2:7)]) / dim(mb.missing.fips)[1], 2)
# write.csv(mb.missing.fips, "output/mb.missing.fips.csv")


############################### apply the selection 62 counties ###############################
kp.row = mb.missing.fips$FIPS[(mb.missing.fips$work<=30)&(mb.missing.fips$retail<=30)]
dff$kp = as.factor(as.character((dff$FIPS %in% kp.row)*1))
write.csv(kp.row, "output.kp.row.csv")
# c(41003, 41005, 41017, 41019, 41029, 41033, 41039, 41043, 41047, 41051, 
#   41067, 41071, 53005, 53011, 53015, 53033, 53035, 53041, 53053, 53057, 
#   53061, 53063, 53067, 53073, 53077, 6001, 6007, 6013, 6017, 6019, 6023, 
#   6025, 6029, 6031, 6037, 6039, 6041, 6047, 6053, 6055, 6057, 6059, 6061, 
#   6065, 6067, 6071, 6073, 6075, 6077, 6079, 6081, 6083, 6085, 6087, 6089, 
#   6095, 6097, 6099, 6101, 6107, 6111, 6113)


############################### summarize the population ###############################
tt = data.frame(dff %>% group_by(kp, date) %>% summarise(s=sum(population)))
tt %>% group_by(kp) %>% summarise(s=mean(s))


############################### summarize the correlation ###############################
complt = complete.cases(dff[,c("cases", "deaths", "grocery", "work", "transit", "retail", "residential")])
dfsub = dff[(dff$kp=="1")&complt,]
sum.cor = matrix(0, nrow=7, ncol=7)
for (id in unique(dfsub$FIPS)) {
  sum.cor = sum.cor + as.matrix(cor(as.matrix(dfsub[dfsub$FIPS == id, c("cases", "deaths", "grocery", "work", "transit", "retail", "residential")])))
}
sum.cor = sum.cor/length(unique(dfsub$FIPS))
sum.cor

# WRONG!!!
# cor(as.matrix(dff[(dff$kp=="1")&complt, c("cases", "deaths", "grocery", "work", "transit", "retail", "residential")]))

summary(dfsub)
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
