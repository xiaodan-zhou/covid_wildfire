# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/1CLYWS

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

############################### select westcoast and save ###############################
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


############################### read ###############################
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

# grocery        work     transit      retail residential        park 
# 46.65       31.81       55.89       40.79       49.41       58.75 

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
  sum.cor = sum.cor + as.matrix(cor(as.matrix(dfsub[dfsub$FIPS == id, 
                                                    c("cases", "deaths", "grocery", "work", "transit", "retail", "residential")])))
}
sum.cor = sum.cor/length(unique(dfsub$FIPS))
sum.cor

# WRONG!!!
# cor(as.matrix(dff[(dff$kp=="1")&complt, c("cases", "deaths", "grocery", "work", "transit", "retail", "residential")]))


