library(lubridate)
library(tidyr)
library(dplyr)
library(rstudioapi)

project.dir = dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(project.dir)
setwd('./data')

################################ get COVID confirmed cases ################################  
# cases = read.csv('https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv', check.names=FALSE)
cases = read.csv("covid_confirmed_usafacts_Feb6_2021.csv")

# remove countyFIPS == 1 which are state aggregation, and filter out the west coast states WA, OR, CA
cases = dplyr::filter(cases, countyFIPS > 2, stateFIPS %in% c(6, 53, 41))

locations = cases[, c("countyFIPS", "County.Name", "State", "stateFIPS")]

cases = tidyr::gather(cases, key=date, value=cases,
               -"countyFIPS", -"County.Name", -"State", -"stateFIPS")

cases$date = ymd(gsub('X', '', cases$date))
cases$FIPS = substr(100000 + cases$countyFIPS, 2, 6)

# recalculate 'cases', previous it means cumulative cases, now it means daily cases 
cases$cumu_cases = cases$cases
cases = data.frame(cases %>% group_by(FIPS) %>% arrange(date) %>%
                     mutate(cases = cases - lag(cases, n=1, default=0)))

cases = cases[, c("FIPS", "date", "cases", "cumu_cases")]

stopifnot(dim(unique(cases)) == dim(cases))

cases = cases[cases$date <= "2020-12-16",]
cases = cases[cases$date >= "2020-03-15",]

################################ get COVID deaths ################################  
# deaths = read.csv('https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv', check.names=FALSE)
deaths = read.csv("covid_deaths_usafacts_Feb6_2021.csv")

# remove countyFIPS == 1 which are state aggregation, and filter out the west coast states WA, OR, CA
deaths = filter(deaths, countyFIPS > 2, stateFIPS %in% c(6, 53, 41))

deaths = gather(deaths, key=date, value=deaths, 
                -"countyFIPS", -"County.Name", -"State", -"stateFIPS")

deaths$date = ymd(gsub('X', '', deaths$date))

deaths$FIPS = substr(100000 + deaths$countyFIPS, 2, 6)

deaths$cumu_deaths = deaths$deaths
deaths = data.frame(deaths %>% group_by(FIPS) %>% arrange(date) %>%
                      mutate(deaths = deaths - lag(deaths, n=1, default=0)))

deaths = deaths[, c("FIPS", "date", "deaths", "cumu_deaths")]

stopifnot(dim(unique(deaths)) == dim(deaths))



################################ get county population ################################  
# pop = read.csv('https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_county_population_usafacts.csv', check.names=FALSE)
pop = read.csv("covid_county_population_usafacts_Feb6_2021.csv")

pop$FIPS = substr(100000 + pop$countyFIPS, 2, 6)

pop = pop[, c("FIPS", "population")]

# exclude 06000
pop = pop[pop$population != 0, ]


################################ get PM2.5 from AirNow ################################ 
pm = read.csv('daily_pm_2020_update.csv')

pm = pm %>% rename(date=day, FIPS=fips)

pm$FIPS = substr(100000 + pm$FIPS, 2, 6)

pm = filter(pm, grepl('^06|^53|^41', FIPS)) 

pm$date = ymd(gsub('X', '', pm$date))

# some counties have multiple records for different ZIPS, take the median  
pm = data.frame(pm %>% group_by(date, FIPS) %>%
                  summarize(pm25 = median(pm25), Long = mean(X), Lat = mean(Y)))



################################ get climate data ################################ 
climate = read.csv("new_weather.csv")

names(climate)[names(climate) == "GEOID"] = "FIPS"

climate$FIPS = substr(100000 + climate$FIPS, 2, 6)

climate$date = ymd(gsub('X', '', climate$date))



################################ get hazard data ################################ 
# 0=nosmoke, 5=(0,10)light, 16=(11-20)medium, 27=(21,32)heavy

hms = read.csv("HMS_county_2020.csv")

hms = tidyr::gather(data=hms, key="date", value="hazardmap", -"County", -"GEOID")

hms$date = mdy("01-01-2020") + (as.numeric(substr(hms$date, 2, 5)) - 1)

hms$hazardmap[is.na(hms$hazardmap)] = 0

names(hms)[names(hms) == "GEOID"] = "FIPS"

hms$FIPS = substr(100000 + hms$FIPS, 2, 6)

hms = hms[, c("FIPS", "date", "hazardmap")]

max(hms$date[hms$hazardmap == 27])


################################ get facebook mobility ################################ 
mobility = read.csv("movement-range.csv")

mobility$date = ymd(mobility$date)

names(mobility)[names(mobility) == "fips"] = "FIPS"

mobility$FIPS = substr(100000 + mobility$FIPS, 2, 6)



################################ get historical pm2.5 ################################
pm_long = read.csv("daily_pm_longterm.csv")

names(pm_long)[names(pm_long) == "fips"] = "FIPS"

pm_long$FIPS = substr(100000 + pm_long$FIPS, 2, 6)

names(pm_long)[names(pm_long) == "day"] = "date"

pm_long$date = ymd(pm_long$date)

pm_long = pm_long[((pm_long$date) >= ymd("2016-01-01"))&((pm_long$date) <= ymd("2019-12-31")), ]

pm_long$month = month(as.POSIXlt(pm_long$date, format="%Y-%m-%d"))

pm_long$day = day(as.POSIXlt(pm_long$date, format="%Y-%m-%d"))

pm_long$md = paste0(pm_long$month, "-", pm_long$day)

pm_long = data.frame(pm_long %>% group_by(md, FIPS) %>% 
                       summarise(pm25_history = median(pm25, na.rm=T), 
                                 pm25_history_max = max(pm25, na.rm=T)))

pm_long = pm_long[c("md", "pm25_history", "pm25_history_max", "FIPS")]



################################ merge all together ################################ 
df = left_join(cases, deaths, by=c("FIPS", "date"))

df = left_join(df, pop, by="FIPS")

df = left_join(df, pm, by=c("FIPS", "date"))

df = left_join(df, climate, by=c("FIPS", "date"))

df = left_join(df, hms, by=c("FIPS", "date"))

df = left_join(df, mobility, by=c("FIPS", "date"))

rm(cases, deaths, pop, pm, climate, hms, mobility)

# date columns
df$month = month(as.POSIXlt(df$date, format="%Y-%m-%d"))

df$day = day(as.POSIXlt(df$date, format="%Y-%m-%d"))

df$md = paste0(df$month, "-", df$day)

df = arrange(df, date)

df = left_join(df, pm_long, by = c("FIPS", "md"))

rm(pm_long)

# state column 
names(locations) = c("FIPS", "County", "State", "StateFIPS")

locations$FIPS = substr(100000 + locations$FIPS, 2, 6)

df = left_join(df, locations, by = "FIPS")

rm(locations)

# date column 
df$dayofweek = weekdays(as.Date(df$date))

df$date_num = as.integer(df$date - min(df$date))

df$date_str = df$date



################################ filter 95 ################################ 
# 95 counties that have no missing pm2.5 during wildfire days 
pm.na.wildfire = data.frame(df[!is.na(df$hazardmap),] %>% group_by(FIPS) %>% 
                              summarise(count=sum((is.na(pm25))*(hazardmap==27))))

c95 = pm.na.wildfire$FIPS[pm.na.wildfire$count==0]
length(c95)

# FIPS = 41011, 41053, 41057 have missing overlap (2020, previous)
for (ifips in c95) {
  subset = df[df$FIPS == ifips, ]
  krow = sum(is.na(subset$pm25)*is.na(subset$pm25_history))
  if (krow > 0) {
    print(subset$FIPS[1])
    print(krow)
    c95 = setdiff(c95, subset$FIPS[1])
  }
}

dim(df[df$FIPS %in% c95, ])

df = df[df$FIPS %in% c95, ]

# check 
sum(is.na(df$pm25)*is.na(df$pm25_history))
rm(c95, ifips, krow, subset, pm.na.wildfire)

# 89 counties that have no missing pm2.5 during wildfire days and little missing during non-wildfire days
# pm.na = df %>% group_by(FIPS) %>% summarise(na = sum(is.na(pm25)))
# c89 = pm.na$FIPS[(pm.na$na < 31)&(pm.na$na != 21)]
# length(c89)

################################ handle missing ################################ 
# cases and deaths
irow = (df$cases < 0) # |is.na(df$cases)
jrow = (df$deaths < 0) # |is.na(df$deaths)
sum(irow) # 177 out of 34181 records
sum(jrow) # 147 out of 34181 records
df$cases[df$cases < 0] = NA
df$deaths[df$deaths < 0] = NA

# negative pm2.5
sum(df$pm25<0, na.rm=T) # 7 out of 34181 records
df$pm25[df$pm25 < 0] = 0

rm(irow, jrow)

# replace null in pm 2020 with historical data
summary(df$pm25)
df$pm25_raw = df$pm25
df$pm25[is.na(df$pm25)] = df$pm25_history[is.na(df$pm25)]
summary(df$pm25)

# mobility
mobility.na = data.frame(df %>% group_by(FIPS) %>% summarise(count=sum(is.na(relative_change_feb))))
mobility.na[mobility.na$count > 0,]
# pop[pop$FIPS %in% mobility.na$FIPS[mobility.na$count > 0], ]

# TODO 
mobility.na$FIPS[mobility.na$count > 0]
irow = df$FIPS %in% mobility.na$FIPS[mobility.na$count > 0]
df$relative_change_feb[irow] = NA
df$ratio_travelers[irow] = NA

rm(mobility.na, irow)

################################ wildifre contribution to pm2.5 2020 ################################
### wildfire period scenario one 
df$wildfire = NA
df$wildfire = (df$hazardmap==27)&(!is.na(df$hazardmap))
rows1 = (df$wildfire==T)&(!is.na(df$wildfire))

df$pm_wildfire = 0
df$pm_wildfire[pm_wildfire] = pmax(df$pm25[pm_wildfire] - df$pm25_history[pm_wildfire], 0)

# check 
sum(rows1)
rm(rows1)

### wildfire period scenario two
# manually classify non-wildfire day with high pm as wildfire day
# df$wf1 = df$wf_index
# th = 66
# df$wf2 = df$wf1
# df$wf2[(df$wf1==F)&(df$pm25>=th)] = T
# rows2 = (df$wf2==T)&(!is.na(df$wf2))
# 
# df$pm_wf2 = 0
# df$pm_wf2[rows2] = pmax(df$pm25[rows2] - df$pm25_history[rows2], 0)
# 
# # check 
# sum((df$wf1==F)&(df$pm25>=th))
# sum(rows2)
# rm(rows2, th)
# plot(df$pm_wf1, df$pm_wf2)


### wildfire period scenario three
# manually classify wildfire day with how pm as non-wildfire day
# th = 15
# df$wf3 = df$wf1
# df$wf3[(df$wf1==T)&(df$pm25<=th)] = F
# rows3 = (df$wf3==T)&(!is.na(df$wf3))
# 
# df$pm_wf3 = 0
# df$pm_wf3[rows3] = pmax(df$pm25[rows3] - df$pm25_history[rows3], 0)
# 
# sum((df$wf1==T)&(df$pm25<=th))
# rm(rows3, th)
# plot(df$pm_wf1, df$pm_wf3)

# ggplot() + geom_point(aes(df$pm25, df$pm_wf1), alpha=.1) + xlim(c(0, 66)) + ylim(c(0, 66))
# ggplot() + geom_point(aes(df$pm25, df$pm_wf1), alpha=.1) + xlim(c(0, 30)) + ylim(c(0, 30))
# ggplot() + geom_point(aes(log(df$pm25+.001), log(df$pm_wf3+.001)), alpha=.1)

################################ save data ################################ 
write.csv(df, 'moddat_Feb2021.csv', row.names=F)

