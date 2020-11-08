library(lubridate)
library(tidyr)
library(dplyr)

setwd('/Users/mac/Documents/GitHub/covid_wildfire/data')

################################ get COVID confirmed cases ################################  
# https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/

cases = read.csv('https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv',
                 check.names=FALSE)
names(cases)[1] = "countyFIPS"

# remove countyFIPS == 1 which are state aggregation, and filter out the west coast states WA, OR, CA
cases = dplyr::filter(cases, countyFIPS > 2, stateFIPS %in% c(6, 53, 41))

# wide data to long data
cases = tidyr::gather(cases, key=date, value=cases, 
               -"countyFIPS", -"County Name", -"State", -"stateFIPS")

# format date and FIPS 
cases$date = mdy(gsub('X', '', cases$date))
cases$FIPS = substr(100000 + cases$countyFIPS, 2, 6)

# recalculate 'cases', previous it means cumulative cases, now it means daily cases 
cases$cumu_cases = cases$cases
cases = data.frame(cases %>% group_by(FIPS) %>% arrange(date) %>%
                     mutate(cases = cases - lag(cases, n=1, default=0)))

# check data with negative daily cases
print(paste("there are ", sum(cases$cases < 0), 
            "records with negative daily cases.")) # 17
# print(cases[cases$cases < 0, ])
# for (i in cases[cases$countyFIPS == 6099,]) print(i)

# remove unnecessary columns 
cases = cases[, c("FIPS", "date", "cases", "cumu_cases")]

# check uniqueness 
stopifnot(dim(unique(cases)) == dim(cases))



################################ get COVID deaths ################################  
deaths = read.csv('https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv',
                   check.names=FALSE)
names(deaths)[1] = "countyFIPS"

deaths = filter(deaths, countyFIPS > 2, stateFIPS %in% c(6, 53, 41))

deaths = gather(deaths, key=date, value=deaths, 
                -"countyFIPS", -"County Name", -"State", -"stateFIPS")

deaths$date = mdy(gsub('X', '', deaths$date))
deaths$FIPS = substr(100000 + deaths$countyFIPS, 2, 6)

deaths$cumu_deaths = deaths$deaths
deaths = data.frame(deaths %>% group_by(FIPS) %>% arrange(date) %>%
                      mutate(deaths = deaths - lag(deaths, n=1, default=0)))

print(paste("there are", sum(deaths$deaths < 0), 
            "records with negative daily deaths.")) # 25
print(paste("they happend in", 
            length(unique(deaths$FIPS[deaths$deaths < 0])), "counties."))

# remove unnecessary columns 
deaths = deaths[, c("FIPS", "date", "deaths", "cumu_deaths")]
stopifnot(dim(unique(deaths)) == dim(deaths))


################################ get county population ################################  
pop = read.csv('https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_county_population_usafacts.csv',
               check.names=FALSE)
names(pop)[1] = "countyFIPS"
pop$FIPS = substr(100000 + pop$countyFIPS, 2, 6)
pop = pop[, c("FIPS", "population")]


################################ get PM2.5 ################################ 
# From AirNow, via Ben Sabath
pm = read.csv('daily_pm_2020.csv')
pm = pm %>% rename(date=day, FIPS=fips)
pm$FIPS = substr(100000 + pm$FIPS, 2, 6)
pm = filter(pm, grepl('^06|^53|^41', FIPS)) 
pm$date = ymd(gsub('X', '', pm$date))

# some counties have multiple records for different ZIPS
# take the median of them 
pm = data.frame(pm %>% group_by(date, FIPS) %>%
                  summarize(pm25 = median(pm25), Long = mean(X), Lat = mean(Y)))

# there is no missing 
# st %>% group_by(FIPS) %>%
#   arrange(date) %>%
#   mutate(pm25 = na_interpolation(pm25))

# check negative values 
print(paste("there are", sum(pm$pm25 < 0), 
            "records with negative pm25 values.")) # 34


################################ get climate data ################################ 
# pr_ = read.csv("pr_county_2020.csv")
# tmmx_ = read.csv("tmmx_county_2020.csv")
# srad_ = read.csv("srad_county_2020.csv")
# sph_ = read.csv("sph_county_2020.csv")
# rmax_ = read.csv("rmax_county_2020.csv")
# climate = dplyr::left_join(pr_, tmmx_, by=c("fips", "date"))
# climate = dplyr::left_join(climate, srad_, by=c("fips", "date"))
# climate = dplyr::left_join(climate, sph_, by=c("fips", "date"))
# climate = dplyr::left_join(climate, rmax_, by=c("fips", "date"))
# limate = data.frame(climate %>% rename(FIPS=fips))
# write.csv(climate, "climate.csv")
# rm(pr_, tmmx_, srad_, sph_, rmax_)

climate = read.csv("climate.csv")
climate$FIPS = substr(100000 + climate$FIPS, 2, 6)
climate$date = ymd(gsub('X', '', climate$date))


################################ merge all together ################################ 
covid_county = left_join(cases, deaths, by=c("FIPS", "date"))
covid_county = left_join(covid_county, pop, by="FIPS")
covid_county = left_join(covid_county, pm, by=c("FIPS", "date"))
covid_county = left_join(covid_county, climate, by=c("FIPS", "date"))

### replace negative number of daily cases with NA 
if (sum(covid_county$cases < 0) > 0) {
  print(paste(sum(covid_county$cases < 0), "records have negative daily cases"))
  covid_county$cases[covid_county$cases < 0] = NA
}
print(sum(covid_county$cases < 0, na.rm = T))

### replace negative number of daily deaths with zero 
if (sum(covid_county$deaths < 0) > 0) {
  print(paste(sum(covid_county$deaths < 0), "records have negative daily deaths"))
  covid_county$deaths[covid_county$deaths < 0] = NA
}
print(sum(covid_county$deaths < 0, na.rm = T))

# cut covid data at 2020-09-24 and 2020-03-15
covid_county = covid_county[covid_county$date <= "2020-09-24",]

# mark cases cumu_cases deaths cumu_deaths before "2020-03-15" as NA
### covid_county = covid_county[covid_county$date >= "2020-03-15",]
covid_county$cases[covid_county$date < "2020-03-15"] = NA
covid_county$deaths[covid_county$date < "2020-03-15"] = NA
covid_county$cumu_cases[covid_county$date < "2020-03-15"] = NA
covid_county$cumu_deaths[covid_county$date < "2020-03-15"] = NA
print(sum(is.na(covid_county$cases)))
print(sum(is.na(covid_county$deaths)))

covid_county$dayofweek = weekdays(as.Date(covid_county$date))
covid_county$date_str = covid_county$date
covid_county$date_num = as.integer(covid_county$date_str - min(covid_county$date_str))
covid_county$date = covid_county$date_str


# exclude 06000
covid_county = covid_county[covid_county$population != 0, ]


write.csv(covid_county, 'moddat_xz1_rerun.csv', row.names=F)
