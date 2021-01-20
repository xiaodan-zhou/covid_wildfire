library(tidyverse)
library(sf)
library(lubridate)
library(tigris)

###############################################################
# Read in, summarize to county-day, and subset to CA, WA, OR
#################################################################
dat <- read.csv('~/covid_wildfire/data/daily_pm_longterm.csv') %>%
  group_by(day, state, fips) %>%
  summarize(pm25=mean(pm25)) %>%
  mutate(day = ymd(day),
         year = year(day),
         yday = yday(day)) %>%
  filter(state %in% c('CA', 'OR', 'WA'))

################################
#Graph TS for random counties
##############################
counties <- unique(dat$fips)

s <- dat %>%
  filter(fips == sample(counties, 1), 
         yday > 50)
ggplot(s) + 
  geom_line(aes(x=yday, y=pm25, color=year==2020, group=year)) + 
  labs(color=s$state[1])

#Looks like in most cases, 2020 is the worst, but there have been other fire years

##############################################
#Get Number of Observation Per County
################################################
cnt <- st_as_sf(counties()) %>%
  mutate(fips = as.numeric(paste0(STATEFP, COUNTYFP)))

dats <- dat %>%
  ungroup %>%
  group_by(fips) %>%
  summarize(n())

m <- merge(cnt, dats)

ggplot(m) + 
  geom_sf(aes(fill=`n()`))

######################################################
# Subset to those with > 2000 and get long-term median
######################################################
sel <- dat %>%
  ungroup %>%
  group_by(fips) %>%
  filter(n() > 2000) %>%
  group_by(fips, yday) %>%
  summarize(pm25_historic = median(pm25)) %>%
  mutate(date_2020 = ymd('2019-12-31') + yday)

write.csv(sel, '~/covid_wildfire/data/daily_pm_longterm_median.csv', row.names=F)

#####################################################
# Visualize
####################################################

new <- bind_rows(dat %>% filter(fips %in% sel$fips) %>% mutate(year = as.character(year)),
                 sel %>% mutate(year = 'long-term', 
                                pm25=pm25_historic,
                                state = case_when(fips < 7000 ~ 'CA',
                                                  fips < 44000 ~ 'OR',
                                                  TRUE ~ 'WA')))

new <- merge(new, cnt %>% st_drop_geometry %>% select(fips, NAMELSAD))

new <- new %>%
  mutate(lab = paste0(NAMELSAD, ', ', state),
         gp = case_when(year == 'long-term' ~ "Long Term Norm",
                        year == '2020' ~ "2020",
                        TRUE ~ "2010 - 2019")) %>%
  filter(yday > 50)

counties <- unique(new$fips)

# Single counties
s <- new %>%
  filter(fips == sample(counties, 1))

ggplot(s) + 
  geom_line(aes(x=yday, y=pm25, color=gp, group=year)) + 
  labs(color=s$lab[1]) + 
  scale_color_manual(values=c('#BBBBBB', '#FF0000', '#0000FF'))

#Multiple counties
s <- new %>%
  filter(fips %in% sample(counties, 20))
ggplot(s) + 
  geom_line(aes(x=yday, y=pm25, color=gp, group=year)) + 
  scale_color_manual(values=c('#BBBBBB', '#FF0000', '#0000FF')) + 
  facet_wrap(. ~ lab, ncol=4)
















