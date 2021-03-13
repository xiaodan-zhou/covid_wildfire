library(rstudioapi)
project.dir = dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(project.dir)
source("scr/Utilities.R")
dff = load.data()

TOTAL.POP = 51344853

####################### statistics across three states ###############################
### number of counties in each state
for (istate in unique(dff$State))
  print(paste(istate, length(unique(dff$FIPS[dff$State == istate])), "counties"))
print(paste("Total", length(unique(dff$FIPS)), "counties"))

### number of days in this analysis
max(dff$date) - min(dff$date) + 1

### population coverage
total.pop = 0
for (ifips in unique(dff$FIPS)) {
  total.pop = total.pop + dff$population[dff$FIPS == ifips][1] }
print(paste("population coverage", round(total.pop / TOTAL.POP * 100, 3), "%"))

### missing in pm2.5 2020 been replaced by historical value
# statistics for these 250 replacement 
sum(is.na(dff$pm25_raw))
summary(dff$pm25_history[is.na(dff$pm25_raw)])
# as a compare, the statistics of know pm2.5 in non-wildfire days 
summary(dff$pm25_raw[dff$wildfire==F])

### the number and percent of of missing cases 
sum(is.na(dff$cases))
sum(is.na(dff$cases)) / dim(dff)[1] * 100 

### the number and percent of of missing deaths 
sum(is.na(dff$deaths))
sum(is.na(dff$deaths)) / dim(dff)[1] * 100 

### the number of counties with missing mobility data 
sum(is.na(dff$relative_change_feb)) / dim(dff)[1]

### the 6 counties with missing in mobility
c6 = unique(dff$FIPS[is.na(dff$relative_change_feb)])
sum(unique(dff$population[dff$FIPS %in% c6])) / TOTAL.POP * 100

######################## Table: statistics by states #####################################
### cases summary
for (istate in unique(dff$State)) {
  subset = dff[dff$State == istate,]
  print(paste0(istate, " & ", 
               round(median(subset$cases, na.rm=T), 0), " (", 
               round(quantile(subset$cases, .25, na.rm=T), 0), " - ", 
               round(quantile(subset$cases, .75, na.rm=T), 0), ")"))
}

print(paste0("Total & ", 
             round(median(dff$cases, na.rm=T), 0), " (", 
             round(quantile(dff$cases, .25, na.rm=T), 0), " - ", 
             round(quantile(dff$cases, .75, na.rm=T), 0), ")"))

### deaths summary
for (istate in unique(dff$State)) {
  subset = dff[dff$State == istate,]
  print(paste0(istate, " & ", 
               round(median(subset$deaths, na.rm=T), 0), " (", 
               round(quantile(subset$deaths, .25, na.rm=T), 0), " - ", 
               round(quantile(subset$deaths, .75, na.rm=T), 0), ")"))
}

print(paste0("Total & ", 
             round(median(dff$deaths, na.rm=T), 0), " (", 
             round(quantile(dff$deaths, .25, na.rm=T), 0), " - ", 
             round(quantile(dff$deaths, .75, na.rm=T), 0), ")"))

## pm2.5 summary
for (istate in unique(dff$State)) {
  subset = dff[dff$State == istate,]
  print(paste0(istate, " & ", 
               round(median(subset$pm25), 1), " (", 
               round(quantile(subset$pm25, .25), 1), " - ", 
               round(quantile(subset$pm25, .75), 1), ")"))
}

print(paste0("Total & ", 
             round(median(dff$pm25), 1), " (", 
             round(quantile(dff$pm25, .25), 1), " - ", 
             round(quantile(dff$pm25, .75), 1), ")"))

## pm2.5 summary during wildfire
for (istate in unique(dff$State)) {
  subset = dff[!is.na(dff$hazardmap), ]
  subset = subset[(subset$State == istate)&(subset$hazardmap==27),]
  print(paste0(istate, " & ", 
               round(median(subset$pm25, na.rm=T), 1), " (", 
               round(quantile(subset$pm25, .25), 1), " - ", 
               round(quantile(subset$pm25, .75), 1), ")"))
}

subset = dff[!is.na(dff$hazardmap), ]
subset = subset[subset$wildfire==T,]
print(paste0("Total & ", 
             round(median(subset$pm25), 1), " (", 
             round(quantile(subset$pm25, .25), 1), " - ", 
             round(quantile(subset$pm25, .75), 1), ")"))


## pm2.5 summary during non-wildfire
for (istate in unique(dff$State)) {
  subset = dff
  subset = subset[(subset$State == istate)&(subset$hazardmap!=27),]
  print(paste0(istate, " & ", 
               round(median(subset$pm25, na.rm=T), 1), " (", 
               round(quantile(subset$pm25, .25, na.rm=T), 1), " - ", 
               round(quantile(subset$pm25, .75, na.rm=T), 1), ")"))
}

subset = dff
subset = subset[subset$wildfire!=T,]
print(paste0("Total & ", 
             round(median(subset$pm25, na.rm=T), 1), " (", 
             round(quantile(subset$pm25, .25, na.rm=T), 1), " - ", 
             round(quantile(subset$pm25, .75, na.rm=T), 1), ")"))


######################## Table in Results #####################################
### number of wildfire/non-wildfire days
tb1 = data.frame(dff %>% group_by(FIPS) %>% 
                   summarise(sum=sum(wildfire==T)))
tb2 = data.frame(dff %>% group_by(FIPS) %>% 
                   summarise(sum=sum(wildfire==F)))
round(quantile(tb1$sum, c(.50, .25, .75), na.rm=T), 1)
round(quantile(tb2$sum, c(.50, .25, .75), na.rm=T), 1)
rm(tb1, tb2)

### Daily PM2.5
round(quantile(dff$pm25, c(.50, .05, .95), na.rm=T), 1)
round(quantile(dff$pm25[dff$wildfire==T], c(.50, .05, .95), na.rm=T), 1)
round(quantile(dff$pm25[dff$wildfire==F], c(.50, .05, .95), na.rm=T), 1)

### Percent of Days with zero COVID19 case
tb0 = data.frame(dff %>% group_by(FIPS) %>% 
                   summarise(sum=sum(cases==0, na.rm=T), n=n()))
round(quantile(tb0$sum/tb0$n*100, c(.50, .25, .75), na.rm=T), 1)

subset1 = dff[dff$wildfire==T, ]
tb1 = data.frame(subset1 %>% group_by(FIPS) %>% 
                   summarise(sum=sum(cases==0, na.rm=T), n=n()))
round(quantile(tb1$sum/tb1$n*100, c(.50, .25, .75), na.rm=T), 1)

subset2 = dff[dff$wildfire==F, ]
tb2 = data.frame(subset2 %>% group_by(FIPS) %>% 
                   summarise(sum=sum(cases==0, na.rm=T), n=n()))
round(quantile(tb2$sum/tb2$n*100, c(.50, .25, .75), na.rm=T), 1)

rm(tb0, subset1, tb1, subset2, tb2)

### Percent of Days with zero COVID19 death
tb0 = data.frame(dff %>% group_by(FIPS) %>% 
                   summarise(sum=sum(deaths==0, na.rm=T), n=n()))
round(quantile(tb0$sum/tb0$n*100, c(.50, .25, .75), na.rm=T), 1)

subset1 = dff[dff$wildfire==T, ]
tb1 = data.frame(subset1 %>% group_by(FIPS) %>% 
                   summarise(sum=sum(deaths==0, na.rm=T), n=n()))
round(quantile(tb1$sum/tb1$n*100, c(.50, .25, .75), na.rm=T), 1)

subset2 = dff[dff$wildfire==F, ]
tb2 = data.frame(subset2 %>% group_by(FIPS) %>% 
                   summarise(sum=sum(deaths==0, na.rm=T), n=n()))
round(quantile(tb2$sum/tb2$n*100, c(.50, .25, .75), na.rm=T), 1)

rm(tb0, subset1, tb1, subset2, tb2)

### Daily Cases Rate (per 10,000)
dff$cases_rate = dff$cases / dff$population
tb0 = data.frame(dff %>% group_by(FIPS) %>% 
                   summarise(mean=mean(cases_rate, na.rm=T)*100*1000))
round(quantile(tb0$mean, c(.50, .25, .75), na.rm=T), 1)

tb1 = data.frame(dff[dff$wildfire==T,] %>% group_by(FIPS) %>% 
                   summarise(mean=mean(cases_rate, na.rm=T)*100*1000))
round(quantile(tb1$mean, c(.50, .25, .75), na.rm=T), 1)

tb2 = data.frame(dff[dff$wildfire==F,] %>% group_by(FIPS) %>% 
                   summarise(mean=mean(cases_rate, na.rm=T)*100*1000))
round(quantile(tb2$mean, c(.50, .25, .75), na.rm=T), 1)

rm(tb0, tb1, tb2)

### Daily Deaths Rate (per million)
dff$deaths_rate = dff$deaths / dff$population
tb0 = data.frame(dff %>% group_by(FIPS) %>% 
                   summarise(mean=mean(deaths_rate, na.rm=T)*1000*1000))
round(quantile(tb0$mean, c(.50, .05, .95), na.rm=T), 2)

tb1 = data.frame(dff[dff$wildfire==T,] %>% group_by(FIPS) %>% 
                   summarise(mean=mean(deaths_rate, na.rm=T)*1000*1000))
round(quantile(tb1$mean, c(.50, .05, .95), na.rm=T), 2)

tb2 = data.frame(dff[dff$wildfire==F,] %>% group_by(FIPS) %>% 
                   summarise(mean=mean(deaths_rate, na.rm=T)*1000*1000))
round(quantile(tb2$mean, c(.50, .05, .95), na.rm=T), 2)

rm(tb0, tb1, tb2)

######################## Table in Results #####################################
### percentage of cases and deaths during wildfire days 
case_fire = sum(dff$cases[dff$wildfire==T], na.rm=T)
death_fire = sum(dff$deaths[dff$wildfire==T], na.rm=T)
case_pct = round(case_fire / sum(dff$cases, na.rm=T) * 100)
death_pct = round(death_fire / sum(dff$deaths, na.rm=T) * 100)
paste(case_fire, "cases during wildfire (", case_pct, "%)")
paste(death_fire, "deaths during wildfire (", death_pct, "%)")

sum(dff$cases[dff$wildfire==T], na.rm=T) / sum(dff$wildfire==T)
sum(dff$deaths[dff$wildfire==T], na.rm=T) / sum(dff$wildfire==T)

sum(dff$cases[dff$wildfire==F], na.rm=T) / sum(dff$wildfire==F)
sum(dff$deaths[dff$wildfire==F], na.rm=T) / sum(dff$wildfire==F)

sum(dff$cases, na.rm=T) / sum(!is.na(dff$cases))
sum(dff$deaths, na.rm=T) / sum(!is.na(dff$deaths))

### pm2.5 in wildfire days
summary(dff$pm25)
summary(dff$pm25[dff$wildfire==T])
summary(dff$pm25[dff$wildfire==F])
# summary(dff$pm25[(dff$wildfire==F)&(dff$date<="2020-11-26")])

### days count 
wc = data.frame(dff %>% group_by(FIPS) %>% 
             summarise(sum=sum(wildfire==T)))
summary(wc$sum)
summary(wc$sum) / 277

wc = data.frame(dff %>% group_by(FIPS) %>% 
                  summarise(sum=sum(wildfire==F)))
summary(wc$sum)
summary(wc$sum) / 277

### daily cases
wc = data.frame(dff %>% group_by(FIPS) %>% 
                  summarise(mean=mean(cases,na.rm=T)))
summary(wc$mean)

wc = data.frame(dff[dff$wildfire==T,] %>% group_by(FIPS) %>% 
                  summarise(mean=mean(cases,na.rm=T)))
summary(wc$mean)

wc = data.frame(dff[dff$wildfire==F,] %>% group_by(FIPS) %>% 
                  summarise(mean=mean(cases,na.rm=T)))
summary(wc$mean)

## 
wc = data.frame(dff %>% group_by(FIPS) %>% 
                  summarise(mean=mean(deaths,na.rm=T)))
summary(wc$mean)

wc = data.frame(dff[dff$wildfire==T,] %>% group_by(FIPS) %>% 
                  summarise(mean=mean(deaths,na.rm=T)))
summary(wc$mean)

wc = data.frame(dff[dff$wildfire==F,] %>% group_by(FIPS) %>% 
                  summarise(mean=mean(deaths,na.rm=T)))
summary(wc$mean)
#############################################################
### pm2.5 the ambient level in wildfire days
summary(dff$pm25[(dff$wildfire==T)])
summary(dff$pm25[(dff$wildfire==F)])
summary(dff$pm_wildfire[(dff$wildfire==T)])
summary(dff$pm_ambient[(dff$wildfire==T)])


for (istate in unique(dff$State)) {
  subset = dff[dff$State == istate, ]
  print(istate)
  print(summary(subset$pm25[(subset$wildfire==T)]))
  print(summary(subset$pm_wildfire[(subset$wildfire==T)]))
  print(summary(subset$pm_ambient[(subset$wildfire==T)]))
  print(summary(subset$pm25[(subset$wildfire==F)]))
}
### total cases and deaths
paste(sum(dff$cases, na.rm=T), "cases")
paste(sum(dff$deaths, na.rm=T), "deaths")


