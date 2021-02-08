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



######################## Table: #####################################
### number of wildfire days by county
subset = dff[!is.na(dff$hazardmap), ]
temp = data.frame(subset %>% group_by(FIPS) %>% summarise(count=sum(hazardmap==27)))
paste(sum(temp$count), "wildfire days in the analysis/")
summary(temp$count)
summary(temp$count) / 277

### total cases and deaths
paste(sum(dff$cases, na.rm=T), "cases")
paste(sum(dff$deaths, na.rm=T), "deaths")

### percentage of cases and deaths during wildfire days 
case_fire = sum(dff$cases[dff$wildfire==T], na.rm=T)
death_fire = sum(dff$deaths[dff$wildfire==T], na.rm=T)

case_pct = round(case_fire / sum(dff$cases, na.rm=T) * 100)
death_pct = round(death_fire / sum(dff$deaths, na.rm=T) * 100)

paste(case_fire, "cases during wildfire (", case_pct, "%)")
paste(death_fire, "deaths during wildfire (", death_pct, "%)")

### pm2.5 in wildfire days
summary(dff$pm25)
summary(dff$pm25[dff$wildfire==T])
summary(dff$pm25[dff$wildfire==F])
# summary(dff$pm25[(dff$wildfire==F)&(dff$date<="2020-11-26")])

#############################################################
### pm2.5 the ambient level in wildfire days
summary(dff$pm25[(dff$wildfire==T)])
summary(dff$pm_wildfire[(dff$wildfire==T)])
summary(dff$pm_ambient[(dff$wildfire==T)])
summary(dff$pm25[(dff$wildfire==F)])
