library(rstudioapi)
project.dir = dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(project.dir)
source("scr/Utilities.R")
dff = load.data()


#############################################################
### how many counties in each state
length(unique(dff$FIPS))
for (istate in unique(dff$State)) {
  print(paste(istate, length(unique(dff$FIPS[dff$State == istate]))))
}

### how many days 
max(dff$date)
min(dff$date)
max(dff$date) - min(dff$date) + 1

### population coverage
total.pop = 0
for (ifips in unique(dff$FIPS)) {
  total.pop = total.pop + dff$population[dff$FIPS == ifips][1] }
total.pop
print(paste("population coverage", total.pop / 51344853 * 100, "%"))

### missing summary
sum(is.na(dff$cases)) / dim(dff)[1]
sum(is.na(dff$deaths)) / dim(dff)[1]
sum(is.na(dff$relative_change_feb)) / dim(dff)[1]

### number of wildfire days
subset = dff[!is.na(dff$hazardmap), ]
temp = data.frame(subset %>% group_by(FIPS) %>% summarise(count=sum(hazardmap==27)))
summary(temp$count)
summary(temp$count) / 277
sum(temp$count)

### pm2.5 been replaced by historical data 
summary(dff$pm25_history[is.na(dff$pm25_raw)])

#############################################################
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
subset = subset[subset$hazardmap==27,]
print(paste0("Total & ", 
             round(median(subset$pm25), 1), " (", 
             round(quantile(subset$pm25, .25), 1), " - ", 
             round(quantile(subset$pm25, .75), 1), ")"))


## pm2.5 summary during non-wildfire
for (istate in unique(dff$State)) {
  subset = dff[!is.na(dff$hazardmap), ]
  subset = subset[(subset$State == istate)&(subset$hazardmap!=27),]
  print(paste0(istate, " & ", 
               round(median(subset$pm25, na.rm=T), 1), " (", 
               round(quantile(subset$pm25, .25), 1), " - ", 
               round(quantile(subset$pm25, .75), 1), ")"))
}

subset = dff[!is.na(dff$hazardmap), ]
subset = subset[subset$hazardmap!=27,]
print(paste0("Total & ", 
             round(median(subset$pm25), 1), " (", 
             round(quantile(subset$pm25, .25), 1), " - ", 
             round(quantile(subset$pm25, .75), 1), ")"))

#############################################################


