library(rstudioapi)
project.dir = dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(project.dir)
source("src/Utilities.R")
dff = load.data()

dff$CountyShort = as.character(dff$County)
for (i in 1:dim(dff)[1])
  dff$CountyShort[i] = strsplit(as.character(dff$County[i]), ' County')[[1]][1]


### 6009 Calaveras ### 
aa = dff$pm25[dff$County == "Calaveras County"] 
bb = 10*dff$deaths[dff$County == "Calaveras County"]
ggplot() + 
  geom_line(aes(1:length(aa[bb!=0]), aa[bb!=0]), color='blue') + 
  geom_point(aes(1:length(aa[bb!=0]), bb[bb!=0]), color='black') +
  scale_y_continuous(sec.axis = sec_axis(~.*1000)) 
mean(aa[bb!=0], na.rm=T)
mean(aa[bb==0], na.rm=T)


dff$has_death = dff$deaths>0
tb0 = data.frame(dff %>% group_by(FIPS, CountyShort) %>% 
                   summarise(pm_has_death = median(pm25[deaths>0], na.rm=T), 
                             pm_no_death = median(pm25[deaths==0], na.rm=T),
                             n_death_days = sum(deaths>0, na.rm=T)))
tb0$diff = tb0$pm_has_death - tb0$pm_no_death
tb0$n_death_days2 = tb0$n_death_days
tb0$n_death_days2[tb0$n_death_days2>50] = 50
tb0 = tb0[order(tb0$diff), ]
ggplot(tb0, aes(pm_has_death, pm_no_death)) + 
  theme_bw() + 
  geom_point(aes(size=n_death_days), color="orange", shape=16) + 
  geom_text(aes(label=CountyShort), color = "black") + 
  xlab("median of pm2.5 on days deaths are happening") + 
  ylab("median of pm2.5 on days no death is happening")

rm(aa, bb)
rm(tb0)


##################
tb1 = data.frame(dff %>% group_by(FIPS, CountyShort) %>% 
                   summarise(pm = median(pm25, na.rm=T),
                             pop = population[1],
                             cases_sum=sum(cases, na.rm=T)))
tb1$log_cases_sum = log10(tb1$cases_sum)
tb1$labels = ""
tb1$labels[tb1$CountyShort %in% 
             c("Mono", "Mariposa", "Plumas", "Inyo")] = "highlight"
ggplot(tb1, aes(log_cases_sum, pm)) + 
  geom_point(aes(size=pop), color="black", shape=16) + 
  theme_bw() + 
  geom_text(aes(label=CountyShort, 
                color=labels)) + 
  xlab("log cases sum") + 
  ylab("median of pm2.5")

rm(tb1)
##################

