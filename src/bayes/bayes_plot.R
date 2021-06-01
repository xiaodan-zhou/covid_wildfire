library(sf)
library(viridis)
library(reshape2)
library(ggpubr)
library(forcats)
library(dplyr)

rm(list = ls())

### Load Data
setwd("~/GitHub/covid_wildfire")
source("src/Utilities.R")
source("src/bayes/bayes_fun.R")

dff <- load.data()
dff$FIPS <- as.numeric(as.character(dff$FIPS))
FIPS <- unique(dff$FIPS)[order(unique(dff$FIPS))]

Y_long_cases <- data.frame(date_num = dff$date_num, FIPS = dff$FIPS, cases = dff$cases)
Y_tmp_cases <- tidyr::spread(Y_long_cases, date_num, cases)
Y_cases <- Y_tmp_cases[order(Y_tmp_cases$FIPS),]

Y_long_death <- data.frame(date_num = dff$date_num, FIPS = dff$FIPS, death = dff$death)
Y_tmp_death <- tidyr::spread(Y_long_death, date_num, death)
Y_deaths <- Y_tmp_death[order(Y_tmp_death$FIPS),]

total_cases <- rowSums(Y_cases[,-1], na.rm = TRUE)
total_deaths <- rowSums(Y_deaths[,-1], na.rm = TRUE)

# Population Size
pop_long <- data.frame(FIPS = dff$FIPS, pop = dff$population)
pop_tmp <- pop_long[!duplicated(pop_long$FIPS),]
pop <- pop_tmp[order(pop_tmp$FIPS),]

cty = read_sf('data/cb_2018_us_county_5m', 'cb_2018_us_county_5m')
cty = cty[cty$STATEFP %in% c('06', '41', '53'), ]
cty$FIPS <- as.numeric(as.character(cty$GEOID))
cty.selected <- cty[which(cty$FIPS %in% Y_deaths[,1]),]
cty.selected$STATE <- with(cty.selected, ifelse(STATEFP == "06", "CA", ifelse(STATEFP == "41", "OR", "WA")))
cty.selected$county <- paste(cty.selected$NAME, ", ", cty.selected$STATE, sep = "")

county <- cty.selected$county[order(cty.selected$FIPS)]

## MCMC
load("~/Dropbox/Projects/Wildfires/Output/bayes/mcmc_cases_28.RData")
load("~/Dropbox/Projects/Wildfires/Output/bayes/mcmc_deaths_28.RData")
lags <- 28:0

### Posterior Distributions of Cumulative Effect

## By County

# cases
eta_cases_tmp <- mcmc_cases[[1]][,sapply(1:29, function(k, ...) paste0("eta[",k,"]"))]
eta_cases_tmp <- unname(eta_cases_tmp)
cum_cases <- 100*(exp(10*rowSums(eta_cases_tmp)) - 1)
eta_cases <- 100*(exp(10*eta_cases_tmp) - 1)
out_cases <- data.frame(lag = eta_cases, cum = cum_cases, county = "Combined", estimate = "eta", pop = sum(pop[,2]))

# deaths
eta_deaths_tmp <- mcmc_deaths[[1]][,sapply(1:29, function(k, ...) paste0("eta[",k,"]"))]
eta_deaths_tmp <- unname(eta_deaths_tmp)
cum_deaths <- 100*(exp(10*rowSums(eta_deaths_tmp)) - 1)
eta_deaths <- 100*(exp(10*eta_deaths_tmp) - 1)
out_deaths <- data.frame(lag = eta_deaths, cum = cum_deaths, county = "Combined", estimate = "eta", pop = sum(pop[,2]))

for (i in 1:nrow(Y_deaths)) {
  
  theta_cases_tmp <- mcmc_cases[[1]][,sapply(1:29, function(k, ...) paste0("theta[",i,",",k,"]"))]
  theta_cases_tmp <- unname(theta_cases_tmp)
  cum_cases <- 100*(exp(rowSums(10*theta_cases_tmp)) - 1)
  theta_cases <- 100*(exp(10*theta_cases_tmp) - 1)
  out_cases_tmp <- data.frame(lag = theta_cases, cum = cum_cases, county = county[i], estimate = "theta", pop = pop[i,2])
  out_cases <- rbind(out_cases, out_cases_tmp)
  
  theta_deaths_tmp <- mcmc_deaths[[1]][,sapply(1:29, function(k, ...) paste0("theta[",i,",",k,"]"))]
  theta_deaths_tmp <- unname(theta_deaths_tmp)
  cum_deaths <- 100*(exp(10*rowSums(theta_deaths_tmp)) - 1)
  theta_deaths <- 100*(exp(10*theta_deaths_tmp) - 1)
  out_deaths_tmp <- data.frame(lag = theta_deaths, cum = cum_deaths, county = county[i], estimate = "theta", pop = pop[i,2])
  out_deaths <- rbind(out_deaths, out_deaths_tmp)
  
}

out_cases$county <- factor(out_cases$county, levels = c(rev(county), "Combined"))
out_deaths$county <- factor(out_deaths$county, levels = c(rev(county), "Combined"))
out_cases <- merge(out_cases, aggregate(out_cases$cum, by = list(out_cases$county), mean), by.x = "county", by.y = "Group.1")
out_deaths <- merge(out_deaths, aggregate(out_deaths$cum, by = list(out_deaths$county), mean), by.x = "county", by.y = "Group.1")


### boxplot with wildfire days 
dff = dff %>% group_by(FIPS) %>% 
  mutate(pm_wildfire_days=sum(wildfire==T, na.rm=T)) %>% 
  ungroup()

dff$county = as.character(dff$County)
for (i in 1:dim(dff)[1])
  dff$county[i] = paste0(strsplit(as.character(dff$County[i]), ' County')[[1]][1], ", ", dff$State[i])

levels(out_cases$county) = c(levels(out_cases$county), "Pooled")
levels(out_cases$county)[match("Combined",levels(out_cases$county))] = "Pooled"

levels(out_deaths$county) = c(levels(out_deaths$county), "Pooled")
levels(out_deaths$county)[match("Combined",levels(out_deaths$county))] = "Pooled"

# dff = subset(dff, select = -x)
dff = merge(dff, unique(out_cases[c("county", "x")]), by="county")
label_colors = ifelse(levels(reorder(out_cases$county, -out_cases$x)) == "Pooled", "red", "black")

cum_cases <- ggplot() + 
  geom_boxplot(data=mutate(out_cases, county = reorder(county, -x), box_col = county=="Pooled"), 
               aes(x = county, y = cum, fill=box_col), lwd=0.4, width=0.8, outlier.size=0.6) + 
  geom_point(data=mutate(dff, county = reorder(county, -x)), aes(x = county, y = (pm_wildfire_days/277*100-35/3)*30/7), color="orange", shape=17) + 
  geom_hline(yintercept = 0, color = "blue") + 
  theme_bw() + 
  coord_flip() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(), 
        axis.text.y = element_text(colour = label_colors),
        legend.position = "none") + 
  scale_fill_manual(values = c("#00BA38", "red")) + 
  scale_y_continuous("Percentage Increase", limits = c(-60, 105), sec.axis = sec_axis(~.*7/30+35/3, name = "% of Wildfire Days"))

dff = subset(dff, select = -x)
dff = merge(dff, unique(out_deaths[c("county", "x")]), by="county")
label_colors = ifelse(levels(reorder(out_deaths$county, -out_deaths$x)) == "Pooled", "red", "black")

cum_deaths <- ggplot() + 
  geom_boxplot(data=mutate(out_deaths, county = reorder(county, -x), box_col = county=="Pooled"),
               aes(x = county, y = cum, fill=box_col), lwd=0.4, width=0.8, outlier.size=0.6) + 
  geom_point(data=mutate(dff, county = reorder(county, -x)), aes(x = county, y = (pm_wildfire_days/277*100-35/3)*30/7), color="orange", shape=17) + 
  geom_hline(yintercept = 0, color = "blue") + 
  theme_bw() + 
  coord_flip() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y=element_blank(),
        axis.text.y = element_text(colour = label_colors),
        legend.position = "none") + 
  scale_fill_manual(values = c("#00BA38", "red")) +
  scale_y_continuous("Percentage Increase", limits = c(-60, 105), sec.axis = sec_axis(~.*7/30+35/3, name = "% of Wildfire Days"))

plot.list = list()
plot.list[[1]] = cum_cases
plot.list[[2]] = cum_deaths
pdf("~/Dropbox/Projects/Wildfires/Output/bayes/county_cumulative_days_28.pdf", width = 10, height = 10)
do.call('grid.arrange', c(plot.list, ncol = 2))
dev.off()

## By Lag Day

# cases
eta_cases <- as.matrix(eta_cases)
colnames(eta_cases) <- c(lags)
cases_lag_coefs <- melt(eta_cases, value.name = "val")
colnames(cases_lag_coefs)[2] <- "lags"
cases_lag_coefs$lags <- factor(cases_lag_coefs$lags, levels = c(0:28))

lag_cases <- ggplot(aes(x = lags, y = val), data = cases_lag_coefs) + theme_bw() + 
  geom_boxplot() +
  ylim(-2,3.5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, color = "blue") + 
  labs(x = "Lag Days", y = "Effect")  

# deaths
eta_deaths <- as.matrix(eta_deaths)
colnames(eta_deaths) <- c(lags)
deaths_lag_coefs <- melt(eta_deaths, value.name = "val")
colnames(deaths_lag_coefs)[2] <- "lags"
deaths_lag_coefs$lags <- factor(deaths_lag_coefs$lags, levels = c(0:28))

lag_deaths <- ggplot(aes(x = lags, y = val), data = deaths_lag_coefs) + theme_bw() + 
  geom_boxplot() +
  ylim(-2,3.5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, color = "blue") + 
  labs(x = "Lag Days", y = "Effect")  

plot.list = list()
plot.list[[1]] = lag_cases
plot.list[[2]] = lag_deaths
pdf("~/Dropbox/Projects/Wildfires/Output/bayes/pct_increases_28.pdf", width = 10, height = 5)
do.call('grid.arrange',c(plot.list, ncol = 2))
dev.off()

### Total Deaths and Cases by county

nxs_cases <- nxs_deaths <- matrix(NA, nrow = 1000, ncol = 92)
colnames(nxs_cases) <- colnames(nxs_deaths) <- FIPS

for (i in 1:92) {
  
  lambda_cases <- mcmc_cases[[1]][,sapply(1:277, function(j, ...) paste0("lambda[",i,",",j,"]"))]
  lambda_deaths <- mcmc_deaths[[1]][,sapply(1:277, function(j, ...) paste0("lambda[",i,",",j,"]"))]
  rho_cases <- mcmc_cases[[1]][,sapply(1:277, function(j, ...) paste0("rho[",i,",",j,"]"))]
  rho_deaths <- mcmc_deaths[[1]][,sapply(1:277, function(j, ...) paste0("rho[",i,",",j,"]"))]
  
  Y_case_mat <- matrix(as.numeric(rep(Y_cases[i,-1], 1000)), byrow = T, nrow = 1000)
  Y_death_mat <- matrix(as.numeric(rep(Y_deaths[i,-1], 1000)), byrow = T, nrow = 1000)  
  
  nxs_cases[,i] <- rowSums((1 - rho_cases/lambda_cases)*Y_case_mat, na.rm = TRUE)
  nxs_deaths[,i] <- rowSums((1 - rho_deaths/lambda_deaths)*Y_death_mat, na.rm = TRUE)
  
  cbind(c(Y_deaths[i,-1]), c(colMeans(rho_deaths/lambda_deaths)))
  
}

total_dat <- data.frame(excess_cases = colMeans(nxs_cases, na.rm = TRUE), 
                        excess_cases_l = apply(nxs_cases,2,hpd)[1,],
                        excess_cases_u = apply(nxs_cases,2,hpd)[2,], 
                        excess_deaths = colMeans(nxs_deaths,na.rm = TRUE), 
                        excess_deaths_l = apply(nxs_deaths, 2, hpd)[1,],
                        excess_deaths_u = apply(nxs_deaths, 2, hpd)[2,],  FIPS = FIPS)

total_dat <- round(total_dat, 3)

cty.total <- merge(total_dat, cty, by = "FIPS", all.x = T)
cty.total$STATE <- with(cty.total, ifelse(STATEFP == "06", "CA", ifelse(STATEFP == "41", "OR", "WA")))
cty.total$county <- paste(cty.total$NAME, ", ", cty.total$STATE, sep = "")
out <- cty.total[,c("county", names(total_dat))]

write.csv(out, file = "~/Dropbox/Projects/Wildfires/Output/bayes/totals_28.csv")

## Excess Events Ranked

pct_cases <- pct_deaths <- matrix(NA, nrow = 1000, ncol = 92)
colnames(pct_cases) <- colnames(pct_deaths) <- FIPS

for (i in 1:92) {
  
  pct_cases[,i] <- 100*nxs_cases[,i]/(total_cases[i] - nxs_cases[,i])
  pct_deaths[,i] <- 100*nxs_deaths[,i]/(total_deaths[i] - nxs_deaths[,i])
  
}

pct_dat <- data.frame(excess_cases = colMeans(pct_cases, na.rm = TRUE), 
                      excess_cases_l = apply(pct_cases,2,hpd)[1,],
                      excess_cases_u = apply(pct_cases,2,hpd)[2,], 
                      excess_deaths = colMeans(pct_deaths, na.rm = TRUE), 
                      excess_deaths_l = apply(pct_deaths, 2, hpd)[1,],
                      excess_deaths_u = apply(pct_deaths, 2, hpd)[2,],  FIPS = FIPS)

pct_dat$excess_deaths[which(is.nan(pct_dat$excess_deaths))] <- 0
pct_dat$excess_deaths_l[which(is.na(pct_dat$excess_deaths_l))] <- 0
pct_dat$excess_deaths_u[which(is.na(pct_dat$excess_deaths_u))] <- 0

cty.pct <- merge(pct_dat, cty, by = "FIPS", all.x = T) 
cty.pct$STATE <- with(cty.pct, ifelse(STATEFP == "06", "CA", ifelse(STATEFP == "41", "OR", "WA")))
cty.pct$county <- paste(cty.pct$NAME, ", ", cty.pct$STATE, sep = "")

nxs_cases_plot <- mutate(cty.pct, county = reorder(county, -excess_cases)) %>%  
  ggplot(mapping = aes(y = excess_cases, x = county)) + theme_bw() + 
  geom_pointrange(aes(ymin = excess_cases_l, ymax = excess_cases_u)) +
  geom_hline(yintercept = 0, color = "blue") +
  coord_flip() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) 

nxs_deaths_plot <- mutate(cty.pct, county = reorder(county, -excess_deaths)) %>% 
  ggplot(mapping = aes(y = excess_deaths, x = county)) + theme_bw() + 
  geom_pointrange(aes(ymin = excess_deaths_l, ymax = excess_deaths_u)) +
  geom_hline(yintercept = 0, color = "blue") +
  coord_flip() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())

plot.list = list()
plot.list[[1]] = nxs_cases_plot
plot.list[[2]] = nxs_deaths_plot
pdf("~/Dropbox/Projects/Wildfires/Output/bayes/nxs_ranked_28.pdf", width = 10, height = 10)
do.call('grid.arrange',c(plot.list, ncol = 2))
dev.off()

## Excess Events Mapped

cty = read_sf('data/cb_2018_us_county_5m', 'cb_2018_us_county_5m')
cty = cty[cty$STATEFP %in% c('06', '41', '53'), ]
cty$FIPS = as.numeric(as.character(cty$GEOID))

cty.pct <- merge(cty, pct_dat, by="FIPS", all.x=T)
cty.pct$STATE <- with(cty.pct, ifelse(STATEFP == "06", "CA", ifelse(STATEFP == "41", "OR", "WA")))
cty.pct$county <- paste(cty.pct$NAME, ", ", cty.pct$STATE, sep = "")
cty.pct$excess_deaths[cty.pct$excess_deaths > 50] <- 49.9
cty.pct$excess_deaths[cty.pct$excess_deaths < -10] <- -9.9

### WA 
wa_cases <- ggplot(data = cty.pct[cty.pct$STATEFP == "53", ]) + 
  ggtitle("Washington") + 
  geom_sf(aes_string(geometry = "geometry", fill = "excess_cases"), color = NA) + 
  theme_void() + 
  labs(fill = "Percentage of COVID19 cases") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_gradient2(low = 'blue', mid = 'beige', high = 'red', 
                       na.value = "lightgrey", limits=c(-10, 20)) + 
  theme(legend.position = "None")

wa_deaths <- ggplot(cty.pct[cty.pct$STATEFP == "53", ]) + 
  ggtitle("Washington") + 
  geom_sf(aes_string(geometry = "geometry", fill = "excess_deaths"), color=NA) + 
  theme_void() + 
  labs(fill = "Percentage of COVID19 deaths")+
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_gradient2(low = 'blue', mid = 'beige', high = 'red', 
                       na.value = "lightgrey", limits=c(-10, 50)) +
  theme(legend.position = "None")

### OR 
or_cases <- ggplot(cty.pct[cty.pct$STATEFP == "41", ]) + ggtitle("Oregon") + 
  geom_sf(aes_string(geometry = "geometry", fill = "excess_cases"), color=NA) + 
  theme_void() + 
  labs(fill = "Percentage of COVID19 cases")+
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_gradient2(low = 'blue', mid = 'beige', high = 'red', 
                       na.value = "lightgrey", limits=c(-10, 20)) + 
  theme(legend.position = "bottom")

or_deaths <- ggplot(cty.pct[cty.pct$STATEFP == "41", ]) + ggtitle("Oregon") + 
  geom_sf(aes_string(geometry = "geometry", fill = "excess_deaths"), color=NA) + 
  theme_void() + 
  labs(fill = "Percentage of COVID19 deaths")+
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_gradient2(low = 'blue', mid = 'beige', high = 'red', 
                       na.value = "lightgrey", limits=c(-10, 50)) + 
  theme(legend.position = "bottom")

### CA
ca_cases <- ggplot(cty.pct[cty.pct$STATEFP == "06", ]) + ggtitle("California") + 
  geom_sf(aes_string(geometry = "geometry", fill= "excess_cases"), color=NA) + 
  theme_void() + 
  labs(fill = "Percentage of COVID19 cases")+
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_gradient2(low = 'blue', mid = 'beige', high = 'red', 
                       na.value = "lightgrey", limits=c(-10, 20)) + 
  theme(legend.position = "None")

ca_deaths <- ggplot(cty.pct[cty.pct$STATEFP == "06", ]) + ggtitle("California") + 
  geom_sf(aes_string(geometry = "geometry", fill= "excess_deaths"), color=NA) + 
  theme_void() + 
  labs(fill = "Percentage of COVID19 deaths")+
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_gradient2(low = 'blue', mid = 'beige', high = 'red', 
                       na.value = "lightgrey", limits=c(-10, 50)) + 
  theme(legend.position = "None")

plot.list = list()
plot.list[[1]] = wa_cases
plot.list[[2]] = or_cases
plot.list[[3]] = ca_cases
pdf("~/Dropbox/Projects/Wildfires/Output/bayes/nxs_mapped_cases_28.pdf", width = 10, height = 10)
do.call('grid.arrange',c(plot.list, ncol = 3,  top = "", bottom="", left=""))
dev.off()

plot.list = list()
plot.list[[1]] = wa_deaths
plot.list[[2]] = or_deaths
plot.list[[3]] = ca_deaths
pdf("~/Dropbox/Projects/Wildfires/Output/bayes/nxs_mapped_deaths_28.pdf", width = 10, height = 10)
do.call('grid.arrange',c(plot.list, ncol = 3, top = "", bottom="", left=""))
dev.off()
