library(sf)
library(viridis)
library(reshape2)
library(ggpubr)
library(forcats)

remove(list = ls())

### Data Loading

setwd("D:/Github/covid_wildfire")
source("scr/Utilities.R")
source("scr/bayes/model.R")
source("scr/bayes/bayes_fun.R")
dff <- load.data()
dff$FIPS <- as.numeric(as.character(dff$FIPS))

# # For Sensitivity
# dff <- dff[dff$pop > median(dff$population),]
# dff <- dff[dff$date > ymd("2020-05-01"),]

Y_long_death <- data.frame(date_num = dff$date_num, FIPS = dff$FIPS, death = dff$death)
Y_tmp_death <- tidyr::spread(Y_long_death, date_num, death)
Y_deaths <- Y_tmp_death[order(Y_tmp_death$FIPS),]

# Population Size
pop_long <- data.frame(FIPS = dff$FIPS, pop = dff$population)
pop_tmp <- pop_long[!duplicated(pop_long$FIPS),]
pop <- pop_tmp[order(pop_tmp$FIPS),]

cty <- read_sf('data/cb_2018_us_county_5m', 'cb_2018_us_county_5m') %>%
  filter(STATEFP %in% c('06', '41', '53'))
cty$FIPS <- as.numeric(as.character(cty$GEOID))
cty.selected <- cty[which(cty$FIPS %in% Y_deaths[,1]),]
cty.selected$STATE <- with(cty.selected, ifelse(STATEFP == "06", "CA", ifelse(STATEFP == "41", "OR", "WA")))
cty.selected$county <- paste(cty.selected$NAME, ", ", cty.selected$STATE, sep = "")

county <- cty.selected$county[order(cty.selected$FIPS)]

### Posterior Distributions of Cumulative Effect

load("output/bayes/mcmc_cases.RData")
load("output/bayes/mcmc_deaths.RData")

## By County

# cases
eta_cases_tmp <- mcmc_cases[[1]][,sapply(1:15, function(k, ...) paste0("eta[",k,"]"))]
eta_cases_tmp <- unname(eta_cases_tmp)
cum_cases <- 100*(exp(10*rowSums(eta_cases_tmp)) - 1)
eta_cases <- 100*(exp(10*eta_cases_tmp) - 1)
out_cases <- data.frame(lag = eta_cases, cum = cum_cases, county = "Combined", estimate = "eta", pop = sum(pop[,2]))

# deaths
eta_deaths_tmp <- mcmc_deaths[[1]][,sapply(1:15, function(k, ...) paste0("eta[",k,"]"))]
eta_deaths_tmp <- unname(eta_deaths_tmp)
cum_deaths <- 100*(exp(10*rowSums(eta_deaths_tmp)) - 1)
eta_deaths <- 100*(exp(10*eta_deaths_tmp) - 1)
out_deaths <- data.frame(lag = eta_deaths, cum = cum_deaths, county = "Combined", estimate = "eta", pop = sum(pop[,2]))

for (i in 1:nrow(Y_deaths)) {
  
  theta_cases_tmp <- mcmc_cases[[1]][,sapply(1:15, function(k, ...) paste0("theta[",i,",",k,"]"))]
  theta_cases_tmp <- unname(theta_cases_tmp)
  cum_cases <- 100*(exp(rowSums(10*theta_cases_tmp)) - 1)
  theta_cases <- 100*(exp(10*theta_cases_tmp) - 1)
  out_cases_tmp <- data.frame(lag = theta_cases, cum = cum_cases, county = county[i], estimate = "theta", pop = pop[i,2])
  out_cases <- rbind(out_cases, out_cases_tmp)
  
  theta_deaths_tmp <- mcmc_deaths[[1]][,sapply(1:15, function(k, ...) paste0("theta[",i,",",k,"]"))]
  theta_deaths_tmp <- unname(theta_deaths_tmp)
  cum_deaths <- 100*(exp(10*rowSums(theta_deaths_tmp)) - 1)
  theta_deaths <- 100*(exp(10*theta_deaths_tmp) - 1)
  out_deaths_tmp <- data.frame(lag = theta_deaths, cum = cum_deaths, county = county[i], estimate = "theta", pop = pop[i,2])
  out_deaths <- rbind(out_deaths, out_deaths_tmp)
  
}

out_cases$county <- factor(out_cases$county, levels = c(rev(county), "Combined"))
out_deaths$county <- factor(out_deaths$county, levels = c(rev(county), "Combined"))

cum_cases <- mutate(subset(out_cases, county != "Combined"), county = fct_reorder(county, desc(pop))) %>% 
  ggplot(aes(x = county, y = cum)) +
  geom_boxplot() +
  ylim(-75,75) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = mean(out_cases$cum[out_cases$county == "Combined"]), color = "red") +
  geom_hline(yintercept = 0, color = "blue") + 
  coord_flip() +
  labs(title = "Cumulative Percent Change - Cases", x = "County", y = "Percentage Change in COVID-19 Cases Cumulatively\nAfter a 10 Unit Increase in PM25\nConsecutively for 14 Days")

cum_deaths <- mutate(subset(out_deaths, county != "Combined"), county = fct_reorder(county, desc(pop))) %>% 
  ggplot(aes(x = county, y = cum)) +
  geom_boxplot() +
  ylim(-75,75) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = mean(out_deaths$cum[out_deaths$county == "Combined"]), color = "red") +
  geom_hline(yintercept = 0, color = "blue") + 
  coord_flip() +
  labs(title = "Cumulative Percent Change - Deaths", x = "County", y = "Percentage Change in COVID-19 Deaths Cumulatively\nAfter a 10 Unit Increase in PM25\nConsecutively for 14 Days")

png(filename = "output/bayes/county_sensitivity.png", width = 1000, height = 1000)  
ggarrange(cum_cases, cum_deaths, ncol = 2, nrow = 1,common.legend = TRUE, legend="bottom")
dev.off()

### By Lag Time

cases <- out_cases[out_cases$county == "Combined",-ncol(out_cases)]
tmp_cases <- cbind(aggregate(out_cases[out_cases$county != "combined",1:16], by = list(out_cases$county), mean)[,c(2:17,1)], estimate = "theta")
names(tmp_cases) <- names(cases)
cases <- rbind(cases, tmp_cases)

deaths <- out_deaths[out_deaths$county == "Combined",-ncol(out_deaths)]
tmp_deaths <- cbind(aggregate(out_deaths[out_deaths$county != "combined",1:16], by = list(out_deaths$county), mean)[,c(2:17,1)], estimate = "theta")
names(tmp_deaths) <- names(deaths)
deaths <- rbind(deaths, tmp_deaths)

cases_long <- melt(cases)
cases_long$variable <- substr(cases_long$variable, 5, 6)
cases_long$variable <- factor(cases_long$variable, levels = c(paste0(15:1), ""), labels = c(0:14, "Total"))
deaths_long <- melt(deaths)
deaths_long$variable <- substr(deaths_long$variable, 5, 6)
deaths_long$variable <- factor(deaths_long$variable, levels = c(paste0(15:1), ""), labels = c(0:14, "Total"))

lag_cases <- ggplot(aes(x = variable, y = value, color = estimate), data = cases_long) +
  geom_boxplot() +
  ylim(-75,75) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Percent Change by Lag Day - Cases", color = "Estimate",
       x = "Lag Days", y = "Percentage Change in COVID-19 Cases \nWith a 10 Unit Increase in PM25")

lag_deaths <- ggplot(aes(x = variable, y = value, color = estimate), data = deaths_long) +
  geom_boxplot() +
  ylim(-25,25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Percent Change by Lag Day - Deaths", color = "Estimate",
       x = "Lag Days", y = "Percentage Change in COVID-19 Deaths \nAfter a 10 Unit Increase in PM25")

png(filename = "output/bayes/lag_sensitivity.png", width = 800, height = 600)  
ggarrange(lag_cases, lag_deaths, ncol = 2, nrow = 1,common.legend = TRUE, legend="bottom")
dev.off()


### Excess Events Ranked by 10mcg/m^3 increase

nxs_cases <- nxs_deaths <- matrix(NA, nrow = 1000, ncol = nrow(Y_deaths))
colnames(nxs_cases) <- colnames(nxs_deaths) <- county

for (i in 1:nrow(Y_deaths)) {
  
  nxs_cases[,i] <- out_cases$cum[out_cases$county == county[i]]
  nxs_deaths[,i] <- out_deaths$cum[out_deaths$county == county[i]] 
  
}

nxs_dat <- data.frame(excess_cases = colMeans(nxs_cases), 
                      excess_cases_l = apply(nxs_cases,2,hpd)[1,],
                      excess_cases_u = apply(nxs_cases,2,hpd)[2,], 
                      excess_deaths = colMeans(nxs_deaths), 
                      excess_deaths_l = apply(nxs_deaths, 2, hpd)[1,],
                      excess_deaths_u = apply(nxs_deaths, 2, hpd)[2,], 
                      FIPS = unique(dff$FIPS)[order(unique(dff$FIPS))])

cty <- read_sf('data/cb_2018_us_county_5m', 'cb_2018_us_county_5m') %>%
  filter(STATEFP %in% c('06', '41', '53'))
cty$FIPS <- as.numeric(as.character(cty$GEOID))
cty.selected <- merge(nxs_dat, cty, by = "FIPS", all.x = T)
cty.selected$STATE <- with(cty.selected, ifelse(STATEFP == "06", "CA", ifelse(STATEFP == "41", "OR", "WA")))
cty.selected$county <- paste(cty.selected$NAME, ", ", cty.selected$STATE, sep = "")

nxs_cases_plot <- mutate(cty.selected, county = fct_reorder(county, desc(excess_cases))) %>% 
  ggplot(mapping = aes(y = excess_cases, x = county)) +
  geom_pointrange(aes(ymin = excess_cases_l, ymax = excess_cases_u)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Cumulative COVID-19 Cases from 10mcg/m^3 PM2.5 Increase", x = "County", y = "Percentage Change in COVID-19 Cases Cumulatively\nAfter a 10 Unit Increase in PM25\nConsecutively for 14 Days")

nxs_deaths_plot <- mutate(cty.selected, county = fct_reorder(county, desc(excess_deaths))) %>% 
  ggplot(mapping = aes(y = excess_deaths, x = county)) +
  geom_pointrange(aes(ymin = excess_deaths_l, ymax = excess_deaths_u)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Cumulative COVID-19 Deaths from 10mcg/m^3 PM2.5 Increase", x = "County", y = "Percentage Change in COVID-19 Deaths Cumulatively\nAfter a 10 Unit Increase in PM25\nConsecutively for 14 Days")

png(filename = "output/bayes/nxs_sensitivity.png", width = 1000, height = 1000)  
ggarrange(nxs_cases_plot, nxs_deaths_plot, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
dev.off()
