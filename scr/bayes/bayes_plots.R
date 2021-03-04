library(sf)
library(viridis)
library(reshape2)
library(ggpubr)
library(forcats)

rm(list = ls())

setwd("D:/Github/covid_wildfire")
source("scr/Utilities.R")
source("scr/bayes/bayes_fun.R")
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

cty <- read_sf('data/cb_2018_us_county_5m', 'cb_2018_us_county_5m') %>%
  filter(STATEFP %in% c('06', '41', '53'))
cty$FIPS <- as.numeric(as.character(cty$GEOID))
cty.selected <- cty[which(cty$FIPS %in% Y_deaths[,1]),]
cty.selected$STATE <- with(cty.selected, ifelse(STATEFP == "06", "CA", ifelse(STATEFP == "41", "OR", "WA")))
cty.selected$county <- paste(cty.selected$NAME, ", ", cty.selected$STATE, sep = "")

county <- cty.selected$county[order(cty.selected$FIPS)]
  
### Posterior Distributions of Cumulative Effect

load("D:/Dropbox (Personal)/Projects/Wildfires/Output/bayes/mcmc_cases.RData")
load("D:/Dropbox (Personal)/Projects/Wildfires/Output/bayes/mcmc_deaths.RData")
lags <- 14:0

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
  ylim(-100,100) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = mean(out_cases$cum[out_cases$county == "Combined"]), color = "red") +
  geom_hline(yintercept = 0, color = "blue") + 
  coord_flip() +
  labs(title = "Cumulative Percent Change - Cases", x = "County", y = "Percentage Change in COVID-19 Cases Cumulatively\nAfter a 10 Unit Increase in PM25\nConsecutively for 14 Days")

cum_deaths <- mutate(subset(out_deaths, county != "Combined"), county = fct_reorder(county, desc(pop))) %>% 
  ggplot(aes(x = county, y = cum)) +
  geom_boxplot() +
  ylim(-100,100) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = mean(out_deaths$cum[out_deaths$county == "Combined"]), color = "red") +
  geom_hline(yintercept = 0, color = "blue") + 
  coord_flip() +
  labs(title = "Cumulative Percent Change - Deaths", x = "County", y = "Percentage Change in COVID-19 Deaths Cumulatively\nAfter a 10 Unit Increase in PM25\nConsecutively for 14 Days")

png(filename = "D:/Dropbox (Personal)/Projects/Wildfires/Output/bayes/county_cumulative.png", width = 1000, height = 1000)  
ggarrange(cum_cases, cum_deaths, ncol = 2, nrow = 1,common.legend = TRUE, legend="bottom")
dev.off()

## By Lag Day

# cases
colnames(eta_cases) <- c(lags)
cases_lag_coefs <- melt(eta_cases, value.name = "val")
colnames(cases_lag_coefs)[2] <- "lags"
cases_lag_coefs$lags <- factor(cases_lag_coefs$lags, levels = c(0:14))

lag_cases <- ggplot(aes(x = lags, y = val), data = cases_lag_coefs) +
  geom_boxplot() +
  ylim(-2,2.5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, color = "blue") + 
  labs(title = "Average Percent Change - Cases", x = "Lag Days", y = "Average Percent Change in Cases\nper Unit Increase in PM2.5")

# deaths
colnames(eta_deaths) <- c(lags)
deaths_lag_coefs <- melt(eta_deaths, value.name = "val")
colnames(deaths_lag_coefs)[2] <- "lags"
deaths_lag_coefs$lags <- factor(deaths_lag_coefs$lags, levels = c(0:14))

lag_deaths <- ggplot(aes(x = lags, y = val), data = deaths_lag_coefs) +
  geom_boxplot() +
  ylim(-2,2.5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, color = "blue") + 
  labs(title = "Average Percent Change - Deaths", x = "Lag Days", y = " Average Percent Change in Deaths\nper Unit Increase in PM2.5")

png(filename = "D:/Dropbox (Personal)/Projects/Wildfires/Output/bayes/pct_increases.png", width = 600, height = 400)  
ggarrange(lag_cases, lag_deaths, ncol = 2, nrow = 1,common.legend = TRUE, legend="bottom")
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
  
}

total_dat <- data.frame(excess_cases = colMeans(nxs_cases), 
                        excess_cases_l = apply(nxs_cases,2,hpd)[1,],
                        excess_cases_u = apply(nxs_cases,2,hpd)[2,], 
                        excess_deaths = colMeans(nxs_deaths), 
                        excess_deaths_l = apply(nxs_deaths, 2, hpd)[1,],
                        excess_deaths_u = apply(nxs_deaths, 2, hpd)[2,],  FIPS = FIPS)

total_dat <- round(total_dat, 3)

cty <- read_sf('data/cb_2018_us_county_5m', 'cb_2018_us_county_5m') %>%
  filter(STATEFP %in% c('06', '41', '53'))
cty$FIPS <- as.numeric(as.character(cty$GEOID))
cty.selected <- merge(total_dat, cty, by = "FIPS", all.x = T)
cty.selected$STATE <- with(cty.selected, ifelse(STATEFP == "06", "CA", ifelse(STATEFP == "41", "OR", "WA")))
cty.selected$county <- paste(cty.selected$NAME, ", ", cty.selected$STATE, sep = "")
out <- cty.selected[,c("county", names(total_dat))]

write.csv(out, file = "D:/Dropbox (Personal)/Projects/Wildfires/Output/bayes/totals.csv")

## Excess Events Ranked

pct_cases <- pct_deaths <- matrix(NA, nrow = 1000, ncol = 92)
colnames(pct_cases) <- colnames(pct_deaths) <- FIPS

for (i in 1:92) {

  pct_cases[,i] <- 100*nxs_cases[,i]/(total_cases[i] - nxs_cases[,i])
  pct_deaths[,i] <- 100*nxs_deaths[,i]/(total_deaths[i] - nxs_deaths[,i])
  
}

pct_dat <- data.frame(excess_cases = colMeans(pct_cases), 
                        excess_cases_l = apply(pct_cases,2,hpd)[1,],
                        excess_cases_u = apply(pct_cases,2,hpd)[2,], 
                        excess_deaths = colMeans(pct_deaths), 
                        excess_deaths_l = apply(pct_deaths, 2, hpd)[1,],
                        excess_deaths_u = apply(pct_deaths, 2, hpd)[2,],  FIPS = FIPS)

cty <- read_sf('data/cb_2018_us_county_5m', 'cb_2018_us_county_5m') %>%
  filter(STATEFP %in% c('06', '41', '53'))
cty$FIPS <- as.numeric(as.character(cty$GEOID))
cty.selected <- merge(pct_dat, cty, by = "FIPS", all.x = T)
cty.selected$STATE <- with(cty.selected, ifelse(STATEFP == "06", "CA", ifelse(STATEFP == "41", "OR", "WA")))
cty.selected$county <- paste(cty.selected$NAME, ", ", cty.selected$STATE, sep = "")

nxs_cases_plot <- mutate(cty.selected, county = fct_reorder(county, desc(excess_cases))) %>% 
  ggplot(mapping = aes(y = excess_cases, x = county)) +
  geom_pointrange(aes(ymin = excess_cases_l, ymax = excess_cases_u)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Excess COVID-19 Cases from Wildfire", x = "County", y = "Excess Case Percentage")

nxs_deaths_plot <- mutate(cty.selected, county = fct_reorder(county, desc(excess_deaths))) %>% 
  ggplot(mapping = aes(y = excess_deaths, x = county)) +
  geom_pointrange(aes(ymin = excess_deaths_l, ymax = excess_deaths_u)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Excess COVID-19 Deaths from Wildfire", x = "County", y = "Excess Death Percentage")

png(filename = "D:/Dropbox (Personal)/Projects/Wildfires/Output/bayes/nxs_ranked.png", width = 1000, height = 1000)  
ggarrange(nxs_cases_plot, nxs_deaths_plot, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
dev.off()

## Excess Events Mapped

cty <- read_sf('data/cb_2018_us_county_5m', 'cb_2018_us_county_5m') %>%
  filter(STATEFP %in% c('06', '41', '53'))
cty$FIPS <- as.numeric(as.character(cty$GEOID))
cty.selected <- merge(cty, pct_dat, by = "FIPS", all.x = T)

### WA 
wa_cases <- ggplot(cty.selected[cty.selected$STATEFP == "53", ]) + ggtitle("Washington Excess Cases") + 
  geom_sf(aes_string(fill = "excess_cases"), color=NA) + 
  theme_void() + 
  scale_fill_viridis()+
  labs(fill = "Excess Event Percentage") +
  theme(plot.title = element_text(hjust = 0.5))

wa_deaths <- ggplot(cty.selected[cty.selected$STATEFP == "53", ]) + ggtitle("Washington Excess Deaths") + 
  geom_sf(aes_string(fill = "excess_deaths"), color=NA) + 
  theme_void() + 
  scale_fill_viridis()+
  labs(fill = "Excess Event Percentage")+
  theme(plot.title = element_text(hjust = 0.5))

### OR 
or_cases <- ggplot(cty.selected[cty.selected$STATEFP == "41", ]) + ggtitle("Oregon Excess Cases") + 
  geom_sf(aes_string(fill = "excess_cases"), color=NA) + 
  theme_void() + 
  scale_fill_viridis()+
  labs(fill = "Excess Event Percentage")+
  theme(plot.title = element_text(hjust = 0.5))

or_deaths <- ggplot(cty.selected[cty.selected$STATEFP == "41", ]) + ggtitle("Oregon Excess Deaths") + 
  geom_sf(aes_string(fill = "excess_deaths"), color=NA) + 
  theme_void() + 
  scale_fill_viridis()+
  labs(fill = "Excess Event Percentage")+
  theme(plot.title = element_text(hjust = 0.5))

### CA
ca_cases <- ggplot(cty.selected[cty.selected$STATEFP == "06", ]) + ggtitle("California Excess Cases") + 
  geom_sf(aes_string(fill= "excess_cases"), color=NA) + 
  theme_void() + 
  scale_fill_viridis()+
  labs(fill = "Excess Event Percentage")+
  theme(plot.title = element_text(hjust = 0.5))

ca_deaths <- ggplot(cty.selected[cty.selected$STATEFP == "06", ]) + ggtitle("California Excess Deaths") + 
  geom_sf(aes_string(fill= "excess_deaths"), color=NA) + 
  theme_void() + 
  scale_fill_viridis() +
  labs(fill = "Excess Event Percentage")+
  theme(plot.title = element_text(hjust = 0.5))

png(filename = "D:/Dropbox (Personal)/Projects/Wildfires/Output/bayes/nxs_mapped.png", width = 1000, height = 1000)  
ggarrange(wa_cases, or_cases, ca_cases, wa_deaths, or_deaths, ca_deaths, ncol = 3, nrow = 2,common.legend = TRUE, legend = "bottom")
dev.off()
