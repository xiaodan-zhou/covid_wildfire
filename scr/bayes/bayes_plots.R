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

X_wildfire_long <- data.frame(date_num = dff$date_num, FIPS = dff$FIPS, pm25 = dff$pm_wildfire)
X_wildfire_tmp <- tidyr::spread(X_wildfire_long, date_num, pm25)
X_wildfire <- X_wildfire_tmp[order(X_wildfire_tmp$FIPS),]

Y_long_cases <- data.frame(date_num = dff$date_num, FIPS = dff$FIPS, cases = dff$cases)
Y_tmp_cases <- tidyr::spread(Y_long_cases, date_num, cases)
Y_cases <- Y_tmp_cases[order(Y_tmp_cases$FIPS),]

Y_long_death <- data.frame(date_num = dff$date_num, FIPS = dff$FIPS, death = dff$death)
Y_tmp_death <- tidyr::spread(Y_long_death, date_num, death)
Y_deaths <- Y_tmp_death[order(Y_tmp_death$FIPS),]

total_cases <- rowSums(Y_cases[,-1], na.rm = TRUE)
total_deaths <- rowSums(Y_deaths[,-1], na.rm = TRUE)
  
### Average Lag Coefficients

load("output/bayes/mcmc_cases.RData")
load("output/bayes/mcmc_deaths.RData")
lags <- 14:0

# cases
eta_cases_tmp <- mcmc_cases[[1]][,sapply(1:15, function(k, ...) paste0("eta[",k,"]"))]
cum_cases <- 100*(exp(10*rowSums(eta_cases_tmp)) - 1)
eta_cases <- 100*(exp(10*eta_cases_tmp) - 1)

cases_lag_coefs_wide <- cbind(eta_cases, cum_cases)
colnames(cases_lag_coefs_wide) <- c(lags, "Total")

cases_lag_coefs <- melt(cases_lag_coefs_wide, value.name = "val")
colnames(cases_lag_coefs)[2] <- "lags"
cases_lag_coefs$lags <- factor(cases_lag_coefs$lags, levels = c(0:14, "Total"))

lag_cases <- ggplot(aes(x = lags, y = val), data = cases_lag_coefs) +
  geom_boxplot() +
  ylim(-5,12) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Pooled Percent Change - Cases", x = "Lag Days", y = "Percent Change in Cases\nper 10mcg/m^3 in PM2.5")

# deaths
eta_deaths_tmp <- mcmc_deaths[[1]][,sapply(1:15, function(k, ...) paste0("eta[",k,"]"))]
cum_deaths <- 100*(exp(rowSums(10*eta_deaths_tmp)) - 1)
eta_deaths <- 100*(exp(10*eta_deaths_tmp) - 1)

deaths_lag_coefs_wide <- cbind(eta_deaths, cum_deaths)
colnames(deaths_lag_coefs_wide) <- c(lags, "Total")

deaths_lag_coefs <- melt(deaths_lag_coefs_wide, value.name = "val")
colnames(deaths_lag_coefs)[2] <- "lags"
deaths_lag_coefs$lags <- factor(deaths_lag_coefs$lags, levels = c(0:14, "Total"))

lag_deaths <- ggplot(aes(x = lags, y = val), data = deaths_lag_coefs) +
  geom_boxplot() +
  ylim(-5,12) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Average Percent Change - Deaths", x = "Lag Days", y = "Percent Change in Deaths\nper 10mcg/m^3 in PM2.5")

png(filename = "output/bayes/pct_increases.png", width = 600, height = 400)  
ggarrange(lag_cases, lag_deaths, ncol = 2, nrow = 1,common.legend = TRUE, legend="bottom")
dev.off()

### Excess Events Ranked

nxs_cases <- nxs_deaths <- matrix(NA, nrow = 1000, ncol = 92)
colnames(nxs_cases) <- colnames(nxs_deaths) <- FIPS

for (i in 1:92) {

  G_cases <- mcmc_cases[[1]][,sapply(1:277, function(j, ...) paste0("G[",i,",",j,"]"))]
  G_deaths <- mcmc_deaths[[1]][,sapply(1:277, function(j, ...) paste0("G[",i,",",j,"]"))]
  H_cases <- mcmc_cases[[1]][,sapply(1:277, function(j, ...) paste0("H[",i,",",j,"]"))]
  H_deaths <- mcmc_deaths[[1]][,sapply(1:277, function(j, ...) paste0("H[",i,",",j,"]"))]

  nxs_cases[,i] <- 100*rowSums(G_cases)/total_cases[i]
  nxs_deaths[,i] <- 100*rowSums(G_deaths)/total_deaths[i]
  
}

nxs_dat <- data.frame(excess_cases = colMeans(nxs_cases), 
                        excess_cases_l = apply(nxs_cases,2,hpd)[1,],
                        excess_cases_u = apply(nxs_cases,2,hpd)[2,], 
                        excess_deaths = colMeans(nxs_deaths), 
                        excess_deaths_l = apply(nxs_deaths, 2, hpd)[1,],
                        excess_deaths_u = apply(nxs_deaths, 2, hpd)[2,],  FIPS = FIPS)

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
  labs(title = "Excess COVID-19 Cases from Wildfire", x = "County", y = "Excess Case Percentage")

nxs_deaths_plot <- mutate(cty.selected, county = fct_reorder(county, desc(excess_deaths))) %>% 
  ggplot(mapping = aes(y = excess_deaths, x = county)) +
  geom_pointrange(aes(ymin = excess_deaths_l, ymax = excess_deaths_u)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Excess COVID-19 Deaths from Wildfire", x = "County", y = "Excess Death Percentage")

png(filename = "output/bayes/nxs_ranked.png", width = 1000, height = 1000)  
ggarrange(nxs_cases_plot, nxs_deaths_plot, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
dev.off()

### Map of Excess Events

cty <- read_sf('data/cb_2018_us_county_5m', 'cb_2018_us_county_5m') %>%
  filter(STATEFP %in% c('06', '41', '53'))
cty$FIPS <- as.numeric(as.character(cty$GEOID))
cty.selected <- merge(cty, nxs_dat, by = "FIPS", all.x = T)

### WA 
wa_cases <- ggplot(cty.selected[cty.selected$STATEFP == "53", ]) + ggtitle("Washington Excess Cases") + 
  geom_sf(aes_string(fill = "excess_cases"), color=NA) + 
  theme_void() + 
  scale_fill_viridis()+
  labs(fill = "Excess Event\nPercentage") +
  theme(plot.title = element_text(hjust = 0.5))

wa_deaths <- ggplot(cty.selected[cty.selected$STATEFP == "53", ]) + ggtitle("Washington Excess Deaths") + 
  geom_sf(aes_string(fill = "excess_deaths"), color=NA) + 
  theme_void() + 
  scale_fill_viridis()+
  labs(fill = "Excess Event\nPercentage")+
  theme(plot.title = element_text(hjust = 0.5))

### OR 
or_cases <- ggplot(cty.selected[cty.selected$STATEFP == "41", ]) + ggtitle("Oregon Excess Cases") + 
  geom_sf(aes_string(fill = "excess_cases"), color=NA) + 
  theme_void() + 
  scale_fill_viridis()+
  labs(fill = "Excess Event\nPercentage")+
  theme(plot.title = element_text(hjust = 0.5))

or_deaths <- ggplot(cty.selected[cty.selected$STATEFP == "41", ]) + ggtitle("Oregon Excess Deaths") + 
  geom_sf(aes_string(fill = "excess_deaths"), color=NA) + 
  theme_void() + 
  scale_fill_viridis()+
  labs(fill = "Excess Event\nPercentage")+
  theme(plot.title = element_text(hjust = 0.5))

### CA
ca_cases <- ggplot(cty.selected[cty.selected$STATEFP == "06", ]) + ggtitle("California Excess Cases") + 
  geom_sf(aes_string(fill= "excess_cases"), color=NA) + 
  theme_void() + 
  scale_fill_viridis()+
  labs(fill = "Excess Event\nPercentage")+
  theme(plot.title = element_text(hjust = 0.5))

ca_deaths <- ggplot(cty.selected[cty.selected$STATEFP == "06", ]) + ggtitle("California Excess Deaths") + 
  geom_sf(aes_string(fill= "excess_deaths"), color=NA) + 
  theme_void() + 
  scale_fill_viridis() +
  labs(fill = "Excess Event\nPercentage")+
  theme(plot.title = element_text(hjust = 0.5))

png(filename = "output/bayes/nxs_mapped.png", width = 1000, height = 1000)  
ggarrange(wa_cases, or_cases, ca_cases, wa_deaths, or_deaths, ca_deaths, ncol = 3, nrow = 2,common.legend = TRUE, legend = "bottom")
dev.off()
