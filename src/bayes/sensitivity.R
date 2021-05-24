library(sf)
library(viridis)
library(reshape2)
library(ggpubr)
library(forcats)

rm(list = ls())

### Load Data

setwd("~/Github/covid_wildfire")
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

cty <- read_sf('data/cb_2018_us_county_5m', 'cb_2018_us_county_5m') %>%
  filter(STATEFP %in% c('06', '41', '53'))
cty$FIPS <- as.numeric(as.character(cty$GEOID))

## MCMC

load("~/Dropbox/Projects/Wildfires/Output/bayes/mcmc_cases_14.RData")
load("~/Dropbox/Projects/Wildfires/Output/bayes/mcmc_deaths_14.RData")

mcmc_cases_14 <- mcmc_cases
mcmc_deaths_14 <- mcmc_deaths

load("~/Dropbox/Projects/Wildfires/Output/bayes/mcmc_cases_21.RData")
load("~/Dropbox/Projects/Wildfires/Output/bayes/mcmc_deaths_21.RData")

mcmc_cases_21 <- mcmc_cases
mcmc_deaths_21 <- mcmc_deaths

load("~/Dropbox/Projects/Wildfires/Output/bayes/mcmc_cases_28.RData")
load("~/Dropbox/Projects/Wildfires/Output/bayes/mcmc_deaths_28.RData")

mcmc_cases_28 <- mcmc_cases
mcmc_deaths_28 <- mcmc_deaths

load("~/Dropbox/Projects/Wildfires/Output/bayes/mcmc_cases_mobility.RData")
load("~/Dropbox/Projects/Wildfires/Output/bayes/mcmc_deaths_mobility.RData")

mcmc_cases_mobility <- mcmc_cases
mcmc_deaths_mobility <- mcmc_deaths

load("~/Dropbox/Projects/Wildfires/Output/bayes/mcmc_cases_flexible.RData")
load("~/Dropbox/Projects/Wildfires/Output/bayes/mcmc_deaths_flexible.RData")

mcmc_cases_flexible <- mcmc_cases
mcmc_deaths_flexible <- mcmc_deaths

load("~/Dropbox/Projects/Wildfires/Output/bayes/mcmc_cases_aggressive.RData")
load("~/Dropbox/Projects/Wildfires/Output/bayes/mcmc_deaths_aggressive.RData")

mcmc_cases_aggressive <- mcmc_cases
mcmc_deaths_aggressive <- mcmc_deaths

load("~/Dropbox/Projects/Wildfires/Output/bayes/mcmc_cases_outliers.RData")
load("~/Dropbox/Projects/Wildfires/Output/bayes/mcmc_deaths_outliers.RData")

mcmc_cases_outliers <- mcmc_cases
mcmc_deaths_outliers <- mcmc_deaths

load("~/Dropbox/Projects/Wildfires/Output/bayes/mcmc_cases_dow.RData")
load("~/Dropbox/Projects/Wildfires/Output/bayes/mcmc_deaths_dow.RData")

mcmc_cases_dow <- mcmc_cases
mcmc_deaths_dow <- mcmc_deaths

lags <- 28:0

### Posterior Distributions of Cumulative Effect

# cases

eta_cases_tmp  <- mcmc_cases_28[[1]][,paste0("eta[",1:29,"]")]
eta_cases <-  100*(exp(10*rowSums(eta_cases_tmp)) - 1)
eta_cases_tmp  <- mcmc_cases_21[[1]][,paste0("eta[",1:22,"]")]
eta_cases <-  cbind(eta_cases, 100*(exp(10*rowSums(eta_cases_tmp)) - 1))
eta_cases_tmp <- mcmc_cases_14[[1]][,paste0("eta[",1:15,"]")]
eta_cases <-  cbind(eta_cases, 100*(exp(10*rowSums(eta_cases_tmp)) - 1))
eta_cases_tmp <- mcmc_cases_mobility[[1]][,paste0("eta[",1:29,"]")]
eta_cases <-  cbind(eta_cases, 100*(exp(10*rowSums(eta_cases_tmp)) - 1))
eta_cases_tmp <- mcmc_cases_flexible[[1]][,paste0("eta[",1:29,"]")]
eta_cases <-  cbind(eta_cases, 100*(exp(10*rowSums(eta_cases_tmp)) - 1))
eta_cases_tmp <- mcmc_cases_aggressive[[1]][,paste0("eta[",1:29,"]")]
eta_cases <-  cbind(eta_cases, 100*(exp(10*rowSums(eta_cases_tmp)) - 1))
eta_cases_tmp <- mcmc_cases_outliers[[1]][,paste0("eta[",1:29,"]")]
eta_cases <- cbind(eta_cases, 100*(exp(10*rowSums(eta_cases_tmp)) - 1))
eta_cases_tmp <- mcmc_cases_dow[[1]][,paste0("eta[",1:29,"]")]
eta_cases <- cbind(eta_cases, 100*(exp(10*rowSums(eta_cases_tmp)) - 1))

colnames(eta_cases) <- c("A", "B", "C", "D", "E", "F", "G", "H")
cases_dat <- melt(eta_cases, value.name = "eta")
names(cases_dat) <- c("row", "sens", "eta")

lag_cases <- ggplot(aes(x = sens, y = eta), data = cases_dat) +
  geom_boxplot() +
  ylim(-10,20) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "blue") + 
  labs(title = "", x = "", y = "Cumulative Effect")

# deaths
eta_deaths_tmp  <- mcmc_deaths_28[[1]][,paste0("eta[",1:29,"]")]
eta_deaths <-  100*(exp(10*rowSums(eta_deaths_tmp)) - 1)
eta_deaths_tmp  <- mcmc_deaths_21[[1]][,paste0("eta[",1:22,"]")]
eta_deaths <-  cbind(eta_deaths, 100*(exp(10*rowSums(eta_deaths_tmp)) - 1))
eta_deaths_tmp <- mcmc_deaths_14[[1]][,paste0("eta[",1:15,"]")]
eta_deaths <-  cbind(eta_deaths, 100*(exp(10*rowSums(eta_deaths_tmp)) - 1))
eta_deaths_tmp <- mcmc_deaths_mobility[[1]][,paste0("eta[",1:29,"]")]
eta_deaths <-  cbind(eta_deaths, 100*(exp(10*rowSums(eta_deaths_tmp)) - 1))
eta_deaths_tmp <- mcmc_deaths_flexible[[1]][,paste0("eta[",1:29,"]")]
eta_deaths <-  cbind(eta_deaths, 100*(exp(10*rowSums(eta_deaths_tmp)) - 1))
eta_deaths_tmp <- mcmc_deaths_aggressive[[1]][,paste0("eta[",1:29,"]")]
eta_deaths <-  cbind(eta_deaths, 100*(exp(10*rowSums(eta_deaths_tmp)) - 1))
eta_deaths_tmp <- mcmc_deaths_outliers[[1]][,paste0("eta[",1:29,"]")]
eta_deaths <- cbind(eta_deaths, 100*(exp(10*rowSums(eta_deaths_tmp)) - 1))
eta_deaths_tmp <- mcmc_deaths_dow[[1]][,paste0("eta[",1:29,"]")]
eta_deaths <- cbind(eta_deaths, 100*(exp(10*rowSums(eta_deaths_tmp)) - 1))

colnames(eta_deaths) <- c("A", "B", "C", "D", "E", "F", "G", "H")
deaths_dat <- melt(eta_deaths, value.name = "eta")
names(deaths_dat) <- c("row", "sens", "eta")

lag_deaths <- ggplot(aes(x = sens, y = eta), data = deaths_dat) +
  geom_boxplot() +
  ylim(-10,20) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "blue") + 
  labs(title = "", x = "", y = "Cumulative Effect")

pdf("~/Dropbox/Projects/Wildfires/Output/bayes/sensitivity.pdf", width = 10, height = 5)
ggarrange(lag_cases, lag_deaths, ncol = 2, nrow = 1)
dev.off()

sig_cases <- apply(eta_cases, 2, function(x, ...) mean(x > 0, na.rm = TRUE))
sig_deaths <- apply(eta_deaths, 2, function(x, ...) mean(x > 0, na.rm = TRUE))

pool_dat <- data.frame(pool_cases = colMeans(eta_cases, na.rm = TRUE), 
                        pool_cases_l = apply(eta_cases,2,hpd)[1,],
                        pool_cases_u = apply(eta_cases,2,hpd)[2,],
                        sig_cases = sig_cases,
                        poool_deaths = colMeans(eta_deaths,na.rm = TRUE), 
                        pool_deaths_l = apply(eta_deaths, 2, hpd)[1,],
                        pool_deaths_u = apply(eta_deaths, 2, hpd)[2,],
                        sig_deaths = sig_deaths)

write.csv(pool_dat, file = "~/Dropbox/Projects/Wildfires/Output/bayes/pooled_summary.csv")

