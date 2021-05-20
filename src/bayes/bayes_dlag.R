library(abind)
library(tidyr)
library(splines)
library(pscl)
library(rjags)
library(sf)

remove(list = ls())

load.module('glm')

### Data Loading

setwd("~/Github/covid_wildfire")
source("src/Utilities.R")
source("src/bayes/model.R")
source("src/bayes/bayes_fun.R")
dff <- load.data()
dff$FIPS <- as.numeric(as.character(dff$FIPS))

dff$pm_counter <- dff$pm25
dff$pm_counter[dff$pm_wildfire != 0] <- dff$pm25_history[dff$pm_wildfire != 0]

# for mobility
dff <- subset(dff, !(FIPS %in% c(6051, 41023, 41025, 41037, 41063, 53013)))

# for outliers_
# dff <- subset(dff, !(FIPS %in% c(6009, 6051)))

### Data Cleaning

# Create Exposure Matrix
X_long <- data.frame(date_num = dff$date_num, FIPS = dff$FIPS, pm25 = dff$pm25)
X_tmp <- tidyr::spread(X_long, date_num, pm25)
X <- X_tmp[order(X_tmp$FIPS),]

# PM counterfactual
X_counter_long <- data.frame(date_num = dff$date_num, FIPS = dff$FIPS, pm25 = dff$pm_counter)
X_counter_tmp <- tidyr::spread(X_counter_long, date_num, pm25)
X_counter <- X_counter_tmp[order(X_counter_tmp$FIPS),]

# Population Size
pop_long <- data.frame(FIPS = dff$FIPS, pop = dff$population)
pop_tmp <- pop_long[!duplicated(pop_long$FIPS),]
pop <- pop_tmp[order(pop_tmp$FIPS),]

# Create Outcome Matrices
Y_long_cases <- data.frame(date_num = dff$date_num, FIPS = dff$FIPS, cases = dff$cases)
Y_tmp_cases <- tidyr::spread(Y_long_cases, date_num, cases)
Y_cases <- Y_tmp_cases[order(Y_tmp_cases$FIPS),]

Y_long_death <- data.frame(date_num = dff$date_num, FIPS = dff$FIPS, death = dff$death)
Y_tmp_death <- tidyr::spread(Y_long_death, date_num, death)
Y_deaths <- Y_tmp_death[order(Y_tmp_death$FIPS),]

# Create Covariate Array
Z_long <- data.frame(FIPS = dff$FIPS, date_num = dff$date_num, tmmx = dff$tmmx, rmax = dff$rmax, 
                     mobility = dff$relative_change_feb, dayofweek = dff$dayofweek)
Z_long <- Z_long[order(Z_long$date_num, Z_long$FIPS),]

# calendar day, tmmx, and rmax are fitted with natural spline basis
Z_bs <- with(Z_long, data.frame(FIPS = FIPS, date_num = date_num, 
                                tmmx = ns(tmmx, 2), rmax = ns(rmax, 2), 
                                mobility = mobility, model.matrix(~ dayofweek)[,-1]))

Z_tmp <- Z_bs[Z_bs$date_num == min(dff$date_num),]
Z <- Z_tmp[order(Z_tmp$FIPS),]

# split array by date_num
for (i in (min(dff$date_num) + 1):max(dff$date_num)){
  
  Z_tmp <- Z_bs[Z_bs$date_num == i,]
  Z_tmp <- Z_tmp[order(Z_tmp$FIPS),]
  
  Z <- abind(Z, Z_tmp, along = 3)

}

total_cases <- rowSums(Y_cases[,-1], na.rm = TRUE)
total_deaths <- rowSums(Y_deaths[,-1], na.rm = TRUE)

### Begin Bayesian analysis

# Data Dimensions
l <- 28 # desired max lag
n <- nrow(X)
m <- ncol(X) - 1
o <- 6
p <- dim(Z)[2] - 2 # covariate dimension
q <- 7 # number of spline basis functions for PM2.5 + 1 for intercept

# hyperparameters
a <- rep(0, q)
b <- rep(0, p)
c <- rep(0, o)
R <- diag(1e-10, q)
S <- diag(1e-10, p)
V <- diag(1e-10, o)
sig <- rep(1e5, q) # scaled gamma/wishart scale

U <- matrix(ns(c(l:0), df = q, intercept = TRUE), nrow = l+1, ncol = q)  # natural spline basis constraint
W <- matrix(ns(min(dff$date_num):max(dff$date_num), df = o), ncol = o)

# get initial values for MCMC
gm_cases <- pm_model(dff, lags=0:l, df.date=o, df.tmmx=2, df.rmax=2, cause = "cases", fullDist = TRUE, model = "Constrained", mobility = TRUE)
gm_deaths <- pm_model(dff, lags=0:l, df.date=o, df.tmmx=2, df.rmax=2, cause = "deaths", fullDist = TRUE, model = "Constrained", mobility = TRUE)

### Cases Model
  
# JAGS call
jagsDat_cases <- list(n = n, m = m, l = l, o = o, p = p, q = q, 
                      X = X[,-1], Y = Y_cases[,-1], Z = Z[,-c(1:2),],
                      U = U, W = W, pop = pop[,-1], X_counter = X_counter[,-1],
                      a = a, b = b, c = c, R = R, S = S, V = V, sig = sig)

jmod_cases <- jags.model(file = "src/bayes/dlag_fit.jags", data = jagsDat_cases, n.chains = 1, n.adapt = 20000, quiet = FALSE,
                         inits = function() list("mu" = gm_cases$mu.init, "xi" = gm_cases$xi.init, "phi" = 1,
                                                 "beta" = gm_cases$beta.init, "delta" = gm_cases$delta.init))
mcmc_cases <- coda.samples(jmod_cases,n.iter = 100000, thin = 100, na.rm = TRUE,
                           variable.names = c("beta",  "xi", "mu", "tau", "theta", "eta", 
                                              "phi", "psi", "sigma", "lambda", "rho"))

# check mixing
pdf(file = "~/Dropbox/Projects/Wildfires/Output/bayes/trace_cases_mobility.pdf")
plot(mcmc_cases)
dev.off()

# check autocorrelation
pdf(file = "~/Dropbox/Projects/Wildfires/Output/bayes/acf_cases_mobility.pdf")
for(i in 1:ncol(mcmc_cases[[1]]))
  acf(mcmc_cases[[1]][,i], main = colnames(mcmc_cases[[1]])[i])
dev.off()

save(mcmc_cases, file = "~/Dropbox/Projects/Wildfires/Output/bayes/mcmc_cases_mobility.RData")
  
### Deaths Model

# JAGS call
jagsDat_deaths <- list(n = n, m = m, l = l, o = o, p = p, q = q, 
                      X = X[,-1], Y = Y_deaths[,-1], Z = Z[,-c(1:2),], 
                      U = U, W = W, pop = pop[,-1], X_counter = X_counter[,-1], 
                      a = a, b = b, c = c, R = R, S = S, V = V, sig = sig)

jmod_deaths <- jags.model(file = "src/bayes/dlag_fit.jags", data = jagsDat_deaths, n.chains = 1, n.adapt = 100000, quiet = FALSE,
                          inits = function() list("mu" = gm_deaths$mu.init, "xi" = gm_deaths$xi.init, "phi" = 1,
                                                  "beta" = gm_deaths$beta.init, "delta" = gm_deaths$delta.init))
mcmc_deaths <- coda.samples(jmod_deaths, n.iter = 100000, thin = 100, na.rm = TRUE,
                            variable.names = c("beta",  "xi", "mu", "tau", "theta", "eta",
                                               "phi", "psi", "sigma", "lambda", "rho"))

# check mixing
pdf(file = "~/Dropbox/Projects/Wildfires/Output/bayes/trace_deaths_mobility.pdf")
plot(mcmc_deaths)
dev.off()

# check autocorrelation
pdf(file = "~/Dropbox/Projects/Wildfires/Output/bayes/acf_deaths_mobility.pdf")
for(i in 1:ncol(mcmc_deaths[[1]]))
  acf(mcmc_deaths[[1]][,i], main = colnames(mcmc_deaths[[1]])[i])
dev.off()

save(mcmc_deaths, file = "~/Dropbox/Projects/Wildfires/Output/bayes/mcmc_deaths_mobility.RData")

### Hypothesis Testing

load("~/Dropbox/Projects/Wildfires/Output/bayes/mcmc_cases_mobility.RData")
load("~/Dropbox/Projects/Wildfires/Output/bayes/mcmc_deaths_mobility.RData")
lags <- 28:0
FIPS <- Y_deaths[,1]

total_cases <- rowSums(Y_cases[,-1], na.rm = TRUE)
total_deaths <- rowSums(Y_deaths[,-1], na.rm = TRUE)

cty <- read_sf('data/cb_2018_us_county_5m', 'cb_2018_us_county_5m') %>%
  filter(STATEFP %in% c('06', '41', '53'))
cty$FIPS <- as.numeric(as.character(cty$GEOID))

cty.selected <- cty[which(cty$FIPS %in% Y_deaths[,1]),]
cty.selected$STATE <- with(cty.selected, ifelse(STATEFP == "06", "CA", ifelse(STATEFP == "41", "OR", "WA")))
cty.selected$county <- paste(cty.selected$NAME, ", ", cty.selected$STATE, sep = "")
county <- cty.selected$county[order(cty.selected$FIPS)]

## per 10 mcg/m^3

# cases
eta_cases_tmp <- mcmc_cases[[1]][,sapply(1:29, function(k, ...) paste0("eta[",k,"]"))]
eta_cases_tmp <- unname(eta_cases_tmp)
cum_cases <- 100*(exp(10*rowSums(eta_cases_tmp)) - 1)
eta_cases <- 100*(exp(10*eta_cases_tmp) - 1)
out_cases <- data.frame(lag = eta_cases, cum = cum_cases, FIPS = 0,
                        county = "Combined", estimate = "eta", pop = sum(pop[,2]))

# deaths
eta_deaths_tmp <- mcmc_deaths[[1]][,sapply(1:29, function(k, ...) paste0("eta[",k,"]"))]
eta_deaths_tmp <- unname(eta_deaths_tmp)
cum_deaths <- 100*(exp(10*rowSums(eta_deaths_tmp)) - 1)
eta_deaths <- 100*(exp(10*eta_deaths_tmp) - 1)
out_deaths <- data.frame(lag = eta_deaths, cum = cum_deaths, FIPS = 0,
                         county = "Combined", estimate = "eta", pop = sum(pop[,2]))

for (i in 1:nrow(Y_deaths)) {
  
  theta_cases_tmp <- mcmc_cases[[1]][,sapply(1:29, function(k, ...) paste0("theta[",i,",",k,"]"))]
  theta_cases_tmp <- unname(theta_cases_tmp)
  cum_cases <- 100*(exp(rowSums(10*theta_cases_tmp)) - 1)
  theta_cases <- 100*(exp(10*theta_cases_tmp) - 1)
  out_cases_tmp <- data.frame(lag = theta_cases, cum = cum_cases, FIPS = FIPS[i],
                              county = county[i], estimate = "theta", pop = pop[i,2])
  out_cases <- rbind(out_cases, out_cases_tmp)
  
  theta_deaths_tmp <- mcmc_deaths[[1]][,sapply(1:29, function(k, ...) paste0("theta[",i,",",k,"]"))]
  theta_deaths_tmp <- unname(theta_deaths_tmp)
  cum_deaths <- 100*(exp(10*rowSums(theta_deaths_tmp)) - 1)
  theta_deaths <- 100*(exp(10*theta_deaths_tmp) - 1)
  out_deaths_tmp <- data.frame(lag = theta_deaths, cum = cum_deaths, FIPS = FIPS[i], 
                               county = county[i], estimate = "theta", pop = pop[i,2])
  out_deaths <- rbind(out_deaths, out_deaths_tmp)
  
}

mean(out_cases$cum[out_cases$county == "Monterey, CA"])
mean(out_cases$cum[out_cases$county == "Sonoma, CA"])
mean(out_deaths$cum[out_deaths$county == "San Bernardino, CA"])
mean(out_deaths$cum[out_deaths$county == "Calaveras, CA"])

hpd(out_cases$cum[out_cases$county == "Monterey, CA"])
hpd(out_cases$cum[out_cases$county == "Sonoma, CA"])
hpd(out_deaths$cum[out_deaths$county == "San Bernardino, CA"])
hpd(out_deaths$cum[out_deaths$county == "Calaveras, CA"])

county_names <- unique(out_cases$county)
sig_cases <- sig_deaths <- rep(NA, length(county_names))

i <- 1

for (name in county_names) {
  
  sig_cases[i] <- mean(out_cases$cum[out_cases$county == name] > 0)
  sig_deaths[i] <- mean(out_deaths$cum[out_deaths$county == name] > 0)
  i <- i + 1
  
}
  
sum(sig_deaths > 0.95)
sum(sig_cases > 0.95)

## Counterfactual Assessment

nxs_cases <- nxs_deaths <- matrix(NA, nrow = 1000, ncol = 92)
colnames(nxs_cases) <- colnames(nxs_deaths) <- FIPS

FIPS <- unique(dff$FIPS)[order(unique(dff$FIPS))]

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

pct_cases <- pct_deaths <- matrix(NA, nrow = 1000, ncol = 92)
colnames(pct_cases) <- colnames(pct_deaths) <- FIPS

for (i in 1:92) {
  
  pct_cases[,i] <- 100*nxs_cases[,i]/(total_cases[i] - nxs_cases[,i])
  pct_deaths[,i] <- 100*nxs_deaths[,i]/(total_deaths[i] - nxs_deaths[,i])
  
}

sig_cases <- apply(pct_cases, 2, function(x, ...) mean(x > 0, na.rm = TRUE))
sig_deaths <- apply(pct_deaths, 2, function(x, ...) mean(x > 0, na.rm = TRUE))

sum(sig_deaths > 0.95, na.rm = T)
sum(sig_cases > 0.95, na.rm = T)

mean(pct_cases[,which(colnames(pct_cases) == 6101)])
mean(pct_cases[,which(colnames(pct_cases) == 6007)])
mean(pct_deaths[,which(colnames(pct_deaths) == 6009)])
mean(pct_deaths[,which(colnames(pct_deaths) == 6007)])

hpd(pct_cases[,which(colnames(pct_cases) == 6101)])
hpd(pct_cases[,which(colnames(pct_cases) == 6007)])
hpd(pct_deaths[,which(colnames(pct_deaths) == 6009)])
hpd(pct_deaths[,which(colnames(pct_deaths) == 6007)])
