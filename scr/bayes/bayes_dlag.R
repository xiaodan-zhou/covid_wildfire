library(abind)
library(tidyr)
library(splines)
library(pscl)
library(rjags)

remove(list = ls())

load.module('glm')

### Data Loading

setwd("D:/Github/covid_wildfire")
source("scr/Utilities.R")
source("scr/bayes/model.R")
source("scr/bayes/bayes_fun.R")
dff <- load.data()
dff$FIPS <- as.numeric(as.character(dff$FIPS))

dff$pm_counter <- dff$pm25
dff$pm_counter[dff$pm_wildfire != 0] <- dff$pm25_history[dff$pm_wildfire != 0]

# for sensitivity
# dff <- dff[dff$pop > quantile(dff$population, 0.75),]
# dff <- dff[dff$date > ymd("2020-05-01"),]

### Data Cleaning

dff_tmp <- load.data()
dff_tmp$FIPS <- as.numeric(as.character(dff_tmp$FIPS))
dff_tmp$date_num <- dff_tmp$date_num

# Create Exposure Matrix
X_long <- data.frame(date_num = dff$date_num, FIPS = dff$FIPS, pm25 = dff$pm25)
X_tmp <- tidyr::spread(X_long, date_num, pm25)
X <- X_tmp[order(X_tmp$FIPS),]

# PM counterfactual
X_counter_long <- data.frame(date_num = dff$date_num, FIPS = dff$FIPS, pm25 = dff$pm_counter)
X_counter_tmp <- tidyr::spread(X_counter_long, date_num, pm25)
X_counter <- X_counter_tmp[order(X_counter_tmp$FIPS),]

# wildfire change
X_wildfire_long <- data.frame(date_num = dff$date_num, FIPS = dff$FIPS, pm25 = dff$pm_wildfire)
X_wildfire_tmp <- tidyr::spread(X_wildfire_long, date_num, pm25)
X_wildfire <- X_wildfire_tmp[order(X_wildfire_tmp$FIPS),]

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
Z_long <- data.frame(FIPS = dff$FIPS, date_num = dff$date_num, tmmx = dff$tmmx, rmax = dff$rmax, dayofweek = dff$dayofweek)
Z_long <- Z_long[order(Z_long$date_num, Z_long$FIPS),]

# calendar day, tmmx, and rmax are fitted with natural spline basis
Z_bs <- with(Z_long, data.frame(FIPS = FIPS, date_num = date_num, 
                                tmmx = ns(tmmx, 2), rmax = ns(rmax, 2),
                                model.matrix(~ dayofweek)[,-1]))

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
l <- 14 # desired max lag
n <- nrow(X)
m <- ncol(X) - 1
o <- 6
p <- dim(Z)[2] - 2 # covariate dimension
q <- 4 # number of spline basis functions for PM2.5 + 1 for intercept

# hyperparameters
a <- rep(0, q)
b <- rep(0, p)
c <- rep(0, o)
R <- diag(1e-6, q)
S <- diag(1e-6, p)
V <- diag(1e-6, o)
sig <- rep(1e4, q) # scaled gamma/wishart scale

U <- matrix(ns(c(l:0), df = q, intercept = TRUE), nrow = l+1, ncol = q)  # natural spline basis constraint
W <- matrix(ns(min(dff$date_num):max(dff$date_num), df = o), ncol = o)

# get initial values for MCMC
gm_cases <- pm_model(dff, lags=0:l, df.date=o, df.tmmx=2, df.rmax=2, cause = "cases", fullDist = TRUE, model = "constrained")
gm_deaths <- pm_model(dff, lags=0:l, df.date=o, df.tmmx=2, df.rmax=2, cause = "deaths", fullDist = TRUE, model = "constrained")

### Cases Model
  
# JAGS call
jagsDat_cases <- list(n = n, m = m, l = l, o = o, p = p, q = q, 
                      X = X[,-1], Y = Y_cases[,-1], Z = Z[,-c(1:2),],
                      U = U, W = W, pop = pop[,2], X_counter = X_counter[,-1],
                      a = a, b = b, c = c, R = R, S = S, V = V, sig = sig)

jmod_cases <- jags.model(file = "scr/bayes/dlag_fit.jags", data = jagsDat_cases, n.chains = 1, n.adapt = 100000, quiet = FALSE,
                         inits = function() list("mu.nb" = gm_cases$mu.nb.init, "mu.bin" = gm_cases$mu.bin.init,
                                                 "xi" = gm_cases$xi.init, "phi" = gm_cases$phi.init, "zeta" = gm_cases$zeta.init,
                                                 "beta" = gm_cases$beta.init, "delta" = gm_cases$delta.init))
mcmc_cases <- coda.samples(jmod_cases,n.iter = 100000, thin = 100, na.rm = TRUE,
                           variable.names = c("beta",  "xi", "zeta", "tau.nb", "mu.nb", "tau.bin", "mu.bin",
                                              "theta", "eta", "phi", "omega", "lambda", "rho"))

# check mixing
pdf(file = "D:/Dropbox (Personal)/Projects/Wildfires/Output/bayes/trace_cases.pdf")
plot(mcmc_cases)
dev.off()

# check autocorrelation
pdf(file = "D:/Dropbox (Personal)/Projects/Wildfires/Output/bayes/acf_cases.pdf")
for(i in 1:ncol(mcmc_cases[[1]]))
  acf(mcmc_cases[[1]][,i], main = colnames(mcmc_cases[[1]])[i])
dev.off()

save(mcmc_cases, file = "D:/Dropbox (Personal)/Projects/Wildfires/Output/bayes/mcmc_cases.RData")
  
### Deaths Model

# JAGS call
jagsDat_deaths <- list(n = n, m = m, l = l, o = o, p = p, q = q, 
                      X = X[,-1], Y = Y_deaths[,-1], Z = Z[,-c(1:2),], 
                      U = U, W = W, pop = pop[,2], X_counter = X_counter[,-1], 
                      a = a, b = b, c = c, R = R, S = S, V = V, sig = sig)

jmod_deaths <- jags.model(file = "scr/bayes/dlag_fit.jags", data = jagsDat_deaths, n.chains = 1, n.adapt = 100000, quiet = FALSE,
                          inits = function() list("mu.nb" = gm_deaths$mu.nb.init, "mu.bin" = gm_deaths$mu.bin.init,
                                                  "xi" = gm_deaths$xi.init, "phi" = gm_deaths$phi.init, "zeta" = gm_deaths$zeta.init,
                                                  "beta" = gm_deaths$beta.init, "delta" = gm_deaths$delta.init))
mcmc_deaths <- coda.samples(jmod_deaths, n.iter = 100000, thin = 100, na.rm = TRUE,
                            variable.names = c("beta",  "xi", "zeta", "tau.nb", "mu.nb", "tau.bin", "mu.bin",
                                               "theta", "eta", "phi", "omega", "lambda", "rho"))

# check mixing
pdf(file = "D:/Dropbox (Personal)/Projects/Wildfires/Output/bayes/trace_deaths.pdf")
plot(mcmc_deaths)
dev.off()

# check autocorrelation
pdf(file = "D:/Dropbox (Personal)/Projects/Wildfires/Output/bayes/acf_deaths.pdf")
for(i in 1:ncol(mcmc_deaths[[1]]))
  acf(mcmc_deaths[[1]][,i], main = colnames(mcmc_deaths[[1]])[i])
dev.off()

save(mcmc_deaths, file = "D:/Dropbox (Personal)/Projects/Wildfires/Output/bayes/mcmc_deaths.RData")
