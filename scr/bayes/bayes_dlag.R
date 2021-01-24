library(abind)
library(tidyr)
library(imputeTS)
library(splines)
library(rjags)

remove(list = ls())

### Data Cleaning

setwd("D:/Github/covid_wildfire")
source("scr/bayes/utilities.R")
source("scr/bayes/model.R")
source("scr/bayes/bayes_fun.R")
dff <- load.data()
dff$FIPS <- as.numeric(as.character(dff$FIPS))
dff$date_num <- dff$date_num

# Create Exposure Matrix
X_long <- data.frame(date_num = dff$date_num, FIPS = dff$FIPS, pm25 = dff$pm25)
X_tmp <- spread(X_long, date_num, pm25)
X_tmp <- X_tmp[order(X_tmp$FIPS),]

# Create Covariate Array
Z_long <- data.frame(FIPS = dff$FIPS, date_num = dff$date_num, tmmx = dff$tmmx, rmax = dff$rmax, d = dff$dayofweek)
Z_long <- Z_long[order(Z_long$date_num, Z_long$FIPS),]

# calendar day, tmmx, and rmax are predicted with natural spline basis
Z_bs <- with(Z_long, data.frame(FIPS = FIPS, date_num, model.matrix(~ 0 + d), 
                                bspline(date_num, 4), tmmx = bspline(tmmx, 2), rmax = bspline(rmax, 2)))

Z_tmp <- Z_bs[Z_bs$date_num == 1,]
Z_tmp <- Z_tmp[order(Z_tmp$FIPS),]

# Split variable by date_num
for (i in 2:max(unique(dff$date_num))){
  
  Z_0 <- Z_bs[Z_bs$date_num == i,]
  Z_0 <- Z_0[order(Z_0$FIPS),]
  
  Z_tmp <- abind(Z_tmp, Z_0, along = 3)
  
}

# Population Size
pop_long <- data.frame(FIPS = dff$FIPS, pop = dff$population)
pop_tmp <- pop_long[!duplicated(pop_long$FIPS),]
pop_tmp <- pop_tmp[order(pop_tmp$FIPS),]

# Create Outcome Matrix
Y_long <- data.frame(date_num = dff$date_num, FIPS = dff$FIPS, cases = dff$cases)
Y_tmp <- spread(Y_long, date_num, cases)
Y_tmp <- Y_tmp[order(Y_tmp$FIPS),]

# remove counties with mostly (or completely) missing exposures
keep <- rowSums(is.na(X_tmp[,-1])) <= 0.9*ncol(X_tmp[,-1])
X <- X_tmp[keep,-1]
Y <- Y_tmp[keep,-1]
Z <- Z_tmp[keep,-c(1:2),]
pop <- pop_tmp[keep,-1]

# Data Dimensions
n <- nrow(X)
m <- ncol(X)
p <- dim(Z)[2]
l <- 14 # desired max lag
df <- 4 # number of spline basis functions for PM2.5

# Impute missing values
X_imp <- t(apply(X, 1, na_interpolation, option = "spline"))
Y_imp <- round(t(apply(Y, 1, na_interpolation, option = "spline")))
Y_imp[which(Y_imp < 0)] <- 0

### Unconstrained Model

# hyperparameters
a <- rep(0, l+1)
b <- rep(0, p)
R <- diag(1e-5, l+1)
S <- diag(1e-5, p)

# JAGS call
jagsDat_un <- list(n = n, m = m, l = l, p = p, 
                X = X_imp, Y = Y_imp, Z = Z, pop = pop,
                a = a, b = b, R = R, S = S)

jmod_un <- jags.model(file = "scr/dlag_unconstrained.jags", data = jagsDat_un, 
                   n.chains = 1, n.adapt = 10000, quiet = FALSE,
                   inits = function() list("sig2" = 1))
mcmc_un <- coda.samples(jmod_un, variable.names = c("beta", "mu", "sig2", "theta", "H"), 
                     n.iter = 100000, thin = 100, na.rm = TRUE)

# check mixing
pdf(file = "output/bdlag_trace_un.pdf")
plot(mcmc_un)
dev.off()

# check autocorrelation
pdf(file = "output/bdlag_acf_un.pdf")
for(i in 1:ncol(mcmc_un[[1]]))
  acf(mcmc_un[[1]][,i], main = colnames(mcmc_un[[1]])[i])
dev.off()

### Constrained Model

# hyperparameter change
a <- rep(0, df)

# basis and penalty matrix
D <- diff(diag(df), differences = 2)
Q <- t(D) %*% D + diag(1e-3,df)
U <- bspline(c(l:0), K = df) # penalized spline basis matrix

# JAGS call
jagsDat_c <- list(n = n, m = m, l = l, p = p, df = df, 
                  X = X_imp, Y = Y_imp, Z = Z, U = U, pop = pop,
                  a = a, b = b, Q = Q, S = S)

jmod_c <- jags.model(file = "scr/dlag_constrained.jags", data = jagsDat_c, 
                   n.chains = 1, n.adapt = 10000, quiet = FALSE,
                   inits = function() list("tau2" = 1, "sig2" = 1))
mcmc_c <- coda.samples(jmod_c, variable.names = c("beta", "mu", "sig2", "theta", "rr"), 
                     n.iter = 100000, thin = 100, na.rm = TRUE)

# check mixing
pdf(file = "output/bdlag_trace_c.pdf")
plot(mcmc_c)
dev.off()

# check autocorrelation
pdf(file = "output/bdlag_acf_c.pdf")
for(i in 1:ncol(mcmc_c[[1]]))
  acf(mcmc_c[[1]][,i], main = colnames(mcmc_c[[1]])[i])
dev.off()

### Output

theta <- mcmc_c[[1]][,grep("theta", colnames(mcmc_c[[1]]))]
theta.mu <- colMeans(theta)
theta.cp <- apply(theta, 2, hpd)
lag.vals <- 14:0
gmat <- data.frame(theta.mu, t(theta.cp), lag.vals)
names(gmat) <- c("theta", "hpd_l", "hpd_u", "lags")

lo <- predict(loess(theta ~ lags, data = gmat), newdata = seq(0, 14, by = 0.01))
lo_l <- predict(loess(hpd_l ~ lags, data = gmat), newdata = seq(0, 14, by = 0.01))
lo_u <- predict(loess(hpd_u ~ lags, data = gmat), newdata = seq(0, 14, by = 0.01))

lmat <- data.frame(x = seq(0, 14, by = 0.01), lo, lo_l, lo_u)

plot(predict(loess(theta ~ lags, data = lmat), newdata = seq(0, 14, by = 0.01)), type = "l", lwd = 2)
