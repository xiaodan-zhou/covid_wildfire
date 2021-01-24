library(abind)
library(tidyr)
library(imputeTS)
library(splines)
library(rjags)
library(mvtnorm)
library(smooth)
library(splines)
library(lme4)
library(reshape2)

remove(list = ls())

setwd("D:/Github/covid_wildfire")
source("scr/bayes/bayes_fun.R")
source("scr/Utilities.R")

### Data Creation

# dimensions
n <- 100
m <- 250
l <- 14
p <- 4

set.seed(42)

lags <- 0:l
time <- 0:(m - 1)

pop <- floor(runif(n, 100, 1000000))

X <- t(replicate(n, 8 + arima.sim(list(ma = 0.5), n = m)))
Z <- ns(time, df = p)
colnames(Z) <- paste("Z", 1:p, sep = "")

alpha <- rnorm(n, -10, 2) # random intercept
theta <- sin(pi*lags/10)/10 + 0.05 # lagged PM2.5

Y <- matrix(NA, n, m)

for (j in 1:m) {
  
  lambda <- exp(alpha + time[j]*sin(pi*time[j]/100)/1000 + log(pop) + 
                  X[,max(1,j-l):j, drop = FALSE]%*%theta[max(1,l-j+2):(l+1)])
  
  Y[,j] <- rpois(n, lambda)
  
}

# hyperparameters
a <- rep(0, l+1)
b <- rep(0, p)
R <- diag(1e-8, l+1)
S <- diag(1e-8, p)
df <- 4 # number of spline basis functions for PM2.5

### Unconstrained LME4 

Y.long <- melt(data.frame(id = 1:n, Y), variable.name = "time", value.name = "Y", id.vars = "id")
Y.long$time <- as.numeric(sub('.', '', Y.long$time))

X.long <- melt(data.frame(id = 1:n, X), variable.name = "time", value.name = "X", id.vars = "id")
X.long$time <- as.numeric(sub('.', '', X.long$time))

long.tmp1 <- merge(Y.long, X.long, by = c("id", "time"))
long.tmp2 <- merge(long.tmp1, cbind(id = 1:n, log.pop = log(pop)), by = "id")
long <- merge(long.tmp2, cbind(time = 1:m, Z = Z), by = "time")
long <- long[order(long$id, long$time),]
long$time <- long$time - 1

lag.names = c()

for (i in l:0) {
  new.var = paste0("l", i)
  lag.names = c(lag.names, new.var)
  long = long %>% 
    dplyr::group_by(id) %>% 
    dplyr::mutate(!!new.var := dplyr::lag(!!as.name("X"), n = i, default = NA))
  long <- data.frame(long)
}

dat.lme <- long[order(long$time, long$id),]
fmla <- paste("Y ~ ", paste("Z", 1:p, collapse = " + ", sep = ""), " + ", 
              paste("l", l:0, collapse = " + ", sep = ""), " + (1|id) + offset(log.pop)", sep = "")

fit <- glmer(fmla, family = poisson, data = dat.lme)

### Unconstrained Model

# JAGS call
jagsDat_un <- list(n = n, m = m, l = l, p = p, 
                   X = X, Y = Y, Z = Z, pop = pop, 
                   a = a, b = b, R = R, S = S)
beta.init <- colMeans(coef(fit)$id)[2:5]
theta.init <- colMeans(coef(fit)$id)[6:20]
mu.init <- colMeans(coef(fit)$id)[1]
tau.init <- 1/summary(fit)$varcor$id[1]

jmod_un <- jags.model(file = "scr/bayes/dlag_unconstrained.jags", data = jagsDat_un, 
                      n.chains = 1, n.adapt = 10000, quiet = FALSE,
                      inits = function() list("tau" = tau.init, "theta" = theta.init, 
                                              "mu" = mu.init, "beta" = beta.init))
mcmc_un <- coda.samples(jmod_un, variable.names = c("tau", "theta", "H"), 
                        n.iter = 100000, thin = 100, na.rm = TRUE)

# check mixing
pdf(file = "output/bdlag_trace_un.pdf")
plot(mcmc_un)
dev.off()

### Constrained lme4 to start

X.l <- dat.lme[,grep("l", colnames(dat.lme))][,-1]
U <- cbind(c(rep(0,l), 1), ns(c(l:0), df = df)) # natural spline basis matrix

spmat <- as.matrix(X.l) %*% as.matrix(U)
colnames(spmat) <- paste("U", 1:(df+1), sep = "")
dat.lme.s <- data.frame(dat.lme, spmat)

fmla <- paste("Y ~ ", paste("Z", 1:p, collapse = " + ", sep = ""), " + ", 
              paste("U", 1:(df+1), collapse = " + ", sep = ""), " + (1|id) + offset(log.pop)", sep = "")

fit.s <- glmer(fmla, family = poisson, data = dat.lme.s)

### Constrained Bayesian Analysis

# hyperparameter change
a <- rep(0, df+1)
R <- diag(1e-8, df+1) 

# penalty matrix
# D <- diff(diag(1e-3, df), differences = 2)
# Q <- t(D) %*% D + diag(1e-10, df)

# JAGS call
jagsDat_c <- list(n = n, m = m, l = l, df = df, p = p,
                  X = X, Y = Y, Z = Z, pop = pop,
                  a = a, b = b, R = R, S = S, U = U)

beta.init <- colMeans(coef(fit.s)$id)[2:5]
gamma.init <- colMeans(coef(fit.s)$id)[6:10]
mu.init <- colMeans(coef(fit.s)$id)[1]
tau.init <- 1/summary(fit.s)$varcor$id[1]

jmod_c <- jags.model(file = "scr/bayes/dlag_constrained.jags", data = jagsDat_c,
                     n.chains = 1, n.adapt = 10000, quiet = FALSE,
                     inits = function() list("tau" = tau.init, "gamma" = gamma.init, 
                                             "mu" = mu.init, "beta" = beta.init))
mcmc_c <- coda.samples(jmod_c, variable.names = c("theta", "gamma", "tau", "mu", "H"), 
                       n.iter = 100000, thin = 100, na.rm = TRUE)

# check mixing
pdf(file = "output/bdlag_trace_c.pdf")
plot(mcmc_c)
dev.off()

cbind(colMeans(mcmc_un[[1]][,grep("theta", colnames(mcmc_un[[1]]))]), theta)
