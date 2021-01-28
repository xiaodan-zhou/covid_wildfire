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
library(ggpubr)

remove(list = ls())

setwd("D:/Github/covid_wildfire")
source("scr/bayes/bayes_fun.R")

### Data Creation

# dimensions
n <- 100 # number of counties
m <- 250 # days
l <- 14 # lag days
p <- 4 # number of spline basis functions for calendar days
q <- 4 # number of spline basis functions for lagged PM2.5

set.seed(42)

lags <- 0:l
time <- 0:(m - 1)

pop <- floor(runif(n, 100, 1000000))

X <- t(replicate(n, 8 + arima.sim(list(ma = 0.5), n = m)))
Z <- ns(time, df = p)
colnames(Z) <- paste("Z", 1:p, sep = "")

# random effects
alpha <- rnorm(n, -10, 2) # random intercept
eta <- sin(pi*lags/10)/10
theta <- t(replicate(n, rnorm(l + 1, sin(pi*lags/10)/10, 0.05))) # lagged PM2.5 coefficients

Y <- matrix(NA, n, m)

for (j in 1:m) {
  
  eta <- rep(NA, n)
  
  for(i in 1:n)
    eta[i] <- c(X[i,max(1,j-l):j, drop = FALSE]%*%theta[i,max(1,l-j+2):(l+1)])
  
  
  lambda <- exp(alpha + time[j]*sin(pi*time[j]/100)/1000 + log(pop) + eta)
  
  Y[,j] <- rpois(n, lambda)
  
}

save(theta, file = "output/theta_sim.RData")

# hyperparameters
a <- rep(0, l+1)
b <- rep(0, p)
R <- diag(1e-10, l+1)
S <- diag(1e-10, p)

### Unconstrained LME4 (fixed lags)

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

### Unconstrained Bayesian Model

# JAGS call
jagsDat_un <- list(n = n, m = m, l = l, p = p, 
                   X = X, Y = Y, Z = Z, pop = pop, 
                   a = a, b = b, R = R, S = S)

mu.init <- colMeans(coef(fit)$id)[1]
beta.init <- colMeans(coef(fit)$id)[2:4]
eta.init <- colMeans(coef(fit)$id)[5:19]
tau.init <- 1/summary(fit)$varcor$id[1]

jmod_un <- jags.model(file = "scr/bayes/dlag_unconstrained.jags", data = jagsDat_un, 
                      n.chains = 1, n.adapt = 20000, quiet = FALSE,
                      inits = function() list("tau" = tau.init, "eta" = eta.init, 
                                              "mu" = mu.init, "beta" = beta.init))
mcmc_sim_un <- coda.samples(jmod_un, variable.names = c("theta", "eta", "mu", "tau", "sigma", "H"), 
                        n.iter = 100000, thin = 100, na.rm = TRUE)

# check mixing
pdf(file = "output/sim_trace_un.pdf")
plot(mcmc_sim_un)
dev.off()

save(mcmc_sim_un, file = "output/mcmc_sim_un.RData")

### Constrained LME4 (fixed lags)

# construct new lme covariates
X.l <- dat.lme[,grep("l", colnames(dat.lme))][,-1]
U <- cbind(c(rep(0,l), 1), ns(c(l:0), df = q)) # natural spline basis matrix

spmat <- as.matrix(X.l) %*% as.matrix(U)
colnames(spmat) <- paste("U", 1:(q+1), sep = "")
dat.lme.s <- data.frame(dat.lme, spmat)

fmla <- paste("Y ~ ", paste("Z", 1:p, collapse = " + ", sep = ""), " + ", 
              paste("U", 1:(q+1), collapse = " + ", sep = ""), " + (1|id) + offset(log.pop)", sep = "")

fit.s <- glmer(fmla, family = poisson, data = dat.lme.s)

### Constrained Bayesian Model

# hyperparameter change
a <- rep(0, q+1)
R <- diag(1e-10, q+1)

# penalty matrix for p-spline
# D <- diff(diag(1e-3, q), differences = 2)
# Q <- t(D) %*% D + diag(1e-10, q)

# JAGS call
jagsDat_c <- list(n = n, m = m, l = l, p = p, q = q,
                  X = X, Y = Y, Z = Z, U = U, pop = pop,
                  a = a, b = b, R = R, S = S)

mu.init <- colMeans(coef(fit.s)$id)[1]
beta.init <- colMeans(coef(fit.s)$id)[2:4]
delta.init <- colMeans(coef(fit.s)$id)[5:9]
tau.init <- 1/summary(fit.s)$varcor$id[1]

jmod_c <- jags.model(file = "scr/bayes/dlag_constrained.jags", data = jagsDat_c,
                     n.chains = 1, n.adapt = 20000, quiet = FALSE,
                     inits = function() list("tau" = tau.init, "delta" = delta.init, 
                                             "mu" = mu.init, "beta" = beta.init))
mcmc_sim_c <- coda.samples(jmod_c, variable.names = c("theta", "eta", "delta", "mu", "sigma", "tau", "H"), 
                       n.iter = 100000, thin = 100, na.rm = TRUE)

# check mixing
pdf(file = "output/sim_trace_c.pdf")
plot(mcmc_sim_c)
dev.off()

save(mcmc_sim_c, file = "output/mcmc_sim_c.RData")

### Plot some stuff

eta <- sin(pi*lags/10)/10
lag.vals <- 14:0

plot_list <- list()
j <- 1

plot_list <- lapply(c(1,25,50,75,100), function(i, ...){

  theta.c <- mcmc_sim_c[[1]][,grep(paste0("theta\\[",i,","), colnames(mcmc_sim_c[[1]]))]
  theta.c.mu <- colMeans(theta.c)
  theta.c.cp <- apply(theta.c, 2, hpd)
  gmat.c <- data.frame(theta.c.mu, t(theta.c.cp), lag.vals)
  names(gmat.c) <- c("theta", "hpd_l", "hpd_u", "lags")
  
  lo.c <- predict(loess(theta ~ lags, data = gmat.c), newdata = seq(0, 14, by = 0.01))
  lo_l.c <- predict(loess(hpd_l ~ lags, data = gmat.c), newdata = seq(0, 14, by = 0.01))
  lo_u.c <- predict(loess(hpd_u ~ lags, data = gmat.c), newdata = seq(0, 14, by = 0.01))
  
  lmat.c <- data.frame(x = seq(0, 14, by = 0.01), lo.c, lo_l.c, lo_u.c)
  
  theta.un <- mcmc_sim_un[[1]][,grep(paste0("theta\\[",i,","), colnames(mcmc_sim_un[[1]]))]
  theta.un.mu <- colMeans(theta.un)
  theta.un.cp <- apply(theta.un, 2, hpd)
  gmat.un <- data.frame(theta.un.mu, t(theta.un.cp), lag.vals)
  names(gmat.un) <- c("theta", "hpd_l", "hpd_u", "lags")
  
  lo.un <- predict(loess(theta ~ lags, data = gmat.un), newdata = seq(0, 14, by = 0.01))
  lo_l.un <- predict(loess(hpd_l ~ lags, data = gmat.un), newdata = seq(0, 14, by = 0.01))
  lo_u.un <- predict(loess(hpd_u ~ lags, data = gmat.un), newdata = seq(0, 14, by = 0.01))
  
  lmat.un <- data.frame(x = seq(0, 14, by = 0.01), lo.un, lo_l.un, lo_u.un)
  
  ggplot() + 
    geom_line(size = 1, aes(x = x, y = lo.c, color = "Constrained"), data = lmat.c) + 
    geom_ribbon(aes(x = x, y = lo.c, ymax = lo_u.c, ymin = lo_l.c), alpha = 0.4, data = lmat.c) +
    geom_line(size = 1, aes(x = x, y = lo.un, color = "Unconstrained"), data = lmat.un) + 
    geom_ribbon(aes(x = x, y = lo.un, ymax = lo_u.un, ymin = lo_l.un), alpha = 0.2, data = lmat.un) +
    geom_point(aes(x = lag.vals, y = theta[i,]), inherit.aes = FALSE) +
    labs(title = paste("County", i), x = "Lag Days", y = "Coefficient Value") +
    scale_color_manual(name = "Model",
                       breaks = c("Constrained", "Unconstrained"),
                       values = c("Constrained" = "blue", "Unconstrained" = "red") )
  
})

eta.c <- mcmc_sim_c[[1]][,sapply(1:15, function(z, ...) grep(paste0("eta\\[",z,"\\]"), colnames(mcmc_sim_c[[1]])))]
eta.c.mu <- colMeans(eta.c)
eta.c.cp <- apply(eta.c, 2, hpd)

gmat.c <- data.frame(eta.c.mu, t(eta.c.cp), lag.vals)
names(gmat.c) <- c("eta", "hpd_l", "hpd_u", "lags")

eta.un <- mcmc_sim_un[[1]][,sapply(1:15, function(z, ...) grep(paste0("eta\\[",z,"\\]"), colnames(mcmc_sim_un[[1]])))]
eta.un.mu <- colMeans(eta.un)
eta.un.cp <- apply(eta.un, 2, hpd)

gmat.un <- data.frame(eta.un.mu, t(eta.un.cp), lag.vals)
names(gmat.un) <- c("eta", "hpd_l", "hpd_u", "lags")

lo.c <- predict(loess(eta ~ lags, data = gmat.c), newdata = seq(0, 14, by = 0.01))
lo_l.c <- predict(loess(hpd_l ~ lags, data = gmat.c), newdata = seq(0, 14, by = 0.01))
lo_u.c <- predict(loess(hpd_u ~ lags, data = gmat.c), newdata = seq(0, 14, by = 0.01))

lmat.c <- data.frame(x = seq(0, 14, by = 0.01), lo.c, lo_l.c, lo_u.c)

lo.un <- predict(loess(eta ~ lags, data = gmat.un), newdata = seq(0, 14, by = 0.01))
lo_l.un <- predict(loess(hpd_l ~ lags, data = gmat.un), newdata = seq(0, 14, by = 0.01))
lo_u.un <- predict(loess(hpd_u ~ lags, data = gmat.un), newdata = seq(0, 14, by = 0.01))

lmat.un <- data.frame(x = seq(0, 14, by = 0.01), lo.un, lo_l.un, lo_u.un)

eta_plot <- ggplot() + 
  geom_line(size = 1, aes(x = x, y = lo.c, color = "Constrained"), data = lmat.c) + 
  geom_ribbon(aes(x = x, y = lo.c, ymax = lo_u.c, ymin = lo_l.c), alpha = 0.4, data = lmat.c) +
  geom_line(size = 1, aes(x = x, y = lo.un, color = "Unconstrained"), data = lmat.un) + 
  geom_ribbon(aes(x = x, y = lo.un, ymax = lo_u.un, ymin = lo_l.un), alpha = 0.2, data = lmat.un) +
  geom_point(aes(x = lag.vals, y = eta), inherit.aes = FALSE) +
  labs(title = "Combined Counties", x = "Lag Days", y = "Coefficient Value") +
  scale_color_manual(name = "Model",
                     breaks = c("Constrained", "Unconstrained"),
                     values = c("Constrained" = "blue", "Unconstrained" = "red") )

pdf(file = "output/sim_fit.pdf")  
ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], plot_list[[5]], eta_plot, ncol=2, nrow=3, common.legend = TRUE, legend="bottom")
dev.off()
