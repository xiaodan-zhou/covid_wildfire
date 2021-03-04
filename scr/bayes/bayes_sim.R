library(abind)
library(tidyr)
library(MASS)
library(imputeTS)
library(pscl)
library(rjags)
library(mvtnorm)
library(smooth)
library(splines)
library(reshape2)
library(ggpubr)

remove(list = ls())

setwd("D:/Github/covid_wildfire")
source("scr/bayes/bayes_fun.R")

load.module("glm")

### Data Creation

# dimensions
n <- 100 # number of counties
m <- 250 # days
l <- 14 # lag days
p <- 4 # number of spline basis functions for calendar days
q <- 4 # number of spline basis functions for lagged PM2.5

lags <- 0:l
time <- 0:(m - 1)

# set.seed(42)
# 
# pop <- floor(runif(n, 10000, 1000000)) # population offset
# 
# X <- t(replicate(n, 8 + arima.sim(list(ma = 0.5), n = m)))
# Z <- ns(time, df = p)
# colnames(Z) <- paste("Z", 1:p, sep = "")
# 
# # random effects
# alpha <- rnorm(n, -10, 2) # random intercept
# eta <- log((l - lags) + 1)*sin((l - lags)*pi/4)/10
# theta <- t(replicate(n, rnorm(l + 1, eta, sqrt(0.01)))) # lagged PM2.5 coefficients
# psi <- rbeta(n, 2, 5)
# 
# # overdispersion
# phi <- 1.5
# 
# Y <- matrix(NA, n, m)
# 
# for (j in 1:m) {
# 
#   for (i in 1:n)
#     lin_pred <- c(X[i,max(1,j-l):j, drop = FALSE]%*%theta[i,max(1,l-j+2):(l+1)])
# 
#   A <- rbinom(n, 1, psi)
#   lambda <- exp(alpha + time[j]*sin(pi*time[j]/100)/1000 + log(pop) + lin_pred)
#   pi <- phi/(phi + (1 - A)*lambda)
# 
#   Y[,j] <- rnbinom(n, phi, pi)
# 
# }
# 
# save(theta, file = "D:/Dropbox (Personal)/Projects/Wildfires/Output/simulation/theta_sim.RData")
# save(eta, file = "D:/Dropbox (Personal)/Projects/Wildfires/Output/simulation/eta_sim.RData")
# save(X, file = "D:/Dropbox (Personal)/Projects/Wildfires/Output/simulation/X_sim.RData")
# save(Y, file = "D:/Dropbox (Personal)/Projects/Wildfires/Output/simulation/Y_sim.RData")
# save(Z, file = "D:/Dropbox (Personal)/Projects/Wildfires/Output/simulation/Z_sim.RData")
# save(pop, file = "D:/Dropbox (Personal)/Projects/Wildfires/Output/simulation/pop_sim.RData")

load("D:/Dropbox (Personal)/Projects/Wildfires/Output/simulation/theta_sim.RData")
load("D:/Dropbox (Personal)/Projects/Wildfires/Output/simulation/X_sim.RData")
load("D:/Dropbox (Personal)/Projects/Wildfires/Output/simulation/Y_sim.RData")
load("D:/Dropbox (Personal)/Projects/Wildfires/Output/simulation/Z_sim.RData")
load("D:/Dropbox (Personal)/Projects/Wildfires/Output/simulation/pop_sim.RData")

# hyperparameters
a <- rep(0, l+1)
b <- rep(0, p)
R <- diag(1e-10, l+1)
S <- diag(1e-10, p)
sig <- rep(1e5, l+1)

### Unconstrained Bayesian Model

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

dat <- long[order(long$time, long$id),]
fmla_un <- paste("Y ~ ", paste("Z", 1:p, collapse = " + ", sep = ""), " + ", 
                 paste("l", l:0, collapse = " + ", sep = ""), " | 1", sep = "")

fit_un <- zeroinfl(as.formula(fmla_un), dist = "negbin", link = "logit", data = dat, offset = log.pop, na.action = na.exclude)

# JAGS call
jagsDat_un <- list(n = n, m = m, l = l, p = p, 
                   X = X, Y = Y, Z = Z, pop = pop, 
                   a = a, b = b, R = R, S = S, sig = sig)

mu.init <- fit_un$coefficients$count[1]
beta.init <- fit_un$coefficients$count[grep("Z", names(fit_un$coefficients$count))]
eta.init <- fit_un$coefficients$count[grep("l", names(fit_un$coefficients$count))]
phi.init <- 1

jmod_un <- jags.model(file = "scr/bayes/dlag_unconstrained.jags", data = jagsDat_un, 
                      n.chains = 1, n.adapt = 10000, quiet = FALSE,
                      inits = function() list("phi" = phi.init, "eta" = eta.init, 
                                              "mu" = mu.init, "beta" = beta.init))
mcmc_sim_un <- coda.samples(jmod_un, variable.names = c("beta", "theta", "eta", "sigma", "mu", "tau", "phi", "psi"), 
                            n.iter = 50000, thin = 50, na.rm = TRUE)

# check mixing
pdf(file = "D:/Dropbox (Personal)/Projects/Wildfires/Output/simulation/sim_trace_un.pdf")
plot(mcmc_sim_un)
dev.off()

save(mcmc_sim_un, file = "D:/Dropbox (Personal)/Projects/Wildfires/Output/simulation/mcmc_sim_un.RData")

### Constrained Bayesian Model

# construct new lme covariates
X.l <- dat[,grep("l", colnames(dat))][,-1]
U <- as.matrix(ns(c(l:0), df = q, intercept = TRUE)) # natural spline basis matrix

spmat <- as.matrix(X.l) %*% as.matrix(U)
colnames(spmat) <- paste("U", 1:q, sep = "")
dat_c <- data.frame(dat, spmat)

fmla_c <- paste("Y ~ ", paste("Z", 1:p, collapse = " + ", sep = ""), " + ", 
                paste("U", 1:q, collapse = " + ", sep = ""), " | 1", sep = "")

fit_c <- zeroinfl(as.formula(fmla_c), dist = "negbin", link = "logit", data = dat_c, offset = log.pop, na.action = na.exclude)

# hyperparameter change
a <- rep(0, q)
R <- diag(1e-10, q)
sig <- rep(1e5, q)

# penalty matrix for p-spline
# D <- diff(diag(1e-3, q), differences = 2)
# Q <- t(D) %*% D + diag(1e-6, q)

# JAGS call
jagsDat_c <- list(n = n, m = m, l = l, p = p, q = q,
                  X = X, Y = Y, Z = Z, U = U, pop = pop,
                  a = a, b = b, R = R, S = S, sig = sig)

mu.init <- fit_c$coefficients$count[1]
beta.init <- fit_c$coefficients$count[grep("Z", names(fit_c$coefficients$count))]
delta.init <- fit_c$coefficients$count[grep("U", names(fit_c$coefficients$count))]
phi.init <- 1

jmod_c <- jags.model(file = "scr/bayes/dlag_constrained.jags", data = jagsDat_c,
                     n.chains = 1, n.adapt = 10000, quiet = FALSE,
                     inits = function() list("phi" = phi.init, "delta" = delta.init, 
                                             "mu" = mu.init, "beta" = beta.init))
mcmc_sim_c <- coda.samples(jmod_c, variable.names = c("beta", "theta", "eta", "delta", "sigma", "mu", "tau", "phi", "psi"), 
                           n.iter = 50000, thin = 100, na.rm = TRUE)

# check mixing
pdf(file = "D:/Dropbox (Personal)/Projects/Wildfires/Output/simulation/sim_trace_c.pdf")
plot(mcmc_sim_c)
dev.off()

save(mcmc_sim_c, file = "D:/Dropbox (Personal)/Projects/Wildfires/Output/simulation/mcmc_sim_c.RData")

### plot some stuff

plot_list <- lapply(c(1,25,50,75,100), function(i, ...){
  
  theta.c <- mcmc_sim_c[[1]][,grep(paste0("theta\\[",i,","), colnames(mcmc_sim_c[[1]]))]
  theta.c.mu <- colMeans(theta.c)
  theta.c.cp <- apply(theta.c, 2, hpd)
  gmat.c <- data.frame(theta.c.mu, t(theta.c.cp), l - lags, model = "Constrained")
  names(gmat.c) <- c("theta", "hpd_l", "hpd_u", "lags", "model")
  
  theta.un <- mcmc_sim_un[[1]][,grep(paste0("theta\\[",i,","), colnames(mcmc_sim_un[[1]]))]
  theta.un.mu <- colMeans(theta.un)
  theta.un.cp <- apply(theta.un, 2, hpd)
  gmat.un <- data.frame(theta.un.mu, t(theta.un.cp), x = l - lags, model = "Unconstrained")
  names(gmat.un) <- names(gmat.c)
  
  gmat.tru <- data.frame(theta[i,], theta[i,], theta[i,], x = l - lags, model = "True Value")
  names(gmat.tru) <- names(gmat.c)
  
  gmat <- rbind(gmat.c, gmat.un, gmat.tru)
  gmat$model <- factor(gmat$model, levels = c("Unconstrained", "Constrained", "True Value"))
  
  ggplot(aes(x = lags, y = theta, color = model, ymax = hpd_u, ymin = hpd_l), data = gmat) + 
    geom_pointrange(position = position_dodge(width = 0.5)) +
    labs(title = paste("County", i), x = "Lag Days", y = "Coefficient Value", color = "Model") + 
    scale_color_manual(values = c("Unconstrained" = "blue", "Constrained" = "red", "True Value" = "black"))
  
})

eta.c <- mcmc_sim_c[[1]][,paste("eta[",1:15,"]", sep = "")]
eta.c.mu <- colMeans(eta.c)
eta.c.cp <- apply(eta.c, 2, hpd)

gmat.c <- data.frame(eta.c.mu, t(eta.c.cp), l - lags, model = "Constrained")
names(gmat.c) <- c("eta", "hpd_l", "hpd_u", "lags", "model")

eta.un <- mcmc_sim_un[[1]][,paste0("eta[",1:15,"]")]
eta.un.mu <- colMeans(eta.un)
eta.un.cp <- apply(eta.un, 2, hpd)

gmat.un <- data.frame(eta.un.mu, t(eta.un.cp), l - lags, model = "Unconstrained")
names(gmat.un) <- names(gmat.c)

gmat.tru <- data.frame(eta, eta, eta, x = l - lags, model = "True Value")
names(gmat.tru) <- names(gmat.c)

gmat <- rbind(gmat.c, gmat.un, gmat.tru)
gmat$model <- factor(gmat$model, levels = c("Unconstrained", "Constrained", "True Value"))

eta_plot <- ggplot() + 
  geom_pointrange(aes(x = lags, y = eta, color = model, ymax = hpd_u, ymin = hpd_l), data = gmat, position = position_dodge(width=0.5)) +
  labs(title = "Combined Counties", x = "Lag Days", y = "Coefficient Value", color = "Model") +
  scale_color_manual(values = c("Unconstrained" = "blue", "Constrained" = "red", "True Value" = "black"))

png(filename = "D:/Dropbox (Personal)/Projects/Wildfires/Output/simulation/sim_fit.png", width = 700, height = 700)  
ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], plot_list[[5]], eta_plot, ncol=2, nrow=3, common.legend = TRUE, legend="bottom")
dev.off()

### table some stuff

load("output/mcmc_sim_un.RData")
load("output/mcmc_sim_c.RData")

eta.un <- mcmc_sim_un[[1]][,sapply(1:15, function(z, ...) grep(paste0("eta\\[",z,"\\]"), colnames(mcmc_sim_un[[1]])))]
eta.c <- mcmc_sim_c[[1]][,sapply(1:15, function(z, ...) grep(paste0("eta\\[",z,"\\]"), colnames(mcmc_sim_c[[1]])))]
omega.un <- mcmc_sim_un[[1]][,sapply(1:15, function(z, ...) grep(paste0("test\\[",z,"\\]"), colnames(mcmc_sim_un[[1]])))]
sigma <- colMeans(sqrt(1/omega.un))

est_out <- round(rbind(rev(eta), rev(eta.init), rev(colMeans(eta.un)), rev(c(U%*%delta.init)), rev(colMeans(eta.c)), rev(sigma)), 3)
rownames(est_out) <- c("Truth", "Unconstrained REML", "Unconstrained Bayes", "Constrained REML", "Constrained Bayes", "Theta Variance")

write.csv(est_out, file = "D:/Dropbox (Personal)/Projects/Wildfires/Output/simulation/sim_out.csv")
