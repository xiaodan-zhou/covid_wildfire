library(rjags)
library(ggplot2)

setwd("D:/Github/covid_wildfire")
source("scr/bayes/bayes_fun.R")

N <- 100
x <- sort(runif(n=N, min = 0, max = 1))
y <- sin(2*pi*x) - 5*x^2 + 3*x + rnorm(n=N, sd=0.25)
df <- 4

dat <- data.frame(y=y, x=x)

X <- ns(x, df)

u <- seq(0, 1, length.out=1001)
U <- ns(u, df)

D <- diff(diag(1e4, df), differences=2)
Q <-  solve(t(D) %*% D + diag(1000, df))
Q <- diag(1e-5, df)

jagsDat <- list(X = X, U = U, y = y, init = rep(0, df),
                n = length(x), m = length(u), df = df, Q = Q)

jmod <- jags.model(file = "scr/bayes/jags_test.jags", data = jagsDat, 
                     n.chains = 1, n.adapt = 10000, quiet = FALSE)
mcmc <- coda.samples(jmod, variable.names = c("mu.rep"), 
                       n.iter = 10000, thin = 10, na.rm = TRUE)

gg.data <- ggplot() + geom_point(aes(x = x, y = y), alpha = 0.5, data = dat) +
  theme_bw() + geom_line(aes(x = u, y = colMeans(mcmc[[1]])))

gg.data
