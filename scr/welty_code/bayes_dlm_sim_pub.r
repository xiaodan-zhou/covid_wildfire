###############################################################################
## Copyright (C) 2005, Leah J. Welty <lwelty@northwestern.edu>
##     
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
###############################################################################

library(splines)
library(MASS)

###############################################
## generate true coefficient values
##############################################

genDLcoeff <- function(nLags = 14, latency.v = c(0,2), maxlag.v = c(7,14), cutoff.v = 1e-2,
                       maxval.v = 1, oscil.v = c(1,-1)) {
  ## generate a list of distributed lag coefficients from exponential, gamma, step and null
  ## distributed lag model, for simulation study

  ## matrix of function arguments for generating exp, gamma, and step dl functions
  par.mat <- expand.grid(nLags = nLags, latency = latency.v, maxlag = maxlag.v,
                         cutoff = cutoff.v, maxval = maxval.v, oscil = oscil.v)
  npar <- nrow(par.mat)

  ## list of coefficients
  coeffs <- vector("list", length = npar * 3 + 1)
  names(coeffs) <- c(paste("exp", 1:npar, sep = ""), paste("step", 1:npar, sep = ""),
                     paste("gamma", 1:npar, sep = ""), "null")
  
  for (i in 1:nrow(par.mat)) {
    pars <- as.list(par.mat[i,])
    coeffs[[paste("exp", i, sep = "")]] <- do.call("expDL", pars)
    coeffs[[paste("step", i, sep = "")]] <- do.call("stepDL", pars)
    coeffs[[paste("gamma", i, sep = "")]] <- do.call("gammaDL", pars)
  }
  coeffs[["null"]] <- rep(0, nLags + 1)
  
  ## (normalized by sums of squares) full coefficient matrix
  ncoeffs <- lapply(coeffs, function(listelt) { if (sum(listelt^2) !=0 ) {
    listelt/sqrt(sum(listelt^2))} else listelt })
  
  return(ncoeffs)
}


expDL <- function(nLags, latency, maxlag, cutoff, maxval, oscil) {
  ## generates distributed lag coefficients with exponential decay
  
  rate <- log(cutoff/maxval)/(latency - maxlag)
  coeff <- maxval * exp(-rate * (0:nLags - latency)) * (0:nLags >= latency)
  coeff <- coeff * oscil^((0:nLags + latency) %/% 2)
  return(coeff)
}

gammaDL <- function(nLags, latency, maxlag, cutoff, maxval, oscil) {
  ## generates distributed lag coefficients in shape of gamma function
  
  ## determine rate of decrease
  bb <- seq(0.0001, 2, by = 0.0001)
  test <- gamma(bb * (maxlag - latency)) - maxval/2 * sqrt(pi)/cutoff
  rate <- bb[test == min(test[test>0])]
  ## set coeff values
  coeff <- c(rep(0, latency + 1),
             0.5 * maxval * sqrt(pi)/gamma(rate * (1:(nLags - latency))))
  coeff <- coeff * oscil^((0:nLags + latency) %/% 2)
  return(coeff)
}

stepDL <- function(nLags, latency, maxlag, cutoff, maxval, oscil) {
  ## generates distributed lag coefficients in shape of step function
  
  coeff <- rep(maxval, nLags + 1) * ((0:nLags) >= latency & (0:nLags) <= maxlag)
  coeff <- coeff * oscil^((0:nLags + latency) %/% 2)
  return(coeff)
}


simSingleModelOutcomes <- function(exposure, coeff.v, err.var, num.gen) {
  ## generate num.gen possible outcome series from simple distributed lag model
  ## outcome = exposure %*% coeff.v + epsilon where epsilon ~ N(0,err.var)
    
  ## argument checking
  stopifnot(ncol(as.matrix(exposure)) == length(coeff.v))

  ## compute linear predictor
  linear.pred <- as.matrix(exposure) %*% as.vector(coeff.v)
  len <- length(linear.pred)
  
  ## simulate outcome series
  sim.out <- vector("list", length = num.gen)
  ## set.seed(28) ## for coeff.const = 0.475
  set.seed(14) ## for coeff.const = 0.25
  for (i in 1:num.gen) {
    sim.out[[i]] <- rnorm(len, 0, err.var) + linear.pred
  }
 
  attr(sim.out, "coeff.v") <- coeff.v
  attr(sim.out, "err.var") <- err.var
  invisible(sim.out)
}

#########################################################
## creating, storing, and reading simulated distributed lag outcome data
## Before these functions can be used, the Chicago data must be
## preprocessed using the `pmNMMAPS' function (in `bayes_dlm_sim.r') as a procFunc for
## buildDB in the the `NMMAPSdata' package.  For more information, type `?buildDB'
## after loading the `NMMAPSdata' package into R.
#########################################################

writeExposure <- function() {
  ## use 1996 PM10 Chicago data (none missing) for exposure as in paper
  
  citydata <- readCity("chic")
  ind <- grep("pm10tmean", names(citydata))
  exposdat <- citydata[3288:3653,ind]  ## select 1996 data

  ## standardize exposure series
  colvar <- apply(exposdat, MARGIN = 2, FUN = var)
  colmeans <- colMeans(exposdat)
  exposdat.std <- t(apply(exposdat, MARGIN = 1, FUN = function(matrow) { (matrow - colmeans)/
                                                                           sqrt(colvar) }))

  wd <- getwd()
  write.table(exposdat.std, file = file.path(wd, "..", "logs", "sim_exposdat"))
}

readExposure <- function() {
  wd <- getwd()
  exposdat <- read.table(file.path(wd, "..", "logs", "sim_exposdat"), header = TRUE)
  invisible(exposdat)
}


#########################################################
## save 500 simulated outcomes for all coefficient models
#########################################################

simAllModelOutcomes <- function(coeff.const = 1){
  
  ## read exposure data
  exposdat <- readExposure()
  
  ## get list of coefficients for all models
  coeffs <- genDLcoeff()
  ## coeff.const determines strength of evidence
  ## coeff.const = 0.475 corresponds to t-stats of approx 4 for all except null models
  coeffs <- lapply(coeffs, function(xx) { xx * coeff.const })

  ## simulate 500 outcome series for each model in coeffs, then save
  wd <- getwd()
  for (i in 1:length(coeffs)) {
    simout <- simSingleModelOutcomes(exposure = exposdat, coeff.v = coeffs[[i]], err.var = 1,
                                     num.gen = 500)
    save(simout, file = file.path(wd, "..", "logs", paste(names(coeffs)[i], "_simout_const_",
                   as.character(coeff.const), sep = "")))
  }
}

#########################################################
## estimate and save dl coeffs for all 500 outcomes
## functions read/write to files
#########################################################

estSingleModelCoeff <- function(exposure, model, pollutant, nLags, pdeg, coeff.const, ...){

  ## load simulated data for model
  wd <- getwd()
  load(file.path(wd, "..", "logs", paste(model, "_simout_const_",
                                         as.character(coeff.const), sep = "")))

  estdlm <- vector("list", 500)
  for (i in 1:500) {
    estdlm[[i]] <- vector("list", 3)
    args <- list(simdata = data.frame(outcome = simout[[i]], exposure),
                 outcome.name = "outcome", pollutant = pollutant, nLags = nLags, pdeg = pdeg)
    mle <- do.call("fitMleDLM", args)
    ## use fitSplineApproxDLM instead of fitBayesApproxDLM since former returns an
    ## equivalent degrees of freedom
    bay <- do.call("fitBayesApproxDLM", args)
    psplbay <- do.call("fitSplineApproxDLM", args)
    poly <- do.call("fitPolyDLM", args)
    psplgcv <- do.call("fitPsplineDLMGCV", args)
    estdlm[[i]] <- list(mle = mle, bay = bay, psplbay = psplbay, poly = poly, psplgcv = psplgcv)
  }
  attr(estdlm, "model") <- model
  attr(estdlm, "coeff.v") <- attr(simout, "coeff.v")
  save(estdlm, file = file.path(wd, "..", "logs", paste(model, "_estdlm_const_",
                 as.character(coeff.const), sep = "")))
  return(estdlm)
  
}
estAllModelCoeff <- function(coeff.const = 1) {

  ## read exposure data
  exposdat <- readExposure()
  
  ## get list of coefficients for all models
  coeffs <- genDLcoeff()
  mod.nam <- names(coeffs)

  ## estimate dl coeffs for each model, then save
  for (i in 1:length(coeffs)) {
    estdlm <- estSingleModelCoeff(exposure = exposdat, model = mod.nam[i], pollutant = "pm10tmean",
                                  nLags = 14, pdeg = 4, coeff.const = coeff.const)
  }
}

######################################################
## summary statistics for comparing models
######################################################

computeTotalEffectMSE <- function(mod.name, coeff.const) {
  ## computes mean equared errors of total effects across all estimation methods for given
  ## coefficient model and coefficient constant
  load(file.path(getwd(), "..", "logs", paste(mod.name, "_estdlm_const_", coeff.const, sep = "")))
  totef <- sum(attr(estdlm, "coeff"))
  semat <- sapply(estdlm, function(xx) { sapply(xx, function(yy) { (sum(yy$dl.est) - totef)^2 } ) } )
  mse <- rowMeans(semat)
  return(mse)
}

computeLagMSE <- function(mod.name, coeff.const) {
  ## computes mean squared errors at each lag across all estimation methods for a given set
  ## set of model coefficients
  load(file.path(getwd(), "..", "logs", paste(mod.name, "_estdlm_const_", coeff.const, sep = "")))
  coeff <- attr(estdlm, "coeff")
  sebysim <- lapply(estdlm, function(xx) { sapply(xx, function(yy) { (yy$dl.est - coeff)^2 }) })
  mse <- sebysim[[1]]
  for (i in 2:length(try)) {
    mse <- mse + sebysim[[i]]
  }
  mse <- mse/length(sebysim)
  return(mse)
}

computeTE <- function(estdlm) {
  ## computes total effects across all estimation methods 
  totmat <- sapply(estdlm, function(xx) { sapply(xx, function(yy) { sum(yy$dl.est) } ) } )
  return(totmat)
}

extractEstDL <- function(estdlm, est.method) {
  ## finds estimated distributed lags for estimated dist lags
  ## and given coefficient model
  stopifnot(sum(est.method==names(estdlm[[1]])) > 0)
  return(as.matrix(sapply(estdlm, function(xx) { xx[[est.method]]$dl.est } ) ) )
}


#########################################################
## functions to estimate DLMs -- MLE, Pspline, approx Bayes, Polynomial
#########################################################

fitMleDLM <- function(simdata, outcome.name, pollutant, nLags, ...) {
  # fits linear model outcome ~ nLags of pollutant variable with simdata
  # nLags = 0 uses current day pollution
  
  # model formula
  pol.f <- pollutant
  if (nLags > 0) {
    pol.f <- paste(c(pol.f, paste("l", 1:nLags, pollutant, sep = "")), collapse = " + ")
  }
  lmform <- formula(paste(outcome.name, "~ -1 + ", pol.f))
  
  # dist lag mles and estimated standard errors
  mod <- lm(formula = lmform, data = simdata, na.action = na.omit)
  th.hat <- summary(mod)$coeff[,1]  # exclude intercept
  cov.hat <- summary(mod)$cov.unscaled

  invisible(list(dl.est = th.hat, dl.cov = cov.hat, degf = nLags))
  
}

getGCVPenalty <- function(coeff.const) {
  # extract info about GCV penalties for given set of estimates
  coeff <- genDLcoeff()
  mod.nam <- names(coeff)
  gcv <- sapply(mod.nam, function(xx) {
    load(file.path(getwd(), "..", "logs",
                   paste(xx, "_estdlm_const_", coeff.const, sep = "")), .GlobalEnv)
    return(sapply(estdlm, function(yy) { yy$psplgcv$penalty })) })
  return(gcv)
}

fitPsplineDLM <- function(logpenalty, simdata, outcome.name, pollutant = "pm10tmean", nLags,
                          pdeg, GCV = FALSE, ...) {
  ## fit penalized spline to smooth distributed lag coefficients with specified penalty
  ## if GCV == TRUE, returns GCV value for penalty
  
  ## relevant pollutant data
  pol.v <- pollutant
  if (nLags > 0) {
    pol.v <- c(pol.v, paste("l", 1:nLags, pollutant, sep = ""))
  }
  poldata <- simdata[, pol.v]
  XX <- as.matrix(poldata)

  ## basis matrix, equiv to that used in fitSplineApprox if pdeg = 3
  BB <- bs(0:nLags, df = nLags + 1, intercept = TRUE, degree = pdeg)
  knots <- attr(BB, "knots")
  
  ## transformation between standard basis matrix and b-spline basis BB
  TT <- t(apply(as.matrix(0:nLags), 1, function(xx) { c(xx^(0:pdeg), pmax(0, (xx - knots))^pdeg) }))
  LL <- solve(TT, BB)
  ## scale diagonal penalty matrix accordingly
  DD <- t(LL) %*% diag(c(rep(0, pdeg + 1), rep(1, times = length(knots)))) %*% LL
  penalty <- 10^logpenalty  ## use log scale for better GCV minimization
  mat <- chol2inv(chol(t(TT) %*% t(XX) %*% XX %*% TT + penalty * DD)) %*% t(TT) %*% t(XX)
  yy <- as.vector(simdata[, outcome.name])
  dl.est <- TT %*% mat %*% yy
  degf <- sum(diag(XX %*% TT %*% mat))

  if (GCV == TRUE) {
    fits <- XX %*% TT %*% mat %*% yy
    gcv <- (t(yy - fits) %*% (yy - fits))/(1-1/length(yy) * degf)^2
    return(gcv)
  }
  else {
    return(list(dl.est = dl.est, degf = degf))
  }
}

fitPsplineDLMGCV <- function(simdata, outcome.name, pollutant, nLags, pdeg) {
  ## smooth distributed lag coefficient estimates using GCV to pick penalty term

  gcvmin <- optimize(f = fitPsplineDLM, interval = c(-10, 12), maximum = FALSE, simdata = simdata,
                     outcome.name = outcome.name, pollutant = pollutant, nLags = nLags,
                     pdeg = pdeg, GCV = TRUE)
  final.fit <- fitPsplineDLM(logpenalty = gcvmin$minimum,
                             simdata = simdata, outcome.name = outcome.name,
                             pollutant = pollutant, nLags = nLags, pdeg = pdeg)
  return(list(dl.est = final.fit$dl.est, degf = final.fit$degf,
              penalty = 10^(gcvmin$minimum), gcv = gcvmin$objective))
}

fitPolyDLM <- function(simdata, outcome.name, pollutant = "pm10tmean", nLags, pdeg, ...) {
  ## fits distributed lag model w/ dist lag coeffs following a degree pdeg polynomial
  
  ## relevant pollutant data
  pol.v <- pollutant
  if (nLags > 0) {
    pol.v <- c(pol.v, paste("l", 1:nLags, pollutant, sep = ""))
  }
  poldata <- simdata[, pol.v]

  ## create transformed covariates for regression
  MM <- t(apply(as.matrix(0:nLags), 1, function(xx) { xx^(0:pdeg) }))   # transformation matrix
  newdata <- data.frame(as.matrix(poldata) %*% MM)

  ## fit linear model
  lmform <- as.formula(paste(outcome.name, " ~ -1 + ", paste(names(newdata), collapse = " + ")))
  mod <- lm(lmform, data = cbind(simdata, newdata))

  ## compute estimated distributed lag coefficients from linear model coeffs
  dl.est <- MM %*% as.numeric(mod$coeff)
  dl.cov <- MM %*% summary(mod)$cov.unscaled
  
  return(list(dl.est = dl.est, dl.cov = dl.cov, degf = pdeg))
}

fitBayesApproxDLM <- function(simdata, outcome.name,
                              pollutant = "pm10tmean", nLags, sigma = NULL, ...) {

  ## get dist lag mles and est std errors
  mod <- fitMleDLM(simdata = simdata, outcome.name = outcome.name,
                       pollutant = pollutant, nLags = nLags)
  th.hat <- mod$dl.est
  cov.hat <- mod$dl.cov
  inv.cov.hat <- solve(cov.hat)

  ## set sigma^2 as 10 x statistical variance of lag 0
  sigma <- sqrt(10 * cov.hat[1,1])
  
  ## discrete hyperprior on eta1 and eta2
  hyper.mat <- expand.grid(seq(-0.35, -0.05, length = 10), seq(-0.37, 0, length = 10))
  n.hyperpar <- nrow(hyper.mat)

  ## initialize posterior mean and covariance vectors
  val <- vector(length = n.hyperpar)
  postmeans <- matrix(nrow = n.hyperpar, ncol = nLags + 1)
  postvar <- array(dim = c(nLags + 1, nLags + 1, n.hyperpar))
  
  for (k in 1:n.hyperpar) {
    param <- as.numeric(hyper.mat[k,])
    pr.cov <- dlCov(param = param, n = nLags + 1, sigmasq = sigma^2)
  
    # compute inv of sum of inverses s.t. don't invert pr.cov
    inv.mat <- solve(pr.cov %*% inv.cov.hat + diag(1, nLags + 1))
    inv.sum.cov.inv <- inv.mat %*% pr.cov
    val[k] <- 0.5 * log(det(inv.mat)) -  0.5 * t(th.hat) %*% (inv.cov.hat - inv.cov.hat %*%
                                                              inv.sum.cov.inv %*% inv.cov.hat) %*%
                                                                th.hat
    postmeans[k,] <- inv.sum.cov.inv %*% inv.cov.hat %*% th.hat
    postvar[,,k] <- inv.sum.cov.inv
  }
  bfac <- vector(length = n.hyperpar)  # weights
  for (k in 1:n.hyperpar) {
    bfac[k] <- sum(exp(val - val[k]))^(-1)
  }
  av.theta <- colSums(postmeans * bfac)
  av.cov <- matrix(rep(0,(nLags + 1)*(nLags + 1)), ncol = nLags + 1,
                   nrow = nLags + 1)
  for (k in 1:n.hyperpar) {
    av.cov <- av.cov + postvar[,,k] * bfac[k]^2
  }
  ## return fewer items for simulation study
  ## return(list(post.means = postmeans, post.var = postvar, bfac = bfac,
  ##            dl.est = av.theta, dl.cov = av.cov))
  return(list(dl.est = av.theta, dl.cov = av.cov))
}

