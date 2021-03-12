
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

## NOTE: Before any functions below except `pmNMMAPS' can be used, the data must be
## preprocessed using the `pmNMMAPS' function as a procFunc for
## buildDB in the the `NMMAPSdata' package.  For more information, type `?buildDB'
## after loading the `NMMAPSdata' package into R.

library(splines)
library(MASS)


#####################################################
## a procFunc for buildDB, yielding basicNMMAPS plus lags 0:14 of pm10tmean and pm25tmean
#####################################################

pmNMMAPS <- function (dataframe) {

  if (all(is.na(dataframe[, c("pm10tmean", "pm25tmean")]))) 
        return(NULL)
    is.na(dataframe[, "death"]) <- as.logical(dataframe[, "markdeath"])
    is.na(dataframe[, "cvd"]) <- as.logical(dataframe[, "markcvd"])
    is.na(dataframe[, "resp"]) <- as.logical(dataframe[, "markresp"])
    
    Age2Ind <- as.numeric(dataframe[, "agecat"] == 2)
    Age3Ind <- as.numeric(dataframe[, "agecat"] == 3)
    dataframe[, "dow"] <- as.factor(dataframe[, "dow"])
    dataframe[, "agecat"] <- as.factor(dataframe[, "agecat"])

    varList <- c("date", "cvd", "death", "resp", "tmpd", "rmtmpd", "dptp", 
        "rmdptp", "time", "agecat", "dow", "pm10tmean", paste("l", 
            1:7, "pm10tmean", sep = ""), "pm25tmean", paste("l", 1:7, "pm25tmean", sep = ""))
    len <- nrow(dataframe)/3
    addLags <- data.frame(matrix(nrow = len, ncol = 14))
    addLags[8:len, 1:7] <- dataframe[1:(len - 7), paste("l", 1:7, "pm10tmean", sep = "")]
    addLags[8:len, 8:14] <- dataframe[1:(len - 7), paste("l", 1:7, "pm25tmean", sep = "")]
    names(addLags) <- c(paste("l", 8:14, "pm10tmean", sep = ""),
                        paste("l", 8:14, "pm25tmean", sep = ""))
    addLags <- rbind(addLags, addLags, addLags)
    data.frame(dataframe[, varList], Age2Ind = Age2Ind, Age3Ind = Age3Ind,
               addLags)
}


#####################################################
## fit a bayesian distributed lag model (BDLM)
#####################################################

bayesDLM <- function(citydata, pollutant = "pm10tmean", nLags = 14, sigma = 0.004,
                     its = 10) {
  time1 <- proc.time()[3]
  
  ## initialize hyperprior parameters, eta1 and eta2
  eta.matrix <- expand.grid(seq(-0.35, -0.05, length = 10), seq(-0.37, 0, length = 10))
  eta1 <- sample(eta.matrix[,1], 1)
  eta2 <- sample(eta.matrix[,2], 1)

  ## initialize beta (non-pollutant coefficients) and get non-pollutant offset
  beta.step <- updateBeta(data = citydata, polloffset = rep(0, nrow(citydata)))
  beta <- beta.step$beta
  nonpolloffset <- beta.step$nonpolloffset

  ## initialize theta (dist lag coeffs) and get pollutant offset
  theta.step <- updateTheta(data = citydata, nonpolloffset = nonpolloffset,
                          pollutant = pollutant, nLags = nLags,
                          sigma = sigma, eta1 = eta1, eta2 = eta2)
  theta <- theta.step$theta
  inv.cov.hat <- theta.step$inv.cov.hat
  polloffset <- theta.step$polloffset

  ## repeatedly update eta, theta, beta, save thetas, etas, reject/accept, and corr btw offsets
  est.matrix <- matrix(ncol = nLags + 6, nrow = its + 1)
  notmiss <- !is.na(nonpolloffset) & !is.na(polloffset)
  est.matrix[1,] <- c(as.numeric(theta), eta1, eta2, NA,
                      cor(nonpolloffset[notmiss], polloffset[notmiss]), NA)
  for (i in 1:its) {
    eta.step <- updateEta(eta.matrix = eta.matrix, theta = theta,
                          inv.cov.hat = inv.cov.hat, nLags = nLags, sigma = sigma)
    eta1 <- eta.step$eta1
    eta2 <- eta.step$eta2

    theta.step <- updateTheta(data = citydata, nonpolloffset = nonpolloffset,
                              pollutant = pollutant, nLags = nLags,
                              sigma = sigma, eta1 = eta1, eta2 = eta2, oldtheta = theta)
    theta <- theta.step$theta
    inv.cov.hat <- theta.step$inv.cov.hat
    polloffset <- theta.step$polloffset

    beta.step <- updateBeta(data = citydata, polloffset = polloffset, oldbet = beta)
    beta <- beta.step$beta
    nonpolloffset <- beta.step$nonpolloffset

    notmiss <- !is.na(nonpolloffset) & !is.na(polloffset)
    est.matrix[i+1,] <- c(as.numeric(theta), eta1, eta2, theta.step$reject,
                          cor(nonpolloffset[notmiss], polloffset[notmiss]), beta.step$reject)
  }

  time1 <- proc.time()[3] - time1
  print(time1)

  rval <- as.data.frame(est.matrix)
  names(rval) <- c(names(theta), "eta1", "eta2", "reject", "offsetcor", "rejectbeta")
  invisible(rval)
}

#####################################################
### support functions for bayesDLM
#####################################################

updateBeta <- function(data, polloffset, oldbet = NULL) {
  
  ## fit model with pollutant offset and non-pollutant covariates
  fit <- fitPartialMod(data = cbind(data, polloffset = polloffset), pollutant = NULL,
                       nonpollutant = TRUE, offset = "polloffset")
  beta.hat <- summary(fit)$coeff[,1]
  cov.hat <- summary(fit)$cov.scaled

  ## generate new beta from candidate distribution
  step <- 0.03
  if (!is.null(oldbet)) {
    newbet <- mvrnorm(n = 1, mu = oldbet, Sigma = step * cov.hat)
  }
  else {
    newbet <- beta.hat
  }

  ## subset data used for fit
  modmat <- model.matrix(fit$formula, data = cbind(data, polloffset))
  usedata <- as.numeric(rownames(modmat))
  
  reject <- NA
  if (!is.null(oldbet)) {
    ## accept or reject beta drawn from proposal distribution, flat prior
    ## only eval lik ratio since using a random walk proposal
    
    lr <- exp(sum(dpois(data$death[usedata], exp(modmat %*% newbet +
                                               polloffset[usedata]), log = TRUE)) - 
              sum(dpois(data$death[usedata], exp(modmat %*% oldbet +
                                        polloffset[usedata]), log = TRUE)))
    ## reject newbeta?
    reject <- (lr < 1) && (sample(c(1,0), size = 1, prob = c(lr, 1-lr)) == 0)
    if (reject) {
      newbet <- oldbet
    }
  }

  ## generate non-pollutant offset with new beta
  nonpolloffset <- rep(NA, nrow(data))
  nonpolloffset[usedata] <- modmat %*% newbet
    
  invisible(list(beta = newbet, nonpolloffset = nonpolloffset, reject = reject))

}


updateTheta <- function(data, nonpolloffset, pollutant, nLags, sigma, eta1, eta2,
                        oldtheta = NULL) {

  ## fit model with nonpollutant offset and pollutant covariates
  fit <- fitPartialMod(data = cbind(data, nonpolloffset = nonpolloffset),
                       pollutant = pollutant, nLags = nLags,
                       nonpollutant = FALSE, offset = "nonpolloffset")
  ## likelihood estimate for theta, prior covariance for theta
  theta.hat <- summary(fit)$coeff[,1]
  cov.hat <- summary(fit)$cov.scaled
  inv.cov.hat <- solve(cov.hat)
  pr.cov <- dlCov(param = c(eta1, eta2), n = nLags + 1, sigmasq = sigma^2)

  ## sample theta from (approximate) posterior distribution
  inv.mat <- solve(pr.cov %*% inv.cov.hat + diag(1, nLags + 1))
  Sigma <- inv.mat %*% pr.cov
  inv.Sigma <- solve(Sigma)
  Mu <- Sigma %*% inv.cov.hat %*% theta.hat
  newtheta <- mvrnorm(n = 1, mu = Mu, Sigma = Sigma)

  ## subset data used for fit
  modmat <- model.matrix(fit$formula, data = cbind(data, nonpolloffset))
  usedata <- as.numeric(rownames(modmat))
  
  reject <- NA
  if (!is.null(oldtheta)) {
    ## accept or reject theta drawn from proposal distribution
    lik.ratio <- exp(sum(dpois(data$death[usedata], exp(modmat %*% newtheta +
                                               nonpolloffset[usedata]), log = TRUE)) - 
              sum(dpois(data$death[usedata], exp(modmat %*% oldtheta +
                                        nonpolloffset[usedata]), log = TRUE)))

    ## ratio of prior distn (and similarly ratio of candidate distn) unstable since 
    ## 0.5 * t(oldtheta) %*% solve(pr.cov) %*% oldtheta can get very large, instead:
    
    ## candidate distn eval at oldtheta divided by prior eval at oldtheta
    oldtheta.ratio <- exp(-0.5 * t(oldtheta - Mu) %*% inv.Sigma %*% (oldtheta - Mu) +
                          0.5 * t(oldtheta) %*% solve(pr.cov) %*% (oldtheta))
    ## prior distn eval at newtheta divided by candidate distn eval at newtheta
    newtheta.ratio <- exp(-0.5 * t(newtheta) %*% solve(pr.cov) %*% (newtheta) +
                          0.5 * t(newtheta - Mu) %*% inv.Sigma %*% (newtheta - Mu))
    rr <- lik.ratio * oldtheta.ratio * newtheta.ratio

    ## reject newtheta?
    reject <- (rr < 1) && (sample(c(1,0), size = 1, prob = c(rr, 1-rr)) == 0)
    if (reject) {
      newtheta <- oldtheta
    }
  }             
  ## pollutant offset
  polloffset <- rep(NA, nrow(data))
  polloffset[usedata] <- modmat %*% newtheta
    
  invisible(list(theta = newtheta, inv.cov.hat = inv.cov.hat, polloffset = polloffset,
                 reject = reject))  
}

updateEta <- function(eta.matrix, theta, inv.cov.hat, nLags, sigma) {

  n.etas <- nrow(eta.matrix)
  logweights <- vector(length = n.etas)
  # generate new weights from eta given theta
  for (k in 1:n.etas) {
    pr.cov <- dlCov(param = c(eta.matrix[k,1], eta.matrix[k,2]),
                    n = nLags + 1, sigmasq = sigma^2)
    inv.pr.cov <- solve(pr.cov)
    logweights[k] <- log(det(inv.pr.cov)) - t(theta) %*% inv.pr.cov %*% theta
  }
  weights <- exp(logweights) / sum(exp(logweights))
  
  ## sample new etas
  k <- sample(1:n.etas, size = 1, prob = weights)
  invisible(list(eta1 = eta.matrix[k,1], eta2 = eta.matrix[k,2]))

}

formPartialMod <- function(cause, pollutant = NULL, nLags = 0, nonpollutant = TRUE,
                         df.Time, df.time, df.Temp, df.Dew, offset = NULL) {

  stopifnot(nonpollutant == is.null(pollutant), !is.null(offset))      
  form.str <- paste(cause, "~")

  ## formula for non-pollutant model
  if (nonpollutant) {
    covariates.f <- "dow + agecat"
    
    ## Smooth functions of temperature and dew points (w/running means)
    weather.f <- paste(c(paste("ns(tmpd,", df.Temp, ")"),
                         paste("ns(rmtmpd,", df.Temp, ")"),
                         paste("ns(dptp,", df.Dew, ")"),
                         paste("ns(rmdptp,", df.Dew, ")")), 
                       collapse = "+")
    
    ## Smooth function(s) of time; separate ones for age categories 2, 3
    time.f <- paste(c(paste("ns(time,", df.Time, ")"),
                      paste("I(ns(time,", df.time, ")*Age2Ind)"),
                      paste("I(ns(time,", df.time, ")*Age3Ind)")),
                    collapse = "+")
    
    form.str <- paste(form.str,
                      paste(c(covariates.f, time.f, weather.f), collapse = "+"))
  }
  
  ## formula for pollutant model
  if (!is.null(pollutant)) {
    form.str <- paste(form.str, "-1 + ", pollutant)
    if (nLags > 0) {
      lagpoll.f <- paste("l", 1:nLags, pollutant, collapse = " + ", sep = "")
      form.str <- paste(form.str, lagpoll.f, sep = "+")
    }
  }

  form.str <- paste(form.str, paste("offset(", offset, ")"), sep = " + ")
  as.formula(form.str)
    
}

fitPartialMod <- function(data, pollutant = "pm10tmean", nLags = 14, cause = "death",
                          nonpollutant = FALSE, df.Time = 98, df.time = 14, df.Temp = 6,
                          df.Dew = 3, offset = NULL, extractors = NULL) {
    ## Argument checking
    stopifnot(is.character(pollutant) | is.null(pollutant),
              is.character(cause), length(cause) == 1)

    modelFormula <- formPartialMod(cause = cause, pollutant = pollutant, nLags = nLags,
                                 nonpollutant = nonpollutant, df.Time = df.Time,
                                 df.time = df.time, df.Temp = df.Temp, df.Dew = df.Dew,
                                 offset = offset)

    ## Fit the model!
    fit <- glm(modelFormula, family = poisson, data = data,
               control = glm.control(epsilon = 1e-10, maxit = 1000),
               na.action = na.omit)
    
    ## Extract information from the fitted glm model object using the
    ## list of functions in `extractors'.  If no extractors are
    ## specified, just return the entire fitted model object.
    rval <- if(is.null(extractors))
        fit
    else 
        lapply(extractors, function(f) f(fit))
    invisible(rval)    
}

#####################################################
### support functions for bayesDLM, bayesDLMapprox, and splineDLMapprox
#####################################################

covToCor <- function(M) {
  ## converts cov matrix to correlation matrix
  S <- diag(1/sqrt(diag(M)))
  return(S %*% M %*% S)
}

varFun <- function(lambda, u) {
  ## variance of distributed lag u
  return(exp(lambda * u))
}

corFun <- function(gamma, u) {
  ## weight function for correlation matrix
  return(exp(gamma * u))
}

genCorMat <- function(gamma,n) {
  ## correlation matrix for distributed lag vector
  D <- diag(corFun(gamma, 0:(n-1)))
  M <- matrix(rep(1,n^2), nrow = n, ncol = n)
  I <- diag(rep(1,n))
  cov <- D %*% t(D) + (I-D) %*% M %*% t(I-D)
  return(covToCor(cov))
}

dlCov <- function(param, n, sigmasq) {
  ## prior covariance matrix for n x 1 distributed lag vector
  ## param is 2 parameters determining covariance of distributed lag vector
  eta1 <- param[1]
  eta2 <- param[2]
  R <- genCorMat(eta2, n)
  V <- diag(sqrt(varFun(eta1, 0:(n-1))))
  return(sigmasq * (V %*% R %*% V))
}

fitSingleCity <- function(data, pollutant = "pm10tmean", nLags = 14, cause = "death",
                          df.Time = 98, df.time = 14, df.Temp = 6,
                          df.Dew = 3, offset = NULL, extractors = NULL) {
    ## Argument checking
    stopifnot(is.character(pollutant) | is.null(pollutant),
              is.character(cause), length(cause) == 1)

    modelFormula <- setupFormula(cause, pollutant, nLags, df.Time, df.time,
                                 df.Temp, df.Dew, offset = offset)

    ## Fit the model!
    fit <- glm(modelFormula, family = poisson, data = data,
               control = glm.control(epsilon = 1e-10, maxit = 1000),
               na.action = na.omit)
    
    ## Extract information from the fitted glm model object using the
    ## list of functions in `extractors'.  If no extractors are
    ## specified, just return the entire fitted model object.
    rval <- if(is.null(extractors))
        fit
    else 
        lapply(extractors, function(f) f(fit))
    invisible(rval)    
}

setupFormula <- function(cause, pollutant = NULL, nLags = 14, df.Time, df.time, df.Temp,
                         df.Dew, offset = NULL) {
    covariates.f <- paste(cause, "~ dow + agecat")

    ## Smooth functions of temperature and dew points (w/running means)
    weather.f <- paste(c(paste("ns(tmpd,", df.Temp, ")"),
                         paste("ns(rmtmpd,", df.Temp, ")"),
                         paste("ns(dptp,", df.Dew, ")"),
                         paste("ns(rmdptp,", df.Dew, ")")), 
                       collapse = "+")

    ## Smooth function(s) of time; separate ones for age categories 2, 3
    time.f <- paste(c(paste("ns(time,", df.Time, ")"),
                      paste("I(ns(time,", df.time, ")*Age2Ind)"),
                      paste("I(ns(time,", df.time, ")*Age3Ind)")),
                    collapse = "+")

    form.str <- paste(c(covariates.f, time.f, weather.f), collapse = "+")
    
    ## Distributed lags of pollutants
    if (!is.null(pollutant)) {
      poll.f <- paste(pollutant, collapse = "+")
      if (nLags > 0) {
        poll.f <- paste(pollutant, "+",
                        paste("l", 1:nLags, pollutant, collapse = " + ", sep = ""))  
      }
      form.str <- paste(form.str, poll.f, sep = " + ")
    }

    if (!is.null(offset)) {
      form.str <- paste(form.str, paste("offset(", offset, ")"), sep = " + ")
    }
    as.formula(form.str)   
}


#####################################################
## fit approximate bayesian distributed lag model using normal approx to Poisson
#####################################################

bayesDLMapprox <- function(data, pollutant = "pm10tmean", nLags = 14, cause = "death",
                           df.Time = 98, df.time = 14, df.Temp = 6, df.Dew = 3, sigma = 0.004,
                           eta.mat = NULL) {

  ## specific values for eta1 and eta2 can be set by specifying eta.v vector
  ## if not specified then hyperprior uniform over grid of eta1 x eta2 

  ## discrete hyperprior on eta1 and eta2
  if (is.null(eta.mat)) {
    hyper.mat <- expand.grid(seq(-0.35, -0.05, length = 10), seq(-0.37, 0, length = 10))
  }
  else {
    stopifnot(ncol(eta.mat) == 2)
    hyper.mat <- eta.mat
  }
  n.hyperpar <- nrow(hyper.mat)
    
  # get dist lag mle's and est standard errors
  mod <- fitSingleCity(data, pollutant = pollutant, nLags = nLags, cause = cause,
                       df.Time = df.Time, df.time = df.time, df.Temp = df.Temp, df.Dew = df.Dew)
  ind <- grep(pollutant, rownames(summary(mod)$coeff))
  th.hat <- summary(mod)$coeff[ind]
  cov.hat <- summary(mod)$cov.scaled[ind,ind]
  inv.cov.hat <- solve(cov.hat)
  
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
    val[k] <- 0.5 * log(det(inv.mat)) -  
      0.5 * t(th.hat) %*% (inv.cov.hat - inv.cov.hat %*% inv.sum.cov.inv
                           %*% inv.cov.hat) %*% th.hat
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
  return(list(post.means = postmeans, post.var = postvar, bfac = bfac,
              av.theta = av.theta, av.cov = av.cov))
}

#####################################################
## fit approximate penalized spline dlm using penalty derived from BDLM prior
#####################################################

splineDLMapprox <- function(data, pollutant = "pm10tmean", nLags = 14, cause = "death",
                            df.Time = 98, df.time = 14, df.Temp = 6, df.Dew = 3, sigma = 0.004,
                            sigma.epsilon = 0.004) {
  
  ## get dist lag mle's and est standard errors
  mod <- fitSingleCity(data, pollutant = pollutant, nLags = nLags, cause = cause,
                       df.Time = df.Time, df.time = df.time, df.Temp = df.Temp, df.Dew = df.Dew)
  ind <- grep(pollutant, rownames(summary(mod)$coeff))
  th.hat <- summary(mod)$coeff[ind]
  cov.hat <- summary(mod)$cov.scaled[ind,ind]

  ## discrete hyperprior on eta1 and eta2
  hyper.mat <- expand.grid(seq(-0.35, -0.05, length = 10), seq(-0.37, 0, length = 10))
  n.hyperpar <- nrow(hyper.mat)
  
  ## initialize posterior mean
  val <- vector(mode = "numeric", length = n.hyperpar)
  postmeans <- matrix(nrow = n.hyperpar, ncol = nLags + 1)
  postvar <- array(dim = c(nLags + 1, nLags + 1, n.hyperpar))
  smotrace <- vector(mod = "numeric", length = n.hyperpar)
  
  ## basis for spline
  xx <- bs(0:nLags, df = nLags + 1, intercept = TRUE)
  QQ <- qr.Q(qr(xx))
  RR <- qr.R(qr(xx))
  RR.inv <- solve(RR)

  ## estimated theta for each value of eta
  for (k in 1:n.hyperpar) {
    param <- as.numeric(hyper.mat[k,])
    pr.cov <- dlCov(param = param, n = nLags + 1, sigmasq = sigma^2)
    inv.pr.cov <- solve(pr.cov)
    inv.lik.cov <- solve(cov.hat)
    
    Gamma <- RR.inv %*% t(QQ) %*% pr.cov %*% QQ %*% t(RR.inv)
    inv.Gamma <- solve(Gamma)

    smoother.mat <- xx %*% solve(t(xx) %*% inv.lik.cov %*% xx + inv.Gamma) %*% t(xx) %*% inv.lik.cov
    smotrace[k] <- sum(diag(smoother.mat))
    postmeans[k,] <- smoother.mat %*% th.hat
    postvar[,,k] <- xx %*% solve(t(xx) %*% inv.lik.cov %*% xx + inv.Gamma) %*% t(xx)

    inv.mat <- solve(Gamma %*% t(xx) %*% inv.lik.cov %*% xx + diag(rep(1,ncol(xx))))
      
    val[k] <- 0.5 * log(det(inv.mat)) - 0.5 * t(th.hat) %*% inv.lik.cov %*% th.hat +
      0.5 * t(th.hat) %*% inv.lik.cov %*% xx %*% solve(t(xx) %*% inv.lik.cov %*% xx +
                                                       inv.Gamma) %*% t(xx) %*% inv.lik.cov %*% th.hat    
  }

  ## posterior probabilities of eta given max lik ests of theta
  bfac <- vector(length = n.hyperpar)
  for (k in 1:n.hyperpar) {
    bfac[k] <- sum(exp(val - val[k]))^(-1)
  }

  ## posterior mean and variance
  av.theta <- colSums(postmeans * bfac)
  av.cov <- matrix(rep(0,(nLags + 1)*(nLags + 1)), ncol = nLags + 1,
                   nrow = nLags + 1)
  for (k in 1:n.hyperpar) {
    av.cov <- av.cov + postvar[,,k] * bfac[k]^2
  }
  ## approximate degrees of freedom for posterior mean theta
  degf <- sum(smotrace * bfac)
  
  return(list(post.means = postmeans, post.var = postvar, bfac = bfac,
              av.theta = av.theta, av.cov = av.cov, degf = degf))
}


########################################################
### simulate values from BDLM approximate posterior distribution
########################################################

simDLMapprox <- function(data, pollutant = "pm25tmean", nLags = 14, sigma = 0.004, its = 1000) {

  ## get approximate dist for dist lag coeffs
  approx <- bayesDLMapprox(data = data, pollutant = pollutant, nLags = nLags, sigma = sigma)
  post.means <- approx$post.means
  post.var <- approx$post.var
  post.bfac <- approx$bfac

  ## simulate thetas from full posterior distribution (mixture of normals)
  n.hyper <- length(post.bfac)
  theta.sim <- matrix(nrow = its, ncol = nLags + 1)
  for (i in 1:its) {
   k <- sample(1:n.hyper, size = 1, prob = post.bfac)
   theta.sim[i,] <- mvrnorm(1, mu = post.means[k,], Sigma = post.var[,,k])
  }
  
  invisible(theta.sim)
}

