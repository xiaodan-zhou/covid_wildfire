pm_model <- function(dff, df.pm = 7, df.date = 6, df.tmmx = 2, df.rmax = 2, lags = 0:14, model = c("Constrained", "Unconstrained"),
                     mobility = NA, cause = "cases", smooth = "ns", group = "FIPS", offset = "population", 
                     control = zeroinfl.control(EM = FALSE), fullDist = FALSE) {
  
  pollutants.name = "pm"
  
  if (model == "Constrained") {
    
    X.l <- create.lag.value(dff, value = "pm25", group = group, lags = lags)
    U <- matrix(ns(c(lags), df = df.pm, intercept = TRUE), nrow = length(lags), ncol = df.pm) # natural spline basis matrix
    lagpm <- as.matrix(X.l) %*% as.matrix(U)
    
  } else
    lagpm <- as.matrix(create.lag.value(dff=dff, value="pm25", group=group, lags=lags))

  lag.data = "lagpm" # as.character(as.name(substitute(base))) # deparse(substitute(base))
  dff$log.pop <- log(dff[,offset])
  
  ### formula
  f <- substitute(~ smooth(tmmx, df.tmmx) + smooth(rmax, df.rmax) +
                    smooth(date_num, df.date) + dayofweek,
                  list(df.date = df.date, df.tmmx = df.tmmx, df.rmax = df.rmax, smooth = as.name(smooth)))
    
  rhs = as.character(f)
  
  ### new mobility
  if (!is.na(mobility) & (mobility == TRUE))
    rhs[-1] = paste(rhs[-1], "relative_change_feb", sep = " + ") 
  
  ### add pollutants
  rhs[-1] = paste(rhs[-1], lag.data, sep = " + ")
  
  ### add Zero model
  # rhs[-1] = paste0(rhs[-1], " | ", smooth, "(date_num, ", df.date, ")")
  rhs[-1] = paste0(rhs[-1], " | 1")
  
  ### add cause
  modelFormula = as.formula(paste(cause, paste(rhs, collapse = "")))
  
  call = substitute(zeroinfl(modelFormula, dist = "negbin", link = "logit", data = dff, 
                             offset = log.pop, control = control, na.action = na.exclude),
                    list(modelFormula = modelFormula, control = substitute(control), lag.name1 = pm))
  
  ### if hit any problems in modelling, returns -1 
  print(call)
  fit = try(eval(call), silent = TRUE)
  if(inherits(fit, 'try-error')) { return(-1) }
  
  ### output variable names 
  if(!fullDist & model == "Constrained"){
    
    var.names <- paste0("count_lagpm", 1:df.pm) 
    mean.dlm <- sum(U%*%(coef(fit)[var.names]))
    lincomb <- rep(1, length(lags))
    std.dlm <- as.vector(sqrt(t(lincomb) %*% U %*% vcov(fit)[var.names, var.names] %*% t(U) %*% lincomb))
    low.dlm <- mean.dlm - 1.96 * std.dlm
    high.dlm <- mean.dlm + 1.96 * std.dlm
    coefs <- c(mean.dlm, low.dlm, high.dlm)
    coefs.names <- c( "pm", "pm.low", "pm.high")
    
    ### transform so that the output is %change given 10ug/m3 increase
    coefs = trans.coef(coefs)
    names(coefs) = coefs.names
    
    return(coefs)
    
  } else if (!fullDist & model == "Unconstrained"){
    
    var.names <- paste0("count_lagpm.l", lags) 
    mean.dlm <- sum(coef(fit)[var.names])
    lincomb <- rep(1, length(lags))
    std.dlm <- as.vector(sqrt(t(lincomb) %*% vcov(fit)[var.names, var.names] %*% lincomb))
    low.dlm <- mean.dlm - 1.96 * std.dlm
    high.dlm <- mean.dlm + 1.96 * std.dlm
    coefs <- c(mean.dlm, low.dlm, high.dlm)
    coefs.names <- c( "pm", "pm.low", "pm.high")
    
    ### transform so that the output is %change given 10ug/m3 increase
    coefs = trans.coef(coefs)
    names(coefs) = coefs.names
    
    return(coefs)
    
  } else if (fullDist & model == "Constrained") {
    
    mu.init <- c(fit$coefficients$count[1])
    beta.init <- c(fit$coefficients$count[c(grep("tmmx", names(fit$coefficients$count)),
                                          grep("rmax", names(fit$coefficients$count)),
                                          grep("relative", names(fit$coefficients$count)),
                                          grep("dayof", names(fit$coefficients$count)))])
    xi.init <- c(fit$coefficients$count[grep("date_num", names(fit$coefficients$count))])
    delta.init <- c(fit$coefficients$count[grep("lagpm", names(fit$coefficients$count))])
    phi.init <- fit$theta
    
    out <- list(mu.init = mu.init, xi.init = xi.init, phi.init = phi.init, beta.init = beta.init, delta.init = delta.init)
    
    return(out)
    
  } else {
    
    mu.init <- fit$coefficients$count[1]
    beta.init <- fit$coefficients$count[c(grep("tmmx", names(fit$coefficients$count)),
                                          grep("rmax", names(fit$coefficients$count)),
                                          grep("relative", names(fit$coefficients$count)),
                                          grep("dayof", names(fit$coefficients$count)))]
    xi.init <- fit$coefficients$count[grep("date_num", names(fit$coefficients$count))]
    eta.init <- fit$coefficients$count[grep("lagpm", names(fit$coefficients$count))]
    phi.init <- fit$theta
    
    out <- list(mu.init = mu.init, xi.init = xi.init, phi.init = phi.init, beta.init = beta.init, eta.init = eta.init)
    
  }
  
}
