pm_model <- function(dff, df.pm = 7, df.date = 6, df.tmmx = 2, df.rmax = 2, lags = 0:28, model = c("Constrained", "Unconstrained"),
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
  dff[,group] <- as.factor(dff[,group])
  
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
  # rhs[-1] = paste0(rhs[-1], " | ", group)
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

pm_avg_model <- function(dff, df.date = 6, df.tmmx = 2, df.rmax = 2, idx = 1:29,
                     cause = "cases", smooth = "ns", group = "FIPS", offset = "population", 
                     control = zeroinfl.control(EM = FALSE)) {
  
  pollutants.name = "pm"
    
  X.l <- create.lag.value(dff, value = "pm25", group = group, lags = 0:28)
  # dff$avg.pm7 <- rowMeans(X.l[,1:8])
  # dff$avg.pm14 <- rowMeans(X.l[,9:15])
  # dff$avg.pm21 <- rowMeans(X.l[,16:22])
  dff$avg.pm28 <- rowMeans(X.l[,idx])
  dff$log.pop <- log(dff[,offset])
  dff[,group] <- as.factor(dff[,group])
  knots <- quantile(dff$avg.pm28, c(0.25, 0.5, 0.75, 0.9), na.rm = T) 
  
  ### formula
  f <- substitute(~ smooth(avg.pm28, knots = knots) + 
                    smooth(tmmx, df.tmmx) + smooth(rmax, df.rmax) +
                    smooth(date_num, df.date) + dayofweek,
                  list(knots = knots, df.date = df.date, df.tmmx = df.tmmx, 
                       df.rmax = df.rmax, smooth = as.name(smooth)))
  
  rhs = as.character(f)
  
  ### add Zero model
  # rhs[-1] = paste0(rhs[-1], " | ", group)
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
  
  pm.pred <- seq(3, 30, by = 0.1)
  # nsmod7 <- ns(dff$avg.pm7, knots = knots)
  # nsmod14 <- ns(dff$avg.pm14, knots = knots)
  # nsmod21 <- ns(dff$avg.pm21, knots = knots)
  nsmod28 <- ns(dff$avg.pm28, knots = knots)
  
  # var.names7 <- grep("ns\\(avg.pm7", names(fit$coefficients$count))
  # var.names14 <- grep("ns\\(avg.pm14", names(fit$coefficients$count))
  # var.names21 <- grep("ns\\(avg.pm21", names(fit$coefficients$count))
  var.names28 <- grep("ns\\(avg.pm28", names(fit$coefficients$count))
  # mean.dlm7 <- predict(nsmod7, pm.pred)%*%coef(fit)[var.names7] - predict(nsmod7, rep(8, length(pm.pred)))%*%coef(fit)[var.names7]
  # mean.dlm14 <- predict(nsmod14, pm.pred)%*%coef(fit)[var.names14] - predict(nsmod14, rep(8, length(pm.pred)))%*%coef(fit)[var.names14]
  # mean.dlm21 <- predict(nsmod21, pm.pred)%*%coef(fit)[var.names21] - predict(nsmod21, rep(8, length(pm.pred)))%*%coef(fit)[var.names21]
  mean.dlm28 <- predict(nsmod28, pm.pred)%*%coef(fit)[var.names28] - predict(nsmod28, rep(8, length(pm.pred)))%*%coef(fit)[var.names28]
  std.dlm28 <- sqrt(diag((predict(nsmod28, pm.pred) - predict(nsmod28, rep(8, length(pm.pred)))) %*%
                           vcov(fit)[var.names28, var.names28] %*%
                           t(predict(nsmod28, pm.pred) - predict(nsmod28, rep(8, length(pm.pred))))))
  
  low.dlm28 <- mean.dlm28 - 1.96 * std.dlm28
  high.dlm28 <- mean.dlm28 + 1.96 * std.dlm28
  
  # out <- rbind(cbind(pm.pred, mean.dlm7, rep(7, length(pm.pred))),
  #       cbind(pm.pred, mean.dlm14, rep(14, length(pm.pred))),
  #       cbind(pm.pred, mean.dlm21, rep(21, length(pm.pred))),
  #       cbind(pm.pred, mean.dlm28, rep(28, length(pm.pred))))
  out <- cbind(pm.pred, mean.dlm28, std.dlm28, low.dlm28, high.dlm28, rep(max(idx) - 1, length(pm.pred)))
  
  colnames(out) <- c("pm", "rr", "se", "hi", "lo", "sma")
  out <- as.data.frame(out)
    
  return(out)
  
}
