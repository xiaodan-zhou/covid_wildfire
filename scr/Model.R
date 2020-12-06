model = function(dff, df.date=8, df.tmmx=3, df.rmax=3, lags=0,
                 cause="cases", pollutants = 1, mobility = NA, 
                 smooth="ns", group="FIPS", control=glm.control(epsilon = 1e-10, maxit = 10000)) {
  # TODO
  if (!is.na(mobility)) stop("mobility data not available yet!")
  
  ## get lag values for pollutants
  if (pollutants == 1) {
    pollutants.name = "pm"
    pm = as.matrix(create.lag.value(dff=dff, value="pm25", group=group, lags=lags))
    lag.data1 = "pm" # as.character(as.name(substitute(base))) # deparse(substitute(base))  
  } else {
    pollutants.name = c("base", "hazard")
    base = as.matrix(create.lag.value(dff=dff, value="pmbase", group=group, lags=lags))
    lag.data1 = "base"
    hazard = as.matrix(create.lag.value(dff=dff, value="pmhazard", group=group, lags=lags))
    lag.data2 = "hazard"
  }
  
  ### formula
  f = substitute(~ smooth(date_num, df.date) + smooth(tmmx, df.tmmx) + 
                   smooth(rmax, df.rmax) + dayofweek,
                 list(df.date = df.date, df.tmmx = df.tmmx, 
                      df.rmax = df.rmax, smooth = as.name(smooth)))
    
  rhs = as.character(f)
  
  ### add mobility
  if (!is.na(mobility)) 
    rhs[-1] = paste(rhs[-1], mobility, sep = "+")
  
  ### add FIPS
  if (dim(unique(dff[group]))[1] > 1) 
    rhs[-1] = paste(rhs[-1], group, sep = "+")
  
  ### add pollutants
  rhs[-1] = paste(rhs[-1], lag.data1, sep = "+")
  if (pollutants == 2) rhs[-1] = paste(rhs[-1], lag.data2, sep = "+")

  ### add cause
  modelFormula = as.formula(paste(cause, paste(rhs, collapse = "")))
  
  ### fit quasipoisson model
  if (pollutants == 1) {
    call = substitute(glm(modelFormula, family = quasipoisson, data = dff, 
                          control = control, na.action = na.exclude),
                      list(modelFormula = modelFormula, control = substitute(control), 
                           lag.name1 = pm))
  } else {
    call = substitute(glm(modelFormula, family = quasipoisson, data = dff, 
                          control = control, na.action = na.exclude),
                      list(modelFormula = modelFormula, control = substitute(control), 
                           lag.name1 = base, lag.name2 = hazard))
    }
  
  ### if hit any problems in modelling, returns -1 
  print(summary(fit))
  fit = try(eval.parent(call), silent=TRUE)
  if('try-error' %in% class(fit)){
    return("-1") }
  print(modelFormula)
  
  ### output variable names 
  coefs = c()
  coefs.names = c()
  
  fit.vcov = vcov(fit)
  if (length(lags) == 1) { 
    ### regular model
    for (pollu in pollutants.name) {
      mean.reg = fit$coefficients[pollu]
      std.reg = sqrt(fit.vcov[pollu, pollu])
      low.reg = mean.reg - 1.96 * std.reg
      high.reg = mean.reg + 1.96 * std.reg 
      coefs = c(coefs, mean.reg, low.reg, high.reg)
      coefs.names = c(coefs.names, pollu, paste0(pollu, ".low"), paste0(pollu, ".high"))
    }
  } else {  
    ### unconstrainted distributed-lag model
    for (pollu in pollutants.name) {
      var.names = c(paste0(pollu, ".l", lags)) 
      mean.dlm = sum(fit$coefficients[var.names])
      std.dlm = sqrt(sum(fit.vcov[var.names, var.names]))
      low.dlm = mean.dlm - 1.96 * std.dlm
      high.dlm = mean.dlm + 1.96 * std.dlm
      coefs = c(coefs, mean.dlm, low.dlm, high.dlm)
      coefs.names = c(coefs.names, pollu, paste0(pollu, ".low"), paste0(pollu, ".high"))
    }
  }
  
  ### transform so that the output is %change given 10ug/m3 increase
  coefs = trans.coef(coefs)
  names(coefs) = coefs.names
  
  if (pollutants == 1) rm(pm)
  if (pollutants == 2) rm(base, hazard)
  return(coefs)
}

