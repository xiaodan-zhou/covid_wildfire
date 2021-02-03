pm_model <- function(dff, df.date = 6, df.tmmx = 2, df.rmax = 2, lags = 14,
                     cause = "cases", smooth = "ns", group = "FIPS", offset = "population", 
                     control = glmerControl(tolPwrss = 1e-4)) {
  
  pollutants.name = "pm"
  pm = as.matrix(create.lag.value(dff=dff, value="pm25", group=group, lags=lags))
  lag.data = "pm" # as.character(as.name(substitute(base))) # deparse(substitute(base))
  
  dff$log.pop <- log(dff[,offset])
  
  ### formula
  f = substitute(~ smooth(tmmx, df.tmmx) + smooth(rmax, df.rmax) + smooth(day_num, df.date),
                 list( df.tmmx = df.tmmx, df.rmax = df.rmax, smooth = as.name(smooth)))
    
  rhs = as.character(f)
  
  ### add pollutants
  rhs[-1] = paste(rhs[-1], lag.data, sep = " + ")
  
  ### add population offset
  rhs[-1] = paste(rhs[-1], " + offset(log.pop)", sep = "")
  
  ### add FIPS
  if (dim(unique(dff[group]))[1] > 1) 
    rhs[-1] = paste(rhs[-1], " + (1|", group, ")", sep = "")
  
  ### add cause
  modelFormula = as.formula(paste(cause, paste(rhs, collapse = "")))
  
  call = substitute(glmer(modelFormula, family = poisson, data = dff, 
                          control = control, na.action = na.exclude),
                    list(modelFormula = modelFormula, control = substitute(control), lag.name1 = pm))
  
  ### if hit any problems in modelling, returns -1 
  print(call)
  fit = try(eval(call), silent = TRUE)
  if(inherits(fit, 'try-error')) { return(-1) }
  print(summary(fit))
  
  ### output variable names 
  
  var.names = c(paste0("pm.l", lags)) 
  mean.dlm = sum(coef(fit)$FIPS[1,var.names])
  lincomb = rep(1, length(lags))
  std.dlm = sqrt(t(lincomb) %*% vcov(fit)[var.names, var.names] %*% lincomb)
  low.dlm = mean.dlm - 1.96 * std.dlm
  high.dlm = mean.dlm + 1.96 * std.dlm
  coefs = c(mean.dlm, low.dlm, high.dlm)
  coefs.names = c( "pm", "pm.low", "pm.high")
  
  ### transform so that the output is %change given 10ug/m3 increase
  coefs = trans.coef(coefs)
  names(coefs) = coefs.names
  
  return(coefs)
  
}

