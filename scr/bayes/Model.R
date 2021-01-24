model = function(dff, df.date = 8, df.tmmx = 3, df.rmax = 3, lags = 0,
                 cause = "cases", pollutants = 1, mobility = NA, fullDist = FALSE, 
                 smooth = "ns", group="FIPS", offset = "population", 
                 control = glmerControl(tolPwrss = 1e-4)) {
  
  # facebook mobility data
  # FIPS.with.mobility = c(41003, 41005, 41017, 41019, 41029, 41033, 41039, 41043, 41047, 41051,
  #                        41067, 41071, 53005, 53011, 53015, 53033, 53035, 53041, 53053, 53057,
  #                        53061, 53063, 53067, 53073, 53077, 6001, 6007, 6013, 6017, 6019, 6023,
  #                        6025, 6029, 6031, 6037, 6039, 6041, 6047, 6053, 6055, 6057, 6059, 6061,
  #                        6065, 6067, 6071, 6073, 6075, 6077, 6079, 6081, 6083, 6085, 6087, 6089,
  #                        6095, 6097, 6099, 6101, 6107, 6111, 6113)
  # FIPS.with.mobility = as.factor(FIPS.with.mobility)
  # if (!is.na(mobility)) {
  #   dff$cases[dff$FIPS %in% FIPS.with.mobility] = NA
  #   dff$deaths[dff$FIPS %in% FIPS.with.mobility] = NA
  # }
  
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
  
  dff$log.pop <- log(dff[,offset])
  
  ### formula
  f = substitute(~ smooth(tmmx, df.tmmx) + smooth(rmax, df.rmax) + smooth(day_num, df.date),
                 list( df.tmmx = df.tmmx, df.rmax = df.rmax, smooth = as.name(smooth)))
    
  rhs = as.character(f)
  
  ### facebook mobility
  # if (!is.na(mobility)&(mobility==T))
  #   rhs[-1] = paste(rhs[-1], "work", "retail", "residential", "grocery", "park", "transit", sep = "+") 
  ### new mobility
  if (!is.na(mobility)&(mobility==T))
    rhs[-1] = paste(rhs[-1], "relative_change_feb", sep = " + ") 
  
  ### add pollutants
  rhs[-1] = paste(rhs[-1], lag.data1, sep = " + ")
  if (pollutants == 2) rhs[-1] = paste(rhs[-1], lag.data2, sep = " + ")
  
  ### add population offset
  rhs[-1] = paste(rhs[-1], " + offset(log.pop)", sep = "")
  
  ### add FIPS
  if (dim(unique(dff[group]))[1] > 1) 
    rhs[-1] = paste(rhs[-1], " + (1|", group, ")", sep = "")
  
  ### add cause
  modelFormula = as.formula(paste(cause, paste(rhs, collapse = "")))
  
  ### fit quasipoisson model
  if (pollutants == 1) {
    call = substitute(glmer(modelFormula, family = poisson, data = dff, 
                          control = control, na.action = na.exclude),
                      list(modelFormula = modelFormula, control = substitute(control), lag.name1 = pm))
  } else {
    call = substitute(glm(modelFormula, family = poisson, data = dff, 
                          control = control, na.action = na.exclude),
                      list(modelFormula = modelFormula, control = substitute(control), 
                           lag.name1 = base, lag.name2 = hazard))
  }
  
  ### if hit any problems in modelling, returns -1 
  print(call)
  fit = try(eval(call), silent=TRUE)
  if(inherits(fit, 'try-error')) { return(-1) }
  print(summary(fit))
  
  ### output variable names 
  coefs = c()
  coefs.names = c()
  
  fit.vcov = vcov(fit)
  if (length(lags) == 1) { 
    ### regular model
    for (pollu in pollutants.name) {
      mean.reg = coef(fit)$FIPS[1,pollu]
      std.reg = sqrt(fit.vcov[pollu, pollu])
      low.reg = mean.reg - 1.96 * std.reg
      high.reg = mean.reg + 1.96 * std.reg 
      coefs = c(coefs, mean.reg, low.reg, high.reg)
      coefs.names = c(coefs.names, pollu, paste0(pollu, ".low"), paste0(pollu, ".high"))
    }
  } else {
    if (!fullDist) {
      ### unconstrainted distributed-lag model
      for (pollu in pollutants.name) {
        var.names = c(paste0(pollu, ".l", lags)) 
        mean.dlm = sum(coef(fit)$FIPS[1,var.names])
        std.dlm = sqrt(sum(fit.vcov[var.names, var.names]))
        low.dlm = mean.dlm - 1.96 * std.dlm
        high.dlm = mean.dlm + 1.96 * std.dlm
        coefs = c(coefs, mean.dlm, low.dlm, high.dlm)
        coefs.names = c(coefs.names, pollu, paste0(pollu, ".low"), paste0(pollu, ".high"))
      }
    } else {
      ### unconstrainted distributed-lag model return all lags' output
      for (pollu in pollutants.name) {
        var.names = c(paste0(pollu, ".l", lags)) 
        mean.dlm = coef(fit)$FIPS[1,var.names]
        std.dlm = sqrt(diag(fit.vcov[var.names, var.names]))
        low.dlm = mean.dlm - 1.96 * std.dlm
        high.dlm = mean.dlm + 1.96 * std.dlm
        coefs = c(coefs, mean.dlm, low.dlm, high.dlm)
        coefs.names = c(coefs.names, var.names, paste0(var.names, ".low"), paste0(var.names, ".high"))
      }
    }
  }
  
  ### transform so that the output is %change given 10ug/m3 increase
  coefs = trans.coef(coefs)
  names(coefs) = coefs.names
  
  if (pollutants == 1) rm(pm)
  if (pollutants == 2) rm(base, hazard)
  return(coefs)
  
}

