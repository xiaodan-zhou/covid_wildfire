model = function(dff, df.date=8, df.tmmx=3, df.rmax=3, lags=0,
                 cause="cases", pollutants = 1, mobility = NA,
                 smooth="ns", group="FIPS", control=glm.control(epsilon = 1e-10, maxit = 10000)) {
  # TODO  check and remove this 
  if (!is.na(mobility)) stop("mobility is not checked yet!!! mobility is not in dataset yet. ")
  
  if (pollutants == 1) {
    ## get lag values for pm25
    pm = as.matrix(create.lag.value(dff=dff, value="pm25", group=group, lags=lags))
    lag.data1 = "pm" # as.character(as.name(substitute(base))) # deparse(substitute(base))  
    lag.var1 = paste0(lag.data1, ".l", 1:length(lags))
    lag.var2 = c()
    
  } else {
    ### get lag values for pollutant1
    base = as.matrix(create.lag.value(dff=dff, value="pmbase", group=group, lags=lags))
    lag.data1 = "base"
    lag.var1 = paste0(lag.data1, ".l", 1:length(lags))
    
    ### get lag values for pollutant2 
    hazard = as.matrix(create.lag.value(dff=dff, value="pmhazard", group=group, lags=lags))
    lag.data2 = "hazard"
    lag.var2 = paste0(lag.data2, ".l", 1:length(lags))
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
  if (pollutants == 2)
    rhs[-1] = paste(rhs[-1], lag.data2, sep = "+")

  ### add cause
  modelFormula = as.formula(paste(cause, paste(rhs, collapse = "")))
  modelFormula
  
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
  fit = try(eval.parent(call), silent=TRUE)
  if('try-error' %in% class(fit)){
    print(fit)
    return("-1") }
  print(summary(fit))
  
  ### collect coefficients to return 
  coefs = c()
  coefs.names = c()
  for (var.name in c(lag.var1, lag.var2)) {
    temp = confint(fit, var.name)
    coefs = c(coefs, fit$coefficients[var.name], temp[1], temp[2])
    coefs.names = c(coefs.names, var.name, paste0(var.name, ".low"), paste0(var.name, ".high"))
  }
  names(coefs) = coefs.names

  return(coefs)
}



# default.model = function(dff, df.date=6, df.tmmx=2, df.rmax=2, lags=0, 
#                          cause="cases", pollutant = "pm25", mobility = NA, 
#                          smooth="ns", group="FIPS", control=glm.control(epsilon = 1e-10, maxit = 10000)) {
#   ## TODO 
#   # if (!is.na(mobility)) stop("mobility is not checked yet!!! ")
#   
#   ### get values for pollutant
#   lag.out = create.lag.value(dff=dff, value=pollutant, group=group, lags=lags)
#   lag.data = as.matrix(lag.out[[1]])
#   lag.data.name = "lag.data" ## TODO change name to lag.pm 
#   
#   ### create lag names
#   lag.var = lag.out[[2]]
#   if (length(lag.var) == 1) {
#     lag.var = c("lag.data")
#   } else {
#     for (i in 1:length(lag.var)) {
#       lag.var[i] = paste0(lag.data.name, lag.var[i])
#     } }
#   
# f = substitute(~ smooth(date_num, df.date) + smooth(tmmx, df.tmmx) + 
#                  smooth(rmax, df.rmax) + dayofweek,
#                list(df.date = df.date, df.tmmx = df.tmmx, 
#                     df.rmax = df.rmax, smooth = as.name(smooth)))
# 
# 
#   rhs = as.character(f)
#   
#   ### create the formula for visualization purpose
#   # modelFormula.vis = paste(cause, rhs[1], paste(rhs[-1], paste(lag.var, collapse = "+"),  sep = "+"))
#   # print(modelFormula.vis)
#   
#   ### add mobility to the model 
#   if (!is.na(mobility)) 
#     rhs[-1] = paste(rhs[-1], mobility, sep = "+")
#   
#   ### TODO add FIPS to the model
#   if (dim(unique(dff[group]))[1] > 1) rhs[-1] = paste(rhs[-1], group, sep = "+")
#   
#   ### add pollutant to the model
#   rhs[-1] = paste(rhs[-1], lag.data.name, sep = "+")
#   
#   ### add cause to the model
#   modelFormula = as.formula(paste(cause, paste(rhs, collapse = "")))
#   
#   ### fit quasipoisson model
#   call = substitute(glm(modelFormula, family = quasipoisson, data = dff, 
#                         control = control, na.action = na.exclude),
#                     list(modelFormula = modelFormula, # data = df.name, 
#                          control = substitute(control), lag.name = lag.data)) 
# 
#   ### if hit any problems in modelling, returns -1 
#   fit = try(eval.parent(call), silent=TRUE)
#   if('try-error' %in% class(fit)){
#     return("-1") }
#   
#   fit.CI = list()
#   ### new method not tested for global model 
#   for (iname in lag.var) {
#     temp = confint(fit, iname) # confint.default(fit, iname)?
#     fit.CI[[iname]][1] = temp[1]
#     fit.CI[[iname]][2] = temp[2]
#   }
# 
#   ### save and returns the results
#   var.out = c("coef", "std", "ci.low", "ci.high", "ilag", "name", 
#               "smooth", "df.date", "df.tmmx", "df.rmax")
#   
#   result = data.frame(matrix(ncol = length(var.out), nrow = length(lags)))
#   colnames(result) = var.out
#   
#   for (ilag in 1:length(lag.var)) {
#     var.name = lag.var[ilag]
#     ci.value = fit.CI[[var.name]]
#     result[ilag,] = c(fit$coefficients[var.name],
#                       summary(fit)$coef[var.name,"Std. Error"],
#                       ci.value[1],
#                       ci.value[2],
#                       lags[ilag],
#                       lag.var[ilag],
#                       smooth, 
#                       df.date, 
#                       df.tmmx, 
#                       df.rmax)
#   }
#   return(list("fit" = fit, "fit.CI" = fit.CI, "modelFormula.vis" = modelFormula.vis, "result" = result))
#   # return(list(fit, fit.CI, modelFormula.vis, result))
# }
# 
# 
# 
