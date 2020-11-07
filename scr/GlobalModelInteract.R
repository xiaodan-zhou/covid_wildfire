# ATTENTION!!! 
# this model wors for one lags only! 
# https://www.econometrics-with-r.org/8-3-interactions-between-independent-variables.html

not working yet

global.model = function(dff, smooth="ns", df.date=8, df.tmmx=3, df.rmax=3, lags=0, 
                        cause="cases", pollutant="pm25", group="FIPS", 
                        control=glm.control(epsilon = 1e-10, maxit = 10000)) {
  
  if (length(lags) > 1) stop("Interaction model only works for 1 lags")
  # dff$fireday = as.factor(dff$fireday)
  
  ### create lag values
  lag.out = add.lag(dff=dff, value=pollutant, group=group, lags=lags)
  lag.data = as.matrix(lag.out[[1]])
  lag.data.name = "lag.data" # if run interactively, lag.data.name = as.name(substitute(lag.data)) why???
  
    ### create lag names
  lag.names = lag.out[[2]]
  if (length(lag.names) == 1) {
    lag.names = c("lag.data")
  } else {
    for (i in 1:length(lag.names)) {
      lag.names[i] = paste0(lag.data.name, lag.names[i])
    } }

  ### initialize model
  df.name = as.name(substitute(dff))

  ### local model doesn't have population
  if (dim(unique(dff[group]))[1] == 1) {
    print("hell !!!!!!")
    f = substitute(~ smooth(date, df.date) + smooth(tmmx, df.tmmx) + 
                     smooth(rmax, df.rmax) + dayofweek,
                   list(df.date = df.date, df.tmmx = df.tmmx, 
                        df.rmax = df.rmax, smooth = as.name(smooth)))
  } else {
    print("im correct !!!!!!")
    f = substitute(~ smooth(date_num, df.date) + smooth(tmmx, df.tmmx) + 
                     smooth(rmax, df.rmax) + dayofweek + log(population) + FIPS,
                   list(df.date = df.date, df.tmmx = df.tmmx, 
                        df.rmax = df.rmax, smooth = as.name(smooth)))
  }

  rhs = as.character(f)
  print(rhs)
  
  ### create the formula for visualization purpose
  # modelFormula.vis = paste(cause, rhs[1], paste(rhs[-1], paste(lag.names, collapse = "+"),  sep = "+"))
  # modelFormula.vis = paste0(modelFormula.vis, ":fireday")
  
  ### add pollutant elements to the model
  rhs[-1] = paste(rhs[-1], lag.data.name, sep = "+")
  rhs[-1] = paste(rhs[-1], paste0(lag.data.name, ":fireday"), sep = "+")
  
  ### add cause to the model
  modelFormula = as.formula(paste(cause, paste(rhs, collapse = "")))
  print(modelFormula)
  
  ### fit quasipoisson model
  call = substitute(glm(modelFormula, family = quasipoisson, data = dff, 
                        control = control, na.action = na.exclude),
                    list(modelFormula = modelFormula, data = df.name, 
                         lag.name = lag.data, control = substitute(control))) 

  ### if hit any problems in modelling, returns -1 
  fit = try(eval.parent(call), silent=TRUE)
  if('try-error' %in% class(fit)){
    return(-1) }
  
  print(summary(fit))
  
  # fit.CI = list()
  # ### original 
  # # fit.CI = try(confint(fit, silent=TRUE)
  # # if('try-error' %in% class(fit.CI)){
  # #   return(-1) }
  # ### new method not tested for global model 
  # for (iname in lag.names) {
  #   temp = confint(fit, iname)
  #   fit.CI[[iname]][1] = temp[1]
  #   fit.CI[[iname]][2] = temp[2]
  # }
  # ### new method 2 not tested for global model 
  # # for (iname in lag.names) {
  # #   temp = confint.default(fit, iname)
  # #   fit.CI[[iname]][1] = temp[1]
  # #   fit.CI[[iname]][2] = temp[2]
  # # }
  # 
  # 
  # ### save and returns the results
  # var.out = c("coef", "std", "ci.low", "ci.high", "ilag", "name", 
  #             "smooth", "df.date", "df.tmmx", "df.rmax")
  # 
  # result = data.frame(matrix(ncol = length(var.out), nrow = length(lags)))
  # colnames(result) = var.out
  # 
  # for (ilag in 1:length(lag.names)) {
  #   var.name = lag.names[ilag]
  #   ci.value = fit.CI[[var.name]]
  #   result[ilag,] = c(fit$coefficients[var.name],
  #                     summary(fit)$coef[var.name,"Std. Error"],
  #                     ci.value[1],
  #                     ci.value[2],
  #                     lags[ilag],
  #                     lag.names[ilag],
  #                     smooth, 
  #                     df.date, 
  #                     df.tmmx, 
  #                     df.rmax)
  # }
  # return(list("fit" = fit, "fit.CI" = fit.CI, "modelFormula.vis" = modelFormula.vis, "result" = result))
  # return(list(fit, fit.CI, modelFormula.vis, result))
}
