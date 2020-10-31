# Global Model 
# add the log of population size as the offset 
# add the option of having an over dispersed Poisson (todo)
# add a random intercept for the county 
# get point estimate and variances of the beta global 
# do separately for lags 0, 1, 2 
# plot the point estimates and the 95% CI for beta global by lags 0, 1, 2 


# testing set up 
# setwd("/Users/mac/Documents/GitHub/covid_wildfire")
# source("scr/Utilities.R")
# df = load.data()
# df.date=4
# df.tmmx=1
# df.sph=1
# lags=0


library(stats)
library(splines)
        
global.model = function(df, smooth = "ns", df.date=8, df.tmmx=3, df.rmax=3, lags=0) {
  ### parameter set up 
  cause = "cases"
  pollutant = "pm25"
  group = "FIPS"
  control = glm.control(epsilon = 1e-10, maxit = 1000)
  
  ### create lags of pollutant
  lag.out = add.lag(df=df, value=pollutant, group=group, lags=lags)
  lag.data = as.matrix(lag.out[[1]])
  lag.data.name = "lag.data" # import function
  # if run interactively, lag.data.name = as.name(substitute(lag.data)) why???
  
  
  ### create lag names
  lag.names = lag.out[[2]]
  if (length(lag.names) == 1) {
    lag.names = c("lag.data")
  } else {
    for (i in 1:length(lag.names)) {
      lag.names[i] = paste0(lag.data.name, lag.names[i])
    } }

  ### initialize model
  df.name = as.name(substitute(df))

  f = substitute(~ smooth(date, df.date) + smooth(tmmx, df.tmmx) + 
                   smooth(rmax, df.rmax) + dayofweek + log(population),
                 list(df.date = df.date, df.tmmx = df.tmmx, 
                      df.rmax = df.rmax, smooth = as.name(smooth)))
  rhs = as.character(f)
  
  ### create the formula for visualization purpose
  modelFormula.vis = paste(cause, rhs[1], paste(rhs[-1], paste(lag.names, collapse = "+"),  sep = "+"))
  print(modelFormula.vis)
  
  ### add pollutant elements to the model
  rhs[-1] = paste(rhs[-1], lag.data.name, sep = "+")
   
  ### add cause to the model
  modelFormula = as.formula(paste(cause, paste(rhs, collapse = "")))
  
  ### fit quasipoisson model
  call = substitute(glm(modelFormula, family = quasipoisson, data = df, 
                        control = control, na.action = na.exclude),
                    list(modelFormula = modelFormula, data = df.name, 
                         lag.name = lag.data, control = substitute(control))) 
  
  ### if hit any problems in modelling, returns -1 
  fit = try(eval.parent(call), silent=TRUE)
  if('try-error' %in% class(fit)){
    return(-1) }
  
  fit.CI = try(confint(fit),silent=TRUE)
  if('try-error' %in% class(fit.CI)){
    return(-1) }
  
  ### save and returns the results
  var.out = c("coef", "ci.low", "ci.high", "ilag", "name", "smooth", "df.date", "df.tmmx", "df.rmax")
  result = data.frame(matrix(ncol = length(var.out), nrow = length(lags)))
  colnames(result) = var.out
  
  for (ilag in 1:length(lag.names)) {
    var.name = lag.names[ilag]
    ci.value = fit.CI[var.name,]
    result[ilag,] = c(fit$coefficients[var.name],
                      ci.value[1],
                      ci.value[2],
                      lags[ilag],
                      lag.names[ilag],
                      smooth, 
                      df.date, 
                      df.tmmx, 
                      df.rmax)
  }
  return(list(fit, fit.CI, modelFormula.vis, result))
}
