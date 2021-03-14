# Effect of environmental pollutants PM-2.5, carbon monoxide, and ozone on the incidence and mortality of SARS-COV-2 infection 
# in ten wildfire affected counties in California

# (1) SCU Lightning Complex: 
#   Santa Cara, 6085
# Alameda, 6001
# Contra Costa, 6013
# San Joaquin, 6077
# Merced, and 6047
# Stanislaus Counties; 6099
# (2) Creek Complex: Fresno County; 6019
# (3) LNU Complex: Napa County; 6055
# (4) August Complex: Glenn County; 6021
# (5) North Complex: Butte County 6007
county_filter = c(6085, 6001, 6013, 6077, 6047, 6099, 6019, 6055, 6021, 6007) 

library(rstudioapi)

project.dir = dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(project.dir)
setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
dff = load.data()

pollutants = 1
cause = "cases"
lags = 0:14
mobility = T
df.date = 6
df.tmmx = 2
df.rmax = 2
group = "FIPS"
smooth = "ns"
control=glm.control(epsilon = 1e-10, maxit = 10000) 
fullDist = FALSE

## get lag values for pollutants
pollutants.name = "pm"
pm = as.matrix(create.lag.value(dff=dff, value="pm25", group=group, lags=lags))
lag.data1 = "pm"

### formula
f = substitute(~ smooth(date_num, df.date) + smooth(tmmx, df.tmmx) + 
                 smooth(rmax, df.rmax) + dayofweek,
               list(df.date = df.date, df.tmmx = df.tmmx, 
                    df.rmax = df.rmax, smooth = as.name(smooth)))

rhs = as.character(f)

### new mobility
if (!is.na(mobility)&(mobility==T))
  rhs[-1] = paste(rhs[-1], "relative_change_feb", sep = "+") 

### add FIPS
if (dim(unique(dff[group]))[1] > 1) 
  rhs[-1] = paste(rhs[-1], group, sep = "+")

### add pollutants
rhs[-1] = paste(rhs[-1], lag.data1, sep = "+")
if (pollutants == 2) rhs[-1] = paste(rhs[-1], lag.data2, sep = "+")

### add cause
modelFormula = as.formula(paste(cause, paste(rhs, collapse = "")))

### fit quasipoisson model
call = substitute(glm(modelFormula, family = quasipoisson, data = dff, 
                      control = control, na.action = na.exclude),
                  list(modelFormula = modelFormula, control = substitute(control), 
                       lag.name1 = pm))

### if hit any problems in modelling, returns -1 
print(call)
fit = try(eval.parent(call), silent=TRUE)
print(summary(fit))

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
  if (!fullDist) {
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
  } else {
    ### unconstrainted distributed-lag model return all lags' output
    for (pollu in pollutants.name) {
      var.names = c(paste0(pollu, ".l", lags)) 
      mean.dlm = fit$coefficients[var.names]
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

