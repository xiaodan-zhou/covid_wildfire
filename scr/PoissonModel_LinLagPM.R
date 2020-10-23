########################################################################
### project: covid - wildfire 
### description: poisson model given different number of PM lags
### cases ~ bs(date, 8) + bs(tmmx, 6) + bs(sph, 6) + dayofweek + lag_pollutant
########################################################################

library(tsModel)
library(splines)
library(ggplot2)
library(stats)
library(reshape)
library(tidyr)
## GAM model 
library(mgcv)

### read data 
setwd("/Users/mac/Documents/GitHub/covid_wildfire")
df = read.csv("data/moddat2.csv")

### clean date
df$date = strptime(df$date,format='%Y-%m-%d')

### make dayofweek categorical
df$dayofweek = as.factor(df$dayofweek)

### replace negative number of daily cases with zero 
print(length(df$cases[df$cases < 0]))
df$cases[df$cases < 0] = 0
# hist(df$cases)

### replace negative number of daily deaths with zero 
print(length(df$deaths[df$deaths < 0]))
df$deaths[df$deaths < 0] = 0 

### remove data without pm record 
# summary(df$pm25)
# df = df[!is.na(df$pm25),]

### rename dataset 
df_full = df
rm(df)


################################ Poisson Model by FIPS ###############################
### FIPS list 
fips = unique(df_full$FIPS)
length(fips)

### parameter set up 
df.name <- as.name(substitute(df))
df.tmmx = 6
df.sph = 6
df.date = 8
smooth = "bs" # "ns"
cause = "cases"
pollutant = "pm25"
n_lag = 14
control = glm.control(epsilon = 1e-10, maxit = 1000)


pm.coefs = c()
pm.coefs.low = c()
pm.coefs.high = c()
lag.index = c()
fips.index = c()

lag_names = c()
for (ilag in 1:(n_lag+1)) 
  lag_names = c(lag_names, paste0("lag_pollutantv1.l", ilag))


for (ifips in 1:length(fips)) {
  fipscode = fips[ifips]
  df = df_full[df_full$FIPS == fipscode,]
  
  ### format model ### 
  f = substitute(~ smooth(date_shifted_100case, df.date) + smooth(tmmx, df.tmmx) + smooth(sph, df.sph) + dayofweek,
                  list(df.date = df.date, df.tmmx = df.tmmx, df.sph = df.sph, 
                       smooth = as.name(smooth)))
  
  # add pollutant element to the model
  rhs = as.character(f)
  lag_pollutant = crossbasis(df[pollutant], lag=c(0, n_lag), argvar=list(type="lin", cen=FALSE), arglag=list(type="integer"))
  rhs[-1] = paste(rhs[-1], "lag_pollutant", sep = "+")
  
  # Add LHS and coerce to `formula'
  modelFormula = as.formula(paste(cause, paste(rhs, collapse = "")))
  
  # fit poisson model, or quasipoisson?  
  call = substitute(glm(modelFormula, family = poisson, data = df, control = control, na.action = na.exclude),
                     list(modelFormula = modelFormula, data = df.name, lag_pollutant = lag_pollutant, 
                          control = substitute(control))) 

  fit = try(eval.parent(call), silent=TRUE)
  if('try-error' %in% class(fit)){
    next }
  
  summary.fit = try(confint(fit),silent=TRUE)
  if('try-error' %in% class(summary.fit)){
    next }
  
  else{
    for (ilag in 1:(n_lag+1)) {
      ci.value = confint(fit)[lag_names[ilag],]
      pm.coefs = c(pm.coefs, fit$coefficients[lag_names[ilag]])
      pm.coefs.low = c(pm.coefs.low, ci.value[1])
      pm.coefs.high = c(pm.coefs.high, ci.value[2])
      lag.index = c(lag.index, ilag)
      fips.index = c(fips.index, fipscode)} 
     } }


### save results  
nn = length(pm.coefs.high)/(n_lag+1)
result = data.frame(pm.coefs = pm.coefs, pm.coefs.low = pm.coefs.low, pm.coefs.high = pm.coefs.high, 
                    lag.index = lag.index, fips = fips.index)
# write.csv(result, paste0("/Users/mac/Documents/GitHub/covid_wildfire/poisson_shift_lag", n_lag, ".csv"))
# result = read.csv("/Users/mac/Documents/GitHub/covid_wildfire/PoissonModel_LinLagPM/poisson_shifted_lag7.csv")


### visualize results 
pm.coef.means = result %>% group_by(lag.index) %>% summarise(means=mean(pm.coefs, na.rm=TRUE))
write.csv(pm.coef.means, paste0("/Users/mac/Documents/GitHub/covid_wildfire/poisson_shifted_lag", n_lag, "_means.csv"))
jitter <- position_jitter(width = 0.2, height = 0)
ggplot() + 
  # geom_line(data=result, aes(x=lag.index,y=pm.coefs,col=as.factor(fips)), alpha=.3) +
  theme(legend.position="none") +
  # geom_point(data = result, aes(x=lag.index,y=pm.coefs.low), alpha=.3, color='green', position = jitter) +
  # geom_point(aes(x=lag.index,y=pm.coefs.high), alpha=.3, color='blue', position = jitter) +
  geom_point(data=result, aes(x=lag.index,y=pm.coefs), color='red', alpha=.3, position = jitter) +
  geom_line(aes(x=1:(n_lag+1),y=pm.coef.means$means)) + 
  geom_line(aes(x=1:(n_lag+1),y=rep(0, n_lag+1)), alpha=.5) + 
  ylim(-.2, .2) + 
  xlab("PM2.5 lag") + ylab("PM2.5 coefficients") + 
  ggtitle(paste("coefficient of PM2.5", paste0(format(terms(modelFormula))[1], format(terms(modelFormula))[2])))


