########################################################################
### https://github.com/rdpeng/tsmodel/blob/master/R/NMMAPS-model.R
### book Stat. Mehtods for Envi. Epid. with R 
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
hist(df$cases)

### replace negative number of daily deaths with zero 
print(length(df$deaths[df$deaths < 0]))
df$deaths[df$deaths < 0] = 0 

### remove data without pm record 
summary(df$pm25)
df = df[!is.na(df$pm25),]

### rename dataset 
df_full = df
rm(df)

########################################################################
# ### select a location
# df = df[df$FIPS == 6013,]
# 
# ### explore ### book P52 ## WHY not working?????? learn more about filter function in stats
# pm.monthly <- filter(df$pm25, rep(1/30, 30))
# pm.left <- df$pm25 - pm.monthly
# pm.weekly <- filter(pm.left, rep(1/7, 7))
# pm.r <- pm.left - pm.weekly
# fit <- lm(df$cases ~ pm.monthly + pm.weekly + pm.r)
# summary(fit)
# 
# pm.decomp = data.frame(as.numeric(pm.monthly), as.numeric(pm.weekly), as.numeric(pm.r), df$date)
# colnames(pm.decomp) = c("pm.monthly", "pm.weekly", "pm.r", "date")
# pm.decomp = melt(data = pm.decomp, id.vars = "date", measure.vars = c("pm.monthly", "pm.weekly", "pm.r"))
# ggplot(data = pm.decomp, aes(x=date,y=value,color=variable)) + geom_line()
# 
# # not working tsdecomp(as.numeric(df$pm25), c(1, 2, 7, 14, 30))




################################ Poisson Model by FIPS ###############################
### run the model
fips = unique(df_full$FIPS)
length(fips)

### parameter set up ###
df.name <- as.name(substitute(df))
df.tmmx = 6
df.sph = 6
df.date = 8
smooth = "bs" # "ns"
cause = "cases"
pollutant = "pm25"
lag_it = TRUE
n_lag = 7
control = glm.control(epsilon = 1e-10, maxit = 1000)

if (lag_it) {
  pm.coefs = c()
  pm.coefs.low = c()
  pm.coefs.high = c()
  fips.used = c()
  lag.index = c()
} else {
  pm.coefs = c()
  pm.coefs.low = c()
  pm.coefs.high = c()
  fips.used = c()

}



for (ifips in 1:length(fips)) {
  fipscode = fips[ifips]
  df = df_full[df_full$FIPS == fipscode,]
  
  ### format model ### 
  f <- substitute(~ smooth(date, df.date) + smooth(tmmx, df.tmmx) + smooth(sph, df.sph) + dayofweek,
                  list(df.date = df.date, df.tmmx = df.tmmx, df.sph = df.sph, smooth = as.name(smooth)))
  
  # add pollutant element to the model
  rhs <- as.character(f)
  
  if (lag_it) {
    lag_pollutant <- crossbasis(df[pollutant], lag=c(0, n_lag), argvar=list(type="lin", cen=FALSE), arglag=list(type="integer"))
    rhs[-1] <- paste(rhs[-1], "lag_pollutant", sep = "+")
    
    # Add LHS and coerce to `formula'
    modelFormula = as.formula(paste(cause, paste(rhs, collapse = "")))
    
    # fit poisson model, or quasipoisson?  
    call <- substitute(glm(modelFormula, family = poisson, data = df, control = control, na.action = na.exclude),
                       list(modelFormula = modelFormula, data = df.name, lag_pollutant = lag_pollutant, 
                            control = substitute(control))) }
  
  else {
    rhs[-1] <- paste(rhs[-1], paste(pollutant, collapse = "+"), sep = "+")

    modelFormula = as.formula(paste(cause, paste(rhs, collapse = "")))
    
    call <- substitute(glm(modelFormula, family = poisson, data = df, control = control, na.action = na.exclude),
                       list(modelFormula = modelFormula, data = df.name, 
                            control = substitute(control))) }
  
  fit = try(eval.parent(call), silent=TRUE)
  if('try-error' %in% class(fit)){
    next }
  
  summary.fit = try(confint(fit),silent=TRUE)
  if('try-error' %in% class(summary.fit)){
    next }
  
  else{
    if (lag_it) {
      lag_names = c()
      for (ilag in 1:(n_lag+1)) lag_names = c(lag_names, paste0("lag_pollutantv1.l", ilag))
      
      for (ilag in 1:(n_lag+1)) {
        ci.value = confint(fit)[lag_names[ilag],]
        pm.coefs = c(pm.coefs, fit$coefficients[lag_names[ilag]])
        pm.coefs.low = c(pm.coefs.low, ci.value[1])
        pm.coefs.high = c(pm.coefs.high, ci.value[2])
        lag.index = c(lag.index, ilag) } }
    
    else {
      pm.coefs = c(pm.coefs, fit$coefficients[22])
      # pm.std = summary.fit$coefficients[22,1]
      pm.coef.low = confint(fit)[22, 1]
      pm.coef.high = confint(fit)[22, 2]
      pm.coefs.low = c(pm.coefs.low, pm.coef.low)
      pm.coefs.high = c(pm.coefs.high, pm.coef.high) } } }

#### visualization 
if (lag_it) {
  nn = length(pm.coefs.high)/(n_lag+1)
  result = data.frame(pm.coefs = pm.coefs, pm.coefs.low = pm.coefs.low, 
                      pm.coefs.high = pm.coefs.high, lag.index = lag.index)
  result$loc = as.factor(rep(1:nn, each=n_lag+1))
  # result = result[order(c(lag.index, result$pm.coefs)),]
  write.csv(result,"/Users/mac/Documents/GitHub/covid_wildfire/poisson_lag7.csv")
  # result = melt(result, 'lag.index')
  # result$xx = rep(1:(n_lag+1), dim(result)[1]/n_lag)
  # ## 
  # result = spread(result, "variable", "value")
  pm.coef.means = result %>% group_by(lag.index) %>% summarise(mean(pm.coefs, na.rm=TRUE))
  pm.coef.means = as.matrix(pm.coef.means)[,2]
  jitter <- position_jitter(width = 0.2, height = 0)
  ggplot() + 
    geom_line(data=result, aes(x=lag.index,y=pm.coefs,col=loc), alpha=.3) + theme(legend.position="none") +
    # geom_point(data = result, aes(x=lag.index,y=pm.coefs.low), alpha=.3, color='green', position = jitter) +
    # geom_point(aes(x=lag.index,y=pm.coefs.high), alpha=.3, color='blue', position = jitter) +
    geom_point(aes(x=lag.index,y=pm.coefs), color='red', label='center', alpha=.3, position = jitter) +
    geom_line(aes(x=1:(n_lag+1),y=pm.coef.means)) + 
    geom_line(aes(x=1:(n_lag+1),y=rep(0, n_lag+1)), alpha=.5) # + 
    ylim(-.1, .1) # + 
    # ylab("pm coefficients") + xlab("FIPS") + 
    # ggtitle(paste0("coefficient of PM2.5 with 95% CI by FIPS (mean ", mean.coef, ") \n", format(terms(modelFormula)))) 
  }





} else {
  ### visualize the result
  nn = length(pm.coefs.high)
  result = data.frame(pm.coefs = pm.coefs, pm.coefs.low = pm.coefs.low, pm.coefs.high = pm.coefs.high)
  result = result[order(result$pm.coefs),]
  result = melt(result)
  result$xx = rep(1:nn, dim(result)[1]/nn)
  
  mean.coef = round(mean(result$value[result$variable == "pm.coefs"]), 5)
  print(mean.coef)
  
  result = spread(result, "variable", "value")
  
  ggplot(data = result) + 
    geom_line(aes(x=xx,y=pm.coefs), color='red', label='zz') + 
    geom_line(aes(x=xx,y=pm.coefs.low), alpha=.8, color='grey') + 
    geom_line(aes(x=xx,y=pm.coefs.high), alpha=.8, color='grey') + 
    # ylim(-.05, .05) + 
    ylab("pm coefficients") + xlab("FIPS") + 
    ggtitle(paste0("coefficient of PM2.5 with 95% CI by FIPS (mean ", mean.coef, ") \n", format(terms(modelFormula)))) }





### todo page89