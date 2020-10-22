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

### read data 
setwd("/Users/mac/Documents/GitHub/covid_wildfire")
df = read.csv("data/moddat2.csv")


### clean data ### 
df$date = strptime(df$date,format='%Y-%m-%d')
df = df[df$cases >= 0,]

summary(df$pm25)
df = df[!is.na(df$pm25),]

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




########################################################################
fips = unique(df_full$FIPS)
length(fips)

pm.coefs = c()
pm.coefs.low = c()
pm.coefs.high = c()

for (i in 1:length(fips)) {
  fipscode = fips[i]
  df = df_full[df_full$FIPS == fipscode,]
  
  ### parameter set up ###
  df.name <- as.name(substitute(df))
  df.tmmx = 6
  df.sph = 6
  df.date = 8
  smooth = "bs" # "ns"
  cause = "cases"
  pollutant = "pm25"
  control = glm.control(epsilon = 1e-10, maxit = 1000)
  
  ### format model ### 
  f <- substitute(~ smooth(date, df.date) + smooth(tmmx, df.tmmx) + smooth(sph, df.sph),
                  list(df.date = df.date, df.tmmx = df.tmmx, df.sph = df.sph, smooth = as.name(smooth)))
  
  # Tack on the pollutant(s) to the end of the formula
  rhs <- as.character(f)
  rhs[-1] <- paste(rhs[-1], paste(pollutant, collapse = "+"), sep = "+")
  
  # Add LHS and coerce to `formula'
  modelFormula = as.formula(paste(cause, paste(rhs, collapse = "")))
  
  # fit quasipoisson model 
  # call <- substitute(glm(modelFormula, family = quasipoisson, data = df, control = control, na.action = na.exclude),
  #                    list(modelFormula = modelFormula, data = df.name, control = substitute(control)))
  # fit <- eval.parent(call)
  
  # fit poisson model 
  call <- substitute(glm(modelFormula, family = poisson, data = df, control = control, na.action = na.exclude),
                     list(modelFormula = modelFormula, data = df.name, control = substitute(control)))
  
  fit = try(eval.parent(call), silent=TRUE)
  if('try-error' %in% class(fit)){
    next
  }
  
  summary.fit = try(confint(fit),silent=TRUE)
  if('try-error' %in% class(summary.fit)){
    next
  }else{
    pm.coefs = c(pm.coefs, fit$coefficients[22])
    # pm.std = summary.fit$coefficients[22,1]
    pm.coef.low = confint(fit)[22, 1]
    pm.coef.high = confint(fit)[22, 2]
    pm.coefs.low = c(pm.coefs.low, pm.coef.low)
    pm.coefs.high = c(pm.coefs.high, pm.coef.high)
  }
  
}

########################################################################
nn = length(pm.coefs.high)
result = data.frame(pm.coefs = pm.coefs, pm.coefs.low = pm.coefs.low, pm.coefs.high = pm.coefs.high)
result = result[order(result$pm.coefs),]
result = melt(result)
result$xx = rep(1:nn, dim(result)[1]/nn)

mean.coef = round(mean(result$value[result$variable == "pm.coefs"]), 5)

ggplot(data = result, aes(x=xx,y=value,color=variable)) + 
  geom_line() + 
  ylim(-.05, .05) + 
  ggtitle(paste0("coefficient of PM2.5 by FIPS (mean ", mean.coef, ")"))

hist(result$value[result$variable == "pm.coefs"], 50)



########################################################################
nn = length(pm.coefs.high)
result = data.frame(pm.coefs = pm.coefs, pm.coefs.low = pm.coefs.low, pm.coefs.high = pm.coefs.high)
result = result[order(result$pm.coefs),]
result = melt(result)
result$xx = rep(1:nn, dim(result)[1]/nn)
result = spread(result, "variable", "value")

ggplot(data = result) + 
  geom_line(aes(x=xx,y=pm.coefs), color='red', label='zz') + 
  geom_line(aes(x=xx,y=pm.coefs.low), alpha=.8, color='grey') + 
  geom_line(aes(x=xx,y=pm.coefs.high), alpha=.8, color='grey') + 
  ylim(-.05, .05) + ylab("pm coefficients") + xlab("FIPS") + 
  ggtitle(paste0("coefficient of PM2.5 with CI95% by FIPS (mean ", mean.coef, ") \n", format(terms(modelFormula))))



# book P70 use poisson family?



### todo page89