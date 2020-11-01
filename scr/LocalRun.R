# County-Level Models 
# fit a separate model without the random intercept 
# pool the county-specific coefficients using meta package in R 
# Plot the beta_c and 95% CI, 3 separate panels for lags0.1.2
# 3D compare the beta pool with the global model 
# Sensitivity analysis by varying the df, using the alpha parameter

library(ggplot2)
library(gridExtra)
library(meta)

setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
source("scr/GlobalModel.R")
df_full = load.data.mc2()

### FIPS list 
fips = unique(df_full$FIPS)
length(fips)

### set up
lags.to.run = 0:3

##debug 
# ilag = 0
# ifips = 1
# df = df[df$FIPS == fips[ifips], ]
# lags = ilag
# df.date=4
# df.tmmx=1
# df.rmax=1
# global.model(df=df.sub, lags=ilag)


result.rbind = c()
for (lags in lags.to.run) {
  for (ifips in 1:length(fips)) {
    dff = df_full[df_full$FIPS == fips[ifips], ]
    gm = global.model(dff=dff, lags=lags, smooth = "ns", 
                      df.date=df.date, df.tmmx=df.tmmx, df.rmax=df.rmax)
    if ( length(gm) > 1) {
      fit = gm[[1]]
      fit.CI = gm[[2]]
      modelFormula.vis = gm[[3]]
      result = gm[[4]]
      if (is.null(result.rbind)) {
        result.rbind = result
      } else {
        result.rbind=rbind(result.rbind, result) }
    }
  }
}

result.rbind$coef = as.numeric(result.rbind$coef)
result.rbind$std = as.numeric(result.rbind$std)
result.rbind$ci.low = as.numeric(result.rbind$ci.low)
result.rbind$ci.high = as.numeric(result.rbind$ci.high)
result.rbind$ilag = as.numeric(result.rbind$ilag)
# write.csv(result.rbind, "LocalModel/lag0to3.csv")
# result.rbind = read.csv("LocalModel/lag0to3.csv")


# weighted by inversed variance
data.frame(result.rbind %>% group_by(ilag) %>% 
             summarise(weighted.coef = round(weighted.mean(coef, w=1./(std)^2), 6)))

# weighted using metagen
for (i in lags.to.run) {
  print(summary(metagen(TE=result.rbind$coef[result.rbind$ilag == i], 
                        seTE=result.rbind$std[result.rbind$ilag == i])))
  }

#################### visualize #####################
##################### save #####################
file.name = paste0("LocalModel/lag", paste0(lags.to.run, collapse=""), "mc2.pdf")
pdf(file.name, width = 12, height = 5)

lags = 0
data.vis = result.rbind[result.rbind$ilag == lags,]
p0 = ggplot(data=data.vis, aes(x=(1:dim(data.vis)[1]))) +
  geom_errorbar(width=.1, aes(ymin=ci.low, ymax=ci.high), colour="red") +
  geom_point(aes(y=coef)) +
  geom_line(aes(y=coef)) +
  xlab("FIPS") + ylab("PM2.5 coefficients") +
  ylim(-0.05, 0.05) +
  ggtitle(paste("lag", lags))
p0

lags = 1
data.vis = result.rbind[result.rbind$ilag == lags,]
p1 = ggplot(data=data.vis, aes(x=(1:dim(data.vis)[1]))) +
  geom_errorbar(width=.1, aes(ymin=ci.low, ymax=ci.high), colour="red") +
  geom_point(aes(y=coef)) +
  geom_line(aes(y=coef)) +
  xlab("FIPS") + ylab("PM2.5 coefficients") +
  ylim(-0.05, 0.05) +
  ggtitle(paste("lag", lags))
p1

lags = 2
data.vis = result.rbind[result.rbind$ilag == lags,]
p2 = ggplot(data=data.vis, aes(x=(1:dim(data.vis)[1]))) +
  geom_errorbar(width=.1, aes(ymin=ci.low, ymax=ci.high), colour="red") +
  geom_point(aes(y=coef)) +
  geom_line(aes(y=coef)) +
  xlab("FIPS") + ylab("PM2.5 coefficients") +
  ylim(-0.05, 0.05) +
  ggtitle(paste("lag", lags))
p2

lags = 3
data.vis = result.rbind[result.rbind$ilag == lags,]
p3 = ggplot(data=data.vis, aes(x=(1:dim(data.vis)[1]))) +
  geom_errorbar(width=.1, aes(ymin=ci.low, ymax=ci.high), colour="red") +
  geom_point(aes(y=coef)) +
  geom_line(aes(y=coef)) +
  xlab("FIPS") + ylab("PM2.5 coefficients") +
  ylim(-0.05, 0.05) +
  ggtitle(paste("lag", lags))
p3



# plot.out = list()
# pdf.save()
# plot.out[[1]] = p0
# plot.out[[2]] = p1
# plot.out[[3]] = p2
# plot.out[[4]] = p3
# do.call('grid.arrange',c(plot.out, ncol = 1, top = "global model"))

dev.off()

# ### parameter set up### parameter set up### parameter set up### parameter set up### parameter set up### parameter set up
# ### parameter set up 
# smooth = "bs" # "ns"
# cause = "cases"
# pollutant = "pm25"
# group = "FIPS"
# control = glm.control(epsilon = 1e-10, maxit = 1000)
# 
# ### create lags of pollutant
# lag.out = add.lag(df=df, value=pollutant, group=group, lags=lags)
# lag.data = as.matrix(lag.out[[1]])
# lag.data.name = "lag.data" # import function
# print(dim(df))
# print(dim(lag.data))
# # if run in main function lag.data.name = as.name(substitute(lag.data)) ???
# 
# 
# ### create lag names
# lag.names = lag.out[[2]]
# if (length(lag.names) == 1) {
#   lag.names = c("lag.data")
# } else {
#   for (i in 1:length(lag.names)) lag.names[i] = paste0(lag.data.name, lag.names[i]) }
# print(length(lag.names))
# 
# ### initialize model
# df.name = as.name(substitute(df))
# 
# f = substitute(~ smooth(date, df.date) + smooth(tmmx, df.tmmx) + 
#                  smooth(sph, df.sph) + dayofweek + log(population),
#                list(df.date = df.date, df.tmmx = df.tmmx, 
#                     df.sph = df.sph, smooth = as.name(smooth)))
# rhs = as.character(f)
# 
# ### create the formula for visualization purpose
# modelFormula.vis = paste(cause, rhs[1], paste(rhs[-1], paste(lag.names, collapse = "+"),  sep = "+"))
# print(modelFormula.vis)
# 
# ### add pollutant elements to the model
# rhs[-1] = paste(rhs[-1], lag.data.name, sep = "+")
# 
# ### add cause to the model
# modelFormula = as.formula(paste(cause, paste(rhs, collapse = "")))
# print(modelFormula)
# 
# ### fit quasipoisson model
# call = substitute(glm(modelFormula, family = quasipoisson, data = df, 
#                       control = control, na.action = na.exclude),
#                   list(modelFormula = modelFormula, data = df.name, 
#                        lag.name = lag.data, control = substitute(control))) 
# 
# fit = try(eval.parent(call), silent=TRUE)
# if('try-error' %in% class(fit)){
#   next }
# 
# fit.CI = try(confint(fit),silent=TRUE)
# if('try-error' %in% class(fit.CI)){
#   next }
# 
# # 
# # fit = eval.parent(call)
# # 
# # fit.CI = confint(fit)
# 
# ### save and returns the results
# result = data.frame(matrix(ncol = 5, nrow = length(lags)))
# colnames(result) = c("coef", "ci.low", "ci.high", "ilag", "name")
# 
# for (ilag in 1:length(lag.names)) {
#   var.name = lag.names[ilag]
#   ci.value = fit.CI[var.name,]
#   result[ilag,] = c(fit$coefficients[var.name],ci.value[1],ci.value[2], lags[ilag], lag.names[ilag])
# }
# ### parameter set up### parameter set up### parameter set up### parameter set up### parameter set up### parameter set up### parameter set up
# 
# 
######################################################################################
# library(package)
# remotes::install_github("rdpeng/tlnise")
# source("scr/bayes_dlm_pub.R")
# df = load.data()
# data = pmNMMAPS()
# citydata = data[data$FIPS == '6001', ]
# bayesDLM(citydata=citydata)
# 
# ######## bayesDLM
# citydata = df[df$FIPS == '6001', ]
# pollutant = "pm25"
# nLags = 14
# sigma = 0.004
# its = 10 

