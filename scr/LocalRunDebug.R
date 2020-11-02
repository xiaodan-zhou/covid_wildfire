setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
source("scr/GlobalModel.R")
# dff = load.data.xz1()
# dff = load.moddat2()
dff = load.data.error()

### FIPS list 
fips = unique(dff$FIPS)
length(fips)

### set up
lags.to.run = 0:3
df.date=8
df.tmmx=3
df.rmax=3
smooth="ns"
cause="cases"
pollutant="pm25"
group="FIPS"
control=glm.control(epsilon = 1e-10, maxit = 10000)
##debug 
ilag = 0
ifips = 2
lags=0

# dff = dff[dff$FIPS == fips[ifips], ]
lags = ilag

gm = global.model(dff, lags=ilag, smooth = smooth, cause = cause, 
                  df.date=df.date, df.tmmx=df.tmmx, df.rmax=df.rmax)
gm[[1]]
# # 
# # ### parameter set up 
# cause = "cases"
# pollutant = "pm25"
# group = "FIPS"
# control = glm.control(epsilon = 1e-10, maxit = 1000)
# 
# ### create lags of pollutant
# lag.out = add.lag(dff=dff, value=pollutant, group=group, lags=lags)
# lag.data = as.matrix(lag.out[[1]])
# lag.data.name = "lag.data" # import function
# # if run interactively, lag.data.name = as.name(substitute(lag.data)) why???
# 
# 
# ### create lag names
# lag.names = lag.out[[2]]
# if (length(lag.names) == 1) {
#   lag.names = c("lag.data")
# } else {
#   for (i in 1:length(lag.names)) {
#     lag.names[i] = paste0(lag.data.name, lag.names[i])
#   } }
# 
# ### initialize model
# df.name = as.name(substitute(dff))
# 
# f = substitute(~ smooth(date, df.date) + smooth(tmmx, df.tmmx) +
#                  smooth(rmax, df.rmax) + dayofweek + log(population),
#                list(df.date = df.date, df.tmmx = df.tmmx,
#                     df.rmax = df.rmax, smooth = as.name(smooth)))
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
# 
# ### fit quasipoisson model
# call = substitute(glm(modelFormula, family = quasipoisson, data = dff,
#                       control = control, na.action = na.exclude),
#                   list(modelFormula = modelFormula, data = df.name,
#                        lag.name = lag.data, control = substitute(control)))
# 
# # test
# # glm("cases ~ ns(date, 4) + ns(tmmx, 1) + ns(rmax, 1) + dayofweek + log(population) + lag.data",
# #     family = quasipoisson, data = dff,
# #     control = control, na.action = na.exclude)
# 
# ### if hit any problems in modelling, returns -1
# fit = try(eval.parent(call), silent=TRUE)
# fit
