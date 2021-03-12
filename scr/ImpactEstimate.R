library(rstudioapi)
project.dir = dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(project.dir)
source("scr/Utilities.R")
df = load.data()

impact.estimate = function(df, effect=0.003, aggressive=NA, conservative=NA) {
  
  if ((!is.na(aggressive))&(!is.na(conservative))) 
    return(NA)
  
  df$wf = df$wf1
  
  if (!is.na(aggressive))
    df$wf[(df$wf1==F)&(df$pm25>=aggressive)] = T
  
  if (!is.na(conservative))
    df$wf[(df$wf1==T)&(df$pm25<=conservative)] = F

  ### baseline
  cases_base0 = sum(df$cases[df$wf1==T], na.rm=T)
  deaths_base0 = sum(df$deaths[df$wf1==T], na.rm=T)
  
  pm_delta0 = mean(df$pm25[df$wf1==T], na.rm=T) - mean(df$pm25[df$wf1==F], na.rm=T)
  
  cases_delta0 = (exp(-pm_delta0 * effect)-1) * cases_base0
  deaths_delta0 = (exp(-pm_delta0 * effect)-1) * deaths_base0
  
  ## estimate
  cases_base = sum(df$cases[df$wf==T], na.rm=T)
  deaths_base = sum(df$deaths[df$wf==T], na.rm=T)
  
  pm_delta = mean(df$pm25[df$wf==T], na.rm=T) - mean(df$pm25[df$wf==F], na.rm=T)
  cases_delta = (exp(-pm_delta*effect)-1) * cases_base
  deaths_delta = (exp(-pm_delta*effect)-1) * deaths_base
  
  cases_relative_change = (cases_delta - cases_delta0) / cases_delta0 * 100
  deaths_relative_change = (deaths_delta - deaths_delta0) / deaths_delta0 * 100
  
  return(list(cases_base=cases_base, 
              deaths_base=deaths_base, 
              cases_change=cases_delta, 
              deaths_change=cases_delta, 
              cases_relative_change=cases_relative_change, 
              deaths_relative_change=deaths_relative_change))
}

### the aggressive estimate is not aggressive at all
rr = c()
for (pm in c(NA, 30, 40, 60, 80)) {
  rr = c(rr, unlist(impact.estimate(df, aggressive=pm)))
  print(sum(df$pm_wf1 > pm))
}
matrix(rr, nrow = 6)


### the conservative estimate could be somthing 
rr = c()
for (pm in c(NA, 20, 10, 5, 3)) {
  rr = c(rr, unlist(impact.estimate(df, conservative=pm)))
  print(sum((df$pm25 - df$pm_wf1) < pm))
}
matrix(rr, nrow = 6)



# ####################################### TESTS #######################################
# df = df[df$date <= "2020-11-26", ]
# 
# effect = 0.003
# 
# # the lower the more aggressive
# df$wf2 = df$wf1
# df$wf2[(df$wf1==F)&(df$pm25>=66)] = T
# sum((df$wf1==F)&(df$pm25>=66))
# 
# # the higher the more conservative
# df$wf3 = df$wf1
# df$wf3[(df$wf1==T)&(df$pm25<=20)] = F
# sum((df$wf1==T)&(df$pm25<=20))
# 
# ####################################### scenario one #######################################
# cases_wildfire1 = sum(df$cases[df$wf1==T], na.rm=T)
# deaths_wildfire1 = sum(df$deaths[df$wf1==T], na.rm=T)
# pm_delta1 = mean(df$pm25[df$wf1==T], na.rm=T) - mean(df$pm25[df$wf1==F], na.rm=T)
# cases_delta1 = (exp(-pm_delta1*effect)-1) * cases_wildfire1
# deaths_delta1 = (exp(-pm_delta1*effect)-1) * deaths_wildfire1
# 
# ####################################### scenario two aggressive #######################################
# cases_wildfire2 = sum(df$cases[df$wf2==T], na.rm=T)
# deaths_wildfire2 = sum(df$deaths[df$wf2==T], na.rm=T)
# pm_delta2 = mean(df$pm25[df$wf2==T], na.rm=T) - mean(df$pm25[df$wf2==F], na.rm=T)
# cases_delta2 = (exp(-pm_delta2*effect)-1) * cases_wildfire2
# deaths_delta2 = (exp(-pm_delta2*effect)-1) * deaths_wildfire2
# 
# ####################################### scenario three conservative #######################################
# ### the wildfire could be aloft!!!
# 
# cases_wildfire3 = sum(df$cases[df$wf3==T], na.rm=T)
# deaths_wildfire3 = sum(df$deaths[df$wf3==T], na.rm=T)
# pm_delta3 = mean(df$pm25[df$wf3==T], na.rm=T) - mean(df$pm25[df$wf3==F], na.rm=T)
# cases_delta3 = (exp(-pm_delta3*effect)-1) * cases_wildfire3
# deaths_delta3 = (exp(-pm_delta3*effect)-1) * deaths_wildfire3
# 
# cases_pct2 = (cases_delta2 - cases_delta1) / cases_delta1 * 100
# deaths_pct2 = (deaths_delta2 - deaths_delta1) / deaths_delta1 * 100
# 
# cases_pct3 = (cases_delta3 - cases_delta1) / cases_delta1 * 100
# deaths_pct3 = (deaths_delta3 - deaths_delta1) / deaths_delta1 * 100
# 
# plot(c(1,2,3), c(cases_delta2, cases_delta1, cases_delta3))
# plot(c(1,2,3), c(deaths_delta2, deaths_delta1, deaths_delta3))
