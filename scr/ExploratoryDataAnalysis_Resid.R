# Exploratory Data Analysis 
# time series plot of PM2.5 levels with the dates of the wildfires (todo)
# smooth the time series plots for both cases and PM2.5
#  this is only for a few locations among the largest locations 
#  For 8, can also do this for 2, 4, 10


# library(tsModel)
library(splines)
library(ggplot2)
library(gridExtra)
library(pracma)



# options(na.action = "na.pass")
# print(options("na.action"))
##########################################################################
setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
df = load.data()

###################### visualize pm and cases in few FIPS ################
### select a few locations with most population
fips.selected = unique(df$FIPS[order(df$population, decreasing=TRUE)])[1:10]
### set up 
df.var = 6
control.week = FALSE
file.name = paste0("ExploratoryDataAnalysis/cases_df", df.var, "_Resid.pdf")
if (control.week) file.name = paste0("ExploratoryDataAnalysis/cases_df", df.var, "control.week_Resid.pdf")



pdf(file.name, width = 12, height = 4)

for (ifips in fips.selected) {
  df.selected = df[df$FIPS == ifips, ]
  
  grid.title = paste0("FIPS ", ifips)

  ### smooth number of cases 
  cases.formula = paste0("cases ~ bs(date, ", df.var, ")")
  if (control.week) cases.formula = paste0(cases.formula, " + dayofweek")
  fit.cases = glm(data = df.selected, formula = cases.formula, family=quasipoisson)
  
  ### visualize number of cases 
  p1 = ggplot() +
    geom_point(data=df.selected, aes(x=date, y=cases, color="Cases")) +
    theme(legend.position="none") + 
    geom_line(data=df.selected, aes(x=date, y=fit.cases$fitted.values, color="Cases"), alpha=.5) + 
    ggtitle(cases.formula)
  
  ### visualize number of cases residuals
  p2 = ggplot() +
    geom_point(data=df.selected, aes(x=date, y=cases-fit.cases$fitted.values, color="Cases")) +
    theme(legend.position="none") + 
    ggtitle(cases.formula)

  ### smooth number of cases 
  cases.formula = paste0("cases ~ bs(date, ", df.var, ")")
  if (control.week) cases.formula = paste0(cases.formula, " + dayofweek + log(population)")
  fit.cases = glm(data = df.selected, formula = cases.formula, family=quasipoisson)
  
  ### visualize residuals 
  p3 = ggplot() +
    geom_point(data=df.selected, aes(x=date, y=cases-fit.cases$fitted.values, color="Residual of Cases")) +
    theme(legend.position="none") + 
    ggtitle(cases.formula)
  
  p4 = ggplot(data=df.selected) + 
    geom_point(data=df.selected, aes(x=date, y=cases-fit.cases$fitted.values, color="Moving Average 7")) +
    geom_line(aes(x=date, y=movavg(cases, 7, "r")))
  
  p4
  
  grid.arrange(p1, p2, p3, ncol=3, top = grid.title)
}
dev.off()



yy = runif(100)
ggplot() + geom_line(x=1:100, y=movavg(yy, 7, 's')) + geom_point(x=1:100, y=yy)

