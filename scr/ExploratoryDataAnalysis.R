# Exploratory Data Analysis 
# time series plot of PM2.5 levels with the dates of the wildfires (todo)
# smooth the time series plots for both cases and PM2.5
#  this is only for a few locations among the largest locations 
#  For 8, can also do this for 2, 4, 10


# library(tsModel)
library(splines)
library(ggplot2)
library(gridExtra)

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
df.var = 4
control.week = TRUE
file.name = paste0("ExploratoryDataAnalysis/pm&cases_df", df.var, ".pdf")
if (control.week) file.name = paste0("ExploratoryDataAnalysis/pm&cases_df", df.var, "control.week.pdf")



pdf(file.name, width = 12, height = 4)

for (ifips in fips.selected) {
  df.selected = df[df$FIPS == ifips, ]
  
  grid.title = paste0("FIPS ", ifips)
  
  ### visualize PM2.5
n
  
  ### visualize number of cases 
  p2 = ggplot() +
    geom_point(data=df.selected, aes(x=date, y=cases, color="Cases")) +
    theme(legend.position="none")
  
  ### smooth number of cases 
  cases.formula = paste0("cases ~ bs(date, ", df.var, ")")
  if (control.week) cases.formula = paste0(cases.formula, " + dayofweek")
  
  fit.cases = glm(data = df.selected, formula = cases.formula, family=quasipoisson)
  
  if (exists("cases.formula")) {
    p2 = p2 + geom_line(data=df.selected, aes(x=date, y=fit.cases$fitted.values, color="Cases"), alpha=.5) + 
      ggtitle(cases.formula)
  } else {
    p2 = p2 + geom_line(data=df.selected, aes(x=date, y=cases, color="Cases"), alpha=.5)
  }

  ### visualize residuals 
  p3 = ggplot() +
    geom_point(data=df.selected, aes(x=date, y=cases-fit.cases$fitted.values, color="Residual of Cases")) +
    theme(legend.position="none")
  
  ### Pearson Correlation 
  missing.index = is.na(df.selected$pm25) | is.na(df.selected$cases)
  v.cor = cor(df.selected[!missing.index, ]$pm25, df.selected[!missing.index, ]$cases)
  if (exists("v.cor")) grid.title = paste0(grid.title, " (Pearson Correlation ", round(v.cor, 3), ")")
  
  grid.arrange(p2, p3, ncol=3, top = grid.title)
}
dev.off()

