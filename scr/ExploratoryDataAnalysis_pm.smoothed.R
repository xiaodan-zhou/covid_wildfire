# Exploratory Data Analysis 
# function: visualize the PM2.5 smoothed with various degree of freedom
# output file name: pm.smoothed.pdf

library(splines)
library(ggplot2)
library(gridExtra)
library(pracma)

setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
df = load.data()

### set up smoothing method and degree of smoothing 
df.var = c(2, 4, 6, 8, 10)
smooth = "ns"

n.col.grid = length(df.var) * 2

file.name = paste0("ExploratoryDataAnalysis/pm.", smooth, ".pdf")

fips.unique = unique(df$FIPS[order(df$population, decreasing=TRUE)])

pdf(file.name, width = 12, height = n.col.grid * length(fips.unique) / 10)

plot.list = list()

iplot = 1 

for (ifips in 1:length(fips.unique)) {
  
  df.selected = df[df$FIPS == fips.unique[ifips], ]
  
    for (i.df in df.var) {

      ### smooth pm 
      modelFormula = as.formula(paste0("pm25 ~ ", smooth, "(date, ", i.df, ")"))
      
      call = substitute(glm(modelFormula, family = gaussian, data = df.selected), #  quasipoisson
                        list(modelFormula = modelFormula))
      
      ### skip failed model
      fit = try(eval.parent(call), silent=TRUE)
      if('try-error' %in% class(fit)){
        iplot = iplot + 2
        next
        }
      
      ### clean data a bit 
      missing.index = is.na(df.selected$pm25)
      df.out = data.frame(xx = df.selected[!missing.index, ]$date,
                          yfit = fit$fitted.values,
                          yraw = df.selected[!missing.index, ]$pm25)
      
      ### visualize pm2.5
      p1 = ggplot() +
        geom_point(data=df.out, aes(x=xx, y=yraw), colour="blue", size = .1, alpha=.5) +
        geom_line(data=df.out, aes(x=xx, y=yfit), color="orange", size = .5)  + 
        theme(legend.position="none", 
              axis.title.x=element_blank(), axis.title.y=element_blank(), 
              axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
              axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
              plot.title = element_text(size = 6)) +
        ggtitle(as.character(fips.unique[ifips]))
      
      plot.list[[iplot]] = p1
      iplot = iplot + 1
      
      ### visualize residuals
      p2 = ggplot() +
        geom_point(data=df.out, aes(x=xx, y=yraw-yfit), color="orange", size = .1) +
        theme(legend.position="none", 
              axis.title.x=element_blank(), axis.title.y=element_blank(), 
              axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
              axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
              plot.title = element_text(size = 6)) +
        ggtitle(paste("df", i.df))
      plot.list[[iplot]] = p2
      iplot = iplot + 1
    }
}

do.call('grid.arrange',c(plot.list, ncol = n.col.grid, top = "PM2.5 smoothing (fitted, residual)"))

dev.off()
