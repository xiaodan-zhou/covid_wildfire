setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
source("scr/GlobalModel.R")
# dff = load.data.xz1()
# dff = load.moddat2()
dff = load.data.xz1()

### set up
# keep.fireday = TRUE
lags.to.run = 0:3
smooth="ns"
cause = "cases"
extra.note = ""
# df.combo = list(c(5,1), c(10, 1), c(15, 2), c(20, 2), c(25, 3), c(30, 3), c(35, 4))
# df.combo = list(c(5,1), c(10, 1), c(15, 2), c(20, 2), c(25, 3), c(30, 3), c(35, 4))
df.combo = list(c(2,1,1),# c(3,1,1), c(4,1,1),
                c(5,2,2))# , #c(6,2,2), c(7,2,2),
                # c(8,2,2))# c(9,3,3), c(10,3,3))
# df.combo = list(c(11,3,3), c(12,3,3), c(14,3,3),
#                 c(16,3,3), c(20,3,3), c(24,4,4))


extra.note = "FIPS.DEBUG"
# if (keep.fireday) {
  # extra.note = ".keepfireday"
# } else {
  # extra.note = ".removefireday.moremore"
  ### fire day setup 
dff$fireday = 0
dff$fireday[dff$pm25 >= 20] = 1 # define those pm>=20 as smoke day
  # dff = dff[dff$fireday == 0, ] # WRONG!!!!!
  # dff$cases[dff$fireday == 1] = NA # NOT WORKING!!!!!!
  # dff$pm[dff$fireday == 1] = NA # remove fireday by tag missing
# }



### output 
temp.name = paste0(paste(lags.to.run, collapse=""), ".", cause, ".sensitivity", extra.note) # confit
file.pdf = paste0("GlobalModel/lag", temp.name, ".pdf")
file.csv = paste0("GlobalModel/lag", temp.name, ".csv")

##################### run global model for lag 0-3 separately #####################
result.rbind = c()
for (ilag in lags.to.run) {
  for (idf.combo in df.combo) {
    gm = global.model(dff, smooth = smooth, lags=ilag, 
                      df.date=idf.combo[1], df.tmmx=idf.combo[2], 
                      df.rmax=idf.combo[2], cause = cause)
    
    if (length(gm) != 1) {
      fit = gm[[1]]
      fit.CI = gm[[2]]
      modelFormula.vis = gm[[3]]
      result = gm[[4]]
      
      if (is.null(result.rbind)) {
        result.rbind = result
      } else {
        result.rbind=rbind(result.rbind, result)
      }
    } else {
      print(paste("failed: ", gm))
      }
   }
}

result.rbind$coef = as.numeric(result.rbind$coef)
result.rbind$ci.low = as.numeric(result.rbind$ci.low)
result.rbind$ci.high = as.numeric(result.rbind$ci.high)
result.rbind$ilag = as.numeric(result.rbind$ilag)
result.rbind$df.date = as.numeric(result.rbind$df.date)
result.rbind$df.tmmx = as.numeric(result.rbind$df.tmmx)
result.rbind$df.rmax = as.numeric(result.rbind$df.rmax)
result.rbind$df.combo = as.character(paste0(result.rbind$df.date,result.rbind$df.tmmx,result.rbind$df.rmax))
write.csv(result.rbind, file.csv)


##################### visualize #####################


plot.out = list()
iplot = 1 

for (ilag in lags.to.run) {
  data.vis = result.rbind[result.rbind$ilag == ilag,]
  
  if (sum(is.na(data.vis)) == 0) {
    p0 = ggplot(data=data.vis, aes(x=(1:dim(data.vis)[1]))) +
      geom_errorbar(width=.1, aes(ymin=ci.low, ymax=ci.high), colour="red") + 
      geom_point(aes(y=coef)) + 
      geom_line(aes(y=coef)) + 
      xlab("df.combo") + ylab("PM2.5 coefficients") + 
      ggtitle(paste("lag", ilag)) + 
      scale_x_continuous(breaks = 1:length(data.vis$df.combo),
                         labels = data.vis$df.combo) + 
      geom_hline(yintercept=0, linetype="dashed", color = "blue", alpha=.6)
    plot.out[[iplot]] = p0
    iplot = iplot + 1
  }
}

pdf(file.pdf, width = 12, height = 3 * length(plot.out))
do.call('grid.arrange',c(plot.out, ncol = 1, top = "global model"))
dev.off()
