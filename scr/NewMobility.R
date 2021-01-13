library(reshape2)
library(ggplot2)
library(gridExtra)
library(lubridate)
setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")


plot.list = list()
iplot = 1
pdf.out = "state_grocery.pdf"

################################ state ###############################
dt = read.csv("data/movement-range.csv")
dt$date = ymd(dt$date)
dt = dt[dt$date <= "2020-09-24", ]
dt = dt[dt$date >= "2020-03-15", ]
# dt = dt[ %in% c("California", "Washington", "Oregon"),]
dt$state = as.integer(as.integer(dt$fips)/1000)
for (istate in unique(dt$state)) {
  p1 = ggplot(dt[dt$state == istate, ]) + geom_line(aes(date,relative_change_feb,col=as.factor(fips))) + 
    ggtitle(istate) + theme_bw()
  plot.list[[iplot]] = p1
  iplot = iplot + 1
}

### Facebook data
old = read.csv("data/dataverse_Dec2/combined_percent_change_from_baseline_CO.csv")
old$date = ymd(old$date)
old = old[old$date <= "2020-09-24", ]
old = old[old$date >= "2020-03-15", ]
old$state = as.integer(as.integer(old$GEOID)/1000)
old = old[old$state %in% c(6, 41, 53),]

dt2=merge(dt, old, by.x=c("fips", "date"), by.y=c("GEOID", "date"), all.x=T)

for (istate in unique(dt2$state.x)) {
  p1 = ggplot(dt2[(dt2$state.x == istate) & (dt2$type == "grocery"), ]) + geom_line(aes(date,relative_change_feb,col=as.factor(fips))) + 
    ggtitle(istate) + theme_bw()
  plot.list[[iplot]] = p1
  iplot = iplot + 1
  p1 = ggplot(dt2[(dt2$state.x == istate) & (dt2$type == "grocery"), ]) + geom_line(aes(date,mobility,col=as.factor(fips))) + 
    ggtitle(istate) + theme_bw()
  plot.list[[iplot]] = p1
  iplot = iplot + 1 
}

pdf(pdf.out, width = 12, height = iplot * 3)
do.call('grid.arrange',c(plot.list, ncol = 1, top = "2020-03-15 to 2020-09-24"))
dev.off()
### Facebook data

dff = load.data()
vars = c("cases", "deaths", "pm25", "pmbase", "pmhazard", "relative_change_feb")  
for (type in c(1)) { # unique(dff$type)
  dt3 = dff[complete.cases(dff), ]
  # dt3 = dt3[dt3$type == type, ]
  sum.cor = matrix(0, nrow=length(vars), ncol=length(vars))
  for (id in unique(dt3$FIPS)) {
    cors = cor(as.matrix(dt3[dt3$fips == id, vars]))
    if (!anyNA(cors)) sum.cor = sum.cor + 
        as.matrix(cor(as.matrix(dt3[dt3$fips == id, vars]), method="spearman"))
  }
  sum.cor = sum.cor/sum.cor[c(1)]
  # print(type)
  print(round(sum.cor, 2))
}

