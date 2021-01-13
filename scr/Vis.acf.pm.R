
#################### ACF of pm ##################
source("scr/Utilities.R")
dff = load.data()

pdf.out = "output/acf.pm.pdf"
plot.list = list()

## pm25
value.list = c(); lag.list = c()
for (ifips in unique(dff$FIPS)) {
  values = dff$pm25[dff$FIPS == ifips]
  if(any(is.na(values))) next 
  output = my.acf(values)
  value.list = c(value.list, output$value)
  lag.list = c(lag.list, output$lag)
  iplot = iplot + 1
}
pm25.acf = data.frame(value = value.list, lag = lag.list)
plot.list[[1]] = ggplot(pm25.acf) + 
  geom_boxplot(aes(x=reorder(as.factor(as.character(lag)),lag) ,y=value), outlier.shape = NA) + 
  theme_bw() + xlab("lag") + ylab("PM2.5 acf")

## pmbase
value.list = c(); lag.list = c()
for (ifips in unique(dff$FIPS)) {
  values = dff$pmbase[dff$FIPS == ifips]
  if(any(is.na(values))) next 
  output = my.acf(values)
  value.list = c(value.list, output$value)
  lag.list = c(lag.list, output$lag)
  iplot = iplot + 1
}
pm25.acf = data.frame(value = value.list, lag = lag.list)
plot.list[[2]] = ggplot(pm25.acf) + 
  geom_boxplot(aes(x=reorder(as.factor(as.character(lag)),lag) ,y=value), outlier.shape = NA) + 
  theme_bw() + xlab("lag") + ylab("PM Baseline acf")

## pmhazard
value.list = c(); lag.list = c()
for (ifips in unique(dff$FIPS)) {
  values = dff$pmhazard[dff$FIPS == ifips]
  if(any(is.na(values))) next 
  output = my.acf(values)
  value.list = c(value.list, output$value)
  lag.list = c(lag.list, output$lag)
  iplot = iplot + 1
}
pm25.acf = data.frame(value = value.list, lag = lag.list)
plot.list[[3]] = ggplot(pm25.acf) + 
  geom_boxplot(aes(x=reorder(as.factor(as.character(lag)),lag) ,y=value), outlier.shape = NA) + 
  theme_bw() + xlab("lag") + ylab("PM2.5 Wildfire acf")


pdf(pdf.out)
do.call('grid.arrange',c(plot.list, ncol = 1, top = ""))
dev.off()
