setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
library(scales) 

df = load.data()
df = df[df$date >= "2020-03-15", ]
df = df[df$date <= "2020-09-24", ]
df$weeks = cut(df$date, breaks="week")

### by week, log 


x.lab = unique(reorder(df$weeks,df$date))
x.lab = paste0(month(x.lab), "/", day(x.lab))
p0 = ggplot(data=df) + 
  geom_boxplot(aes(x=reorder(weeks,date),y=pm25), outlier.shape = NA) + 
  facet_wrap(. ~ state.x, scales='free', ncol = 1) + theme_bw() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10(limits = c(1,1e3)) +
  ylab("PM2.5") + xlab("Date") + 
  scale_x_discrete(labels = x.lab)# + 
  # theme(strip.text = element_text(size = 11))

file.pdf = "output/Vis.PMbySate.pdf"
pdf(file.pdf, width = 10, height = 6)
do.call('grid.arrange',c(list(p0), top = ""))
dev.off()

### by day, log
# ggplot(data=df) + 
#   geom_boxplot(aes(x=as.factor(date),y=log(pm25+.1)),outlier.shape = NA) + 
#   facet_wrap(. ~ state.x, ncol = 1) + theme_bw()

### by week, raw 
# ggplot(data=df) + 
#   geom_boxplot(aes(x=format(date, "%Y/%W"),y=pm25),outlier.shape = NA) + 
#   facet_wrap(. ~ state.x, ncol = 1) + theme_bw()



