setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
library(scales) 
library(latex2exp)
df = load.data()
df$weeks = cut(df$date, breaks="week")
df = df[df$date >= ymd("2020-03-16"), ]

### by week, log 
x.lab = unique(reorder(df$weeks,df$date))
x.lab = paste0(month(x.lab), "/", day(x.lab))
x.lab[!((1:length(x.lab))%%2)] = ""
p0 = ggplot(data=df) + 
  geom_boxplot(aes(x=reorder(weeks,date),y=pm25), outlier.shape = NA) + 
  facet_wrap(. ~ state, scales='free', ncol = 1) + 
  theme_bw() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10(limits = c(1,1e3)) +
  scale_x_discrete(labels = x.lab) + 
  ylab(TeX("$PM_{2.5}$ ($\\mu g/m^3$ log scaled)")) + 
  xlab("Date")

file.pdf = "output/pm_by_states.pdf"
pdf(file.pdf, width = 10, height = 8)
# pdf(file.pdf, width = 8, height = 6) #v2
do.call('grid.arrange',c(list(p0), top = ""))
dev.off()

### by day, log
# ggplot(data=df) + 
#   geom_boxplot(aes(x=as.factor(date),y=log(pm25+.1)),outlier.shape = NA) + 
#   facet_wrap(. ~ state.x, ncol = 1) + theme_bw()