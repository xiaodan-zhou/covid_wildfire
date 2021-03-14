library(rstudioapi)
project.dir = dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(project.dir)
source("scr/Utilities.R")
library(scales) 
library(latex2exp)

#########################
df = load.data()
df = df[!is.na(df$hazardmap), ]
df = df[df$date >= ymd("2020-03-16"), ]
df$weeks = cut(df$date, breaks="week")

x.lab = unique(reorder(df$weeks, df$date))
x.lab = paste0(month(x.lab), "/", day(x.lab))
x.lab[!((1:length(x.lab))%%2)] = ""

### pm2.5
p0 = ggplot(data=df) + 
  geom_boxplot(aes(x=reorder(weeks,date),y=pm25), outlier.shape = NA) + 
  facet_wrap(. ~ State, scales='free', ncol = 1) + 
  theme_bw() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10(limits = c(1,1e3)) +
  scale_x_discrete(labels = x.lab) + 
  ylab(TeX("$PM_{2.5}$ ($\\mu g/m^3$ log scaled)")) + 
  xlab("Date")

file.pdf = "output/pm_by_states.pdf"
pdf(file.pdf, width = 10, height = 8)
do.call('grid.arrange',c(list(p0), top = ""))
dev.off()


#########################
df = load.data()
df = df[, c("pm25", "pm25_history", "date", "State")]
### put pm25 and pm25_history in the same column
df = df %>% gather(type, value, pm25, pm25_history, -State, -date)
df$type[df$type == "pm25"] = "2020"
df$type[df$type == "pm25_history"] = "2016-2019"

# df = df[!is.na(df$hazardmap), ]
df = df[df$date >= ymd("2020-03-16"), ]
df$weeks = cut(df$date, breaks="week")

x.lab = unique(reorder(df$weeks, df$date))
x.lab = paste0(month(x.lab), "/", day(x.lab))
x.lab[!((1:length(x.lab))%%2)] = ""

### pm2.5
p0 = ggplot(data=df) + 
  geom_boxplot(aes(x=as.factor(date), y=value, fill=type), outlier.shape = NA) + 
  facet_wrap(. ~ State, scales='free', ncol = 1) + 
  theme_bw() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10(limits = c(1,1e3)) +
  scale_x_discrete(labels = x.lab) + 
  ylab(TeX("$PM_{2.5}$ ($\\mu g/m^3$ log scaled)")) + 
  xlab("Date")

file.pdf = "output/pm_by_states_add_history.pdf"
pdf(file.pdf, width = 10, height = 8)
do.call('grid.arrange',c(list(p0), top = ""))
dev.off()


#########################
df = load.data()
df$weeks = cut(df$date, breaks="week")

x.lab = unique(reorder(df$weeks, df$date))
x.lab = paste0(month(x.lab), "/", day(x.lab))
x.lab[!((1:length(x.lab))%%2)] = ""

### pm2.5
p0 = ggplot(data=df) + 
  geom_point(aes(x=1:dim(df)[1], y=pm25_history[order(pm25, decreasing=T)]), color="orange", alpha=.3) +
  geom_point(aes(x=1:dim(df)[1], y=pm25[order(pm25, decreasing=T)]), color="black", alpha=.3) + 
  facet_wrap(. ~ State, scales='free', ncol = 1) + 
  theme_bw() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10(limits = c(1,1e3)) +
  ylab(TeX("$PM_{2.5}$ ($\\mu g/m^3$ log scaled)")) + 
  xlab(TeX("Sorted $PM_{2.5}$ in 2020 from high to low"))

file.pdf = "output/pm_by_states_another_design2.pdf"
pdf(file.pdf, width = 10, height = 8)
do.call('grid.arrange',c(list(p0), top = ""))
dev.off()






#########################
### pm2.5 from wildfire
p1 = ggplot(data=df) + 
  geom_boxplot(aes(x=reorder(weeks, date),y=pm_wildfire), outlier.shape = NA) + 
  facet_wrap(. ~ State, scales='free', ncol = 1) + 
  theme_bw() +
  scale_x_discrete(labels = x.lab) +
  ylab(TeX("$PM_{2.5}$ ($\\mu g/m^3$)")) + 
  xlab("Date")

file.pdf = "output/pm_by_states_wfire.pdf"
pdf(file.pdf, width = 10, height = 8)
do.call('grid.arrange',c(list(p1), top = ""))
dev.off()
