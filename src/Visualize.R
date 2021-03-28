library(rstudioapi)
project.dir = dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(project.dir)
source("src/Utilities.R")
library(sf)
library(viridis)
library(scales) 
library(latex2exp)
library(dplyr)

dff = load.data()

###############################################################
dfs = dff %>% group_by(FIPS) %>% 
  summarise(pm25.high = sum(pm25>=35, na.rm=T), 
            hazard = sum(hazardmap>=27, na.rm=T))

ndays = 277 
dfs$pm25.high = dfs$pm25.high / ndays * 100

cty = read_sf('data/cb_2018_us_county_5m', 'cb_2018_us_county_5m')
cty = cty[cty$STATEFP %in% c('06', '41', '53'), ]
cty$FIPS = as.numeric(as.character(cty$GEOID))

cty.selected = merge(cty, dfs, by="FIPS", all.x=T)
cty.selected$hazard_pct = cty.selected$hazard / ndays * 100
rm(dfs)

###################### map_wildfire_pct #########################################
### set up 
vis = "hazard_pct"
ylim = c(0, max(cty.selected$hazard_pct, na.rm=T))

### WA 
p0 = 
  ggplot(cty.selected[cty.selected$STATEFP == "53", ]) + ggtitle("Washington") + 
  geom_sf(aes_string(fill=vis), color=NA) + 
  theme_void() + 
  scale_fill_gradient(low="blue", high="red", na.value = "lightgrey", limits=ylim) + 
  theme(legend.position = "None")

### OR 
p1 = ggplot(cty.selected[cty.selected$STATEFP == "41", ]) + ggtitle("Oregon") + 
  geom_sf(aes_string(fill=vis), color=NA) + 
  theme_void() + 
  scale_fill_gradient(low="blue", high="red", na.value = "lightgrey", limits=ylim,
                      name = "% of wildfire days") + 
  theme(legend.position = "bottom")

### CA
p2 = ggplot(cty.selected[cty.selected$STATEFP == "06", ]) + ggtitle("California") + 
  geom_sf(aes_string(fill=vis), color=NA) + 
  theme_void() + 
  scale_fill_gradient(low="blue", high="red", na.value = "lightgrey", limits=ylim) + 
  theme(legend.position = "None")

### 
plot.list = list()
plot.list[[1]] = p0
plot.list[[2]] = p1
plot.list[[3]] = p2
pdf("output/map_wildfire_pct_.pdf", width = 10, height = 3)
do.call('grid.arrange',c(plot.list, ncol = 3, top = "", bottom="", left=""))
dev.off()


########################### pm_cases_deaths_top6counties ####################################
### set up 
n.col.grid = 6
file.name = paste0("output/pm_cases_deaths_top6counties_v4.pdf")
fips.unique = unique(dff$FIPS[order(dff$population, decreasing=TRUE)])[1:6]
point.size = 0.6

plot.list = list()
iplot = 1

for (ifips in fips.unique) {
  df.selected = dff[dff$FIPS == ifips, ]
  
  ### visualize PM2.5
  p1 = ggplot() + 
    geom_point(data=df.selected, aes_string(x="date", y="pm25", 
                                            color=shQuote("PM25")), size = point.size) +
    geom_rect(data=df.selected, aes(xmin = start27, xmax = end27,
                                    ymin = pm25_history, ymax = pm25), 
              fill = "orange", colour = "orange", alpha = .001) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          axis.text.x = element_text(size=6), 
          axis.text.y = element_text(size=6), 
          plot.margin = unit(c(0,0,0,0), "cm"), 
          plot.title = element_text(hjust = 0.5, size = 8)) +  
    scale_color_manual(name="", values=c(PM25="blue")) +
    xlab("Date") + ggtitle(paste0(df.selected$County[1], ", ", df.selected$State[1]))
  
  p1 = p1 + theme(legend.position = "none")
  
  plot.list[[iplot]] = p1
  iplot = iplot + 1
  
  ### visualize number of cases
  p2 = ggplot() + 
    geom_point(data=df.selected, aes_string(x="date", y="cases", 
                                            color=shQuote("Cases")), size = point.size) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),  
          axis.title.y = element_blank(),  
          axis.text.x = element_text(size=6), 
          axis.text.y = element_text(size=6), 
          plot.margin = unit(c(0,0,0,0), "cm"), 
          plot.title = element_text(hjust = 0.5, size = 8)) +
    scale_color_manual(name="", values=c(Cases="red")) +
    xlab("Date") + ggtitle(" ")
  
  p2 = p2 + theme(legend.position = "none")
  plot.list[[iplot]] = p2
  iplot = iplot + 1
  
  ### visualize number of deaths
  p3 = ggplot() +
    geom_point(data=df.selected, aes_string(x="date", y="deaths", 
                                            color=shQuote("Deaths")), size = point.size) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),  
          axis.title.y = element_blank(),  
          axis.text.x = element_text(size=6), 
          axis.text.y = element_text(size=6), 
          plot.margin = unit(c(0,0,0,0), "cm"), 
          plot.title = element_text(hjust = 0.5, size = 8)) +
    scale_color_manual(name="", values=c(Deaths="black")) +
    xlab("Date") + ggtitle("")
  p3 = p3 + theme(legend.position = "none")
  
  plot.list[[iplot]] = p3
  iplot = iplot + 1
}
### 
pdf(file.name, width = 10, height = iplot/n.col.grid * 4/3)
do.call('grid.arrange',c(plot.list, ncol = n.col.grid))
dev.off()


############################# pm_vs_historical_LA ##################################
### set up 
file.name = paste0("output/pm_vs_historical_LA_.pdf")
fips.unique = c("6037")
point.size = 1.5
line.size = 0.8

plot.list = list()
iplot = 1

### vis 
for (ifips in fips.unique) {
  df.selected = dff[dff$FIPS == ifips, ]
  
  p1 = ggplot() + 
    geom_rect(data=df.selected, aes(xmin = start27, xmax = end27,
                                    ymin = -Inf, ymax = Inf), fill = "grey", 
              colour = "grey", alpha = 1) +
    geom_rect(data=df.selected[df.selected$pm25_history < df.selected$pm25, ], 
              aes(xmin = start27, xmax = end27, ymin = pm25_history, ymax = pm25), 
              fill = "orange", colour = "orange", alpha = 1) +
    geom_point(data=df.selected, aes_string(x="date", y="pm25", 
                                            color=shQuote("PM25")), size = point.size) +
    geom_line(data=df.selected, aes_string(x="date", y="pm25_history", 
                                           color=shQuote("PMHIST")), size = line.size) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          plot.title = element_text(hjust = 0.5, size = 8)) +
    scale_color_manual(name="", values=c(PM25="blue", PMHIST="black")) +
    xlab("Date") + ggtitle(paste0(df.selected$County[1], ", ", df.selected$State[1]))
  
  p1 = p1 + theme(legend.position = "none")
  
  plot.list[[iplot]] = p1
  iplot = iplot + 1
}

### 
pdf(file.name, width = 10, height = 4)
do.call('grid.arrange',c(plot.list, ncol = 1))
dev.off()

########################### pm_by_states ####################################
### put pm25 and pm25_history in the same column
### 40 weeks, 42 counties in CA, 30 in WA, 20 in OR 
df = dff[, c("pm25", "pm25_history", "date", "State", "wildfire", "hazardmap", "County")]
df = df %>% gather(Year, value, pm25, pm25_history, -State, -County, -date, -wildfire, -hazardmap)
df$Year[df$Year == "pm25"] = "PM2.5 2020"
df$Year[df$Year == "pm25_history"] = "PM2.5 2016-2019"
df$wildfire[df$Year == "2016-2019"] = FALSE
df = df[df$date >= ymd("2020-03-16"), ]
df$weeks = cut(df$date, breaks="week")
df = df %>% 
  group_by(State, weeks) %>% 
  mutate(flag=sum(wildfire, na.rm=T)/(n()/2)*100) %>% 
  ungroup()
df$flag[df$flag==0] = NA

x.lab = unique(reorder(df$weeks, df$date))
x.lab = paste0(month(x.lab), "/", day(x.lab))
x.lab[!((1:length(x.lab))%%2)] = ""
cols <- c("Wildfire Frequency"="orange")

p0 = ggplot(data=df) + 
  geom_boxplot(aes(x=reorder(weeks, date), y=value, fill=Year), 
               lwd=0.1, width=0.8, outlier.shape = NA) + 
  geom_point(aes(x=reorder(weeks, date), y=flag, colour="Wildfire Frequency"), 
             shape=17, size=2) +
  facet_wrap(. ~ State, scales='free', ncol = 1) + 
  theme_bw() + 
  scale_x_discrete("Date", labels = x.lab) + 
  scale_y_continuous(sec.axis = sec_axis(~ log10(.),
                                         name = "Number of Wildfire Days")) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(limits = c(1,1e3), minor_breaks = c(1, 10, 100, 1000)) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        panel.grid.major = element_blank(),
        legend.title = element_blank()) + 
  scale_colour_manual(name="Points", values=cols)
p0
  # ylab(TeX("$PM_{2.5}$ ($\\mu g/m^3$ log scaled)")) +
  # xlab("Date")

file.pdf = "output/pm_by_states_2.pdf"
pdf(file.pdf, width = 10, height = 8)
do.call('grid.arrange',c(list(p0), top = ""))
dev.off()
###############################################################

