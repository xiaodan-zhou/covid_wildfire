setwd("/Users/mac/Documents/GitHub/covid_wildfire")
source("scr/Utilities.R")
library(sf)
library(viridis)

dff = load.data()
df = df[df$date <= "2020-11-26", ]
dff = dff %>% group_by(FIPS) %>% summarise(pm25.high = sum(pm25>=12, na.rm=T), 
                                           hazard = sum(hazardmap>=27, na.rm=T))

cty = read_sf('data/cb_2018_us_county_5m', 'cb_2018_us_county_5m') %>%
  filter(STATEFP %in% c('06', '41', '53'))
cty$FIPS = as.numeric(as.character(cty$GEOID))

cty.selected = merge(cty, dff, by="FIPS", all.x=T)

palette = "viridis"

###############################################################
### set up 
n.col.grid = 3
file.name = paste0("output/vis.map_pm_above12.pdf")
plot.list = list()

vis = "pm25.high"
ylim = c(0, max(dff$pm25.high))

### WA 
p0 = 
  ggplot(cty.selected[cty.selected$STATEFP == "53", ]) + ggtitle("Washington") + 
  geom_sf(aes_string(fill=vis), color=NA) + 
  theme_void() + 
  scale_fill_gradient(low="blue", high="red", na.value = "lightgrey", limits=ylim) + 
  theme(legend.position = "None")
plot.list[[1]] = p0

### OR 
p1 = ggplot(cty.selected[cty.selected$STATEFP == "41", ]) + ggtitle("Oregon") + 
  geom_sf(aes_string(fill=vis), color=NA) + 
  theme_void() + 
  scale_fill_gradient(low="blue", high="red", na.value = "lightgrey", limits=ylim) + 
  theme(legend.position = "None")
plot.list[[2]] = p1

### CA
p2 = ggplot(cty.selected[cty.selected$STATEFP == "06", ]) + ggtitle("California") + 
  geom_sf(aes_string(fill=vis), color=NA) + 
  theme_void() + 
  scale_fill_gradient(low="blue", high="red", na.value = "lightgrey", limits=ylim, 
                      name = "Number of days")
plot.list[[3]] = p2

### 
pdf(file.name, width = 10, height = 4)
do.call('grid.arrange',c(plot.list, ncol = 3, top = "", bottom="", left=""))
dev.off()






###############################################################
### set up 
n.col.grid = 3
file.name = paste0("output/vis.map_hazard27.pdf")
plot.list = list()

vis = "hazard"
ylim = c(0, max(dff$hazard))

### WA 
p0 = 
  ggplot(cty.selected[cty.selected$STATEFP == "53", ]) + ggtitle("Washington") + 
  geom_sf(aes_string(fill=vis), color=NA) + 
  theme_void() + 
  scale_fill_gradient(low="blue", high="red", na.value = "lightgrey", limits=ylim) + 
  theme(legend.position = "None")
plot.list[[1]] = p0

### OR 
p1 = ggplot(cty.selected[cty.selected$STATEFP == "41", ]) + ggtitle("Oregon") + 
  geom_sf(aes_string(fill=vis), color=NA) + 
  theme_void() + 
  scale_fill_gradient(low="blue", high="red", na.value = "lightgrey", limits=ylim) + 
  theme(legend.position = "None")
plot.list[[2]] = p1

### CA
p2 = ggplot(cty.selected[cty.selected$STATEFP == "06", ]) + ggtitle("California") + 
  geom_sf(aes_string(fill=vis), color=NA) + 
  theme_void() + 
  scale_fill_gradient(low="blue", high="red", na.value = "lightgrey", limits=ylim, 
                      name = "Number of days")
plot.list[[3]] = p2

### 
pdf(file.name, width = 10, height = 4)
do.call('grid.arrange',c(plot.list, ncol = 3, top = "", bottom="", left=""))
dev.off()
