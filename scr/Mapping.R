library(rstudioapi)
project.dir = dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(project.dir)
source("scr/Utilities.R")
library(sf)
library(viridis)

dff = load.data()
df = df[df$date <= "2020-11-26", ]
dff = dff %>% group_by(FIPS) %>% summarise(pm25.high = sum(pm25>=35, na.rm=T), 
                                           hazard = sum(hazardmap>=27, na.rm=T))
ndays = 277 
dff$pm25.high = dff$pm25.high / ndays * 100

cty = read_sf('data/cb_2018_us_county_5m', 'cb_2018_us_county_5m') %>%
  filter(STATEFP %in% c('06', '41', '53'))
cty$FIPS = as.numeric(as.character(cty$GEOID))

cty.selected = merge(cty, dff, by="FIPS", all.x=T)
cty.selected$hazard_pct = cty.selected$hazard / ndays * 100

palette = "viridis"

###############################################################
### set up 
n.col.grid = 3
file.name = paste0("output/vis.map_pm_above35.pdf")
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
                      name = "Percent of days")
plot.list[[3]] = p2

### 
pdf(file.name, width = 10, height = 4)
do.call('grid.arrange',c(plot.list, ncol = 3, top = "", bottom="", left=""))
dev.off()






###############################################################
### set up 
n.col.grid = 3
plot.list = list()

# file.name = paste0("output/vis.map_hazard27.pdf")
# vis = "hazard"
# ylim = c(0, max(dff$hazard))
# legand.lab = "Number of days"

file.name = paste0("output/vis.map_hazard27_pct2.pdf")
vis = "hazard_pct"
ylim = c(0, max(cty.selected$hazard_pct, na.rm=T))
legand.lab = "% of wildfire days"


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
                      name = legand.lab)
plot.list[[3]] = p2

### 
pdf(file.name, width = 10, height = 4)
do.call('grid.arrange',c(plot.list, ncol = 3, top = "", bottom="", left=""))
dev.off()
