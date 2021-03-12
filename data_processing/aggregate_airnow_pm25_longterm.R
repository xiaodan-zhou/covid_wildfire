## Updated by Matt Cooper
## From https://github.com/NSAPH/data_requests/tree/master/request_projects/may2020_airnow_api_no2

library(data.table)
library(tigris)
library(sf)
library(lubridate)
library(dplyr)

WRITEPATH = "/home/mattcoop/covid_wildfire/data/" 
READPATH = "/home/mattcoop/covid_wildfire/data_processing/airnow_raw/"

airnow_names <- c("lat",
                  "lon",
                  "datetime",
                  "parameter",
                  "concentration",
                  "unit",
                  "site_name",
                  "site_agency",
                  "aqs_id",
                  "full_aqs_id")

airnow_classes <- c("numeric",
                    "numeric",
                    "character",
                    "character",
                    "numeric",
                    "character",
                    "character",
                    "character",
                    "character",
                    "character")

monitor_data <- rbindlist(lapply(list.files(READPATH, full.names = T, pattern='csv'), 
                                 function(x) fread(x, colClasses=airnow_classes)))
names(monitor_data) <- airnow_names

monitor_data[, day := date(ymd_hm(datetime))]
monitor_data <- monitor_data[, .(pm25 = mean(concentration, na.rm = T), 
                                 lat = max(lat), lon = max(lon)), by = c("full_aqs_id", "day")]

monitor_data <- st_as_sf(monitor_data, coords = c("lon","lat"))
st_crs(monitor_data) <- "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"

# join with states
geom <- st_as_sf(states())
monitor_data <- st_join(monitor_data, geom, left = F)
monitor_data <- select(monitor_data, full_aqs_id, day, pm25, state = STUSPS)

# join with counties
geom <- st_as_sf(counties())
monitor_data <- st_join(monitor_data, geom)
monitor_data <- select(monitor_data, full_aqs_id, day, pm25, state, fips = GEOID)

monitor_data[ , c("X", "Y")] <- st_coordinates(monitor_data)

fwrite(st_drop_geometry(monitor_data), paste0(WRITEPATH, "daily_pm_longterm.csv"), row.names=F)

#write coordinates for earth engine
uu <- monitor_data %>% select(X, Y) %>% unique
str <- "var geometry = /* color: #d63000 */ee.Geometry.MultiPoint([\n"
for (i in 1:nrow(uu)){
  str <- paste0(str, '[', uu$X[i], ', ', uu$Y[i], '],\n')
}
str <- paste0(str, ']);')
cat(str, file=paste0(WRITEPATH, 'ee_pts.js'))
