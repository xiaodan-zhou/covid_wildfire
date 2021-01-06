library(tidyverse)

dat <- read.delim('movement-range-2020-12-16.txt')

sel <- dat %>%
  filter(country == 'USA' & substr(as.character(polygon_id), 1, 2) %in% c('06', '41', '53'))

clean <- sel %>%
  select(date=ds, fips=polygon_id, 
         relative_change_feb=all_day_bing_tiles_visited_relative_change,
         ratio_travelers=all_day_ratio_single_tile_users)

write.csv(clean, 'movement-range.csv', row.names=F)
