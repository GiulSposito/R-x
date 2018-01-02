# this script test the ggmap feature
# setwd("./runtastic")

library(lubridate)
library(ggplot2)
library(ggmap)
library(gganimate)
library(tidyverse)

data <- readRDS("./data/gpx_processed.rds")

# mxlat <- max(data$lat)
# mnlat <- min(data$lat)
# mxlon <- max(data$lon)
# mnlon <- min(data$lon)

bbox <- make_bbox(lon = data$lon, lat=data$lat, f=.1)
bbox

sq_map <- get_map( location=bbox, maptype = "terrain", source="google")
ret_map <- get_googlemap(center=c(-46.738256,-22.696416),
                         zoom=12, scale=4,
                         maptype="terrain",
                         size=c(1280,1280),
                         sensor=F)
ggmap(ret_map)



data %>%
  #filter( year(time)==2017 ) %>%
  group_by( id ) %>%
  filter( cumtime == max(cumtime) ) %>%
  arrange(desc(cumtime)) %>%
  head(15) -> top.rides

data %>%
  filter( id %in% top.rides$id ) %>%
  mutate( data.time = floor_date(time, unit="5 minutes") ) %>%
  mutate( id = as.factor(id) ) %>% 
  arrange( data.time ) -> rides

ggmap(sq_map) + 
  geom_path(data=rides, mapping=aes(lon, lat, group=id, frame=data.time, cumulative=T),
            color="yellow", alpha = 0.5, size = 0.8, lineend = "round") +
  geom_path(data=rides, mapping=aes(lon, lat, group=id, frame=data.time, cumulative=F),
            size=1.2, lineend = "round", color="red") +
  coord_fixed() +
  theme_void() +
  theme( legend.position = "none" ) -> p

p

gganimate(p, interval=0.003, ani.width=640, ani.height=640)
