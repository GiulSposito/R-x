# set working dir
# setwd("./runtastic")

library(tidyverse)
library(ggplot2)
library(gganimate)
library(lubridate)

data <- readRDS("./data/gpx_processed.rds")

data %>%
  filter( year(time)==2017 ) %>%
  mutate( data.time = floor_date(time, unit="3 minutes") ) %>%
  group_by(id, data.time) %>%
  summarise( lat = min(lat), 
             lon = min(lon), 
             ele = min(ele),
             dist_to_prev = min(dist_to_prev), 
             cumdist = min(cumdist),
             cumtime = min(cumtime) ) %>% 
  ungroup() %>%
  mutate( id = as.factor(id) ) %>% 
  arrange( id, data.time ) %>% ggplot() +
  geom_path(aes(lon, lat, group=id, color=id, frame=data.time, cumulative=T),
            alpha = 0.3, size = 0.7, lineend = "round") +
  coord_fixed() +
  theme_void() +
  theme( legend.position = "none" ) -> p

gganimate(p, interval=0.005, ani.width=800, ani.height=400)


#### tentando arredondar sem sumir com os pontos
data %>%
  filter( year(time)==2017 ) %>%
  mutate( data.time = floor_date(time, unit="5 minutes") ) %>%
  mutate( id = as.factor(id) ) %>% 
  arrange( id, data.time ) %>%
  ggplot() +
  geom_path(aes(lon, lat, group=id, color=id, frame=data.time, cumulative=T),
            alpha = 0.3, size = 0.7, lineend = "round") +
  coord_fixed() +
  theme_void() +
  theme( legend.position = "none" ) -> p