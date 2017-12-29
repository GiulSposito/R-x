# set working dir
# setwd("./runtastic")

library(lubridate)
library(ggplot2)
library(gganimate)
library(tidyverse)

data <- readRDS("./data/gpx_processed.rds")

#### tentando arredondar sem sumir com os pontos
data %>%
  filter( year(time)==2017 ) %>%
  mutate( data.time = floor_date(time, unit="5 minutes") ) %>%
  mutate( id = as.factor(id) ) %>% 
  arrange( data.time ) %>% 
  head(60000) %>%
  ggplot() +
  geom_path(aes(lon, lat, group=id, frame=data.time, cumulative=T),
            alpha = 0.2, size = 0.8, lineend = "round") +
  geom_path(aes(lon, lat, frame=data.time, cumulative=F),
            size=1.2, lineend = "round", color="red") +
  coord_fixed() +
  theme_void() +
  theme( legend.position = "none" ) -> p

p

gganimate(p, interval=0.003, ani.width=800, ani.height=400)
