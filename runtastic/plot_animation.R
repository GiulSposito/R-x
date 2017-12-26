# set working dir
# setwd("./runtastic")

library(tidyverse)
library(ggplot2)
library(gganimate)

data <- readRDS("./data/gpx_processed.rds")

data %>%
  filter(id %in% 1) %>%
  rename( data.time = time ) %>%
  arrange( data.time ) %>% ggplot() +
  geom_path(aes(lon, lat, group=id, frame=data.time, cumulative=T),
            alpha = 0.3, size = 0.3, lineend = "round") +
  theme_void() ->p

gganimate(p,interval=0.05)
