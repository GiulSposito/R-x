# script to plot a path in 3D
# setwd("./plotly3d")

library(ggplot2)
library(oce)
library(tidyverse)

# Read in pre-processed data
data <- readRDS("./data/gpx_processed.rds")

# selecting one ride
data %>%
  group_by( id ) %>%
  summarise( max.ele = max(ele, na.rm = T),
             min.ele = min(ele, na.rm = T), 
             mean.ele = mean(ele, na.rm=T), 
             sd.ele   = sd(ele, na.rm = ), 
             total.dist = max(cumdist, na.rm=T)) %>%
  arrange( max.ele, min.ele ) %>%
  View()

## p
data %>%
  filter(id %in% c(34,35,26,32,8,12,14), ele >= 600, ele <= 1000) %>%
  mutate( id = as.factor(id),
          lon = -lon,
          ele.smth = despike(ele, reference = "median",n=0.2,k=101) ) %>%
  plot_ly(x = ~lat, y = ~lon, z = ~ele.smth, color=~id) %>%
  add_paths() %>%
  layout(scene = list(xaxis = list(title = 'lat'),
                      yaxis = list(title = 'lon'),
                      zaxis = list(title = 'ele'),
                      aspectratio = list(x=1, y=2, z=0.5)))
?layout
