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
             total.dist = max(cumdist, na.rm=T),
             total.dur  = max( cumtime )) %>%
  arrange( desc(total.dist) ) -> ids

View(ids)
# id %in% c(34,35,26,32,8,12,14),




## p
data %>%
  filter( ele >= 550, ele <= 1200,
          (id %in% ids$id[1:10])) %>%
  mutate( id = as.factor(id),
          lon = -lon,
          ele.smth = despike(ele, reference = "median",n=0.2,k=101) ) %>%
  plot_ly(x = ~lat, y = ~lon, z = ~ele.smth, color=~id) %>%
  add_paths() %>%
  layout(scene = list(xaxis = list(title = 'lat'),
                      yaxis = list(title = 'lon'),
                      zaxis = list(title = 'ele'),
                      aspectratio = list(x=1, y=2, z=0.2))) -> p


p

# remenber to follow the account setup: https://plot.ly/r/getting-started/
link <- api_create(p, sharing = "public")

# chart_link = plotly_POST(p, filename="runtastic/elevation")

# 22.68624-22.65500   -> 3000 km
# 0.03124 -> 300




