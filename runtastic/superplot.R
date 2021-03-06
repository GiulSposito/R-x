# Load packages ----
library(XML)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(gganimate)

# library(devtools)
# devtools::install_github("thomasp85/patchwork")
library(patchwork)

file <- "./runtastic/11654237848.tcx"

pfile <- htmlTreeParse(file = file,
                       error = function (...) {},
                       useInternalNodes = TRUE)

features <- c("time", "position/latitudedegrees", "position/longitudedegrees",
              "altitudemeters", "distancemeters", "heartratebpm/value")

fnames <- c("dt", "lat", "lon", "alt", "dist", "hbpm")

"//trackpoint/" %>%
  paste0(features) %>%
  map(function(p){xpathSApply(pfile, path = p, xmlValue)}) %>%
  setNames(fnames) %>%
  as_data_frame() %>% 
  mutate(
    dt = lubridate::as_datetime(dt),
    lat = as.numeric(lat),
    lon = as.numeric(lon),
    alt = as.numeric(alt),
    dist = as.numeric(dist),
    hbpm  = as.integer(hbpm),
    tm.prev.s = c(0, diff(dt)),
    tm.cum.min  = round(cumsum(tm.prev.s)/60,1)
  ) %>%
  mutate(
    dt = floor_date(dt, "3 minutes")
  ) -> track

bbox <- make_bbox(lon = track$lon, lat=track$lat, f=.1)
gmap <- get_map( location=bbox, maptype = "roadmap", source="google")
ggmap(gmap)

ggplot() + 
  geom_path(data=track, mapping=aes(lon, lat, frame=dt, cumulative=T),
            color="yellow", alpha = 1, size = 0.8, lineend = "round") +
  geom_path(data=track, mapping=aes(lon, lat, frame=dt, cumulative=F),
            size=1.2, lineend = "round", color="red") +
  coord_fixed() +
  theme_void() +
  theme( legend.position = "none" ) -> p1

ggplot(track, aes(dt, alt)) +
  geom_line(aes(frame=dt, cumulative=T)) + 
  theme_bw() -> p2

g <- p1+p2

gganimate(p1, interval=0.01)

gganimate(p, interval=0.01, ani.width=400, ani.height=400)

