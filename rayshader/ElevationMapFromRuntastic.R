library(XML)
library(lubridate)
library(tidyverse)
library(oce)

tcx <- htmlTreeParse("./rayshader/runtastic_20180531_1400_Cycling.tcx", useInternalNodes = T)

features <- c("time", "position/latitudedegrees", "position/longitudedegrees",
              "altitudemeters", "distancemeters")

fnames <- c("dt", "lat", "lon", "alt", "dist")

"//trackpoint/" %>%
  paste0(features) %>%
  map(function(p){xpathSApply(tcx, path=p, xmlValue)}) %>% 
  setNames(fnames) %>% 
  as_data_frame() %>% 
  mutate_at(vars(lat:dist), as.numeric) %>%
  mutate(dt = lubridate::as_datetime(dt),
         ele = despike(alt, reference = "median",n=0.2,k=101)) -> track

 
  
  
ggplot(track) +
  geom_path(aes(lon,lat,color=alt)) +
  coord_fixed() +
  theme_void() +
  scale_colour_gradient(low = "black", high = "white")

ggplot(track) +
  geom_line(aes(x=dt, y=alt))

