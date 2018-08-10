# devtools::install_github("tylermorganwall/rayshader")
# install.packages("rgdal") # rastering TIFF images
# install.packages("elevatr")

library(rayshader)
library(magrittr)
library(elevatr)


eltif <- raster::raster("./rayshader/dem_01.tif")

elmat <- matrix(
  raster::extract(eltif,raster::extent(eltif),buffer=1000),
  nrow=ncol(eltif),ncol=nrow(eltif))

#We first texture the map with sphere_shade and one of rayshader's built in textures, "desert."
#By default, the highlight is towards the NW.
elmat %>%
  sphere_shade(texture = "desert") %>%
  plot_map()

#sphere_shade can shift the sun direction to the NE:
elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  plot_map()

#detect_water and add_water adds a water layer to the map:
elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  plot_map()

#And we can add a raytraced layer from that sun direction as well:
elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  add_shadow(ray_shade(elmat,sunangle = 45)) %>%
  plot_map()

#And finally, we add an ambient occlusion layer:
elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  add_shadow(ray_shade(elmat,sunangle = 45)) %>%
  add_shadow(ambient_shade(elmat)) %>%
  plot_map()

# trying 3D plot
elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  add_shadow(ray_shade(elmat,sunangle = 45)) %>%
  add_shadow(ambient_shade(elmat)) %>%
  plot_3d(elmat/30, solid=T, shadow = T, water=T, wateralpha = 0.5)
  

#or from the elevatr package
elevation <- get_elev_raster(lake, z = 11, src = "aws")

elmat2 = matrix(raster::extract(elevation,raster::extent(elevation),buffer=10000),nrow=ncol(elevation),ncol=nrow(elevation))

elmat2 %>%
  sphere_shade(sunangle=35, texture="desert") %>% 
  add_water(detect_water(elmat2), color="imhof2") %>%
  plot_map()

# other
volcano %>%
  sphere_shade() %>% # add shade (texture)
  add_water(detect_water(volcano)) %>% # add "water level"
  add_shadow(ray_shade(volcano)) %>% # add shadow
  add_shadow(ambient_shade(volcano)) %>% # add occlusion
  plot_map()


data_frame(
  x = rnorm(100),
  y = rnorm(100),
  z = rnorm(100, mean = 20, sd = 10)
) %>%
  ggplot(aes(x=x,y=y,z=z)) +
  geom_point(aes(color=z), show.legend = F) +
  theme_void()

ggsave("./rayshader/myPlot.png")

eltif <- raster::raster("./rayshader/myPlot.png")

elmat <- matrix(
  raster::extract(eltif,raster::extent(eltif),buffer=1000),
  nrow=ncol(eltif),ncol=nrow(eltif))*-1

#And finally, we add an ambient occlusion layer:
elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  add_shadow(ray_shade(elmat,sunangle = 45)) %>%
  add_shadow(ambient_shade(elmat)) %>%
  plot_3d(elmat, solid=T, shadow = T)

