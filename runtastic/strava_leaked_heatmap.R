# Plot activities as small multiples

# Load packages
library(tidyverse)

# Read in pre-processed data
data <- readRDS("./runtastic/data/gpx_processed.rds")

# Specify lat / lon window
lon_min <- min(data$lon)
lon_max <- max(data$lon)
lat_min <- min(data$lat)
lat_max <- max(data$lat)

# Create plot
p <- ggplot(data) +
  geom_path(aes(lon, lat, group = id),
            alpha = 0.8, size = 0.3, lineend = "round",
            colour="darkred") +
  geom_path(aes(lon, lat, group = id),
            alpha = 0.2, size = 0.3, lineend = "round",
            colour="yellow") +
  geom_path(aes(lon, lat, group = id),
            alpha = 0.05, size = 0.3, lineend = "round",
            colour="white") +
  coord_fixed() +
  theme_void() +
  theme( panel.background = element_rect(fill = "black", color  =  NA) )

p

# Save plot
ggsave("./map001.png", p, width = 30, height = 15, units = "cm", dpi = 600)

?heatmap
