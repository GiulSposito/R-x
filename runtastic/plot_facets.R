# Plot activities as small multiples
devtools::install_github("marcusvolz/ggart")

# Load packages
library(ggart) # devtools::install_github("marcusvolz/ggart")
library(tidyverse)

# Read in pre-processed data
data <- readRDS("./data/gpx_processed.rds")

# Create plot
ggplot() +
  geom_path(aes(lon, lat, group = id), data, size = 0.35, lineend = "round") +
  facet_wrap(~id, scales = "free", labeller = function(...){return("")}) +
  theme_void() +
  theme(panel.spacing = unit(0, "lines"))

# Save plot
ggsave("./plots/facets001.png", p, width = 20, height = 20, units = "cm")