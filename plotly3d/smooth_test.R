# script to remove the "spiky" data in the elevation record
# setwd("./plotly3d")

# Read in pre-processed data
data <- readRDS("./data/gpx_processed.rds")

# Load packages
library(ggplot2)
library(lubridate)
library(tidyverse)

data %>% 
  filter (id==32) %>%
  mutate ( ele.smth = despike(ele, reference = "median",n=0.2,k=101) ) %>%
  arrange( time ) %>%
  ggplot(aes(x=time)) +
  geom_line(aes(y=ele), color="black") +
  geom_line(aes(y=ele.smth), color="red")
