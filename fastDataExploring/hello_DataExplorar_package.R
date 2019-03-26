library(tidyverse)

mtcars %>% 
  mutate( car.name = row.names(.) ) %>% 
  as.tibble() 
