# install.packages("alluvial")

library(alluvial)
library(tidyverse)
library(xlsx)
library(reshape2)

dt.hoshin <- read.xlsx("./alluvial_diagram/dados.xlsx", sheetIndex = 1) %>%
  filter(!is.na(Role)) %>%
  as.tibble()


1:nrow(dt) %>%
  map_df(function(i,.dt=dt){
    tibble(
      Role  = rep(.dt[i,]$Role, as.integer(.dt[i,]$Value)),
      State = rep(.dt[i,]$State, as.integer(.dt[i,]$Value)),
      Freq  = rep(1, as.integer(.dt[i,]$Value))
    )
  }) -> dt.alluv

dt.alluv %>%
  dcast(State~Role) -> x

alluvial(x)

