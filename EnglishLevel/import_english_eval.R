library(googlesheets)
library(tidyverse)
library(lubridate)

# autenti
gs_auth()

# lista a planilhas
plans <- gs_ls(regex = "englishlevel.*", ignore.case = TRUE)

plans %>%
  pull(sheet_key) %>% 
  map( gs_key ) %>% 
  map( gs_read, ws=1) -> evals


bind_rows(evals) %>% View()
saveRDS(evals,"./EnglishLevel/evals.rds")
evals <- readRDS("./EnglishLevel/evals.rds")
eval <- bind_rows(evals)

glimpse(eval)

eval %>%
  select(Login, `CI&T / I`)
