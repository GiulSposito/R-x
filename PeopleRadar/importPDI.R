library(googlesheets)
library(tidyverse)

gs_auth()
  
fichas <- tribble(
  ~login, ~key,
  "fferraz","15EpANnX7bPA6HcOCpcGNeSpjZnGOXuxDQvMapvO2Ck0",
  "ptirado","1-MLAX17AyrLUsC3a9tScnnjH3ZZrcJlLN0nm-AfHlEc"
)

fichas %>%
  pull(key) %>%
  map(gs_key) %>%
  map(gs_read, ws="Proficiência no Papel")

fichas %>%
  mutate( eval = map(key,gs_key) %>% map(gs_read, ws="Proficiência no Papel") ) %>%
  unnest() -> evals

names(evals)[3] <- "skill"

glimpse(evals)

evals %>%
  mutate( 
    N1 %>% gsub("%","",.) %>% as.numeric(),
  )
