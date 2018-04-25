library(brotools) # devtools::install_github("b-rodrigues/brotools")
library(mice)
library(tidyverse)

data(boys)

brotools::describe(boys) %>%
  select(variable, type, n_missing, everything())


start <- Sys.time()
imp_boys <- mice(boys, m = 5, maxit = 100, printFlag = FALSE)
end <- Sys.time() - start

print(end)

boys_complete <- mice::complete(imp_boys)

brotools::describe(imp_boys) %>%
  select(variable, type, n_missing, everything())

start <- Sys.time()
imp_boys_purrr <- map(rep(1, 5), ~mice(data = boys, m = ., maxit = 100, printFlag = FALSE))
end <- Sys.time() - start

print(end)

imp_boys_purrr_complete <- map(imp_boys_purrr, mice::complete)
map(imp_boys_purrr_complete, brotools::describe)

imp_boys_purrr <- map2(.x = seq(1,5), .y = imp_boys_purrr_complete, ~mutate(.y, imp_id = as.character(.x)))
imp_boys_purrr <- bind_rows(imp_boys_purrr)

imp_boys_purrr %>%
  brotools::describe() %>%
  select(variable, type, n_missing, everything())

# devtools::install_github("DavisVaughan/furrr")
library(furrr)
plan(multiprocess)

start <- Sys.time()
imp_boys_future <- future_map(rep(1, 5), ~mice(data = boys, m = ., maxit = 100, printFlag = FALSE))
end <- Sys.time() - start

print(end)

