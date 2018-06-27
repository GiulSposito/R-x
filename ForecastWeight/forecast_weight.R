library(googlesheets)
library(tidyverse)
library(tsibble)
library(mice)
library(lubridate)

# download data from google spreadsheets
gs_auth()

raw_data <- gs_key("1P1q58DYs4Jy5cXKXCrdl11ru4Rop1Mu7r8fXEraCX9M") %>%
  gs_read_csv(ws=1)

# handles date/weight
measures <- raw_data %>%
  select(1:2) %>%
  mutate(Peso=Peso/10) %>%
  set_names(c("date","weight"))

head(measures, 20)

weight.target <- measures %>%
  filter( date > ymd(20180601) )

measures <- measures %>%
  filter( date <= ymd(20180601) )

# explicit NA

measures %>%
  as_tsibble() %>%
  fill_na() -> measures

head(measures,20)

# complete missing values

# see missing values
md.pattern(measures)

# complete values
measures %>%
  mice(method = "pmm", m=5, maxit = 50, seed=42) %>% # five imputation for missing point
  mice::complete("long") %>%
  group_by(date) %>% # average them 
  summarise( weight = mean(weight) ) -> measures_completed

# compare original data and missing values
measures_completed %>%
  inner_join(measures, by="date") %>%
  set_names(c("date","completed","original")) %>%
  tidyr::gather(type,weight,-date) %>%
  ggplot() + geom_point(aes(date,weight,color=type))

