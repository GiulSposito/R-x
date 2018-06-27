library(googlesheets)
library(tidyverse)
library(tsibble)
library(mice)
library(lubridate)
library(tibbletime)
library(forecast)

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


# smothing the measuring using moving average (7 days)
mean_roll_7 <- rollify(mean,7)

# see the smoothing
measures_completed %>%
  mutate( mean_roll_7 = mean_roll_7(weight) ) %>%
  tidyr::gather(type,weight,-date) %>%
  ggplot() + geom_line(aes(date,weight,color=type))
  
# models the time series
model <- measures_completed %>%
  pull(weight) %>%
  as.ts() %>%
  auto.arima()

prediction <- model %>%
  forecast(h=30) %>%
  as.tibble() %>%
  mutate( date = max(measures_completed$date) + 1:30 ) 

prediction %>%
  rename( weight = `Point Forecast`) %>%
  bind_rows(measures_completed) %>%
  ggplot(aes(x=date)) + geom_line(aes(date,weight)) +
  geom_ribbon(aes(ymin=`Lo 80`, ymax=`Hi 80`), alpha=0.2) +
  geom_ribbon(aes(ymin=`Lo 95`, ymax=`Hi 95`), alpha=0.2) +
  geom_point(x=ymd(20180625), y=87)

?geom_ribbon
  
  bind_rows()