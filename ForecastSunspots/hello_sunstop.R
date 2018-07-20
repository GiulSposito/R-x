library(tidyverse)
library(forecast)
library(tsibble)
library(lubridate)
library(prophet)

# the sunspot dataset
sp <- datasets::sunspot.month

# see what is 
sp %T>%
  str()  %>%
  plot()

# convert to a tibble
sp %>%
  as_tsibble() %>%
  mutate(index=as_date(index)) %>%
  rename(ds=index, y=value) -> sp.dt

# check data range
summary(sp.dt)

# data to "train"
sp.train <- sp.dt %>%
  filter(ds <= ymd(20120901) )

# data to predict
sp.test <- sp.dt %>%
  filter(ds > ymd(20120901) )

# model
model <- prophet(sp.train)
pred  <- make_future_dataframe(model, periods = 12)
forecast <- predict(model,pred)

plot(model,forecast)
prophet_plot_components(model,forecast)
