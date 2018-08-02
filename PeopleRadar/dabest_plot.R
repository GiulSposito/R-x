
filter(CO2, conc=="1000") %>%
  ggplot(aes(x=Type, y=uptake, color=Treatment)) + geom_point()