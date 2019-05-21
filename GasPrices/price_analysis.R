library(tidyverse)
library(lubridate)
prices <- readRDS("./GasPrices/data/precos.rds")


prices %>% glimpse()

prices %>% 
  group_by(produto, data) %>% 
  summarise(
    preco = mean(valor_venda, na.rm = T)
  ) %>% 
  ggplot(aes(x=data, y=preco, group=produto))+
  geom_line(aes(color=produto)) +
  theme_minimal()



prices %>% 
  ggplot(aes(x=data, y=valor_venda)) +
  geom_point(aes(color=produto)) +
  theme_minimal()


prices$produto %>% head(50)
prices %>% 
  filter(!(produto %in% c("DIESEL", "DIESEL S10", "ETANOL", "GASOLINA", "GNV"))) %>% 
  View()


prices$data %>% 
  hist(breaks=360)
