library(tidyverse)
library(googlesheets)
library(lubridate)


parseRealValue <- function(x) parse_number(
    gsub(pattern = "R$ *", replacement = "", x = x), 
    locale=locale(grouping_mark=".", decimal_mark=",")
  )

key <- "1k0u_xV21AUEBzfi_e8rZtiAgEJD2OGsQu0QW-IJ_kCU"
gs_auth(new_user = T)

gs_key(key) %>%
  gs_read(ws=1) %>%
  as.tibble() %>%
  select(Ativo, Qtd, Valor, Taxas, Total, Carteira, Data) %>%
  setNames(c("ativo","volume","cotacao","taxas", "valor", "carteira", "data")) %>%
  filter(complete.cases(.)) %>%
  mutate(
    cotacao = parseRealValue(cotacao),
    taxas   = parseRealValue(taxas),
    valor   = parseRealValue(valor),
    carteira = as.factor(carteira),
    data    = ymd(data)
  ) %>%
  saveRDS("./fii/data/carteira.rds")
