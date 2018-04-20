library(tidyverse)

cart <- readRDS("./fii/data/carteira.rds")


readRDS("./fii/data/cotacoes_fii.rds") %>%  
  group_by(ativo) %>%
  filter(cot.data == max(cot.data)) -> cotacoes


proventos <- readRDS("./fii/data/proventos.rds")


cart %>%
  inner_join(cotacoes, by="ativo") %>%
  mutate(
    custodia = volume * cot.ult
  ) %>%
  View()


setdiff(cart$ativo, intersect(cart$ativo, cotacoes$ativo))
setdiff(cart$ativo, intersect(cart$ativo, proventos$ativo))
