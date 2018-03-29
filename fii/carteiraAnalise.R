library(tidyverse)

cart <- readRDS("./fii/data/carteira.rds")
cotacoes <- readRDS("./fii/data/cotacoes_fii.rds")
proventos <- readRDS("./fii/data/proventos.rds")


cart %>%
  inner_join(cotacoes, by="ativo") %>%
  mutate(
    custodia = volume * cot.ult
  ) %>%
  View()


setdiff(cart$ativo, intersect(cart$ativo, cotacoes$ativo))
setdiff(cart$ativo, intersect(cart$ativo, proventos$ativo))
