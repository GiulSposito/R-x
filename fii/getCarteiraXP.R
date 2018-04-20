library(tidyverse)
library(lubridate)
library(tabulizer)

cnames <- c("cart.peso","fii.ticker", "fii.patrimonio",
            "fii.valor.mercado", "fii.valor.patrimonial",
            "fii.vm_vp", "fii.provento.ultimo","fii.provento.dy","fii.provento.dy12",
            "fii.provento.dy12.proj","fii.vacancia.anunciada")

read_delim("./fii/files/xp_capital.txt", delim=" ", na = "-",
           col_names = cnames, col_types = "ccccccccccc") %>%
  mutate( cart.tipo = "capital" ) -> cart.capital

read_delim("./fii/files/xp_renda.txt", delim=" ", na = "-",
           col_names = cnames,col_types = "ccccccccccc") %>%
  mutate( cart.tipo = "renda" ) -> cart.renda


View(cart.capital)
