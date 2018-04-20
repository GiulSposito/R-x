https://www.coinvalores.com.br/produtos_e_servicos/fundos_imobiliarios/distribuicao

library(tabulizer)
library(tidyverse)
library(reshape2)

prov.2017 <- extract_tables("./fii/files/proventos_2017.pdf")
cname <- c("fundo", as.character(seq.Date(ymd(20170101), ymd(20171201), "month")))

prov.2017[[1]] %>%
  as.tibble() %>%
  setNames(cname) %>%
  mutate( fundo = as.factor(fundo) ) %>%
  mutate_if(is.character, .parseNumPtBr) -> x

x %>%
  reshape2::melt(id="fundo", variable.name="ref.date", value.name="provento") %>%
  as.tibble()


  
  

  
  
  








