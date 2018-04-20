source("./fii/getPortfolio.R")
source("./fii/getProventos.R")
source("./fii/getCotacoes.R")

operacoes <- getPortfolio()
proventos <- getProventos()
cotacoes  <- getCotacoes()


operacoes %>%
  left_join(proventos, by="ativo") %>%
  View()

 %>%
  


operacoes %>%
  left_join(filter(cotacoes, cot.data == max(cot.data)), by="ativo") %>%
  View()


filter(cotacoes, ativo=="BBVJ11")
