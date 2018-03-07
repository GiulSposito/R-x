# script de scrapt do MJ
library(rvest)
library(tidyverse)
library(lubridate)

# pesquisa população IBGE
# https://sidra.ibge.gov.br/tabela/6579

read.table("./seguranca/data/tabela6579.csv",
           sep = ";",
           skip=2,
           nrows = 83956,
           encoding = "UTF-8",
           header = F) %>%
  setNames(c("Nivel","Cod","Nome","Mes","Ano","Populacao")) -> pop

str(pop)

issues <- readRDS("./seguranca/data/ocorrencias.rds")
str(issues)
