library(zoo) 
library(lubridate)
library(plotly)
library(tidyverse)

source("./fii/proventos.R")
source("./fii/portfolio.R") 

prov <- getProventos()
port <- getPortfolio()

port %>%
  mutate( data.update=data.compra,
          valor = 0, 
          data.pagamento = data.compra, 
          data.base = data.compra, 
          cota.base = cotacao.compra,
          rendimento = 0 ) %>%
  select( ativo, data.update, valor, 
          data.pagamento, data.base, cota.base,
          rendimento) %>%
  bind_rows(prov) %>%
  arrange(data.base) -> proventos

proventos %>%
  group_by(ativo, data.pagamento, data.base, cota.base) %>%
  summarise(valor=sum(valor)) -> prv

port %>%
  mutate( operacao = 1:n() ) %>%
  inner_join( prv, by="ativo" ) %>%
  filter( data.pagamento >= data.compra  ) %>%
  mutate( total.proventos = volume * valor ) %>%
  group_by( operacao ) %>%
  mutate( proventos.acumulados = cumsum(total.proventos) ) %>%
  ungroup() %>%
  mutate( capital = volume * cota.base, 
          posicao = capital + proventos.acumulados, 
          rendimentos = round(100*(posicao-valor.invest)/valor.invest,1)) -> pos

pos %>%
  ggplot(aes(data.base, rendimentos, group=operacao)) +
  geom_line(aes(color=ativo)) +
  ylab("Rendimento (%)") -> g

ggplotly(g)
  
pos %>%
  mutate( mes.base = floor_date(data.base,unit="month")) %>%
  group_by(ativo, mes.base) %>%
  summarise(valor.invest=sum(valor.invest), 
            posicao = sum(posicao)) %>% View()
  glimpse() 
  

pos %>%
  group_by(operacao, ativo) %>%
  filter( data.base == max(data.base) ) %>%
  mutate( duracao.dias = (data.base-data.compra) ) %>%
  select(operacao, data.compra, ativo, cotacao.compra, volume, valor.invest,
         data.base, cota.base, capital, proventos.acumulados,
         posicao, rendimentos, duracao.dias) %>%
  mutate ( valor.invest = sum(valor.invest),
             capital = sum(capital), 
             proventos = sum(proventos.acumulados),
             posicao = sum(posicao) ) %>%
  mutate( rendimento.capital = round(100*(capital-valor.invest)/valor.invest,1),
          rendimento.proventos = round(100*proventos/valor.invest,1),
          rendimento.total = round(100*(posicao-valor.invest)/valor.invest,1) ) %>%
  select( operacao, ativo, duracao.dias, rendimento.capital, rendimento.proventos, rendimento.total)

