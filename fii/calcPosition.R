library(plotly)

source("./fii/getPortfolio.R")
source("./fii/cotacoesGetBatchSymbols.R")

port <- getPortfolio() %>%
  mutate( operacao = 1:n())

prices <- getPrices()


port %>%
  inner_join(prices, by=c("ativo"="ticker")) %>%
  group_by(operacao) %>%
  filter( ref.date >= data.compra ) %>%
  mutate(
    valor = case_when( ref.date == data.compra ~ valor.invest,
                       ref.date > data.compra ~ volume.x * price.close )
  ) %>% 
  ggplot(aes(ref.date,valor, group=operacao)) +
  geom_line(aes(color=ativo)) -> p

  ggplotly(p)

position %>%
  filter(ativo=="VLOL11") %>%
  View()
