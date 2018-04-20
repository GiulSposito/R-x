library(zoo)

port %>%
  mutate( operacao = 1:n()) %>%
  inner_join(prices, by=c("ativo"="ticker")) %>%
  group_by(operacao) %>%
  filter( ref.date >= data.compra ) %>%
  mutate(
    valor = case_when( ref.date == data.compra ~ valor.invest,
                       ref.date > data.compra ~ volume.x * price.close )
  ) -> capital



capital %>% View()


port %>%
  mutate( operacao = 1:n()) %>%
  inner_join(proventos, by="ativo") %>%
  filter( data.pagamento >= data.compra ) %>%
  mutate( proventos = volume * valor ) %>%
  group_by( operacao, ativo, data.pagamento ) %>%
  summarise( proventos = sum(proventos, na.rm = T)) %>%
  mutate( proventos.cum = cumsum(proventos) ) %>%
  arrange( ativo, data.pagamento ) %>%
  ungroup() -> rendimentos

rendimentos %>%
  ggplot(aes(data.pagamento, proventos.cum, group=operacao)) +
  geom_line(aes(color=ativo)) -> g1

rendimentos %>%
  mutate( mes = floor_date(data.pagamento, unit="month") ) %>%
  group_by(mes) %>%
  summarise(proventos = sum(proventos)) -> rent

280/50000

capital
port

port %>%
  mutate(mes = floor_date(data.compra, unit="month")) %>%
  group_by(mes) %>%
  summarise( valor.invest=sum(valor.invest)) %>%
  mutate( capital = cumsum(valor.invest) ) %>%
  inner_join(rent) %>%
  mutate( rendimento = proventos/valor.invest )

capital %>%
  mutate( mes = ceiling_date(ref.date,unit="month") ) %>%
  group_by(ativo,mes) %>%
  filter(ref.date==max(ref.date)) %>%
  summarise(valor.invest=sum(valor.invest),
            valor = sum(valor)) %>%
  group_by(mes) %>%
  summarise( valor.invest = sum(valor.invest),
             valor = sum(valor, na.rm = T) ) %>%
  mutate( rend = valor/valor.invest) %>%
  View()
  