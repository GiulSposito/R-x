library(googlesheets)
library(magrittr)
library(dplyr)
library(gglot2)

# reforca autenticacao
gs_auth()

# planilha com os dados
ss <- gs_key("185vwmR_Cvw1lzZTHK0cUYT5tXKf7GrwX-JZPwzms-JA")

# dataset 
ss %>%
  gs_read(ws="Projects") -> proj

# metricas
ss %>% 
  gs_read(ws="Data") %>%
  mutate( prod.burn  = `Worklog Burn`/`BCPs In DEV`,
          prod.build = `Worklog Build`/`BCPs Potential`,
          prod.all   = `Worklog All Work` / metr$`BCPs Activated` ) -> metr
  

metr %>%
  filter(  `Project Id` == "a0bfa309-7d6a-4446-b057-d8508d2a8053",
    Customer=="Dasa") %>%
  select( Period, prod.burn, prod.build, prod.all)
  


metr %>%
  mutate( prod.burn =  `Worklog Burn`/`BCPs In DEV` ) %>%
  filter( prod.all < 50 ) %>%
  select( prod.all ) %>%
  unlist() %>%
  summary()


  hist(col="red",breaks=50)
