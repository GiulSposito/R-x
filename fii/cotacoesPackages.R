library(BatchGetSymbols)
library(GetHFData)
source("./fii/getPortfolio.R")


port <- getPortfolio()

tickers <- unique(paste0(port$ativo, ".SA"))

cotacoes <- BatchGetSymbols::BatchGetSymbols(tickers)

install.packages("GetHFData")


df.out <- ghfd_get_HF_data(c("VISC11","TBOF11","MALL11"), first.date = ymd(20180401), last.date = ymd(20180413),
                           type.output = "raw")

df.out <- ghfd_get_HF_data(
  c("VISC11","TBOF11","MALL11"),
  first.date = ymd(20180412),
  last.date = ymd(20180413),
  type.market = "BMF",
  first.time = "10:00:00",
  last.time = "18:00:00",
  type.output = "agg",
  agg.diff = "1 day"
)

df.out <- ghfd_get_HF_data("TBOF11", first.date = ymd(20180401), last.date = ymd(20180413),
                           type.output = "agg", agg.diff = "1 day",
                           #type.market = "BMF",
                           first.time = "00:00:00", last.time = "23:59:59")
