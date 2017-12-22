install.packages("quantmod")
library(quantmod)

library(quantmod)
getSymbols("PETR4.SA",src="yahoo")
chartSeries(PETR4.SA, subset='last 48 months')
addBBands()
