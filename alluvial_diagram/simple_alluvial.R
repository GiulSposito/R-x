library(alluvial)
library(magrittr)
library(xlsx)
tit <- as.data.frame(Titanic)

tit2d <- aggregate( Freq ~ Class + Survived, data=tit, sum)


alluvial( tit2d[,1:2], freq = tit2d$Freq, 
          xw=0.0, alpha=0.8,
          gap.width = 0.1, col="steelblue", border="white",
          layer=tit2d$Survived != "Yes")


alluvial( tit[,1:4], freq=tit$Freq, border=NA,
          hide = tit$Freq < quantile(tit$Freq, .50),
          col=ifelse( tit$Survived == "No", "red", "gray") )


dt.hoshin <- read.xlsx("./alluvial_diagram/dados.xlsx", sheetIndex = 1) %>%
  na.omit() %>%
  as.data.frame()


alluvial( dt.hoshin[,1:4], freq=tit$Freq, border=NA,
          hide = tit$Freq < quantile(tit$Freq, .50),
          col=ifelse( tit$Survived == "No", "red", "gray") )



dt.hoshin
