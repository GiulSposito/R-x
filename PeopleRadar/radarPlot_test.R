library(tidyverse)

dtf <- tibble(
  attrib = 1:12,
  eval   = sample(1:10,12,replace = T)
)

dtf %>%
  ggplot(aes(x=factor(attrib),y=eval, fill=factor(attrib))) +
  geom_col(width=1) + coord_polar() + labs(x="",y="") +
  geom_col(width=1, fill = NA, colour = "white") +
  theme( legend.position="none" )
