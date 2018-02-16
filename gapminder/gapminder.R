library(gapminder)
library(tidyverse)
library(broom)


gapminder %>%
  mutate( year1950 = year - 1950) %>%
  group_by( continent, country ) %>%
  nest() -> gap

fit_model <- function(gapdata){
  lm(lifeExp ~ year1950, data = gapdata) 
}

gap %>%
  mutate( model = data %>% map(fit_model) ) %>%
  mutate(
    gancle  = model  %>% map(broom::glance),
    tidy    = model  %>% map(broom::tidy),
    augment = model  %>% map(broom::augment),
    rsq     = gancle %>% map_dbl("r.squared")
  ) -> gap


gap %>%
  ggplot(aes(rsq,reorder(country, rsq))) +
  geom_point(aes(color=continent))


gap %>%
  unnest(tidy) %>%
  select(continent, country, term, estimate, rsq) %>%
  spread(term, estimate) %>%
  ggplot(aes(`(Intercept)`, year1950) ) + 
    geom_point(aes(color=continent, size=rsq)) +
    geom_smooth(se=F) +
    ylab("Life Expectance") +
    ylab("Yearly Improvement") +
    scale_size_area()
