library(ffanalytics)
library(forcats)

glimpse(player_table)
str(player_table)


my_scrap <- scrape_data(src = c("CBS", "ESPN", "Yahoo"), 
                        pos = c("QB", "RB", "WR", "TE", "DST"),
                        season = 2018, week = 3)

str(my_scrap)

my_projections <- projections_table(my_scrap)
str(my_projections)

my_projections <- my_projections %>% 
  add_ecr() %>% 
  add_risk() %>%
  add_adp() %>% 
  add_aav()

my_projections %>% 
  inner_join(player_table) %>% 
  mutate( full_name=paste0(first_name, " ", last_name),
          tier = as.factor(tier) ) %>% 
  filter(pos=="DST", avg_type=="robust", points>1) %>% 
  head(32) %>% 
  ggplot(aes(x=reorder(full_name,points), group=tier)) +
  geom_pointrange(aes(y=points, ymin=floor, ymax=ceiling, color=tier)) +
  theme_light() + xlab(" ") +
  coord_flip()

my_projections %>% pull(avg_type) %>% unique()  
