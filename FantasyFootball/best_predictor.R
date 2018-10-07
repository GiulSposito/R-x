library(ffanalytics)
source("./FantasyFootball/score_settings.R")

my_scrap <- readRDS("./FantasyFootball/weekly_scrap_week3.rds")
names(my_scrap)

scrap <- my_scrap$week4

?source_points(scrap, dudes.score.settings)

ffanalytics::projection_sources(scrap, dudes.score.settings)

projections_table(scrap, dudes.score.settings) ->  p

source_points(scrap, dudes.score.settings) -> prev

result <- readRDS("./FantasyFootball/week4_results.rds")


str(prev)
str(result)

result$QB$w

lapply(result, function(pts){
  select(pts, id, weekpts)
}) %>% bind_rows() -> player_stats


inner_join(prev, player_stats) %>% add_player_info() %>% 
  rename(prev.pts = points,
         real.pts = weekpts ) -> comp


comp %>% 
  # filter(pos=="RB") %>% 
  ggplot(aes(x=prev.pts, y=real.pts, color=data_src)) + 
  geom_point() +
  facet_wrap( ~pos ) +
  theme_light()


comp %>% 
  group_by( pos ) %>% 
  mutate( y.bar = mean(real.pts) ) %>% 
  ungroup() %>% 
  mutate( sr = (real.pts - prev.pts)^2 ,
          st = (real.pts - y.bar   )^2 ) %>% 
  group_by( data_src, pos ) %>% 
  mutate( ssr   = sum(sr),
          sstot = sum(st),
          r     = 1 - ssr/sstot ) %>% 
  select( data_src, pos, ssr, sstot, r ) %>% 
  distinct() %>% View()
  
