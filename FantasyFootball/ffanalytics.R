library(ffanalytics)
source("FantasyFootball/score_settings.R")

# src = c("CBS", "Yahoo", "ESPN", "NFL", "FFToday",
#         "NumberFire", "FantasyPros", "FantasySharks",
#         "FantasyFootballNerd", "FleaFlicker"), 

my_scrap <- scrape_data(pos = c("QB", "RB", "WR", "TE", "DST", "K"),
                        season = 2018, 
                        week = 6)


my_scrap %>% 
  saveRDS("./FantasyFootball/week7_scrap.rds")

my_projections <- projections_table(my_scrap, scoring_rules = dudes.score.settings) 
  #%>% saveRDS("./FantasyFootball/my_projects.rds")

my_stats <- ffanalytics::aggregate_stats(my_scrap)

my_projections$avg_type %>% unique()

my_projections %>% 
  filter(avg_type=="weighted") %>% 
  add_player_info() %>% 
  mutate( full_name = paste0(first_name, " ", last_name),
          tier = as.factor(tier) ) %>%
  group_by( id ) %>% 
  mutate( r_pos = max(ceiling, points) ) %>% 
  ungroup() %>% 
  filter( !(team %in% c("FA","FA*")) ) %>% 
  arrange(pos_rank) %>% 
  head(30) %>% 
  ggplot(aes(group=tier)) +
  geom_pointrange(aes(x=reorder(pos_rank,desc(pos_rank)),
                      y=points, 
                      ymin=floor, 
                      ymax=ceiling, 
                      color=tier)) +
  geom_text(aes(x=reorder(pos_rank,desc(pos_rank)), 
                y=r_pos, 
                color=tier, 
                label=last_name),nudge_y = 1, nudge_x = 0.1, size=3) +
  theme_classic() + xlab("Rank") + ylab("Points") +
  theme( panel.grid.major.x=element_line(colour="#EEEEEE"),
         legend.position="none" ) +
  scale_x_discrete(breaks = seq(1,32,4) ) +
  ggtitle("Quaterback",paste0("week4")) +
  coord_flip()

?waiver


my_projections %>% 
  add_ecr() %>% 
  add_risk() %>%
  add_player_info() %>% 
  mutate(
    full_name =paste0(first_name, " ", last_name),
    tier = as.factor(tier)
  ) -> proj.week2
  

proj.week2 %>% 
  filter(pos=="QB", avg_type=="weighted") %>%
  arrange(desc(tier), desc(points)) %>%
  plot_ly(type = "box") %>%
  add_trace(x=~points, y=~full_name, color=~tier)
  
  




my_projections %>% 
  inner_join(player_table) %>% 
  mutate( full_name=paste0(first_name, " ", last_name),
          tier = as.factor(tier) ) %>% 
  filter(pos=="DST", avg_type=="weighted", points>1) %>% 
  head(32) %>% 
  ggplot(aes(x=reorder(full_name,points), group=tier)) +
  geom_pointrange(aes(y=points, ymin=floor, ymax=ceiling, color=tier)) +
  theme_light() + xlab(" ") +
  coord_flip()

my_projections %>% pull(avg_type) %>% unique()  




my_projections <- w4_projetions %>% 
  filter(avg_type=="weighted") %>% 
  add_player_info() %>% 
  mutate( full_name = paste0(first_name, " ", last_name),
          tier = as.factor(tier) ) %>%
  group_by( id ) %>% 
  mutate( r_pos = max(ceiling, points) ) %>% 
  ungroup() %>% 
  filter( !(team %in% c("FA","FA*")) )


map(results, function(rslt){
  rslt %>% 
    select(id, weekpts) %>% 
    return()
}) %>%
  bind_rows() %>%
  inner_join(my_projections) -> my_projections


plotProjections <- function(.proj, .pos, .title, .subtitle) {
  proj_data <- .proj %>% 
    filter(position==.pos) %>% 
    arrange(pos_rank) %>% 
    head(30)
  
  nu_y <- max(proj_data$ceiling, na.rm = T) / 21
  
  proj_data %>% 
    ggplot(aes(group=tier)) +
    geom_pointrange(aes(x=reorder(pos_rank,desc(pos_rank)),
                        y=points, 
                        ymin=floor, 
                        ymax=ceiling, 
                        color=tier)) +
    geom_point(aes(x=reorder(pos_rank,desc(pos_rank)),
                   y=weekpts,
                   color=tier), shape=8, size=1) +
    geom_text(aes(x=reorder(pos_rank,desc(pos_rank)), 
                  y=r_pos, 
                  color=tier, 
                  label=last_name), nudge_y = nu_y, nudge_x = 0.1, size=2.5) +
    theme_classic() + xlab("Rank") + ylab("Points") +
    theme( panel.grid.major.x=element_line(colour="#EEEEEE"),
           legend.position="none" ) +
    scale_x_discrete(breaks = seq(1,32,4) ) +
    ggtitle(.title,.subtitle) +
    coord_flip()
}  

plotProjections(my_projections, "QB", "QB", "week 4")
ggsave("./FantasyFootball/qb.png",device = "png", width = 8, height = 8, units="in")

plotProjections(my_projections, "WR", "WR", "week 4")
ggsave("./FantasyFootball/wr.png",device = "png", width = 8, height = 8, units="in")

plotProjections(my_projections, "RB", "RB", "week 4")
ggsave("./FantasyFootball/rb.png",device = "png", width = 8, height = 8, units="in")

plotProjections(my_projections, "TE", "TE", "week 4")
ggsave("./FantasyFootball/te.png",device = "png", width = 8, height = 8, units="in")

plotProjections(my_projections, "DST", "DEF", "week 4")
ggsave("./FantasyFootball/def.png",device = "png", width = 8, height = 8, units="in")
