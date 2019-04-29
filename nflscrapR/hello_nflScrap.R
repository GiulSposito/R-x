# playing with nflscrapR package
# basically understanding the data and capability
# reproducing the post http://tlfvincent.github.io/2017/10/08/nlf-running-back-deep-dive/

# install nflscrapR pacakge
# library(devtools)
# usethis::browse_github_pat()
# install_github(repo = "maksimhorowitz/nflscrapR", auth_token = devtools::github_pat())

library(nflscrapR)
library(tidyverse)

games <- season_games(Season = 2016)
games <- as_tibble(games)

games %>% 
  filter(home=="PIT", away=="NE") %>% 
  pull(GameID) -> gid

pbp_2016 <- game_play_by_play(gid) %>% 
  as_tibble()

pbp_2016 %>% 
  select(Remaining=TimeSecs, Home_WP_post, HomeTeam, Away_WP_post, Away_WP_post) %>% 
  ggplot(aes(x=Remaining)) +
  geom_line(aes(y=Home_WP_post), size=1, color="black") +
  geom_line(aes(y=Away_WP_post), size=1, color="red") +
  annotate("text", x = 3000, y = .75, label = "NE", color = "red", size = 8) + 
  annotate("text", x = 3000, y = .25, label = "PIT", color = "black", size = 8) +
  scale_x_reverse(breaks = seq(0, 3600, 300)) +
  theme_minimal()
