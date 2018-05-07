readTable("matches",.BQ_PROJ, "paul_the_octopus_dataset") %>%
  mutate( away = case_when(
    away == " Switzerland" ~ "Switzerland",
    TRUE ~ away
  )) -> matches

.tables$teams %>%
  mutate( team.name = case_when(
    team.cod == "IR" ~ "IR Iran",
    team.cod == "KR" ~ "Korea Republic",
    TRUE ~ team.name
)) -> teams

matches %>%
  left_join(teams, by=c("home"="team.name")) %>%
  rename(home.team.cod = team.cod) %>%
  left_join(teams, by=c("away"="team.name")) %>%
  rename(away.team.cod = team.cod) -> matches
