library(jsonlite)
library(tidyverse)
library(xlsx)

techs <- readRDS("../itau-staffing/data/technologies.rds")

filename = "./tech_stack/projects_stack.json"
hits <- read_json(filename)$responses[[1]]$hits$hits

hits %>%
  lapply(function(hit){
    proj <- hit$`_source`
    technology <- as.character(unlist(proj$stack))
    proj.stack <- tibble(technology=technology)
    team.size <- length(proj$team)
    tibble(
      tower = proj$tower,
      contract = proj$contract,
      squad = proj$flow,
      team.size = team.size,
      tech = list(proj.stack)
    ) %>% return()
  }) %>%
  bind_rows() %>%
  unnest(tech) %>%
  mutate(technology = tolower(technology)) %>%
  distinct -> stacks


stacks %>%
  select(squad, technology) %>%
  distinct() %>%
  group_by(technology) %>%
  tally(sort = T) %>%
  top_n(n=100) %>%
  mutate(pct=n/213) -> used.tech


stacks %>%
  select(squad) %>%
  distinct() %>%
  tally()


techs %>%
  select(name, shortDescription, website) %>%
  distinct() %>%
  mutate(name=tolower(name)) %>%
  left_join(used.tech,., by=c("technology"="name")) -> top.tech

top.tech %>%
  write.xlsx("./tech_stack/tecnologias.xlsx")
