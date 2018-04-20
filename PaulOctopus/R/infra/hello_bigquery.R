library(bigrquery)
library(dplyr)
library(tidyverse)

# use service token
set_service_token("./PaulOctopus/Project Paul the Octopus-e9ee8f16859c.json")

# connection with bigquery
con <- DBI::dbConnect(dbi_driver(),
                      project = "project-paul-the-octopus",
                      dataset = "paul_the_octopus_dataset")

# list tha tables
DBI::dbListTables(con)

# top 10 teams by Fifa Ranking
con %>%
  tbl("fifa_rank") %>%
  as.tibble() %>%
  arrange(desc(Total_Points)) %>%
  top_n(10,Total_Points) -> top10Teams

# show data
top10Teams

# creating a personal dataset
insert_dataset(project = "project-paul-the-octopus",
               dataset = "paul_the_octopus_gsposito_dataset")

# connection to this new dataset
myCon <- DBI::dbConnect(dbi_driver(),
                      project = "project-paul-the-octopus",
                      dataset = "paul_the_octopus_gsposito_dataset")

# criar uma tabela
insert_table(project = "project-paul-the-octopus",
             dataset = "paul_the_octopus_gsposito_dataset",
             table = "top10Teams")

# inserir dados na tabela
db_insert_into(myCon, "top10Teams", top10Teams)

# list tha tables
DBI::dbListTables(myCon)

# ler da tabela
myCon %>%
  tbl("top10Teams") %>%
  as.tibble() %>%
  arrange(desc(Total_Points)) %>%
  top_n(10,Total_Points)

# apagar a tabela
delete_table(project = "project-paul-the-octopus",
             dataset = "paul_the_octopus_gsposito_dataset",
             table = "top10Teams")

# list tha tables
DBI::dbListTables(myCon)

# apagar o dataset
delete_dataset(project = "project-paul-the-octopus",
               dataset = "paul_the_octopus_gsposito_dataset")

