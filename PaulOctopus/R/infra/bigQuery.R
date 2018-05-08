library(bigrquery)
library(tidyverse)
library(lubridate)

# use service token
set_service_token("./PaulOctopus/Project Paul the Octopus-e9ee8f16859c.json")

# working dataset
.BQ_PROJ = "project-paul-the-octopus"
.BQ_DATASET = "giul_paul_octopus_dataset"

# setup dataset
.BQ_WORK_DSET = bq_dataset(.BQ_PROJ,.BQ_DATASET)
if(!bq_dataset_exists(.BQ_WORK_DSET)) .BQ_WORK_DSET <- bq_dataset_create(.BQ_WORK_DSET)

# apaga um dataset no projeto PaulOctopus
deleteWorkingDataset <- function() bq_dataset_delete(.BQ_WORK_DSET,T)

# createTable
createTable <- function(table.name, table.values, .project=.BQ_PROJ, .dataset=.BQ_DATASET){
  table.values <- set_names(table.values, names(table.values) %>% gsub("\\.","_",.))
  bq.dset  <- bq_dataset(.project, .dataset)
  bq.table <- bq_table(bq.dset, table.name)
  if(bq_table_exists(bq.table)) bq_table_delete(bq.table)
  bq.table <- bq_table_create(bq.table)
  bq.table <- bq_table_upload(bq.table, values=table.values)
}

readTable <- function(table.name, .project=.BQ_PROJ, .dataset=.BQ_DATASET){
  bq.dset   <- bq_dataset(.project, .dataset)
  bq.table  <- bq_table(bq.dset, table.name)
  bq.values <- bq_table_download(bq.table) 
  bq.values <- set_names(bq.values, names(bq.values) %>% gsub("_","\\.",.))
  return(bq.values)
}

deleteTable <- function(table.name, .project=.BQ_PROJ, .dataset=.BQ_DATASET){
  bq.dset  <- bq_dataset(.project, .dataset)
  bq.table <- bq_table(bqdset, table.name)
  bq_table_delete(bq.table)
}
  