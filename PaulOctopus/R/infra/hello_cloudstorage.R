# escopo de acesso
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/cloud-platform")
library(googleCloudStorageR)
library(tidyverse)

# forca autenticação via prompt :(
gcs_auth()

# lista buckets 
gcs_list_buckets("project-paul-the-octopus")

# obtem o bucket 
bucket <- gcs_get_bucket("ciandt_projectoctopus_2018_gsposito")
bucket

# trabalhando com o "bucket" obtido
objects <- gcs_list_objects()
objects

# fazendo um uploads

# text
gcs_upload(file = "./PaulOctopus/README.md",
           name = "README.md") # content type -> text/x-markdown


# csv
fname <- "./PaulOctopus/export/mtcars.csv"
write.csv(mtcars, fname)
gcs_upload(file = fname,
           name = "mtcars.csv",
           type = "text/csv")

# list to json
gcs_upload(list(a=1,b=3,c=list(d=2,e=5)),
           name = "list.json") # content type -> application/json

# Visualizando
gcs_list_objects() %>%
  View()

# removing objects
gcs_delete_object("mtcars.csv")
gcs_delete_object("list.json")
gcs_delete_object("README.md")

