# setup
library(future)
library(tidyverse)
library(lubridate)
library(future.apply)
library(googleComputeEngineR)

# basic configurations
cluster_prefix <- "giul-rcluster"
cluster_size   <- 5
n              <- cluster_size

## names for your cluster
vm_names <- paste0(cluster_prefix,"-",formatC(1:cluster_size,width = 2, flag="0"))

# start VMs
jobs <- lapply(vm_names, gce_vm_start)

# get VMs
vms <- lapply(vm_names, gce_vm)

# setup shh
vms <- lapply(vms, 
              gce_ssh_setup, 
              key.pub = file.path("C:/Users/gsposito/.ssh", "google_compute_engine.pub"),
              key.private = file.path("C:/Users/gsposito/.ssh", "google_compute_engine"))

# compose the cluster
plan(cluster, workers=as.cluster(vms))

# check cluster
future_lapply(1:n, 
              function(x) Sys.info()["nodename"],
              future.scheduling = F) %>% unlist()

# check clustered operations
future_lapply(1:100,
              function(x) paste0(Sys.info()["nodename"],": ", x^2),
              future.scheduling = F)

# install some packages
res <- future.apply::future_lapply(vms, function(x){
  install.packages(c("future","future.apply", "dplyr","tibble","magrittr"))
  installed.packages()
})

  
# execute some more sofisticated
future_lapply(1:n,
              function(x){
                require(tidyverse)
                require(future)
                installed.packages() %>% 
                  as.tibble() %>%
                  filter(NeedsCompilation=="yes") %>%
                  arrange(desc(Version),Package) %>%
                  return()
              },
              future.scheduling = F,
              future.packages = c("future","future.apply", "dplyr","tibble","magrittr"))



res %>%
  lapply(function(x){
    x %>% 
      as.tibble() %>%
      filter(Package=="dplyr")
  })


future_lapply(vms, 
              function(x) as.data.frame(installed.packages()),
              future.scheduling = F) -> res

future_lapply(vms, 
              function(x) install.packages("dplyr"),
              future.scheduling = F) -> res


res[[1]] %>% View()
