# main script

# setup local google environment
source("./PaulOctopus/R/infra/buildCluster.R")
gce_rcluster <- createRCluster()

# create cluster
plan(cluster, workers=as.cluster(vms))

future.apply::future_lapply(vms, function(x) Sys.info())
res <- future.apply::future_lapply(vms, function(x){
  install.packages(c("tidyverse","lubridate"))
  installed.packages()
})

res <- future.apply::future_lapply(vms,
                                   function(x){
                                     require(tidyverse)
                                     installed.packages() %>%
                                       as.tibble() %>%
                                       list(packages=., session=sessionInfo()) %>%
                                       return()
                                   })

# stop cluster
stopRCluster(vms)

vms <- startRCluster(vms)
vms <- gce_vm(vms)

# setup instances environment
# scrap football results

# stop instances

# scrap football domains


