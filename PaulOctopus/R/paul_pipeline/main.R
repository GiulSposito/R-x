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

# stop cluster
stopRCluster(vms)

# setup instances environment
# scrap football results

# stop instances

# scrap football domains


