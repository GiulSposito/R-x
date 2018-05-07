library(googleComputeEngineR)

job <- gce_vm_container(#file = "./PaulOctopus/docker/r-base.yaml",
                       cloud_init = readChar(file("./PaulOctopus/docker/r-base-plus.yaml"),nchars=10000),
                       name = "giul-my-rbase",
                       predefined_type = "n1-highmem-2")


vm <- gce_vm("giul-my-rbase")

library(future)
library(future.apply)
library(tidyverse)


vm <- gce_ssh_setup(vm,
  key.pub = file.path("C:/Users/gsposito/.ssh", "google_compute_engine.pub"),
  key.private = file.path("C:/Users/gsposito/.ssh", "google_compute_engine"))

plan(cluster, workers=as.cluster(vm))

future_lapply(1, function(x) installed.packages(), future.scheduling = F) %>%
  .[[1]] %>%
  as.tibble() %>%
  arrange(Package) %>%
  View()


gce_vm_stop(vm)
gce_vm_delete(vm)
