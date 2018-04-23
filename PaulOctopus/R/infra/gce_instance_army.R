# setup
library(googleComputeEngineR)
library(future)

# 10 instance names
vm_names <- paste0("gcer-army-",1:10)

# cheapest VM
preemptible = list(preemptible=T)

# start up the VMs
vms <- lapply(vm_names, gce_vm, predefined_type="n1-standard-1", template="r-base",
              scheduling = preemptible)

# setup ssh between them
vms <- lapply(vms, gce_ssh_setup)

# create cluster
plan(cluster, workers=as.cluster(vms))

# remote function
simulation <- function(x){
  rnorm(x*10)
}

# remote invocation
all_results <- future_lapply(1:50, simulation)

# stop the VMs
lapply(vms, FUN=gce_vm_stop)

# delete the VMs
lapply(vms, FUN=gce_vm_delete)    
