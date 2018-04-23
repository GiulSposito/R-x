library(future)
library(googleComputeEngineR)

## names for your cluster
vm_names <- c("r-vm1","r-vm2","r-vm3")

# cheapest VM
preemptible = list(preemptible=T)

## create the cluster using default template for r-base
## creates jobs that are creating VMs in background
jobs <- lapply(vm_names, function(x) {
  gce_vm_container(file = get_template_file("r-base"),
                   predefined_type = "n1-highmem-2",
#                   preemptible = preemptible,
                   name = x)
})
jobs


## check status of jobs
lapply(jobs, gce_get_op)

## get the VM objects
vms <- lapply(vm_names, gce_vm)

# setup ssh connection
vms <- lapply(vms, 
              gce_ssh_setup, 
              key.pub = file.path("C:/Users/gsposito/.ssh", "google_compute_engine.pub"),
              key.private = file.path("C:/Users/gsposito/.ssh", "google_compute_engine"))

# create cluster
plan(cluster, workers=as.cluster(vms))

# remote function
simulation <- function(x){
  return(Sys.info()["nodename"])
}

# remote invocation
all_results <- future_lapply(1:50, simulation, future.scheduling = F)
all_results


# stop the VMs
lapply(vms, FUN=gce_vm_stop)

# delete the VMs
lapply(vms, FUN=gce_vm_delete)    
