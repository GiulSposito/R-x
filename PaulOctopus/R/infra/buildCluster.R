library(future)
library(future.apply)
library(googleComputeEngineR)

# create a R cluster in GCP
createRCluster <- function(name = "giul-rcluster",
                           n = 5,
                           setEnvFunction = NULL){

  ## names for your cluster
  vm_names <- paste0(name,"-",formatC(1:n,width = 2, flag="0"))
  
  ## create the cluster using default template for r-base
  ## creates jobs that are creating VMs in background
  jobs <- lapply(vm_names, function(x) {
    gce_vm_container(file = get_template_file("r-base"),
                     predefined_type = "n1-highmem-2",
                     name = x)
  })
    
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
    return(sessionInfo())
  }

    # remote invocation to get status
  all_results <- future_lapply(1:n, FUN=simulation, future.scheduling = F)
    
  return(list(vms=vms, vms_check=all_results))
}

stopRCluster <- function(vms) lapply(vms, FUN=gce_vm_stop)
startRCluster <- function(vms) lapply(vms, FUN=gce_vm_start)
deleteRCluster <- function(vms) lapply(vms, FUN=gce_vm_delete) 
