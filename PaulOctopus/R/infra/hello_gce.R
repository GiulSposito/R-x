# environment

# put this in the .Renviron

# google GCE vars
# GCE_AUTH_FILE = "./PaulOctopus/xxxx.json"
# GCE_DEFAULT_PROJECT_ID = "xxxx"
# GCE_DEFAULT_ZONE = "xxx"

# setup
library(googleComputeEngineR)

# check defaults GCE vars
gce_get_global_project()
gce_get_global_zone()

# get instances
vm_list <- gce_list_instances()

# create one instance
vm <- gce_vm(template = "rstudio",
             name="rstudio-gcer",
             username="rstudio",
             password="pauloctopus",
             predefined_type = "n1-highmem-2")

# refresh status
vm <- gce_vm(vm)
vm

# stop instance
gce_vm_stop(vm)

# remove instance
gce_vm_delete(vm)
