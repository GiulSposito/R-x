library(googleComputeEngineR)

vm <- gce_vm_template(
                name = "giul-rstudio2",
                username = "giul", password = 'biggiul',
                predefined_type = "n1-highmem-2",
                file="C:/Users/gsposito/Documents/R/win-library/3.5/googleComputeEngineR/dockerfiles/rstudio-gcloud/")



gce_vm_container(
  name = "giul-rstudio",
  predefined_type = "n1-highmem-2",
  cloud_init = readChar(file("C:/Users/gsposito/Documents/R/win-library/3.5/googleComputeEngineR/dockerfiles/rstudio-gcloud/Dockerfile2"),nchars=10000)
)

vm <- gce_vm("giul-rstudio")
gce_vm_stop(vm)
gce_vm_delete(vm)


install.packages(format)
