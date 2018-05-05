library(googleComputeEngineR)

vm <- gce_vm_container(
  name = "giul-rbase",
  predefined_type = "n1-highmem-2",
  file = "./PaulOctopus/docker/r-base-plus/Dockerfile"
)
