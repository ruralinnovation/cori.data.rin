## code to prepare `rin_service_areas` dataset goes here

library(targets)

targets::tar_make()

rin_service_areas <- targets::tar_read(rin_service_areas)
