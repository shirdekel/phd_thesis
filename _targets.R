source("./packages.R")

lapply(list.files("./R", full.names = TRUE), source)

list(

  tar_target(target2, function_to_make2(arg))

)
