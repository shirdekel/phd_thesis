source("./packages.R")

lapply(list.files("./R", full.names = TRUE), source)

list(
  tar_map(
    values = get_values(),
    names = c("thesis_project", "experiment_number"),
    tar_target(
      plot,
      get_plot(data)
    ),
    tar_target(
      results,
      get_results(data)
    ),
    tar_target(
      descriptives,
      get_descriptives(data, iv)
    ),
    tar_file(
      materials,
      get_materials(thesis_project, experiment_number)
    )
  )
)
