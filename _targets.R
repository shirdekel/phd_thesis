source("./packages.R")

lapply(list.files("./R", full.names = TRUE), source)

thesis_deps <-
  get_thesis_deps()

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
  ),
  tar_target(
    samuelson_distribution,
    get_samuelson_distribution()
  ),
  tar_target(
    prospect_theory,
    plot_prospect_theory()
  ),
  tar_file(input, "index.Rmd"),
  tar_file(config_file, "_bookdown.yml"),
  tar_file(deps, unlist(thesis_deps)),
  tar_file(
    thesis,
    render_with_deps(
      input = input,
      config_file = config_file,
      deps = c(
        !!tar_knitr_deps_expr(thesis_deps$rmd),
        deps
      )
    )
  )
)
