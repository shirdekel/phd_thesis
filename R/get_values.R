##' @title Get static branching values
##'
##' @return
##' @author Shir Dekel
##' @export
get_values <- function() {
  tibble(
    thesis_project = get_thesis_project(),
    experiment_number = get_experiment_number(),
    data = get_data()
  ) %>%
    rowwise() %>%
    mutate_function_call(
      "plot",
      thesis_project,
      experiment_number
    ) %>%
    mutate_function_call(
      "results",
      thesis_project,
      experiment_number
    )
}
