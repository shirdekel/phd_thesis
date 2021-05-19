##' @title Mutate function call
##' @param data
##' @param name
##' @param thesis_project
##' @param experiment_number
##' @return
##' @author Shir Dekel
##' @export
mutate_function_call <- function(data, name, thesis_project, experiment_number) {
  data %>%
    mutate(
      "get_{ name }" := get_function_call(
        name,
        thesis_project,
        experiment_number
      )
    )
}
