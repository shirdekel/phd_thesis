##' @title Get function call
##' @param name
##' @param thesis_project
##' @param experiment_number
##' @return
##' @author Shir Dekel
##' @export
get_function_call <- function(name, thesis_project, experiment_number) {
  function_call <-
    str_c(
      "get",
      name,
      thesis_project,
      experiment_number,
      sep = "_"
    ) %>%
    rlang::syms()
  return(function_call)
}
