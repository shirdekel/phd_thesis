#' @title Get data
#'
#' From data packages
##' @param thesis_project
##' @param experiment_number
#' @return
#' @author Shir Dekel
#' @export
get_data <- function(thesis_project, experiment_number) {
  list(
    aggregation1::data,
    aggregation2::data,
    aggregation3::data,
    aggregation4::data,
    alignment1::data,
    alignment2::data,
    alignment3::data,
    alignment4::data,
    alignment5::data,
    alignment6::data,
    alignment7::data,
    alignment8::data,
    anecdotes1::data,
    anecdotes2::data
  )
}
