#' @title Get materials
#'
#' From data packages
##' @param thesis_project
##' @param experiment_number
#' @return
#' @author Shir Dekel
#' @export
get_materials <- function(thesis_project, experiment_number) {
  system.file("materials", package = paste0(thesis_project, experiment_number))
}
