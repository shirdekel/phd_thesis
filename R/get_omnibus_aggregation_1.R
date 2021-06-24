#' @title Get omnibus model for aggregation 1 experiment
##' @param data
#' @return
#' @author Shir Dekel
#' @export
get_omnibus_aggregation_1 <- function(data) {
  data %>%
    nest_by(id, similarity, awareness, presentation, proportion) %>%
    aov_ez(
      dv = "proportion",
      id = "id",
      between = c("similarity", "awareness"),
      within = c("presentation"),
      type = 2
    )
}
