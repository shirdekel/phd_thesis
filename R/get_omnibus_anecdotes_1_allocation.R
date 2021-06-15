#' @title Get omnibus for allocation data
##' @param data
#' @return
#' @author Shir Dekel
#' @export
get_omnibus_anecdotes_1_allocation <- function(data) {
  data %>%
    filter(!anecdote == "statistics") %>%
    aov_ez(
      id = "id",
      dv = "allocation_projectA",
      between = c("anecdote", "alignment"),
      data = .,
      type = 2
    )
}
