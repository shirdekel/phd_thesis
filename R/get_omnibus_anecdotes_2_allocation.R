#' @title Get omnibus analysis for allocation data

#' @return
#' @author Shir Dekel
#' @export
#' @param data
get_omnibus_anecdotes_2_allocation <- function(data = anecdotes2::data) {
  data %>%
    aov_ez(
      id = "id",
      dv = "allocation",
      between = "anecdote_between",
      within = c("similarity", "valence"),
      data = .,
      type = 2
    )
}
