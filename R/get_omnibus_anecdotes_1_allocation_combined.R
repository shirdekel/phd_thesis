#' @title Get omnibus for combined and statistics only analysis
##' @param data
#' @return
#' @author Shir Dekel
#' @export
get_omnibus_anecdotes_1_allocation_combined <- function(data) {
  data %>%
    unite(condition, c(anecdote, alignment)) %>%
    filter(
      condition %in% c(
        "combined_high",
        "combined_low",
        "statistics_NA"
      )
    ) %>%
    aov_ez(
      id = "id",
      dv = "allocation_projectA",
      between = "condition",
      data = .,
      type = 2
    )
}
