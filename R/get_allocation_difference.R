#' @title Get allocation difference results
##' @param data
#' @return
#' @author Shir Dekel
#' @export
get_allocation_difference <- function(data) {
  allocation_difference_omnibus <-
    data %>%
    get_omnibus_anecdotes_2_allocation_difference()

  allocation_difference_omnibus_apa <-
    allocation_difference_omnibus %>%
    apa_print(es = "pes", mse = FALSE) %>%
    pluck("full_result")

  simple_effects_allocation_difference <-
    allocation_difference_omnibus %>%
    emmeans(c("similarity", "valence")) %>%
    contrast(
      by = "valence",
      "pairwise",
      adjust = "none"
    ) %>%
    apa_print() %>%
    pluck("full_result")

  c(
    allocation_difference_omnibus_apa,
    simple_effects_allocation_difference
  )
}
