##' @title Get anecdotes 2 results
##'
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_results_anecdotes_2 <- function(data = anecdotes2::data) {
  set_sum_contrasts()

  allocation <-
    data %>%
    get_allocation()

  allocation_difference <-
    data %>%
    get_allocation_difference()

  follow_up <-
    data %>%
    get_follow_up_anecdotes_2()

  lst(
    allocation,
    allocation_difference,
    follow_up
  )
}
