#' @title Get experiment IVs
#'
#' To be used for `get_descriptives()`

#' @return
#' @author Shir Dekel
#' @export
get_iv <- function() {
  list(
    ## Aggregation 1
    c(
      "similarity",
      "awareness",
      "presentation"
    ),
    ## Aggregation 2
    c(
      "awareness",
      "distribution",
      "presentation"
    ),
    ## Aggregation 3
    "similarity",
    ## Aggregation 4
    "awareness",
    ## Alignment 1
    c(
      "alignment",
      "reliability_amount"
    ),
    ## Alignment 2
    c(
      "alignment",
      "reliability_amount",
      "npv_amount"
    ),
    ## Alignment 3
    c(
      "alignment",
      "reliability_amount"
    ),
    ## Alignment 4
    c(
      "alignment",
      "reliability_amount"
    ),
    ## Alignment 5
    c(
      "alignment",
      "reliability_amount"
    ),
    ## Alignment 6
    c(
      "hint",
      "variance"
    ),
    ## Alignment 7
    c(
      "reliability_type",
      "reliability_amount",
      "npv_amount"
    ),
    ## Alignment 8
    c(
      "alignment",
      "reliability_type",
      "reliability_amount",
      "npv_amount"
    ),
    ## Anecdotes 1
    c(
      "anecdote",
      "alignment"
    ),
    ## Anecdotes 2
    c(
      "anecdote_between",
      "similarity",
      "valence"
    )
  )
}
