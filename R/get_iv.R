#' @title Get experiment IVs
#'
#' To be used for `get_descriptives()`

#' @return
#' @author Shir Dekel
#' @export
get_iv <- function() {
  list(
    c(
      "similarity",
      "awareness",
      "presentation"
    ),
    c(
      "awareness",
      "distribution",
      "presentation"
    ),
    "similarity",
    "awareness",
    c(
      "alignment",
      "reliability_amount"
    ),
    c(
      "alignment",
      "reliability_amount",
      "npv_amount"
    ),
    c(
      "alignment",
      "reliability_amount"
    ),
    c(
      "alignment",
      "reliability_amount"
    ),
    c(
      "alignment",
      "reliability_amount"
    ),
    c(
      "hint",
      "variance"
    ),
    c(
      "reliability_type",
      "reliability_amount",
      "npv_amount"
    ),
    c(
      "alignment",
      "reliability_type",
      "reliability_amount",
      "npv_amount"
    ),
    c(
      "anecdote",
      "alignment"
    ),
    c(
      "anecdote_between",
      "similarity",
      "valence"
    )
  )
}
