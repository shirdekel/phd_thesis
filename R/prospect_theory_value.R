#' @title Prospect theory value function
#'
#' Need `exponent` to avoid issues with negative bases and need `ifelse` to
#' avoid issues with vectorisation when plotting with ggplot.

#' @return
#' @author Shir Dekel
#' @export
prospect_theory_value <- function(x, alpha, lambda) {
  ifelse(x < 0, exponent((lambda * x), alpha), exponent(x, alpha))
}
