#' @title Prospect theory value function
#'
#' Need `ifelse` to avoid issues with vectorisation when plotting with ggplot.

#' @return
#' @author Shir Dekel
#' @export
prospect_theory_value <- function(x, alpha, lambda) {
  ifelse(x < 0, -1 * lambda * (-1 * x)^alpha, x^alpha)
}
