#' @title Exponentiate
#'
#' Needed for when exponentiating a negative base with a fractional power
#' From https://stackoverflow.com/a/29920304/13945974
##' @param a
##' @param pow
#' @return
#' @author Shir Dekel
#' @export
exponent <- function(a, pow) {
  (abs(a)^pow) * sign(a)
}
