##' @title Fit `lme::glmer`
##'
##' Avoiding environment issues
##'
##' From https://github.com/ropensci/drake/issues/1012#issuecomment-665630922

##' @param formula
##'
##' @param data
##' @param family
##'
##' @return
##' @author Shir Dekel
##' @export
fit_glmer <- function(formula, data, family) {
  envir <- environment()
  envir$data <- data
  f <- as.formula(formula, env = envir)
  glmer(formula = f, data = data, family = family)
}
