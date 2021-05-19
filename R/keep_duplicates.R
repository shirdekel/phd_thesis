##' @title Keep duplicate values

##' @return
##' @author Shir Dekel
##' @export
##' @param x
keep_duplicates <- function(x) {
  if (length(unique(x)) == 1) x else NA
}
