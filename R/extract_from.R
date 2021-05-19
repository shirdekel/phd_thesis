##' @title Extract elements of x present in y

##' @return
##' @author Shir Dekel
##' @export
extract_from <- function(x, y) {
  x %>%
    .[x %in% y]
}
