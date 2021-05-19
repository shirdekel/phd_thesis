##' @title Diffuse and remove NA

##' @param ... strings (vector or multiple arguments)
##' @return
##' @author Shir Dekel
##' @export
diffuse_non_na <- function(...) {
  if (length(list(...)) > 1) {
    diffused <-
      enexprs(...) %>%
      discard(is.na) %>%
      syms()
  } else {
    diffused <-
      enexprs(...) %>%
      unlist() %>%
      discard(is.na) %>%
      syms()
  }

  return(diffused)
}
