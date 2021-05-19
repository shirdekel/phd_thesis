##' @title Get thesis project
##'
##' For `get_values()`

##' @return
##' @author Shir Dekel
##' @export
get_thesis_project <- function() {
  c(
    c("aggregation") %>%
      rep(4),
    "alignment" %>%
      rep(8),
    "anecdotes" %>%
      rep(2)
  )
}
