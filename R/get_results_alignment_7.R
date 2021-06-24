##' @title Get alignment results
##'
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_results_alignment_7 <- function(data = alignment7::data) {
  allocation <-
    c("low", "high") %>%
    map(
      ~ data %>%
        get_results_all_alignment_7(.x, "allocation")
    ) %>%
    set_names("alignment_low", "alignment_high")

  lst(
    allocation
  )
}
