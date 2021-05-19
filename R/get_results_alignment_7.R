##' @title Get alignment results
##'
##' @param data_clean_alignment_7
##' @param iv
##' @param dv
##' @return
##' @author Shir Dekel
##' @export
get_results_alignment_7 <- function(data_clean_alignment_7, iv, dv) {
  allocation <-
    c("low", "high") %>%
    map(
      ~ data_clean_alignment_7 %>%
        get_results_all_alignment_7(.x, "allocation")
    ) %>%
    set_names("alignment_low", "alignment_high")

  lst(
    allocation
  )
}
