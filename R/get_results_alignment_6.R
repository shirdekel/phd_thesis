##' @title Get alignment 6 results
##'
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_results_alignment_6 <- function(data = alignment6::data) {
  dv_label <-
    c(
      "allocation",
      "ranking"
    )

  dv_label %>%
    map(
      ~ data %>%
        nest_by(id, allocation, ranking, npv_amount, variance, hint) %>%
        get_all_results_alignment_6(.x)
    ) %>%
    set_names(dv_label)

}
