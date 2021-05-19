##' @title Get alignment 6 results
##'
##' @param data_clean_alignment_6
##' @param iv
##' @param dv
##' @return
##' @author Shir Dekel
##' @export
get_results_alignment_6 <- function(data_clean_alignment_6, iv, dv) {
  dv_label <-
    c(
      "allocation",
      "ranking"
    )

  dv_label %>%
    map(
      ~ data_clean_alignment_6 %>%
        nest_by(id, allocation, ranking, npv_amount, variance, hint) %>%
        get_all_results_alignment_6(.x)
    ) %>%
    set_names(dv_label)

}
