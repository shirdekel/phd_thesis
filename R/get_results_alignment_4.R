##' @title Get alignment 4 results
##'
##' @param data_clean_alignment_4
##' @param iv
##' @param dv
##' @return
##' @author Shir Dekel
##' @export
get_results_alignment_4 <- function(data_clean_alignment_4, iv, dv) {
  dv_label <-
    c(
      "forecast_mean",
      "forecast_sd"
    )

  dv_label %>%
    map(
      ~ data_clean_alignment_4 %>%
        nest_by(
          id, forecast_mean, forecast_sd, npv_amount, reliability_amount,
          alignment
        ) %>%
        get_all_results_alignment_4(.x)
    ) %>%
    set_names(dv_label)
}
