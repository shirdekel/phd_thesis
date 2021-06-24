##' @title Get alignment 4 results
##'
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_results_alignment_4 <- function(data = alignment4::data) {
  dv_label <-
    c(
      "forecast_mean",
      "forecast_sd"
    )

  dv_label %>%
    map(
      ~ data %>%
        nest_by(
          id, forecast_mean, forecast_sd, npv_amount, reliability_amount,
          alignment
        ) %>%
        get_all_results_alignment_4(.x)
    ) %>%
    set_names(dv_label)
}
