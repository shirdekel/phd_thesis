##' @title Get plot for alignment 4
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_plot_alignment_4 <- function(data = alignment4::data) {
  forecast_mean <-
    data %>%
    nest_by(
      id, forecast_mean,
      npv_amount, reliability_amount, alignment
    ) %>%
    get_omnibus_alignment_4("forecast_mean") %>%
    afex_plot_alignment_4("forecast_mean")

  forecast_sd <-
    data %>%
    nest_by(
      id, forecast_sd,
      npv_amount, reliability_amount, alignment
    ) %>%
    get_omnibus_alignment_4("forecast_sd") %>%
    afex_plot_alignment_4("forecast_sd")

  lst(
    forecast_mean,
    forecast_sd
  )
}
