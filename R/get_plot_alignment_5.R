##' @title Get plot for alignment 5
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_plot_alignment_5 <- function(data_clean_alignment_5) {
  forecast_mean <-
    data_clean_alignment_5 %>%
    nest_by(
      id, forecast_mean,
      npv_amount, reliability_amount, alignment
    ) %>%
    arrange(npv_amount) %>%
    aov_ez(
      id = "id",
      dv = "forecast_mean",
      between = c("alignment", "reliability_amount"),
      within = "npv_amount",
      data = .,
      type = 2
    ) %>%
    afex_plot_alignment_4("forecast_mean")

  forecast_sd <-
    data_clean_alignment_5 %>%
    nest_by(
      id, forecast_sd,
      npv_amount, reliability_amount, alignment
    ) %>%
    arrange(npv_amount) %>%
    aov_ez(
      id = "id",
      dv = "forecast_sd",
      between = c("alignment", "reliability_amount"),
      within = "npv_amount",
      data = .,
      type = 2
    ) %>%
    afex_plot_alignment_4("forecast_sd")

  lst(
    forecast_mean,
    forecast_sd
  )
}
