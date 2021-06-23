##' @title Get plot for alignment 2
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_plot_alignment_2 <- function(data = alignment2::data) {
  allocation <-
    data %>%
    nest_by(id, allocation, npv_amount, reliability_amount, alignment) %>%
    get_omnibus_alignment_2("allocation") %>%
    afex_plot_alignment_1(dv_label = "Allocation (%)")

  ranking <-
    data %>%
    nest_by(id, ranking, npv_amount, reliability_amount, alignment) %>%
    get_omnibus_alignment_2("ranking") %>%
    afex_plot_alignment_1(dv_label = "Ranking")

  confidence <-
    data %>%
    nest_by(id, confidence, npv_amount, reliability_amount, alignment) %>%
    get_omnibus_alignment_2("confidence") %>%
    afex_plot_alignment_1(dv_label = "Confidence")

  forecast_mean <-
    data %>%
    nest_by(id, forecast_mean, npv_amount, reliability_amount, alignment) %>%
    get_omnibus_alignment_2("forecast_mean") %>%
    afex_plot_alignment_1(dv_label = "Forecast mean")

  forecast_sd <-
    data %>%
    nest_by(id, forecast_sd, npv_amount, reliability_amount, alignment) %>%
    get_omnibus_alignment_2("forecast_sd") %>%
    afex_plot_alignment_1(dv_label = "Forecast sd")

  lst(
    allocation,
    ranking,
    confidence,
    forecast_mean,
    forecast_sd
  )
}
