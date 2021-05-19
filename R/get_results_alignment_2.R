##' @title Get alignment 2 results
##'
##' fitting with lmer led to singularity even when reduced to just subject level
##' random intercept.
##'
##' @param data_clean_alignment_2
##' @param iv
##' @param dv
##' @return
##' @author Shir Dekel
##' @export
get_results_alignment_2 <- function(data_clean_alignment_2, iv, dv) {
  results_1_dv <-
    c(
      "allocation",
      "ranking",
      "confidence"
    )

  results_1 <-
    results_1_dv %>%
    map(
      ~ data_clean_alignment_2 %>%
        nest_by(id, allocation, ranking, confidence, npv_amount,
                reliability_amount, alignment) %>%
        get_all_results_alignment_2(.x)
    ) %>%
    set_names(results_1_dv)

  results_2_dv <-
    c(
      "forecast_mean",
      "forecast_sd"
    )

  results_2 <-
    results_2_dv %>%
    map(
      ~ data_clean_alignment_2 %>%
        filter(!is.na(forecast_mean)) %>%
        nest_by(id, forecast_mean, forecast_sd, npv_amount, reliability_amount,
                alignment) %>%
        get_all_results_alignment_2(.x)
    ) %>%
    set_names(results_2_dv)

  results_1 %>%
    append(results_2)
}
