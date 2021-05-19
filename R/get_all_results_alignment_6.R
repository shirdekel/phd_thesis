#' @title All results

#' @return
#' @author Shir Dekel
#' @export
get_all_results_alignment_6 <- function(data, dv) {
  omnibus <-
    data %>%
    get_omnibus_alignment_6(dv)

  omnibus_apa <-
    omnibus %>%
    apa_print(es = "pes", mse = FALSE) %>%
    pluck("full_result")

  simple_effects_all <-
    omnibus %>%
    emmeans(
      c("variance", "npv_amount"),
      by = "hint",
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("pairwise", "poly"),
      adjust = "none"
    ) %>%
    apa_print() %>%
    pluck("full_result")

  omnibus_apa %>%
    append(simple_effects_all)
}
