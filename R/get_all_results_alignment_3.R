#' @title Get all results
#' @param data
#' @param dv
#' @return
#' @author Shir Dekel
#' @export
get_all_results_alignment_3 <- function(data, dv) {
  omnibus <-
    data %>%
    get_omnibus_alignment_3(dv)

  omnibus_apa <-
    omnibus %>%
    apa_print(es = "pes", mse = FALSE) %>%
    pluck("full_result")

  simple_effects_all <-
    omnibus %>%
    emmeans(
      c("npv_amount", "alignment", "reliability_amount"),
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("poly", "consec", "consec"),
      adjust = "sidak"
    ) %>%
    apa_print() %>%
    pluck("full_result")

  simple_effects_npv_alignment <-
    omnibus %>%
    emmeans(
      c("npv_amount", "alignment"),
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("poly", "consec")
    ) %>%
    apa_print() %>%
    pluck("full_result")

  simple_effects_all_by_alignment <-
    omnibus %>%
    emmeans(
      c("npv_amount", "reliability_amount"),
      by = "alignment",
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("poly", "consec")
    ) %>%
    apa_print() %>%
    pluck("full_result")

  omnibus_apa %>%
    append(simple_effects_all) %>%
    append(simple_effects_npv_alignment) %>%
    append(simple_effects_all_by_alignment)
}
