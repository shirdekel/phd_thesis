##' @title Get alignment results
##'
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_results_alignment_1 <- function(data = alignment1::data) {
  allocation_omnibus <-
    data %>%
    get_omnibus_alignment_1("allocation")

  allocation_apa <-
    allocation_omnibus %>%
    apa_print(es = "pes", mse = FALSE) %>%
    pluck("full_result")

  allocation_simple_effects_all <-
    allocation_omnibus %>%
    emmeans(
      c("reliability_amount", "npv_amount"),
      by = "alignment",
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("consec", "poly"),
      adjust = "none"
    ) %>%
    apa_print() %>%
    pluck("full_result")

  allocation_simple_effects_npv_amount_alignment <-
    allocation_omnibus %>%
    emmeans(
      c("npv_amount"),
      by = "alignment",
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("poly"),
      adjust = "none"
    ) %>%
    apa_print() %>%
    pluck("full_result")

  confidence_omnibus <-
    data %>%
    get_omnibus_alignment_1("confidence")

  confidence_apa <-
    confidence_omnibus %>%
    apa_print(es = "pes", mse = FALSE) %>%
    pluck("full_result")

  confidence_simple_effects_reliability_amount_alignment <-
    confidence_omnibus %>%
    emmeans(
      "reliability_amount",
      by = "alignment",
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("consec"),
      adjust = "sidak"
    ) %>%
    apa_print() %>%
    pluck("full_result")

  confidence_simple_effects_npv_amount_alignment <-
    confidence_omnibus %>%
    emmeans(
      "npv_amount",
      by = "alignment",
      model = "multivariate"
    ) %>%
    contrast(
      interaction = "poly",
      adjust = "sidak"
    ) %>%
    apa_print() %>%
    pluck("full_result")

  allocation <-
    allocation_apa %>%
    append(allocation_simple_effects_all) %>%
    append(allocation_simple_effects_npv_amount_alignment)

  confidence <-
    confidence_apa %>%
    append(confidence_simple_effects_reliability_amount_alignment) %>%
    append(confidence_simple_effects_npv_amount_alignment)

  lst(
    allocation,
    confidence
  )
}
