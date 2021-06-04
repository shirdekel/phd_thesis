#' @title Get all results

#' @return
#' @author Shir Dekel
#' @export
get_all_results_alignment_2 <- function(data, dv) {
  omnibus <-
    data %>%
    get_omnibus_alignment_2(dv)

  omnibus_apa <-
    omnibus %>%
    apa_print(es = "pes", mse = FALSE) %>%
    pluck("full_result")

  simple_effects_all <-
    omnibus %>%
    emmeans(
      c("reliability_amount", "npv_amount"),
      by = "alignment",
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("pairwise", "poly"),
      adjust = "none"
    ) %>%
    apa_print() %>%
    pluck("full_result")


  omnibus_no_npv_excluded <-
    data %>%
    filter(reliability_amount != "noNPV") %>%
    get_omnibus_alignment_2(dv)

  no_npv_excluded_alignment <-
    omnibus_no_npv_excluded %>%
    emmeans(
      c("alignment", "npv_amount"),
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("pairwise", "poly"),
      adjust = "none"
    ) %>%
    apa_print() %>%
    pluck("full_result")

  no_npv_excluded_three_way <-
    omnibus_no_npv_excluded %>%
    emmeans(
      c("reliability_amount", "npv_amount", "alignment"),
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("pairwise", "poly", "pairwise"),
      adjust = "none"
    ) %>%
    apa_print() %>%
    pluck("full_result")

  no_npv_excluded <-
    c(
      no_npv_excluded_alignment,
      no_npv_excluded_three_way
    )

  omnibus_apa %>%
    append(simple_effects_all) %>%
    append(no_npv_excluded)
}
