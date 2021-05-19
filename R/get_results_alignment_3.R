##' @title Get alignment 3 results
##'
##' @param data_clean_alignment_3
##' @param iv
##' @param dv
##' @return
##' @author Shir Dekel
##' @export
get_results_alignment_3 <- function(data_clean_alignment_3, iv, dv) {
  results_1_dv <-
    c(
      "allocation",
      "ranking",
      "confidence"
    )

  results_1 <-
    results_1_dv %>%
    map(
      ~ data_clean_alignment_3 %>%
        filter(phase == "pre") %>%
        nest_by(
          id, allocation, ranking, confidence, npv_amount,
          reliability_amount, alignment
        ) %>%
        get_all_results_alignment_3(.x)
    ) %>%
    set_names(results_1_dv)

  npv_knowledge_omnibus <-
    data_clean_alignment_3 %>%
    filter(phase == "pre") %>%
    nest_by(id, rating, npv_knowledge) %>%
    aov_ez(
      id = "id",
      dv = "npv_knowledge",
      within = "rating",
      data = .,
      type = 2
    )

  npv_knowledge_apa <-
    npv_knowledge_omnibus %>%
    apa_print(es = "pes", mse = FALSE) %>%
    pluck("full_result")

  npv_knowledge_simple_effects <-
    npv_knowledge_omnibus %>%
    emmeans("rating",
      model = "multivariate"
    ) %>%
    pairs() %>%
    apa_print() %>%
    pluck("full_result")

  npv_knowledge <-
    npv_knowledge_apa %>%
    append(npv_knowledge_simple_effects)

  results_1 %>%
    append(lst(npv_knowledge))
}
