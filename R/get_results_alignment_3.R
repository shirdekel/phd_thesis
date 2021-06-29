##' @title Get alignment 3 results
##'
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_results_alignment_3 <- function(data = alignment3::data) {
  results_1_dv <-
    c(
      "allocation",
      "ranking",
      "confidence"
    )

  results_1 <-
    results_1_dv %>%
    map(
      ~ data %>%
        filter(phase == "pre") %>%
        nest_by(
          id, allocation, ranking, confidence, npv_amount,
          reliability_amount, alignment
        ) %>%
        get_all_results_alignment_3(.x)
    ) %>%
    set_names(results_1_dv)

  npv_knowledge_omnibus <-
    data %>%
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

  variance_lecture_omnibus <-
    data %>%
    get_variance_lecture_omnibus()

  variance_lecture_apa <-
    variance_lecture_omnibus %>%
    apa_print(es = "pes", mse = FALSE) %>%
    pluck("full_result")

  variance_lecture_three_way <-
    variance_lecture_omnibus %>%
    emmeans(
      c(
        "npv_amount",
        "phase",
        "alignment_between",
        "reliability_amount"
      ),
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("poly", "consec", "consec"),
      by = "alignment_between"
    ) %>%
    apa_print() %>%
    pluck("full_result")

  variance_lecture_two_way <-
    variance_lecture_omnibus %>%
    emmeans(
      c(
        "npv_amount",
        "phase",
        "alignment_between"
      ),
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("poly", "consec"),
      by = "alignment_between"
    ) %>%
    apa_print() %>%
    pluck("full_result")


  variance_lecture <-
    variance_lecture_apa %>%
    append(
      c(
        variance_lecture_three_way,
        variance_lecture_two_way
      )
    )

  results_1 %>%
    append(
      c(
        lst(npv_knowledge),
        lst(variance_lecture)
      )
    )
}
