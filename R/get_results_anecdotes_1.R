##' @title Get anecdotes 2 results
##'
##' @param data_clean_anecdotes_1
##' @param iv
##' @param dv
##' @return
##' @author Shir Dekel
##' @export
get_results_anecdotes_1 <- function(data_clean_anecdotes_1, iv, dv) {
  set_sum_contrasts()
  allocation_omnibus <-
    data_clean_anecdotes_1 %>%
    filter(!anecdote == "statistics") %>%
    aov_ez(
      id = "id",
      dv = "allocation_projectA",
      between = c("anecdote", "alignment"),
      data = .,
      type = 2
    )

  allocation_omnibus_apa <-
    allocation_omnibus %>%
    apa_print(es = "pes", mse = FALSE) %>%
    pluck("full_result")

  allocation_two_way <-
    allocation_omnibus %>%
    emmeans(c("anecdote", "alignment")) %>%
    contrast(interaction = "pairwise") %>%
    apa_print() %>%
    pluck("full_result")

  allocation_anecdote_alignment <-
    allocation_omnibus %>%
    emmeans("anecdote", by = "alignment") %>%
    contrast("pairwise") %>%
    apa_print() %>%
    pluck("full_result")

  allocation_omnibus_combined_statistics <-
    data_clean_anecdotes_1 %>%
    unite(condition, c(anecdote, alignment)) %>%
    filter(
      condition %in% c(
        "combined_high",
        "combined_low",
        "statistics_NA"
      )
    ) %>%
    aov_ez(
      id = "id",
      dv = "allocation_projectA",
      between = "condition",
      data = .,
      type = 2
    )

  allocation_omnibus_combined_statistics_apa <-
    allocation_omnibus_combined_statistics %>%
    apa_print(es = "pes", mse = FALSE) %>%
    pluck("full_result")

  allocation_combined_statistics <-
    allocation_omnibus_combined_statistics %>%
    emmeans("condition") %>%
    contrast("pairwise", adjust = "none") %>%
    apa_print() %>%
    pluck("full_result")

  allocation_anecdote <-
      allocation_omnibus %>%
      emmeans("anecdote") %>%
      contrast("pairwise", adjust = "none") %>%
      apa_print() %>%
      pluck("full_result")

  allocation <-
    c(
      allocation_omnibus_apa,
      allocation_two_way,
      allocation_anecdote_alignment,
      allocation_omnibus_combined_statistics_apa,
      allocation_combined_statistics,
      allocation_anecdote
    )


  similarity <-
    data_clean_anecdotes_1 %>%
    filter(!anecdote == "statistics") %>%
    aov_ez(
      id = "id",
      dv = "follow_up_similarity_rating",
      between = c("anecdote", "alignment"),
      data = .,
      type = 2
    ) %>%
    apa_print(es = "pes", mse = FALSE) %>%
    pluck("full_result")

  relevance_specific <-
    data_clean_anecdotes_1 %>%
    filter(!anecdote == "statistics") %>%
    aov_ez(
      id = "id",
      dv = "follow_up_relevance_specific_rating",
      between = c("anecdote", "alignment"),
      data = .,
      type = 2
    ) %>%
    apa_print(es = "pes", mse = FALSE) %>%
    pluck("full_result")

  relevance_general_omnibus <-
    data_clean_anecdotes_1 %>%
    filter(!anecdote == "statistics") %>%
    aov_ez(
      id = "id",
      dv = "follow_up_relevance_general_rating",
      between = c("anecdote", "alignment"),
      data = .,
      type = 2
    )

  relevance_general_omnibus_apa <-
    relevance_general_omnibus %>%
    apa_print(es = "pes", mse = FALSE) %>%
    pluck("full_result")

  relevance_general_anecdote <-
    relevance_general_omnibus %>%
    emmeans(c("anecdote")) %>%
    contrast("pairwise", adjust = "bonferroni") %>%
    apa_print() %>%
    pluck("full_result")

  relevance_general <-
    c(
      relevance_general_omnibus_apa,
      relevance_general_anecdote
    )

  allocation_similarity <-
    data_clean_anecdotes_1 %>%
    filter(!anecdote == "statistics") %>%
    lm(
      allocation_projectA ~ follow_up_similarity_rating,
      data = .
    ) %>%
    apa_print() %>%
    pluck("full_result", "follow_up_similarity_rating")

  allocation_relevance_specific_alignment <-
    data_clean_anecdotes_1 %>%
    filter(!anecdote == "statistics") %>%
    lm(
      allocation_projectA ~ follow_up_relevance_specific_rating * alignment,
      data = .
    ) %>%
    apa_print() %>%
    pluck("full_result", "follow_up_relevance_specific_rating_alignment1")

  relevance_specific_similarity <-
    data_clean_anecdotes_1 %>%
    filter(!anecdote == "statistics") %>%
    lm(
      follow_up_relevance_specific_rating ~ follow_up_similarity_rating,
      data = .
    ) %>%
    apa_print() %>%
    pluck("full_result", "follow_up_similarity_rating")

  lm <-
    lst(
      allocation_similarity,
      allocation_relevance_specific_alignment,
      relevance_specific_similarity
    )

  lst(
    allocation,
    similarity,
    relevance_specific,
    relevance_general,
    lm
  )
}
