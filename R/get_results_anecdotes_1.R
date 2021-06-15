##' @title Get anecdotes 1 results
##'
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_results_anecdotes_1 <- function(data = anecdotes1::data) {
  set_sum_contrasts()
  allocation_omnibus <-
    data %>%
    get_omnibus_anecdotes_1_allocation()

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
    data %>%
    get_omnibus_anecdotes_1_allocation_combined()

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
    data %>%
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
    data %>%
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
    data %>%
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
    data %>%
    filter(!anecdote == "statistics") %>%
    lm(
      allocation_projectA ~ follow_up_similarity_rating,
      data = .
    ) %>%
    apa_print() %>%
    pluck("full_result", "follow_up_similarity_rating")

  allocation_relevance_specific_alignment <-
    data %>%
    filter(!anecdote == "statistics") %>%
    lm(
      allocation_projectA ~ follow_up_relevance_specific_rating * alignment,
      data = .
    ) %>%
    apa_print() %>%
    pluck("full_result", "follow_up_relevance_specific_rating_alignment1")

  relevance_specific_similarity <-
    data %>%
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
