##' @title Get plot for anecdotes 1
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_plot_anecdotes_1 <- function(data_clean_anecdotes_1) {
  allocation <-
    data_clean_anecdotes_1 %>%
    mutate(
      across(
        anecdote,
        recode,
        anecdote = "Anecdote only",
        combined = "Anecdote & statistics",
        enhanced = "Anecdote & enhanced statistics",
        statistics = "Statistics only",
      )
    ) %>%
    apa_plot(
      iv1 = "anecdote",
      iv2 = "alignment",
      iv1.lab = "Evidence type",
      iv2.lab = "Similarity",
      dv = "allocation_projectA",
      dv.lab = "Mean allocation to the target project"
    ) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))

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
    afex_plot(
      x = "anecdote",
      trace = "alignment"
    )

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
    afex_plot(
      x = "anecdote",
      trace = "alignment"
    )

  relevance_general <-
    data_clean_anecdotes_1 %>%
    filter(!anecdote == "statistics") %>%
    aov_ez(
      id = "id",
      dv = "follow_up_relevance_general_rating",
      between = c("anecdote", "alignment"),
      data = .,
      type = 2
    ) %>%
    afex_plot(
      x = "anecdote",
      trace = "alignment"
    )

  allocation_similarity <-
    data_clean_anecdotes_1 %>%
    filter(!anecdote == "statistics") %>%
    ggplot(aes_string(x = "follow_up_similarity_rating", y = "allocation_projectA")) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = lm, color = "black") +
    labs(
      x = "Similarity rating",
      y = "Mean allocation to the target project"
    ) +
    theme_apa()

  allocation_relevance_specific_alignment <-
    data_clean_anecdotes_1 %>%
    filter(!anecdote == "statistics") %>%
    ggplot(aes_string(x = "follow_up_relevance_specific_rating", y = "allocation_projectA", linetype = "alignment")) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = lm, color = "black") +
    labs(
      x = "Specific relevance rating",
      linetype = "Similarity",
      y = "Mean allocation to the target project"
    ) +
    theme_apa()

  relevance_specific_similarity <-
    data_clean_anecdotes_1 %>%
    filter(!anecdote == "statistics") %>%
    ggplot(aes_string(x = "follow_up_similarity_rating", y = "follow_up_relevance_specific_rating")) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = lm, color = "black") +
    labs(
      x = "Similarity rating",
      y = "Specific relevance rating"
    ) +
    theme_apa()

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
