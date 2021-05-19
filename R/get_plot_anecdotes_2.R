##' @title Get plot for anecdotes 2
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_plot_anecdotes_2 <- function(data_clean_anecdotes_2) {
  allocation <-
    data_clean_anecdotes_2 %>%
    get_plot_simulation_anecdotes_2()

  similarity <-
    data_clean_anecdotes_2 %>%
    filter(anecdote_within == "anecdote") %>%
    aov_ez(
      id = "id",
      dv = "similarity_rating",
      between = "anecdote_between",
      within = c("valence", "similarity"),
      data = .,
      type = 2
    ) %>%
    afex_plot(
      x = "anecdote_between",
      trace = "similarity",
      panel = "valence"
    )

  relevance_specific <-
    data_clean_anecdotes_2 %>%
    filter(anecdote_within == "anecdote") %>%
    aov_ez(
      id = "id",
      dv = "relevance_specific_rating",
      between = "anecdote_between",
      within = c("valence", "similarity"),
      data = .,
      type = 2
    ) %>%
    afex_plot(
      x = "anecdote_between",
      trace = "similarity",
      panel = "valence"
    )

  relevance_general <-
    data_clean_anecdotes_2 %>%
    filter(anecdote_within == "anecdote") %>%
    aov_ez(
      id = "id",
      dv = "relevance_general_rating",
      between = "anecdote_between",
      within = c("valence", "similarity"),
      data = .,
      type = 2
    ) %>%
    afex_plot(
      x = "anecdote_between",
      trace = "similarity",
      panel = "valence"
    )

  allocation_similarity <-
    data_clean_anecdotes_2 %>%
    filter(anecdote_within == "anecdote") %>%
    ggplot(aes_string(x = "similarity_rating", y = "allocation")) +
    facet_grid(cols = vars(valence)) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = lm, color = "black") +
    labs(
      x = "Similarity rating",
      y = "Mean allocation to the target project"
    ) +
    theme_apa()

  allocation_relevance_specific_similarity <-

    data_clean_anecdotes_2 %>%
    filter(anecdote_within == "anecdote") %>%
    ggplot(aes_string(x = "relevance_specific_rating", y = "allocation", linetype = "similarity")) +
    facet_grid(cols = vars(valence), rows = vars(anecdote_between)) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = "lm", color = "black") +
    labs(
      x = "Specific relevance rating",
      color = "Similarity",
      y = "Mean allocation to the target project"
    ) +
    theme_apa()

  relevance_specific_similarity <-
    data_clean_anecdotes_2 %>%
    filter(anecdote_within == "anecdote") %>%
    ggplot(aes_string(x = "similarity_rating", y = "relevance_specific_rating")) +
    facet_grid(cols = vars(valence)) +
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
      allocation_relevance_specific_similarity,
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
