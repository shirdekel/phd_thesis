##' @title Get plot for anecdotes 2
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_plot_anecdotes_2 <- function(data = anecdotes2::data) {
  allocation <-
    data %>%
    get_omnibus_anecdotes_2_allocation() %>%
    plot_allocation_anecdotes_2()

  allocation_difference <-
    data %>%
    get_omnibus_anecdotes_2_allocation_difference() %>%
    plot_allocation_difference_anecdotes_2()

  similarity <-
    data %>%
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
    data %>%
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
    data %>%
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
    data %>%
    ggplot(aes_string(x = "similarity_rating", y = "allocation")) +
    facet_grid(cols = vars(valence)) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = lm, color = "black") +
    labs(
      x = "Similarity rating",
      y = "Mean allocation to the target project"
    ) +
    papaja::theme_apa()

  allocation_relevance_specific_similarity <-
    data %>%
    mutate(
      across(
        similarity,
        ~ .x %>%
          fct_relevel("low", "high") %>%
          recode(low = "Low", high = "High")
      ),
      across(
        valence,
        recode,
        negative = "Negative valence",
        positive = "Positive valence"
      )
    ) %>%
    ggplot(aes_string(
      x = "relevance_specific_rating", y = "allocation",
      linetype = "similarity", color = "similarity"
    )) +
    facet_grid(cols = vars(valence)) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = "lm") +
    labs(
      x = "Specific relevance rating",
      color = "Similarity",
      linetype = "Similarity",
      y = "Mean allocation to the target project"
    ) +
    papaja::theme_apa()

  relevance_specific_similarity <-
    data %>%
    ggplot(aes_string(x = "similarity_rating", y = "relevance_specific_rating")) +
    facet_grid(cols = vars(valence)) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = lm, color = "black") +
    labs(
      x = "Similarity rating",
      y = "Specific relevance rating"
    ) +
    papaja::theme_apa()

  lm <-
    lst(
      allocation_similarity,
      allocation_relevance_specific_similarity,
      relevance_specific_similarity
    )

  lst(
    allocation,
    allocation_difference,
    similarity,
    relevance_specific,
    relevance_general,
    lm
  )
}
