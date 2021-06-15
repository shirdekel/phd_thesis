##' @title Get plot for anecdotes 1
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_plot_anecdotes_1 <- function(data = anecdotes1::data) {
  allocation <-
    data %>%
    get_omnibus_anecdotes_1_allocation() %>%
    afex::afex_plot(
      x = "anecdote",
      trace = "alignment",
      mapping = c("shape", "color"),
      factor_levels = list(
        alignment = c(
          low = "Low",
          high = "High"
        ),
        anecdote = c(
          anecdote = "Anecdote only",
          enhanced = "Anecdote & enhanced statistics",
          combined = "Anecdote & statistics"
        )
      ),
      legend_title = "Similarity"
    ) +
    labs(
      x = "Anecdote",
      y = "Mean allocation to the target project"
    ) +
    theme_apa() +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))

  allocation_combined <-
    data %>%
    get_omnibus_anecdotes_1_allocation_combined() %>%
    afex::afex_plot(
      x = "condition",
      factor_levels = list(
        condition = c(
          combined_high = "High similarity anecdote & statistics",
          combined_low = "Low similarity anecdote & statistics",
          statistics_NA = "Statistics only"
        )
      )
    ) +
    labs(
      x = "Condition",
      y = "Mean allocation to the target project"
    ) +
    theme_apa() +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))

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
    afex_plot(
      x = "anecdote",
      trace = "alignment"
    )

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
    afex_plot(
      x = "anecdote",
      trace = "alignment"
    )

  relevance_general <-
    data %>%
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
    data %>%
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
    data %>%
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
    data %>%
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
    allocation_combined,
    similarity,
    relevance_specific,
    relevance_general,
    lm
  )
}
