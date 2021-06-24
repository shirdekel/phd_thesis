##' @title Get plot for anecdotes 1
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_plot_anecdotes_1 <- function(data = anecdotes1::data) {
  dodge_width <- 0.5
  allocation <-
    data %>%
    get_omnibus_anecdotes_1_allocation() %>%
    afex::afex_plot(
      x = "anecdote",
      trace = "alignment",
      mapping = c("shape", "color"),
      error_arg = list(width = 0.1),
      data_geom = ggbeeswarm::geom_quasirandom,
      point_arg = list(size = 3),
      data_arg = list(
        dodge.width = dodge_width,
        color = "darkgrey"
      ),
      dodge = dodge_width,
      factor_levels = list(
        alignment = c(
          low = "Low",
          high = "High"
        ),
        anecdote = c(
          anecdote = "Anecdote only",
          combined = "Anecdote & statistics",
          enhanced = "Anecdote & enhanced statistics"
        )
      ),
      legend_title = "Similarity"
    ) +
    labs(
      x = "Anecdote",
      y = "Mean allocation to the target project"
    ) +
    papaja::theme_apa() +
    scale_x_discrete(labels = scales::wrap_format(20))

  allocation_combined <-
    data %>%
    get_omnibus_anecdotes_1_allocation_combined() %>%
    afex::afex_plot(
      x = "condition",
      mapping = c("shape", "color"),
      error_arg = list(width = 0.1),
      data_geom = ggbeeswarm::geom_quasirandom,
      point_arg = list(size = 3),
      factor_levels = list(
        condition = c(
          statistics_NA = "Statistics only",
          combined_low = "Low similarity anecdote & statistics",
          combined_high = "High similarity anecdote & statistics"
        )
      )
    ) +
    labs(
      x = "Condition",
      y = "Mean allocation to the target project"
    ) +
    papaja::theme_apa() +
    scale_x_discrete(labels = scales::wrap_format(20)) +
    theme(legend.position = "none")


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
    ) +
    papaja::theme_apa()

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
    ) +
    papaja::theme_apa()

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
    ) +
    papaja::theme_apa()

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
    papaja::theme_apa()

  allocation_relevance_specific_alignment <-
    data %>%
    mutate(
      across(
        alignment,
        ~ .x %>%
          fct_relevel("low", "high") %>%
          recode(low = "Low", high = "High")
      )
    ) %>%
    filter(!anecdote == "statistics") %>%
    ggplot(aes_string(
      x = "follow_up_relevance_specific_rating",
      y = "allocation_projectA",
      color = "alignment",
      linetype = "alignment"
    )) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = lm) +
    labs(
      x = "Specific relevance rating",
      color = "Similarity",
      linetype = "Similarity",
      y = "Mean allocation to the target project"
    ) +
    papaja::theme_apa()

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
    papaja::theme_apa()

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
