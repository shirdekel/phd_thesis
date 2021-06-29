##' @title Plot anecdotes 2 allocation

##' @return
##' @author Shir Dekel
##' @export
##' @param omnibus
plot_allocation_anecdotes_2 <- function(omnibus) {
  dodge_width <- 0.5

  omnibus %>%
    afex_plot(
      x = "anecdote_between",
      trace = "similarity",
      panel = "valence",
      mapping = c("shape", "color"),
      error = "none",
      error_arg = list(width = 0.1),
      data_geom = ggbeeswarm::geom_quasirandom,
      data_arg = list(
        dodge.width = dodge_width,
        color = "darkgrey"
      ),
      point_arg = list(size = 3),
      dodge = dodge_width,
      factor_levels = list(
        similarity = c(
          low = "Low",
          high = "High"
        ),
        valence = c(
          negative = "Negative valence",
          positive = "Positive valence"
        ),
        anecdote_between = c(
          anecdote_only = "Anecdote only",
          combined = "Anecdote & statistics"
        )
      ),
      legend_title = "Similarity"
    ) +
    labs(
      x = "Evidence type",
      y = "Mean allocation to the target project"
    ) +
    papaja::theme_apa() +
    scale_x_discrete(labels = scales::wrap_format(10))
}
