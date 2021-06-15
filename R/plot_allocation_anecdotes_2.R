##' @title Plot anecdotes 2 allocation

##' @return
##' @author Shir Dekel
##' @export
##' @param omnibus
plot_allocation_anecdotes_2 <- function(omnibus) {
  omnibus %>%
    afex::afex_plot(
      x = "similarity",
      trace = "anecdote_between",
      panel = "valence",
      mapping = c("shape", "color"),
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
      legend_title = "Anecdote"
    ) +
    labs(
      x = "Similarity",
      y = "Mean allocation"
    ) +
    theme_apa()
}
