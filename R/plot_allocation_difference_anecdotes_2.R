#' @title Plot allocation difference

#' @return
#' @author Shir Dekel
#' @export
#' @param omnibus
plot_allocation_difference_anecdotes_2 <- function(omnibus) {
  dodge_width <- 0.5
  omnibus %>%
    afex::afex_plot(
      x = "similarity",
      trace = "valence",
      mapping = c("shape", "color"),
      error = "within",
      error_arg = list(width = 0.1),
      data_geom = ggbeeswarm::geom_quasirandom,
      point_arg = list(size = 3),
      data_arg = list(
        dodge.width = dodge_width,
        color = "darkgrey"
      ),
      dodge = dodge_width,
      factor_levels = list(
        similarity = c(
          low = "Low",
          high = "High"
        ),
        valence = c(
          negative = "Negative",
          positive = "Positive"
        )
      ),
      legend_title = "Valence"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    labs(
      x = "Similarity",
      y = "Mean allocation difference score"
    ) +
    papaja::theme_apa()
}
