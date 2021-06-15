#' @title Plot allocation difference

#' @return
#' @author Shir Dekel
#' @export
#' @param omnibus
plot_allocation_difference_anecdotes_2 <- function(omnibus) {
  omnibus %>%
    afex::afex_plot(
      x = "similarity",
      trace = "valence",
      error = "within",
      mapping = c("shape", "color"),
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
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      x = "Similarity",
      y = "Mean allocation difference from statistics only condition"
    ) +
    theme_apa()
}
