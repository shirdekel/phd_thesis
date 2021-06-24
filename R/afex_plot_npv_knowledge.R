#' @title Plot NPV knowledge ratings

#' @return
#' @author Shir Dekel
#' @export
afex_plot_npv_knowledge <- function(afex_aov) {
  afex_aov %>%
    afex_plot(
      x = "rating",
      error = "within",
      data_geom = geom_boxplot,
      emmeans_arg = list(model = "multivariate"),
      factor_levels = list(
        rating = c(
          X1 = "Pre-explanation",
          X2 = "Post-explanation",
          X3 = "Post-allocation",
          X4 = "Pre-lecture",
          X5 = "Post-lecture"
        )
      )
    ) +
    labs(
      x = "Experiment phase",
      y = "NPV knowledge rating"
    ) +
    papaja::theme_apa()
}
