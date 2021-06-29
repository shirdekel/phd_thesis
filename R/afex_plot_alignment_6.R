#' @title Afex plot

#' @return
#' @author Shir Dekel
#' @export
afex_plot_alignment_6 <- function(afex_aov, dv_label) {
  afex_aov %>%
    afex_plot(
      x = "npv_amount",
      trace = "variance",
      panel = "hint",
      error = "within",
      mapping = c("shape", "color"),
      data_geom = geom_point,
      emmeans_arg = list(model = "multivariate"),
      factor_levels = list(
        npv_amount = c(
          X100 = "100",
          X300 = "300",
          X500 = "500",
          X700 = "700",
          X900 = "900"
        )
      ),
      legend_title = "Variance"
    ) +
    labs(
      x = "NPV ($)",
      y = dv_label
    ) +
    papaja::theme_apa()
}
