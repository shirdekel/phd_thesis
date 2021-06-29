#' @title Afex plot

#' @return
#' @author Shir Dekel
#' @export
#' @param afex_aov
#' @param dv_label
afex_plot_alignment_4 <- function(afex_aov, dv_label) {
  afex_aov %>%
    afex_plot(
      x = "npv_amount",
      trace = "reliability_amount",
      panel = "alignment",
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
        ),
        reliability_amount = c(
          low = "Low",
          high = "High"
        ),
        alignment = c(
          low = "Low alignment",
          high = "High alignment"
        )
      ),
      legend_title = "NPV Presence"
    ) +
    labs(
      x = "NPV ($)",
      y = dv_label
    ) +
    papaja::theme_apa()
}
