#' @title Plot afex aov object
#' @param afex_aov
#' @param dv_label
#' @return
#' @author Shir Dekel
#' @export
afex_plot_alignment_1 <- function(afex_aov, dv_label = "Allocation (%)") {
  dodge_width <- 0.5
  afex_aov %>%
    afex_plot(
      x = "npv_amount",
      trace = "reliability_amount",
      panel = "alignment",
      error = "none",
      mapping = c("shape", "color"),
      data_geom = ggbeeswarm::geom_quasirandom,
      data_arg = list(
        dodge.width = dodge_width,
        color = "darkgrey"
      ),
      dodge = dodge_width,
      factor_levels = list(
        npv_amount = c(
          X100 = "100",
          X300 = "300",
          X500 = "500",
          X700 = "700",
          X900 = "900"
        ),
        reliability_amount = c(
          noNPV = "No NPV",
          low = "Low",
          high = "High"
        ),
        alignment = c(
          low = "Low alignment",
          high = "High alignment"
        )
      ),
      legend_title = "Reliability level"
    ) +
    labs(
      x = "NPV ($)",
      y = dv_label
    ) +
    papaja::theme_apa()
}
