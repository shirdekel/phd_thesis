#' @title Plot afex aov object
#' @param afex_aov
#' @param dv_label
#' @return
#' @author Shir Dekel
#' @export
afex_plot_alignment_8 <- function(afex_aov, dv_label) {
  dodge_width <- 0.5

  afex_aov %>%
    afex_plot(
      x = "npv_amount",
      trace = "reliability_amount",
      panel = c("alignment", "reliability_type"),
      mapping = c("shape", "color"),
      error = "within",
      error_arg = list(width = 0.1),
      data_geom = ggbeeswarm::geom_quasirandom,
      data_arg = list(
        dodge.width = dodge_width,
        color = "darkgrey"
      ),
      dodge = dodge_width,
      factor_levels = list(
        npv_amount = c(
          X400 = "400",
          X500 = "500",
          X600 = "600",
          X700 = "700",
          X800 = "800"
        ),
        reliability_amount = c(
          low = "Low",
          high = "High"
        ),
        reliability_type = c(
          explicit = "Verbal reliability",
          implicit = "Numerical reliability"
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
