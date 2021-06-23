#' @title Plot afex aov object
#' @param afex_aov
#' @param dv_label
#' @return
#' @author Shir Dekel
#' @export
afex_plot_alignment_1 <- function(afex_aov, dv_label = "Allocation (%)") {
  suppressMessages({
    afex_aov %>%
      afex_plot(
        x = "npv_amount",
        trace = "reliability_amount",
        panel = "alignment",
        error = "none",
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
            high = "High",
            low = "Low"
          ),
          alignment = c(
            high = "High",
            low = "Low"
          )
        ),
        legend_title = "Reliability amount"
      ) +
      labs(
        x = "NPV Amount ($)",
        y = dv_label
      ) +
      theme_apa()
  })
}
