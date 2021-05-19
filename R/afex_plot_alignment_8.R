#' @title Plot afex aov object
#' @param afex_aov
#' @param dv_label
#' @return
#' @author Shir Dekel
#' @export
afex_plot_alignment_8 <- function(afex_aov, dv_label = "Allocation (%)") {
  suppressWarnings({
    afex_aov %>%
      afex_plot(
        x = "npv_amount",
        trace = "reliability_amount",
        panel = c("alignment", "reliability_type"),
        error = "within",
        mapping = c("shape", "color"),
        data_geom = geom_point,
        emmeans_arg = list(model = "multivariate"),
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
        legend_title = "Reliability amount"
      ) +
      labs(
        x = "NPV Amount ($)",
        y = dv_label
      ) +
      theme_apa()
  })
}
