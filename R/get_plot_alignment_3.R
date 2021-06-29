##' @title Get plot for alignment 3
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_plot_alignment_3 <- function(data = alignment3::data) {
  allocation <-
    data %>%
    filter(phase == "pre") %>%
    nest_by(id, allocation, npv_amount, reliability_amount, alignment) %>%
    get_omnibus_alignment_3("allocation") %>%
    afex_plot_alignment_3(dv_label = "Mean allocation (%)")

  ranking <-
    data %>%
    filter(phase == "pre") %>%
    nest_by(id, ranking, npv_amount, reliability_amount, alignment) %>%
    get_omnibus_alignment_3("ranking") %>%
    afex_plot_alignment_3(dv_label = "Mean ranking")

  confidence <-
    data %>%
    filter(phase == "pre") %>%
    nest_by(id, confidence, npv_amount, reliability_amount, alignment) %>%
    get_omnibus_alignment_3("confidence") %>%
    afex_plot_alignment_3(dv_label = "Mean confidence rating")

  npv_knowledge <-
    data %>%
    filter(phase == "pre") %>%
    nest_by(id, rating, npv_knowledge) %>%
    aov_ez(
      id = "id",
      dv = "npv_knowledge",
      within = "rating",
      data = .,
      type = 2
    ) %>%
    afex_plot_npv_knowledge()

  dodge_width <- 0.5

  variance_lecture <-
    data %>%
    get_variance_lecture_omnibus() %>%
    afex_plot(
      x = "npv_amount",
      trace = "reliability_amount",
      panel = c("phase", "alignment_between"),
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
          X100 = "100",
          X300 = "300",
          X500 = "500",
          X700 = "700",
          X900 = "900"
        ),
        phase = c(
          pre = "Pre-lecture",
          post = "Post-lecture"
        ),
        reliability_amount = c(
          low = "Low",
          high = "High"
        ),
        alignment_between = c(
          low = "Low alignment",
          high = "High alignment"
        )
      ),
      legend_title = "Reliability level"
    ) +
    labs(
      x = "NPV ($)",
      y = "Mean allocation (%)"
    ) +
    papaja::theme_apa()

  lst(
    allocation,
    ranking,
    confidence,
    npv_knowledge,
    variance_lecture
  )
}
