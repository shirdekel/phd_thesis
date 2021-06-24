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

  lst(
    allocation,
    ranking,
    confidence,
    npv_knowledge
  )
}
