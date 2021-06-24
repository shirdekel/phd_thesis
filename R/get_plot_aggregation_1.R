##' @title Get plot for Experiment 1
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_plot_aggregation_1 <- function(data = aggregation1::data) {
  data_proportion <-
    data %>%
    nest_by(id, awareness, presentation, similarity, proportion)

  dv.lab <- "Mean proportions of project acceptance"

  awareness <-
    apa_plot(
      data = data_proportion,
      iv1 = "awareness",
      iv1.lab = "Awareness of project set size",
      dv = "proportion",
      dv.lab = dv.lab
    )

  presentation <-
    apa_plot(
      data = data_proportion,
      iv1 = "presentation",
      dv = "proportion",
      dv.lab = dv.lab
    )

  similarity_presentation <-
    apa_plot(
      data = data_proportion,
      iv1 = "similarity",
      iv1.lab = "Similarity",
      iv2 = "presentation",
      dv = "proportion",
      dv.lab = dv.lab
    )

  trials <-
    data %>%
    ggplot(aes(x = project_order, y = choice, linetype = similarity)) +
    geom_smooth(method = "loess", color = "black") +
    facet_wrap(vars(awareness, presentation), labeller = "label_both") +
    scale_x_continuous("Trial", breaks = 1:10) +
    papaja::theme_apa() +
    labs(y = "Proportion of project acceptance")

  trials_separate_awareness <-
    data %>%
    filter(presentation == "separate") %>%
    ggplot(aes(x = project_order, y = choice, linetype = awareness)) +
    geom_smooth(method = "loess", color = "black") +
    scale_x_continuous("Trial", breaks = 1:10) +
    papaja::theme_apa() +
    labs(y = "Proportion of project acceptance")

  lst(
    awareness,
    presentation,
    similarity_presentation,
    trials,
    trials_separate_awareness
  )
}
