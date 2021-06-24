##' @title Get plot for Experiment 2
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_plot_aggregation_2 <- function(data = aggregation2::data) {
  dv_label <- "Mean proportions of project acceptance"

  data_split <-
    split_data(data)

  choice_proportion_omnibus <-
    list(
      data_split,
      names(data_split)
    ) %>%
    pmap(
      ~ .x %>%
        get_omnibus_aggregation("proportion", .y)
    )

  choice_proportion_presentation <-
    choice_proportion_omnibus$presentation %>%
    afex_plot(
      x = "presentation",
      mapping = c("shape", "color"),
      error_arg = list(width = 0.05),
      data_geom = ggbeeswarm::geom_quasirandom,
      point_arg = list(size = 3),
      factor_levels = list(
        presentation = c(
          separate = "Separate",
          joint = "Joint"
        )
      )
    ) +
    labs(
      x = "Presentation",
      y = dv_label
    ) +
    papaja::theme_apa() +
    theme(legend.position = "none")

  choice_proportion_awareness <-
    choice_proportion_omnibus$awareness %>%
    afex_plot(
      x = "awareness",
      mapping = c("shape", "color"),
      error_arg = list(width = 0.05),
      data_geom = ggbeeswarm::geom_quasirandom,
      point_arg = list(size = 3),
      factor_levels = list(
        awareness = c(
          naive = "Naive",
          aware = "Aware"
        )
      )
    ) +
    labs(
      x = "Awareness",
      y = dv_label
    ) +
    papaja::theme_apa() +
    theme(legend.position = "none")

  choice_proportion_distribution <-
    choice_proportion_omnibus$distribution %>%
    afex_plot(
      x = "distribution",
      mapping = c("shape", "color"),
      error_arg = list(width = 0.05),
      data_geom = ggbeeswarm::geom_quasirandom,
      point_arg = list(size = 3),
      factor_levels = list(
        distribution = c(
          absent = "Absent",
          present = "Present"
        )
      )
    ) +
    labs(
      x = "Distribution",
      y = dv_label
    ) +
    papaja::theme_apa() +
    theme(legend.position = "none")

  choice_proportion <-
    cowplot::plot_grid(
      choice_proportion_presentation,
      choice_proportion_awareness + ylab(NULL),
      choice_proportion_distribution + ylab(NULL),
      nrow = 1
    )

  choice_binary <-
    plot_choice(
      data_split,
      choice,
      "Mean choice of project acceptance"
    )

  portfolio_number <-
    plot_choice(
      data_split,
      portfolio_number,
      "Mean number of project acceptance"
    )

  portfolio_binary <-
    plot_choice(
      data_split,
      portfolio_binary,
      "Mean choice of complete project portfolio acceptance"
    )

  choice_trials <-
    data %>%
    plot_choice_trials(awareness) +
    labs(
      color = "Awareness",
      linetype = "Awareness"
    )

  project_number <-
    plot_project_number(data, condition)

  gamble_values <-
    plot_gamble_values(data)

  trials <-
    plot_trials(data)

  plot_aggregation_2 <-
    lst(
      choice_binary,
      choice_proportion,
      portfolio_number,
      portfolio_binary,
      choice_trials,
      project_number,
      gamble_values,
      trials
    )

  return(plot_aggregation_2)
}
