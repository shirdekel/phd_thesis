##' @title Get plot for choice x trial with optional grouping
##'
##' @param grouping
##' @param data
##'
##' @return
##' @author Shir Dekel
##' @export
plot_choice_trials <- function(data, grouping = NULL) {
  choice_trials_plot <-
    data %>%
    filter(presentation == "separate", distribution == "absent") %>%
    mutate(
      across(
        where(is.factor),
        str_to_sentence
      )
    ) %>%
    ggplot(
      aes(
        x = project_order,
        y = choice,
        color = {{ grouping }},
        linetype = {{ grouping }}
      )
    ) +
    geom_smooth(method = "loess") +
    scale_x_continuous("Trial", breaks = 1:max(data$project_order)) +
    papaja::theme_apa() +
    labs(y = "Proportion of project acceptance")

  return(choice_trials_plot)
}
