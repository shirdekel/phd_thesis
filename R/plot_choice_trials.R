##' @title Get plot for choice x trial with optional linetype
##'
##' @param linetype
##' @param data
##'
##' @return
##' @author Shir Dekel
##' @export
plot_choice_trials <- function(data, linetype = NULL) {

  choice_trials_plot <-
    data %>%
    filter(presentation == "separate", distribution == "absent") %>%
    ggplot(aes(x = project_order, y = choice, linetype = {{ linetype }})) +
    geom_smooth(method = "loess", color = "black") +
    scale_x_continuous("Trial", breaks = 1:max(data$project_order)) +
    theme_apa() +
    labs(y = "Proportion of project acceptance")

  return(choice_trials_plot)

}
