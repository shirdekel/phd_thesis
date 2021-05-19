##' @title Plot gamble values
##' @param data
##' @return
##' @author Shir Dekel
##' @export
plot_gamble_values <- function(data) {

  expected_value_plot <-
    data %>%
    ggplot(aes(y = choice, x = expected_value)) +
    geom_smooth(method = "loess", color = "black") +
    theme_apa()

  probability_positive_plot <-
    data %>%
    ggplot(aes(y = choice, x = probability_positive)) +
    geom_smooth(method = "loess", color = "black") +
    theme_apa() +
    theme(axis.title.y = element_blank())

  outcome_positive_plot <-
    data %>%
    ggplot(aes(y = choice, x = outcome_positive)) +
    geom_smooth(method = "loess", color = "black") +
    theme_apa()+
    theme(axis.title.y = element_blank())

  gamble_values_plot <-
    expected_value_plot + probability_positive_plot + outcome_positive_plot

  return(gamble_values_plot)

}
