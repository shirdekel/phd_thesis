##' @title Get plot for Experiment 4
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_plot_aggregation_4 <- function(data) {

  choice_binary <-
    data %>%
    apa_plot(
      iv1 = "awareness",
      iv1.lab = "Awareness condition",
      dv = "choice",
      dv.lab = "Mean choice of project acceptance")


  choice_proportion <-
    data %>%
    nest_by(subject, awareness, proportion) %>%
    ungroup() %>%
    apa_plot(
      iv1 = "awareness",
      iv1.lab = "Awareness condition",
      dv = "proportion",
      dv.lab = "Mean proportion of project acceptance")

  portfolio_number <-
    data %>%
    nest_by(subject, awareness, portfolio_number) %>%
    ungroup() %>%
    apa_plot(
      iv1 = "awareness",
      iv1.lab = "Awareness condition",
      dv = "portfolio_number",
      dv.lab = "Mean number of project acceptance")

  portfolio_binary <-
    data %>%
    nest_by(subject, awareness, portfolio_binary) %>%
    ungroup() %>%
    apa_plot(
      iv1 = "awareness",
      iv1.lab = "Awareness condition",
      dv = "portfolio_binary",
      dv.lab = "Mean choice of complete project portfolio acceptance")

  project_expectation <-
    data %>%
    nest_by(subject, awareness, project_expectation) %>%
    ungroup() %>%
    apa_plot(
      iv1 = "awareness",
      iv1.lab = "Awareness condition",
      dv = "project_expectation",
      dv.lab = "Mean number of expected projects")

  choice_trials <-
    plot_choice_trials(data, awareness)

  project_number <-
    plot_project_number(data, awareness)

  gamble_values <-
    plot_gamble_values(data)

  trials <-
    plot_trials(data)

  plot_aggregation_4 <-
    lst(
      choice_binary,
      choice_proportion,
      portfolio_number,
      portfolio_binary,
      project_expectation,
      choice_trials,
      project_number,
      gamble_values,
      trials
    )

  return(plot_aggregation_4)

}
