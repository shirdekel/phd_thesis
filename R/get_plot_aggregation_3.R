##' @title Get plot for Experiment 3
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_plot_aggregation_3 <- function(data) {

  choice_binary <-
    data %>%
    apa_plot(
      iv1 = "similarity",
      iv1.lab = "Similarity condition",
      dv = "choice",
      dv.lab = "Mean choice of project acceptance")


  choice_proportion <-
    data %>%
    nest_by(subject, similarity, proportion) %>%
    ungroup() %>%
    apa_plot(
      iv1 = "similarity",
      iv1.lab = "Similarity condition",
      dv = "proportion",
      dv.lab = "Mean proportion of project acceptance")

  portfolio_number <-
    data %>%
    nest_by(subject, similarity, portfolio_number) %>%
    ungroup() %>%
    apa_plot(
      iv1 = "similarity",
      iv1.lab = "Similarity condition",
      dv = "portfolio_number",
      dv.lab = "Mean number of project acceptance")

  portfolio_binary <-
    data %>%
    nest_by(subject, similarity, portfolio_binary) %>%
    ungroup() %>%
    apa_plot(
      iv1 = "similarity",
      iv1.lab = "Similarity condition",
      dv = "portfolio_binary",
      dv.lab = "Mean choice of complete project portfolio acceptance")

  project_expectation <-
    data %>%
    nest_by(subject, similarity, project_expectation) %>%
    ungroup() %>%
    apa_plot(
      iv1 = "similarity",
      iv1.lab = "Similarity condition",
      dv = "project_expectation",
      dv.lab = "Mean number of expected projects")

  choice_trials <-
    plot_choice_trials(data, linetype = similarity)

  project_number <-
    plot_project_number(data, similarity)

  gamble_values <-
    plot_gamble_values(data)

  trials <-
    plot_trials(data)

  plot_aggregation_3 <-
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

  return(plot_aggregation_3)

}
