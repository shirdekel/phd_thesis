##' @title Get plot for Experiment 2
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_plot_aggregation_2 <- function(data) {

  data_split <-
    split_data(data)

  choice_binary <-
    plot_choice(
      data_split,
      choice,
      "Mean choice of project acceptance"
    )

  choice_proportion <-
    plot_choice(
      data_split,
      proportion,
      "Mean proportion of project acceptance"
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
    plot_choice_trials(data, linetype = awareness)

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
