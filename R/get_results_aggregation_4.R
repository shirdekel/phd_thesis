##' @title Experiment 4 results

##' @param data
##' @param iv
##' @param dv
##' @return
##' @author Shir Dekel
##' @export
get_results_aggregation_4 <- function(data, iv, dv) {
  choice <-
    data %>%
    nest_by(id, awareness, choice) %>%
    glm(
      choice ~ awareness,
      family = binomial,
      data = .
    ) %>%
    apa_print()

  awareness_project_order <-
    data %>%
    nest_by(id, awareness, choice, project_order) %>%
    glmer(
      choice ~ awareness * project_order + (project_order || id),
      family = binomial,
      data = .
    ) %>%
    apa_print()

  proportion <-
    data %>%
    nest_by(id, awareness, proportion) %>%
    ungroup() %>%
    get_ttest_apa(
      iv = "awareness",
      dv = "proportion"
    )

  project_expectation <-
    data %>%
    nest_by(id, awareness, project_expectation) %>%
    ungroup() %>%
    get_ttest_apa(
      iv = "awareness",
      dv = "project_expectation"
    )

  portfolio_binary <-
    glm(portfolio_binary ~ awareness,
      family = binomial,
      data = data %>%
        nest_by(id, awareness, portfolio_binary)
    ) %>%
    apa_print()

  portfolio_number <-
    data %>%
    nest_by(id, awareness, portfolio_number) %>%
    ungroup() %>%
    get_ttest_apa(
      iv = "awareness",
      dv = "portfolio_number"
    )

  results_experiment4 <-
    lst(
      choice,
      awareness_project_order,
      proportion,
      project_expectation,
      portfolio_binary,
      portfolio_number
    )

  return(results_experiment4)
}
