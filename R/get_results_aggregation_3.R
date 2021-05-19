##' @title Experiment 3 results

##' @param data
##' @param iv
##' @param dv
##' @return
##' @author Shir Dekel
##' @export
get_results_aggregation_3 <- function(data, iv, dv) {
  choice <-
    data %>%
    nest_by(id, similarity, choice) %>%
    glm(
      choice ~ similarity,
      family = binomial,
      data = .
    ) %>%
    apa_print()

  similarity_project_order <-
    data %>%
    nest_by(id, similarity, choice, project_order) %>%
    glmer(
      choice ~ similarity * project_order + (project_order | id),
      family = binomial,
      data = .
    ) %>%
    apa_print()

  proportion <-
    data %>%
    nest_by(id, similarity, proportion) %>%
    ungroup() %>%
    get_ttest_apa(
      iv = "similarity",
      dv = "proportion"
    )

  project_expectation <-
    data %>%
    nest_by(id, similarity, project_expectation) %>%
    ungroup() %>%
    get_ttest_apa(
      iv = "similarity",
      dv = "project_expectation"
    )

  portfolio_binary <-
    glm(
      portfolio_binary ~ similarity,
      family = binomial,
      data = data %>%
        nest_by(id, similarity, portfolio_binary)
    ) %>%
    apa_print()

  portfolio_number <-
    data %>%
    nest_by(id, similarity, portfolio_number) %>%
    ungroup() %>%
    get_ttest_apa(
      iv = "similarity",
      dv = "portfolio_number"
    )

  results_experiment3 <-
    lst(
      choice,
      similarity_project_order,
      proportion,
      project_expectation,
      portfolio_binary,
      portfolio_number
    )

  return(results_experiment3)
}
