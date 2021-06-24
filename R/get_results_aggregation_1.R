##' @title Get aggregation 1 results
##'
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_results_aggregation_1 <- function(data = aggregation1::data) {
  set_sum_contrasts()

  proportion_omnibus <-
    data %>%
    get_omnibus_aggregation_1()

  anova <-
    proportion_omnibus %>%
    papaja::apa_print(es = "pes", mse = FALSE) %>%
    pluck("full_result")

  simple_effects <-
    proportion_omnibus %>%
    emmeans(~ similarity * presentation) %>%
    pairs(adjust = "none") %>%
    papaja::apa_print() %>%
    pluck("full_result")

  simple_effects_all <-
    proportion_omnibus %>%
    emmeans(~ similarity * presentation * awareness) %>%
    pairs(adjust = "none") %>%
    papaja::apa_print() %>%
    pluck("full_result")

  neg_sum_apa <-
    data %>%
    nest_by(id, choice_neg_1, choice_neg_2) %>%
    mutate(neg_sum = sum(choice_neg_1, choice_neg_2)) %>%
    pull(neg_sum) %>%
    as.integer() %>%
    sum() %>%
    papaja::printnum(numerals = FALSE)


  outcome_positive <-
    seq(from = 200, to = 100, length.out = 10) %>%
    round(-1)

  prob_positive <-
    seq(from = .2, to = .45, length.out = 5) %>%
    c(seq(from = .55, to = .6, length.out = 5)) %>%
    round(2)

  max_probability_negative <-
    prob_positive %>%
    {
      1 - .
    } %>%
    max()

  aggregated_values <-
    shirthesis::get_aggregated_values(
      outcome_positive = outcome_positive,
      prob_positive = prob_positive,
      outcome_dif = 240,
      sort = FALSE
    )

  loss_prob <-
    shirthesis::get_loss_prob(
      aggregated_values$outcome_aggregated,
      aggregated_values$prob_aggregated
    )

  aggregated_values_correct <-
    shirthesis::get_aggregated_values(
      outcome_positive = outcome_positive,
      prob_positive = prob_positive,
      outcome_dif = 240,
    )

  loss_prob_correct <-
    shirthesis::get_loss_prob(
      aggregated_values_correct$outcome_aggregated,
      aggregated_values_correct$prob_aggregated
    )

  aggregated_values_samuelson <-
    shirthesis::get_aggregated_values(
      outcome_positive = 200 %>%
        rep(10),
      prob_positive = 0.5 %>%
        rep(10),
      300
    )

  loss_prob_samuelson <-
    shirthesis::get_loss_prob(
      aggregated_values_samuelson$outcome_aggregated,
      aggregated_values_samuelson$prob_aggregated
    )

  data_individual <-
    data %>%
    nest_by(id, presentation, proportion)

  gambles_individual <-
    c("separate", "joint") %>%
    map(~ data_individual %>%
      filter(presentation == .x) %>%
      pull(proportion))

  gambles_aggregated <-
    data %>%
    nest_by(id, choice_aggregated) %>%
    pull(choice_aggregated)

  individual_aggregated <-
    gambles_individual %>%
    map(~ t_print(gambles_aggregated, .x, paired = TRUE)) %>%
    set_names("separate", "joint")

  trials_separate_awareness_model <-
    data %>%
    filter(presentation == "separate") %>%
    glm(choice ~ awareness * project_order, ., family = binomial)

  trials_separate_awareness <-
    trials_separate_awareness_model %>%
    papaja::apa_print()

  trials_separate_awareness_slope <-
    trials_separate_awareness_model %>%
    emtrends(~awareness, var = "project_order") %>%
    apa_print_emtrends() %>%
    printy::super_split(awareness) %>%
    map(pull, apa)

  lst(
    neg_sum_apa,
    loss_prob,
    loss_prob_correct,
    loss_prob_samuelson,
    individual_aggregated,
    trials_separate_awareness,
    trials_separate_awareness_slope,
    max_probability_negative
  ) %>%
    append(anova) %>%
    append(simple_effects) %>%
    append(simple_effects_all)
}
