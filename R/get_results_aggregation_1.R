##' @title Get aggregation 1 results
##'
##' @param data_clean_aggregation_1
##' @param iv
##' @param dv
##' @return
##' @author Shir Dekel
##' @export
get_results_aggregation_1 <- function(data_clean_aggregation_1, iv, dv) {
  set_sum_contrasts()

  model <-
    data_clean_aggregation_1 %>%
    nest_by(id, similarity, awareness, presentation, proportion) %>%
    aov_ez(
      dv = "proportion",
      id = "id",
      between = c("similarity", "awareness"),
      within = c("presentation"),
      anova_table = "pes",
      type = 2
    )

  anova <-
    model %>%
    apa_print(es = "pes", mse = FALSE) %>%
    pluck("full_result")

  simple_effects <-
    model %>%
    emmeans(~ similarity * presentation) %>%
    pairs(adjust = "none") %>%
    apa_print() %>%
    pluck("full_result")

  simple_effects_all <-
    model %>%
    emmeans(~ similarity * presentation * awareness) %>%
    pairs(adjust = "none") %>%
    apa_print() %>%
    pluck("full_result")

  neg_sum_apa <-
    data_clean_aggregation_1 %>%
    nest_by(id, choice_neg_1, choice_neg_2) %>%
    mutate(neg_sum = sum(choice_neg_1, choice_neg_2)) %>%
    pull(neg_sum) %>%
    as.integer() %>%
    sum() %>%
    printnum(numerals = FALSE)

  aggregated_values_samuelson <-
    get_aggregated_values(
      outcome_positive = 200 %>%
        rep(10),
      prob_positive = 0.5 %>%
        rep(10),
      300
    )

  loss_prob_samuelson <-
    get_loss_prob(
      aggregated_values_samuelson$outcome_aggregated,
      aggregated_values_samuelson$prob_aggregated
    )

  data_individual <-
    data_clean_aggregation_1 %>%
    nest_by(id, presentation, proportion)

  gambles_individual <-
    c("separate", "joint") %>%
    map(~ data_individual %>%
      filter(presentation == .x) %>%
      pull(proportion))

  gambles_aggregated <-
    data_clean_aggregation_1 %>%
    nest_by(id, choice_aggregated) %>%
    pull(choice_aggregated)

  individual_aggregated <-
    gambles_individual %>%
    map(~ t_print(gambles_aggregated, .x, paired = TRUE)) %>%
    set_names("separate", "joint")

  trials_separate_awareness_model <-
    data_clean_aggregation_1 %>%
    filter(presentation == "separate") %>%
    glm(choice ~ awareness * project_order, ., family = binomial)

  trials_separate_awareness <-
    trials_separate_awareness_model %>%
    apa_print()

  trials_separate_awareness_slope <-
    trials_separate_awareness_model %>%
    emtrends(~awareness, var = "project_order") %>%
    apa_print_emtrends() %>%
    super_split(awareness) %>%
    map(pull, apa)

  lst(
    neg_sum_apa,
    loss_prob_samuelson,
    individual_aggregated,
    trials_separate_awareness,
    trials_separate_awareness_slope
  ) %>%
    append(anova) %>%
    append(simple_effects) %>%
    append(simple_effects_all)
}
