#' @title Get all results

#' @return
#' @author Shir Dekel
#' @export
get_results_all_alignment_7 <- function(data_clean_alignment_7,
                                        alignment_condition, dv) {
  omnibus <-
    data_clean_alignment_7 %>%
    nest_by(
      id, allocation, ranking, npv_cond, reliability_amount, reliability_type,
      alignment, project
    ) %>%
    group_by(
      id, npv_cond, reliability_amount, reliability_type,
      alignment
    ) %>%
    mutate(
      project_variation = seq_len(2)
    ) %>%
    ungroup() %>%
    filter(alignment == alignment_condition) %>%
    aov_ez(
      id = "id",
      dv = dv,
      within = c("reliability_type", "npv_cond", "reliability_amount"),
      data = .,
      type = 2
    )

  omnibus_apa <-
    omnibus %>%
    apa_print(es = "pes", mse = FALSE) %>%
    pluck("full_result")

  simple_effects <-
    omnibus %>%
    emmeans(c("reliability_amount", "npv_cond"),
      by = "reliability_type"
    ) %>%
    pairs(adjust = "bonferroni") %>%
    apa_print() %>%
    pluck("full_result")

  omnibus_apa %>%
    append(simple_effects)
}
