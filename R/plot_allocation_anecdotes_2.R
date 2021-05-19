##' @title Plot anecdotes 2 allocation

##' @return
##' @author Shir Dekel
##' @export
##' @param data_clean_anecdotes_2
##' @param valence
plot_allocation_anecdotes_2 <- function(data_clean_anecdotes_2, valence) {
  dodgewidth <- 0.9
  data_clean_anecdotes_2 %>%
    filter(valence %in% c(!!valence, "NA")) %>%
    nest_by(
      id,
      anecdote_between,
      anecdote_within,
      similarity,
      allocation
    ) %>%
    ungroup() %>%
    mutate(
      across(c(similarity), ~ .x %>%
        fct_explicit_na(na_level = "NA")),
      across(where(is.factor), as.character)
    ) %>%
    ggplot(
      mapping = aes(
        x = anecdote_between,
        y = allocation,
        fill = similarity,
        colour = anecdote_within,
        shape = anecdote_within
      )
    ) +
    geom_violin() +
    stat_summary(
      fun.data = mean_cl_normal,
      geom = "errorbar",
      size = 0.5,
      width = 0.1,
      position = position_dodge(width = dodgewidth)
    ) +
    stat_summary(
      fun = mean,
      geom = "point",
      size = 3,
      position = position_dodge(width = dodgewidth)
    ) +
    scale_fill_grey(start = 0.3, end = 1) +
    labs(
      x = "Anecdote (between-subjects)",
      fill = "Similarity",
      y = "Allocation",
      colour = "Anecdote (within-subjects)",
      shape = "Anecdote (within-subjects)"
    ) +
    theme_apa(base_size = 10)
}
