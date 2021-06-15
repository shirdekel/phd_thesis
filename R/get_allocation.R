#' @title Get anecdotes 2 allocation results
#'
#' Excluding the statistics only condition

#' @return
#' @author Shir Dekel
#' @export
#' @param data
get_allocation <- function(data) {
  allocation_omnibus <-
    data %>%
    get_omnibus_anecdotes_2_allocation()

  allocation_omnibus_apa <-
    allocation_omnibus %>%
    apa_print(es = "pes", mse = FALSE) %>%
    pluck("full_result")

  anecdotes_only_similarity <-
    c("anecdote_only_negative_low_high", "anecdote_only_positive_low_high") %>%
    map(
      ~ allocation_omnibus %>%
        get_anecdotes_only_similarity() %>%
        apa_print() %>%
        pluck("full_result", .x)
    ) %>%
    set_names("valence_negative", "valence_positive")

  similarity_high_anecdote <-
    c(
      "high_negative_anecdote_only_combined",
      "high_positive_anecdote_only_combined"
    ) %>%
    map(
      ~ allocation_omnibus %>%
        get_similarity_high_anecdote() %>%
        apa_print() %>%
        pluck("full_result", .x)
    ) %>%
    set_names("valence_negative", "valence_positive")

  allocation_omnibus_apa %>%
    c(
      lst(
        anecdotes_only_similarity,
        similarity_high_anecdote
      )
    )
}
