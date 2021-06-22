##' @title Get allocation specific relevance correlation

##' The relationship between allocation and specific relevance is moderated by
##'  similarity

##'   Negative valence

##'     Low similarity: no correlation between allocation and specific
##'     relevance rating

##'     High similarity: negative correlation between allocation and specific
##'     relevance rating

##'   Positive valence

##'     Low similarity: no correlation between allocation and specific
##'     relevance rating

##'     High similarity: positive correlation between allocation and specific
##'     relevance rating

##' @return
##' @author Shir Dekel
##' @export
##' @param data
get_allocation_specific_relevance <- function(data) {
  emm_allocation_specific_relevance <-
    data %>%
    lm(
      allocation ~
      valence * similarity * relevance_specific_rating,
      data = .
    ) %>%
    emtrends(c("valence", "similarity"), var = "relevance_specific_rating")

  individual <-
    emm_allocation_specific_relevance %>%
    contrast(by = "valence", adjust =  "none") %>%
    apa_print() %>%
    pluck("full_result")

  emm_allocation_specific_relevance %>%
    contrast(interaction = "pairwise", by = "valence", adjust =  "none") %>%
    apa_print() %>%
    pluck("full_result") %>%
    c(individual, .)
}
