##' @title Get allocation x similarity rating correlations
##' Allocation is influenced by perceived similarity
##'   Negative valence
##'     Negative correlation between allocation and similarity rating
##'   Positive valence
##'     Positive correlation between allocation and similarity rating

##' @return
##' @author Shir Dekel
##' @export
##' @param data_analysis
get_allocation_similarity_rating <- function(data_analysis) {
  data_analysis %>%
    lm(
      allocation ~
      valence * similarity * similarity_rating,
      data = .
    ) %>%
    emtrends(c("valence", "similarity"), var = "similarity_rating") %>%
    pairs(by = c("valence")) %>%
    apa_print() %>%
    pluck("full_result")
}
