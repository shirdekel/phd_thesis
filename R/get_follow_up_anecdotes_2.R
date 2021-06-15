#' @title Get follow up results
##' @param data
#' @return
#' @author Shir Dekel
#' @export
get_follow_up_anecdotes_2 <- function(data) {
  model_similarity_rating <-
    data %>%
    get_model_similarity_rating()

  similarity_rating_similarity <-
    model_similarity_rating %>%
    get_similarity_rating_similarity()

  allocation_similarity_rating <-
    data %>%
    get_allocation_similarity_rating()

  allocation_specific_relevance <-
    data %>%
    get_allocation_specific_relevance()

  lst(
    similarity_rating_similarity,
    allocation_similarity_rating,
    allocation_specific_relevance
  )
}
