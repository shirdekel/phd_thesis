##' @title Get similarity effect for similarity rating
##' Manipulation check  Similarity conditions are rated accordingly
##'   Similarity rating: High similarity > low similarity
##'   Regardless of condition
##' @return
##' @author Shir Dekel
##' @export
##' @param model_similarity_rating
get_similarity_rating_similarity <- function(model_similarity_rating) {
  model_similarity_rating %>%
    apa_print(es = "pes", mse = FALSE) %>%
    pluck("full_result", "similarity")
}
