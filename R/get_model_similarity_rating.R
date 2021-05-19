##' @title Get anecdotes 2 similarity rating model

##' @return
##' @author Shir Dekel
##' @export
##' @param data_analysis
get_model_similarity_rating <- function(data_analysis) {
  data_analysis %>%
    aov_4(
      similarity_rating ~
      anecdote_between +
        (c(similarity * valence) | id),
      data = .
    )
}
