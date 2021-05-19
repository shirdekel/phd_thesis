##' @title Get similarity analysis in the anecdotes only condition
##'
##' For Anecdotes 2
##'
##' Similarity check
##'   Negative valence
##'     Anecdotes only: low similarity > high similarity
##'   Positive valence
##'     Anecdotes only: high similarity > low similarity
##' @return
##' @author Shir Dekel
##' @export
##' @param model
get_anecdotes_only_similarity <- function(model) {
  model %>%
    emmeans(c("anecdote_between", "similarity", "valence")) %>%
    pairs(by = c("anecdote_between", "valence"))
}
