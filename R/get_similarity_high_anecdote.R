##' @title Get anecdote analysis in the high similarity condition
##'
##' For Anecdotes 2
##' Effect of statistics

##'   Negative valence
##'     High similarity: combined > anecdote only
##'   Positive valence
##'     High similarity: anecdote only > combined

##' @return
##' @author Shir Dekel
##' @export
##' @param model
get_similarity_high_anecdote <- function(model) {
  model %>%
    emmeans(c("anecdote_between", "similarity", "valence")) %>%
    pairs(by = c("similarity", "valence"))
}
