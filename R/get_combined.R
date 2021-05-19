##' @title Get combined anecdote condition analysis
##' Anecdotal bias moderated by similarity
##'   Negative valence
##'     Statistics only > combined high similarity

##'     Combined: Low similarity > high similarity

##'     Statistics only = combined low similarity

##'   Positive valence
##'     Statistics only > combined low similarity

##'     Combined: High similarity > low similarity

##'     Statistics only > combined high similarity
##' @return
##' @author Shir Dekel
##' @export
##' @param data_clean
get_combined <- function(data, valence) {
  data %>%
    filter(
      valence %in% c(!!valence, "NA"),
      anecdote_between == "combined"
    ) %>%
    mutate(
      within = case_when(
        anecdote_within == "anecdote" ~ str_c("similarity", similarity,
          sep = "_"
        ),
        TRUE ~ anecdote_within
      )
    ) %>%
    lm(
      allocation ~ within,
      data = .
    ) %>%
    emmeans("within") %>%
    pairs(adjust = "none")
}
