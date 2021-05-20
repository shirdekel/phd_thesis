##' @title Generate outcome probability distribution
##'
##' For Samuelson gamble
##'
##' Note, order of outcomes (positive, negative) before aggregation doesn't
##' matter

##' @return
##' @author Shir Dekel
##' @export
get_samuelson_distribution <- function() {
  aggregated_values <-
    shirthesis::get_aggregated_values(
      200 %>%
        rep(10),
      .5 %>%
        rep(10),
      300
    )

  shirthesis::get_distribution(
    aggregated_values$outcome_aggregated,
    aggregated_values$prob_aggregated
  )
}
