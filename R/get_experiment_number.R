##' @title Get experiment number
##' 
##' For `get_values()`

##' @return
##' @author Shir Dekel
##' @export
get_experiment_number <- function() {
  c(
    seq_len(4),
    seq_len(8),
    seq_len(2)
  )
}
