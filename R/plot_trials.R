##' @title Plot trials

##' @param data
##'
##' @return
##' @author Shir Dekel
##' @export
plot_trials <- function(data) {

  trials_plot <-
    data %>%
    apa_plot(iv1 = "gamble",
             dv = "choice") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

  return(trials_plot)

}
