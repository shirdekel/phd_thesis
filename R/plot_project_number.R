##' @title Plot project number data
##'
##' @param condition
##' @param data
##'
##' @return
##' @author Shir Dekel
##' @export
plot_project_number <- function(data, condition = NULL) {

  project_number_plot <- data %>%
    nest_by(subject, {{condition}}, project_number) %>%
    ggplot(aes(project_number, color={{condition}})) +
    geom_freqpoly(bins = 20) +
    theme_apa()

  return(project_number_plot)

}
