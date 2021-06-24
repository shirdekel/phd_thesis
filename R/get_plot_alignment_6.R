##' @title Get plot for alignment 6
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_plot_alignment_6 <- function(data = alignment6::data) {

  allocation <-
      data %>%
      nest_by(id, allocation, npv_amount, variance, hint) %>%
      get_omnibus_alignment_6("allocation") %>%
      afex_plot_alignment_6(dv_label = "Allocation (%)")

  ranking <-
      data %>%
      nest_by(id, ranking, npv_amount, variance, hint) %>%
      get_omnibus_alignment_6("ranking") %>%
      afex_plot_alignment_6(dv_label = "Ranking")

    lst(
      allocation,
      ranking
    )
}
