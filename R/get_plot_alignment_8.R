##' @title Get plot for alignment 8
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_plot_alignment_8 <- function(data = alignment8::data) {
  allocation <-
    data %>%
    get_omnibus_alignment_8("allocation") %>%
    afex_plot_alignment_8(dv_label = "Mean allocation (%)")

  ranking <-
    data %>%
    get_omnibus_alignment_8("ranking") %>%
    afex_plot_alignment_8(dv_label = "Mean ranking")

  lst(
    allocation,
    ranking
  )
}
