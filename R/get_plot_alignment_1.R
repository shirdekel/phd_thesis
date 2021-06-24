##' @title Get plot for alignment 1
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_plot_alignment_1 <- function(data = alignment1::data) {
  allocation <-
    data %>%
    get_omnibus_alignment_1("allocation") %>%
    afex_plot_alignment_1(dv_label = "Allocation (%)")

  confidence <-
    data %>%
    get_omnibus_alignment_1("confidence") %>%
    afex_plot_alignment_1(dv_label = "Confidence")

  lst(
    allocation,
    confidence
  )
}
