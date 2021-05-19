##' @title Get plot for alignment 1
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_plot_alignment_1 <- function(data_clean_alignment_1) {
  allocation <-
    data_clean_alignment_1 %>%
    get_omnibus_alignment_1("allocation") %>%
    afex_plot_alignment_1(dv_label = "Allocation (%)")

  confidence <-
    data_clean_alignment_1 %>%
    get_omnibus_alignment_1("confidence") %>%
    afex_plot_alignment_1(dv_label = "Confidence")

  lst(
    allocation,
    confidence
  )
}
