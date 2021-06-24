#' @title Omnibus results for alignment 1
##' @param data
#' @return
#' @author Shir Dekel
#' @export
get_omnibus_alignment_1 <- function(data, dv) {
  data %>%
    ## Makes sure that npv levels are in the correct order for the emmeans
    ## linear trend analysis
    arrange(npv_amount) %>%
    aov_ez(
      id = "id",
      dv = dv,
      between = "reliability_amount",
      within = c("npv_amount", "alignment"),
      data = .,
      type = 2
    )
}
