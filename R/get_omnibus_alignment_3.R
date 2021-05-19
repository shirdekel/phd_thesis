#' @title Omnibus results for alignment 3
##' @param data_clean_alignment_3
#' @return
#' @author Shir Dekel
#' @export
get_omnibus_alignment_3 <- function(data_clean_alignment_3, dv) {
  data_clean_alignment_3 %>%
    ## Makes sure that npv levels are in the correct order for the emmeans
    ## linear trend analysis
    arrange(npv_amount) %>%
    aov_ez(
      id = "id",
      dv = dv,
      within = c("alignment", "reliability_amount", "npv_amount"),
      data = .,
      type = 2
    )
}
