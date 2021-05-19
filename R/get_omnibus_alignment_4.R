#' @title Omnibus
##' @param dv
#' @return
#' @author Shir Dekel
#' @export
get_omnibus_alignment_4 <- function(data_clean_alignment_4, dv) {
  data_clean_alignment_4 %>%
    ## Makes sure that npv levels are in the correct order for the emmeans
    ## linear trend analysis
    arrange(npv_amount) %>%
    aov_ez(
      id = "id",
      dv = "forecast_mean",
      between = c("alignment", "reliability_amount"),
      within = "npv_amount",
      data = .,
      type = 2
    )
}
