#' @title Omnibus
##' @param dv
#' @return
#' @author Shir Dekel
#' @export
get_omnibus_alignment_6 <- function(data_clean_alignment_6, dv) {
  data_clean_alignment_6 %>%
    arrange(npv_amount) %>%
    aov_ez(
        id = "id",
        dv = dv,
        between = c("hint", "variance"),
        within = "npv_amount",
        data = .,
        type = 2
    )
}
