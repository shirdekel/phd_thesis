##' @title Get plot for alignment 7
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_plot_alignment_7 <- function(data_clean_alignment_7) {
  allocation <-
    c("low", "high") %>%
    map(
    ~ data_clean_alignment_7 %>%
      filter(alignment == .x) %>%
    nest_by(
      id,
      reliability_amount,
      npv_cond,
      allocation,
      reliability_type
    )  %>%
      apa_plot(
        dv = "allocation",
        iv1 = "reliability_amount",
        iv2 = "npv_cond",
        iv3 = reliability_type
      )
    ) %>%
  set_names("alignment_low", "alignment_high")

  lst(
      allocation
  )

}
