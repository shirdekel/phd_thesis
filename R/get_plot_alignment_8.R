##' @title Get plot for alignment 8
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_plot_alignment_8 <- function(data_clean_alignment_8) {
  data_clean_filtered <-
    data_clean_alignment_8 %>%
    nest_by(
      id,
      reliability_amount,
      npv_amount,
      allocation,
      ranking,
      alignment,
      reliability_type
    )

  allocation <-
    data_clean_filtered %>%
    get_omnibus_alignment_8("allocation") %>%
    afex_plot_alignment_8(dv_label = "Allocation (%)")


  ranking <-
    data_clean_filtered %>%
    get_omnibus_alignment_8("ranking") %>%
    afex_plot_alignment_8(dv_label = "Ranking")

  lst(
    allocation,
    ranking
  )
}
