##' @title Get alignment results
##'
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_results_alignment_8 <- function(data = alignment8::data) {
  allocation_omnibus <-
    data %>%
    get_omnibus_alignment_8("allocation")

  four_way <-
    allocation_omnibus %>%
    apa_print(es = "pes", mse = FALSE) %>%
    pluck(
      "full_result",
      "alignment_reliability_type_npv_amount_reliability_amount"
    )

  simple_effects <-
    allocation_omnibus %>%
    get_simple_effects_alignment_8()

  allocation <-
    lst(four_way) %>%
    append(simple_effects)

  lst(
    allocation
  )
}
