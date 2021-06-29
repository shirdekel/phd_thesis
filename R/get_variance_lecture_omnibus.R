#' @title Get omnibus for variance lecture analysis
##' @param data
#' @return
#' @author Shir Dekel
#' @export
get_variance_lecture_omnibus <- function(data) {
  data %>%
    mutate(
      alignment_between = case_when(
        eval_align == 1 & alignment == "low" ~ "low",
        eval_align == 2 & alignment == "high" ~ "high"
      )
    ) %>%
    filter(!is.na(alignment_between)) %>%
    afex::aov_ez(
      dv = "allocation",
      id = "id",
      within = c("npv_amount", "phase", "reliability_amount"),
      between = "alignment_between",
      type = 2
    )
}
