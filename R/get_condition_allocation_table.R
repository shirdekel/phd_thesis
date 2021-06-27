##' @title Get condition allocation table
##' Use IV character vector, but only count between-subjects IVs
##' Return just a tibble with n if no between-subjects IVs
##' @param data
##' @param iv

##' @return
##' @author Shir Dekel
##' @export
get_condition_allocation_table <- function(data, iv) {
  diffused_iv <-
    diffuse_non_na(iv)

  allocation_raw <-
    data %>%
    nest_by(id, !!!diffused_iv) %>%
    ungroup() %>%
    select_between_subjects_iv(diffused_iv) %>%
    clean_condition_names()

  allocation_columns <-
    iv %>%
    extract_from(names(allocation_raw)) %>%
    diffuse_non_na()

  condition_allocation_table <-
    allocation_raw %>%
    nest_by(id, !!!allocation_columns) %>%
    ungroup() %>%
    count(!!!allocation_columns)

  if (!is_empty(allocation_columns)) {
    condition_allocation_table <-
      condition_allocation_table %>%
      adorn_totals("row", fill = " ")
  }
  return(condition_allocation_table)
}
