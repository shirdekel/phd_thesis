##' @title Remove columns that have within-subjects variables
##'
##' Group by ID, then check if each group for each column is a duplicate or not.
##' If yes, then keep the values. If not, output NA and remove.
##' @param data
##' @param diffused_iv
##' @return
##' @author Shir Dekel
##' @export
select_between_subjects_iv <- function(data, diffused_iv) {
  data %>%
    nest_by(id, !!!diffused_iv) %>%
    ungroup() %>%
    group_by(id) %>%
    mutate(
      across(
        everything(),
        keep_duplicates
      )
    ) %>%
    remove_empty("cols") %>%
    ungroup()
}
