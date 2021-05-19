#' @title Clean condition names

#' @return
#' @author Shir Dekel
#' @export
clean_condition_names <- function(data) {
  data %>%
    mutate(
      across(
        -id,
        ~ .x %>%
          as.character() %>%
          to_sentence_case() %>%
          str_replace("^Na$", "NA") %>%
          as.factor()
      )
    )
}
