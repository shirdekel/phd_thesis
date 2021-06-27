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
          snakecase::to_sentence_case() %>%
          str_replace_all(
            c(
              "^Na$" = "NA",
              "npv" = "NPV",
              "^Anecdote$" = "Anecdote only",
              "Combined" = "Anecdote & statistics",
              "Enhanced" = "Anecdote & enhanced statistics",
              "Statistics" = "Statistics only"
            )
          ) %>%
          as.factor()
      )
    )
}
