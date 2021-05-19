##' @title Anecdotes simulation plot
##' @param data_clean_anecdotes_2

##' @return
##' @author Shir Dekel
##' @export
get_plot_simulation_anecdotes_2 <- function(data_clean_anecdotes_2) {
  c("negative", "positive") %>%
    map(
      ~ data_clean_anecdotes_2 %>%
        plot_allocation_anecdotes_2(.x)
    ) %>%
    set_names("negative", "positive")
}
