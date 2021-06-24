##' @title Split data into effects
##' @param data
##' @return
##' @author Shir Dekel
##' @export
split_data <- function(data) {

  data_distribution <-
    data %>%
    filter(presentation == "separate", awareness == "naive") %>%
    mutate(across(distribution, ~ .x %>%
                    fct_relevel("present")))

  data_presentation <-
    data %>%
    filter(distribution == "absent", awareness == "naive") %>%
    mutate(across(presentation, ~ .x %>%
                    fct_relevel("joint")))

  data_awareness <-
    data %>%
    filter(distribution == "absent", presentation == "separate") %>%
    mutate(across(awareness, ~ .x %>%
                    fct_relevel("aware")))

  data_effects <-
    list(data_presentation, data_awareness, data_distribution) %>%
    set_names("presentation", "awareness", "distribution")

  return(data_effects)

}
