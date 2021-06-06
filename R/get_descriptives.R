##' @title Get descriptives
##' @param data_clean
##' @param iv
##' @return
##' @author Shir Dekel
##' @export
get_descriptives <- function(data_clean, iv) {
  condition_allocation_table <-
    data_clean %>%
    get_condition_allocation_table(iv) %>%
    janitor::clean_names(case = "sentence")

  total <-
    condition_allocation_table %>%
    pull(N) %>%
    last()

  total_apa <-
    total %>%
    papaja::printnum(numerals = FALSE, capitalize = TRUE)

  ## Alignment 3 should be the only experiment in which specific sex data was
  ## not collected
  if (any(names(data_clean) %in% "sex")) {
    sex <-
      data_clean %>%
      nest_by(id, sex) %>%
      ungroup() %>%
      count(sex)

    sex_female <-
      sex %>%
      mutate(
        across(sex, str_to_lower)
      ) %>%
      filter(sex == "female") %>%
      pull(n)

    sex_text <- "The average age was "
  } else {
    sex_female <- 28
    sex_text <- "Age information was not recorded"
  }

  numerical_names_raw <-
    c("age", "business_exp", "business_edu", "total_time")

  numerical_names <-
    data_clean %>%
    names() %>%
    extract_from(numerical_names_raw)

  numerical <-
    numerical_names %>%
    map(
      ~ data_clean %>%
        summarise(
          across(
            all_of(.x),
            lst(mean, sd, min, max),
            .names = "{fn}",
            na.rm = TRUE
          )
        ) %>%
        mutate(across(everything(), ~ .x %>%
          papaja::printnum(drop0trailing = TRUE)))
    ) %>%
    set_names(numerical_names) %>%
    map(
      ~ str_c(
        .x$mean,
        " (*SD* = ",
        .x$sd,
        ", *min* = ",
        .x$min,
        ", *max* = ",
        .x$max,
        ")"
      )
    )

  sample <-
    data_clean %>%
    pull(sample) %>%
    unique()

  if (sample == "prolific") {
    sample_description <-
      "the online recruitment platform Prolific. Participants were compensated at a rate of £5 an hour."
  } else if (sample == "sona") {
    sample_description <-
      "a Psychology undergraduate sample at The University of Sydney. Participants were compensated with course credit."
  } else if (sample == "masters") {
    sample_description <-
      "a Masters of Management course at an Australian university."
  } else if (sample == "reddit") {
    sample_description <-
      "Reddit. Participants were compensated with a virtual Gold Award, which gives the recipient a week of a premium version of Reddit and 100 virtual coins."
  } else if (sample == "prolific_sona") {
    sample_description <-
      "both the online recruitment platform Prolific and a Psychology undergraduate sample at The University of Sydney. Participants from Prolific were compensated at a rate of £5 an hour, and participants from the undergraduate sample were compensated with course credit."
  }

  apa <-
    str_c(
      total_apa,
      str_c(
        "people (",
        sex_female
      ),
      "female) were recruited from",
      sample_description,
      str_c(
        sex_text,
        numerical$age,
        "."
      ),
      sep = " "
    )

  if (all(numerical_names_raw %in% names(numerical))) {
    apa <-
      str_c(
        apa,
        "Participants reported an average of",
        numerical$business_exp,
        "years of work in a business setting, and an average of",
        numerical$business_edu,
        "years of business education. The mean completion time was",
        numerical$total_time,
        "minutes.",
        sep = " "
      )
  }

  descriptives <-
    lst(
      condition_allocation_table,
      total,
      total_apa,
      sex_female,
      numerical,
      apa
    )

  return(descriptives)
}
