##' @title Get descriptives
##' @param data
##' @param iv
##' @return
##' @author Shir Dekel
##' @export
get_descriptives <- function(data, iv) {
  condition_allocation_table <-
    data %>%
    get_condition_allocation_table(iv) %>%
    janitor::clean_names(case = "sentence") %>%
    rename_with(
      recode,
      "Alignment" = "Project alignment",
      "Reliability amount" = "Reliability of net present value (NPV)",
      "Anecdote between" = "Evidence type",
      "Anecdote" = "Evidence type"
    )

  total <-
    condition_allocation_table %>%
    pull(N) %>%
    last()

  total_apa <-
    total %>%
    papaja::printnum(numerals = FALSE, capitalize = TRUE)

  ## Alignment 3 should be the only experiment in which specific sex data was
  ## not collected
  if (any(names(data) %in% "sex")) {
    sex <-
      data %>%
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

  unit_raw <-
    c(
      "years" %>%
        rep(3),
      "min"
    )

  numerical_names <-
    data %>%
    names() %>%
    extract_from(numerical_names_raw)

  unit <-
    numerical_names_raw %in%
    numerical_names %>%
    unit_raw[.]

  numerical <-
    numerical_names %>%
    map(
      ~ data %>%
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
    map2(
      unit,
      ~ str_c(
        .x$mean,
        " ",
        .y,
        " (*SD* = ",
        .x$sd,
        ", *min.* = ",
        .x$min,
        ", *max.* = ",
        .x$max,
        ")"
      )
    )

  sample <-
    data %>%
    pull(sample) %>%
    unique()

  if (sample == "prolific") {
    sample_description <-
      "the online recruitment platform Prolific. Participants were compensated at a rate of £5 an hour (Prolific is based in the UK)."
  } else if (sample == "sona") {
    sample_description <-
      "a cohort of psychology undergraduates at The University of Sydney. Participants were compensated with course credit."
  } else if (sample == "masters") {
    sample_description <-
      "a Master of Management degree at an Australian university."
  } else if (sample == "reddit") {
    sample_description <-
      "Reddit. Participants were compensated with a virtual Gold Award, which gives the recipient a week of a premium version of Reddit and 100 virtual coins."
  } else if (sample == "prolific_sona") {
    sample_description <-
      "both the online recruitment platform Prolific and a cohort of psychology undergraduates at The University of Sydney. Participants from Prolific were compensated at a rate of £5 an hour (Prolific is based in the UK), and participants from the undergraduate sample were compensated with course credit."
  }

  apa <-
    str_c(
      total_apa,
      str_c(
        "participants (",
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
        "working in a business setting, and an average of",
        numerical$business_edu,
        "of business education. The mean completion time of the task was",
        str_c(
          numerical$total_time,
          "."
        ),
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
