##' @title Plot choice data
##'
##' @param data_effects
##' @param dv_label
##' @param dv
##'
##' @return
##' @author Shir Dekel
##' @export
plot_choice <- function(data_effects, dv, dv_label) {

  if(deparse(substitute(dv)) == "choice") {
    data_presentation <- data_effects$presentation
    data_awareness <- data_effects$awareness
    data_distribution <- data_effects$distribution
  } else {
    data_presentation <- data_effects$presentation %>%
      nest_by(subject, presentation, {{dv}}) %>%
      ungroup()
    data_awareness <- data_effects$awareness %>%
      nest_by(subject, awareness, {{dv}}) %>%
      ungroup()
    data_distribution <- data_effects$distribution %>%
      nest_by(subject, distribution, {{dv}}) %>%
      ungroup()
  }

  plot_presentation <-
    data_presentation %>%
    mutate(across(presentation, ~ .x %>%
                    fct_relevel("separate"))) %>%
    apa_plot(iv1 = "presentation",
             iv1.lab = "Presentation condition",
             dv = quo({{dv}}),
             dv.lab = dv_label)

  plot_awareness <-
    data_awareness %>%
    mutate(across(awareness, ~ .x %>%
                    fct_relevel("naive"))) %>%
    apa_plot(iv1 = "awareness",
             iv1.lab = "Awareness condition",
             dv = quo({{dv}}),
             dv.lab = dv_label) +
    theme(axis.title.y = element_blank())

  plot_distribution <-
    data_distribution %>%
    mutate(across(distribution, ~ .x %>%
                    fct_relevel("absent"))) %>%
    apa_plot(iv1 = "distribution",
             iv1.lab = "Distribution condition",
             dv = quo({{dv}}),
             dv.lab = dv_label) +
    theme(axis.title.y = element_blank())

  choice_plot <-
    plot_presentation + plot_awareness + plot_distribution

  return(choice_plot)

}
