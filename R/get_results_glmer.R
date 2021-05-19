##' @title Analyse using glmer

##' @param data_effects
##'
##' @param dv
##'
##' @return
##' @author Shir Dekel
##' @export
get_results_glmer <- function(data_effects, dv) {

  data_effects_names <-
    names(data_effects)

  if(dv == "choice") {
    data_nested <- data_effects
  } else {
    data_nested <- data_effects_names %>%
      map2(data_effects, ~ {
        condition <- sym(.x)
        dv <- sym(dv)
        .y %>%
          nest_by(id, {{condition}}, {{dv}})
      })
  }

  results_glmer <-
    data_effects_names %>%
    map2(data_nested, ~
           reformulate(termlabels = c(.x, "(1 | id)"), response = dv) %>%
           fit_glmer(family = binomial, data = .y) %>%
           apa_print()
    ) %>%
    set_names(data_effects_names)

  return(results_glmer)

}
