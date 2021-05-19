##' @title Analyse t-test
##' @param data_effects
##'
##' @param dv
##'
##' @return
##' @author Shir Dekel
##' @export
get_results_ttest <- function(data_effects, dv) {

  data_effects_names <- names(data_effects)

  t <-
    data_effects_names %>%
    map2(data_effects, ~ {
      condition <- sym(.x)
      dv <- sym(dv)
      data_nested <- .y %>%
        nest_by(subject, {{condition}}, {{dv}})
      reformulate(termlabels = .x, response = dv) %>%
        t.test(data_nested, var.equal = TRUE) %>%
        .[["statistic"]]
    }) %>%
    set_names(data_effects_names)

  n <-
    data_effects_names %>%
    map2(data_effects, ~ {
      condition <- sym(.x)
      .y %>%
            nest_by(subject, {{condition}}) %>%
            ungroup() %>%
            count({{condition}}) %>%
            pull(n)})

  results_ttest <- t %>%
    map2(n, ~ d.ind.t.t(.x, .y[1], .y[2]) %>%
           .[c("estimate", "statistic")] %>%
           str_c(collapse = ", "))

  return(results_ttest)

}
