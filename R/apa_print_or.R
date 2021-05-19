##' @title Format glm(er) model as OR in APA
##' @param model
##'
##' Need to remove ran_pars row for glmer objects because for some reason printp
##' doesn't play nice with the NA in p.value. And for some reason this only
##' happens when running through drake.

##' @return
##' @author Shir Dekel
##' @export
apa_print_or <- function(model) {
  tidy <-
    model %>%
    broom.mixed::tidy(exponentiate = T, conf.int = TRUE)
  if (any(class(model) %in% "glmerMod")) {
    tidy <- tidy %>%
      filter(!effect == "ran_pars")
  }

  tidy %>%
    rowwise() %>%
    mutate(
      across(
        c(estimate, statistic, conf.low, conf.high),
        printnum
      ),
      across(
        p.value, ~ .x %>%
          papaja::printp(add_equals = TRUE)
      ),
      apa = str_c(
        "$OR = ",
        estimate,
        "$, 95\\% CI $[",
        conf.low,
        ", ",
        conf.high,
        "]$, $z = ",
        statistic,
        "$, $p ",
        p.value,
        "$"
      )
    )
}
