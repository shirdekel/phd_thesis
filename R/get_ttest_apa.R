##' @title T-test

##' @param data
##'
##' @param iv
##' @param dv
##'
##' @return
##' @author Shir Dekel
##' @export
get_ttest_apa <- function(data, iv, dv) {

  t <-
    reformulate(termlabels = iv, response = dv) %>%
    t.test(
      data = data,
      var.equal = TRUE
    ) %>%
    .[["statistic"]]

  iv_group <- sym(iv)

  n <-
    data %>%
    count({{iv_group}}) %>%
    pull(n)

  ttest_apa <-
    d.ind.t.t(
      t = t,
      n1 = n[1],
      n2 = n[2]
    ) %>%
    .[c("estimate", "statistic")] %>%
    str_c(collapse = ", ")

  return(ttest_apa)

}
