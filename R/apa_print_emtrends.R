##' @title APA format emtrends output

##' @return
##' @author Shir Dekel
##' @export
apa_print_emtrends <- function(emtrends) {
  emtrends %>%
    tidy(conf.int = TRUE) %>%
    rowwise() %>%
    rename_with(
      ~ .x %>%
        str_match(".*(trend)") %>%
        .[, 2],
      contains("trend")
    ) %>%
    mutate(
      across(
        c(trend, z.ratio, asymp.LCL, asymp.UCL),
        printnum
      ),
      across(
        p.value, ~ .x %>%
          printp(add_equals = TRUE)
      ),
      apa = str_c(
        "$b = ",
        trend,
        "$, 95\\% CI $[",
        asymp.LCL,
        ", ",
        asymp.UCL,
        "]$, $z = ",
        z.ratio,
        "$, $p ",
        p.value,
        "$"
      )
    )
}
