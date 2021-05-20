#' Print R Markdown output to a t-test
#'
#' Includes effect size with CIs.
#'
#' @param x a vector for Group 1.
#' @param y a vector for Group 2.
#' @param var.equal whether to assume equal variance or not (i.e., to use
#'   Welch's test for the t-test or not)
#' @param paired a logical indicating whether to consider the values as paired
#'
#' @return
#' @export
#'
t_print <- function(x, y, var.equal = TRUE, paired = FALSE) {
    t <- t.test(x, y, var.equal = var.equal, paired = paired) %>%
        papaja:::apa_print.htest()
    es <- effsize::cohen.d(x, y, paired = paired)
    if (paired) {
        d <- "d_z"
    } else {
        d <- "d_s"
    }
    paste0(
        t$statistic,
        ", $",
        d,
        " = ",
        papaja::printnum(es$estimate),
        "$, 95\\% CI $[",
        paste0(papaja::printnum(es$conf.int), collapse = ", "),
        "]$"
    )
}
