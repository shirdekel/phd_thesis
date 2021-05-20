##' @title Render bookdown and force Rmd file dependencies
##' @param input
##' @param config_file
##' @param output_format
##' @return
##' @author Shir Dekel
##' @export
render_with_deps <- function(input,
                             config_file,
                             deps) {
  bookdown::render_book(
    input = input,
    config_file = config_file,
    output_format = "all"
  )

  file.remove(list.files(pattern = "*.(log|mtc|maf|aux|bbl|blg|xml)"))

  "compiled_thesis"
}
