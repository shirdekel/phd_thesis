##' @title Get thesis dependencies
##'
##' For use in `render_with_deps()`

##' @return
##' @author Shir Dekel
##' @export
get_thesis_deps <- function() {
  rmd <-
    file.path("_bookdown.yml") %>%
    yaml::read_yaml() %>%
    pluck("rmd_files")

  templates <-
    file.path("templates") %>%
    list.files(full.names = TRUE)

  lst(rmd, templates)
}
