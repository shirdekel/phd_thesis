##' @title Get thesis dependencies
##'
##' For use in `render_with_deps()`

##' @return
##' @author Shir Dekel
##' @export
get_thesis_deps <- function() {
  front_matter <-
    file.path("rmd", "front_matter") %>%
    list.files(full.names = TRUE)

  rmd <-
    file.path("_bookdown.yml") %>%
    yaml::read_yaml() %>%
    pluck("rmd_files") %>%
    c(front_matter)

  templates <-
    file.path("templates") %>%
    list.files(full.names = TRUE)

  lst(rmd, templates)
}
