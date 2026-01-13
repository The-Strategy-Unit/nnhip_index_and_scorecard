# Helper functions ------------------------------------------------------------
# These are short utility functions to help facilitate tasks.

#' Render the readme file
#'
#' @description
#' Helper script to render the README.md from the README.Rmd file.
#'
#' @returns NULL
#'
#' @noRd
render_readme <- function() {
  rmarkdown::render("README.Rmd", output_format = "md_document")
}
