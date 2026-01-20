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

#' Generates a hex sticker
#'
#' @description
#' Generates a hex sticker for this
#'
#' @returns
#'
#' @export
#' @examples
render_hex_sticker <- function() {
  # somewhere to save it
  temp_path <- "inst/figures/hex.png"

  # bring own image
  image_path <- here::here("inst", "figures", "nnhip_logo_losenge.png")
  image_png <- png::readPNG(image_path)

  # build and write the hex
  gex::open_device(
    file_path = temp_path,
    resolution = 300
  )
  gex::add_hex(col = "#f5f6fa")
  gex::add_image(
    img = image_png,
    y = 0.38,
    width = 0.5
  )
  gex::add_text(
    string = "NNHIP",
    x = 0.5,
    y = 0.7,
    col = "#2c2825",
    family = "Sans",
    size = 20
  )
  gex::add_border(col = "#f9bf07", width = 0.09)
  gex::close_device()
}
render_hex_sticker()
