# source the functions to generate data
source(here::here("R", "generate_scorecard_data.R"))

#' Generate and save synthetic scorecard data for a given month
#'
#' @description
#' This function generates synthetic NNHIP scorecard data for a specified month and saves it as an `.Rds` file inside the interal project directory structure. If the required folder heirarchy does not already exisit, it is created automatically.
#'
#' @details
#' The function performs three main tasks:
#' \enumerate{
#'   \item Ensures that the directory `".secret/data/scorecard/test"` exists, creating it recursively if necessary.
#'   \item Calls `generate_month()` to construct the synthetic dataset for the specified month.
#'   \item Saves the resulting tibble as an `.Rds` file named `"<month>.Rds"` within the target directory.
#' }
#'
#' The directory paths are constructed using \pkg{here}, ensuring that file locations remain stable regardless of the user's working directory.
#'
#' @param month A character string in `"YYYY-MM"` format indicating the month of data to generate. Defaults to `"2026-01"`
#'
#' @returns Invisibly returns the path to the saved `.Rds` file
#'
#' @examples
#' \dontrun{
#' generate_and_save_data("2026-01")
#' generate_and_save_data("2027-05")
#' }
#'
#' @keywords internal
#' @noRd
generate_and_save_data <- function(month = "2026-01") {
  # ensure the folders are set up
  path <- here::here(".secret", "data", "scorecard", "test")
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  # generate the data
  df <- generate_month(month = month)

  # save the data
  saveRDS(
    object = df,
    file = here::here(
      ".secret",
      "data",
      "scorecard",
      "test",
      glue::glue("{month}.Rds")
    )
  )
}

# Save each month separately
generate_and_save_data(month = "2026-01")
generate_and_save_data(month = "2026-02")
generate_and_save_data(month = "2026-03")
generate_and_save_data(month = "2026-04")
generate_and_save_data(month = "2026-05")
generate_and_save_data(month = "2026-06")
generate_and_save_data(month = "2026-07")
generate_and_save_data(month = "2026-08")
generate_and_save_data(month = "2026-09")
generate_and_save_data(month = "2026-10")
generate_and_save_data(month = "2026-11")
generate_and_save_data(month = "2026-12")
generate_and_save_data(month = "2027-01")
generate_and_save_data(month = "2027-02")
generate_and_save_data(month = "2027-03")
