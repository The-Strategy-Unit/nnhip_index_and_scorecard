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
  # main readme file for the project
  rmarkdown::render("README.Rmd", output_format = "md_document")

  # test files readme
  rmarkdown::render(
    input = here::here("tests", "input", "README.Rmd"),
    output_format = "md_document"
  )
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

#' Get a Strategy Unit LakeMart Parquet File
#'
#' @description
#' Retrieves and consolidates all indvidual `part-*` files from a Parquet
#' dataset stored in the Strategy Unit LakeMart area of UDAL, returning the
#' combined data as a single tibble.
#'
#' @details
#' This function relies on several environment variables to construct the
#' connection path and authenticate against Azure Data Lake Storage (ADLS).
#' These must be available in the R session before calling the function.
#'
#' @section Required environment variables:
#' The following environment variables must be set (e.g. via `.Renviron`,
#' `Sys.setenv()`, or your project settings):
#'
#' - **`container`** - the ADLS container name
#' - **`lake`** - the ADLS account name including domain name
#' - **`folder`** - the folder path within the container where the Parquet
#' files reside.
#' - **`azure_key_vault`** - The name of the Azure Key Vault containing SAS
#' tokens.
#'
#' The function expects a read-access SAS token stored in Key Vault under the
#' name: `"alp-{container}-StrategyUnit-r"`.
#'
#' @section How file matching works:
#' The `str_file_pattern` argument is used to filter file names returned from
#' ADLS. It should match part of the Parquet dataset path, for example:
#' ```
#' "matching_referrals.parquet/part"
#' ```
#' All matching Parquet files are downloaded and read using
#' `arrow::read_parquet()`, then combined row-wise into a single tibble.
#'
#' @section Error handling:
#' The function performs several validation checks and will abort with a clear
#' diagnostic message if:
#' - required environment variables are missing or empty,
#' - the Azure Key Vault cannot be accessed,
#' - the expected SAS token is not found in Key Vault,
#' - no files match the supplied `str_file_pattern`
#'
#' These checks are intended to make configuration issues easier to diagnose.
#'
#' @param str_file_pattern String. A substring used to identify the Parquet dataset within the Strategy Unit LakeMart folder structure.
#'
#' @returns A tibble containing the combined data from all matching Parquet part files.
#'
#' @examples
#' \dontrun{
#' df <- get_su_lakemart_parquet_file("matching_referrals.parquet/part")
#' }
get_su_lakemart_parquet_file <- function(str_file_pattern = "") {
  # ensure a file pattern was supplied
  if (nchar(str_file_pattern) == 0) {
    cli::cli_abort(
      "{.var str_file_pattern} must be a string matching part of a parquet file, e.g. {.val matching_referrals.parquet/part}"
    )
  }

  # validate required environment variables
  required_env <- c("container", "lake", "folder", "azure_key_vault")
  missing_env <- required_env[nchar(Sys.getenv(required_env)) == 0]

  if (length(missing_env) > 0) {
    cli::cli_abort(
      "Missing environment variables: {.var missing_env}.
      Please ensure these are set before calling this function."
    )
  }

  # set path to lake mart
  container <- Sys.getenv("container")
  lake <- Sys.getenv("lake")
  folder <- Sys.getenv("folder")
  eval_path <- glue::glue("abfss://{container}@{lake}{folder}")

  # set up access settings -------------------------------------------------------
  # get shared access signature auth for data
  # vault <- AzureKeyVault::key_vault(Sys.getenv("azure_key_vault"))
  # nb, follow any authentication directions in the console.
  vault_nm <- Sys.getenv("azure_key_vault")
  vault <- tryCatch(
    AzureKeyVault::key_vault(vault_nm),
    error = function(e) {
      cli::cli_abort(
        "Failed to access Azure Key Vault {.val {vault_nm}}.
        Check your authentication and network connection. \n{e$message}"
      )
    }
  )

  # read from lake ---------------------------------------------------------------
  sas_nm <- glue::glue("alp-{container}-StrategyUnit-r")
  # get the SAS for the relevant data, r = read access, w = write access
  # az_sas <- vault$secrets$get(glue::glue("alp-{container}-StrategyUnit-r"))
  az_sas <- tryCatch(
    vault$secrets$get(sas_nm),
    error = function(e) {
      cli::cli_abort(
        "Could not retrieve SAS token {.val {sas_nm}} from Key Vault.
        Please ensure the secret exists and you have access. \n{e$message}"
      )
    }
  )

  if (is.null(az_sas$value) || nchar(az_sas$value) == 0) {
    cli::cli_abort(
      "SAS token {.val {sas_name}} is empty.
      Please check the Key Vault secret configuration."
    )
  }

  # set connection string
  az_endpoint <- AzureStor::storage_endpoint(
    glue::glue("https://{lake}"),
    sas = az_sas$value
  )

  # set the storage container
  az_container <- AzureStor::storage_container(az_endpoint, container)

  # list files in the project folder
  az_files <- AzureStor::list_adls_files(az_container, folder, recursive = TRUE)

  # identify parquet files in the matching_variables folder
  requested_file <-
    az_files |>
    tibble::as_tibble() |>
    dplyr::filter(stringr::str_detect(
      string = name,
      pattern = str_file_pattern
    )) |>
    dplyr::pull(name)

  if (length(requested_file) == 0) {
    cli::cli_abort(
      "No files found matching pattern {.val {str_file_pattern}} in folder {.val {folder}}."
    )
  }

  # read all parquet files to a single df
  df_return <-
    purrr::map_dfr(
      .x = requested_file,
      .f = \(.x) {
        AzureStor::storage_download(
          container = az_container,
          src = .x,
          dest = NULL
        ) |>
          arrow::read_parquet()
      }
    )

  # return the df
  return(df_return)
}

#' List all packages referenced via `pkg::function` syntax
#'
#' @description
#' This function scans R code and extracts the names of all packages reference using the `pkg::function` syntax. It is useful for identifying dependencies in scripts, functions or entire directories of R files.
#'
#' The input can be:
#' - a character vector of code lines
#' - a file path to an `.R` script
#' - or a function object (which will be deparsed)
#'
#' The function returns a unique, alphabetically sorted character vector of package names.
#'
#' @param x A character vector of code, a file path or a function object
#'
#' @returns A character vector of unique package names referenced via `pkg::fun`
#'
#' @examples
#' # from a character vector
#' list_used_packages("dplyr::mutate(stringr::str_detect(x, 'a'))")
#'
#' # from a file
#' \dontrun{
#' list_used_packages("R/process_submission.R")
#' }
#'
#' # from a function
#' my_fun <- function() dplyr::mutate(mtcars, cyl2 = cyl * 2)
#' list_used_packages(my_fun)
#'
list_used_packages <- function(x) {
  # x can be: a character vector of code, a file path, or a function

  # If x is a file path, read it
  if (length(x) == 1 && file.exists(x)) {
    x <- readLines(x, warn = FALSE)
  }

  # If x is a function, deparse it
  if (is.function(x)) {
    x <- deparse(x)
  }

  # Extract all occurrences of pkg::fun
  matches <- stringr::str_extract_all(x, "\\b[[:alnum:].]+(?=::)")

  # Flatten, drop NULLs, unique, sorted
  pkgs <- matches |>
    unlist() |>
    unique() |>
    sort()

  pkgs
}
