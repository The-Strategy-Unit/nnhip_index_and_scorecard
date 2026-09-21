#' ----------------------------------------------------------------------------
#' PREPARE DATA FOR USE IN THE SCORECARD SHINY APP
#'
#' This code is designed to be run each month to:
#' 1. identify and load monthly Excel submissions from
#'    Neighbourhood Places
#' 2. process this data to a tidy long format
#' 3. update the data on the SU's Posit Connect server so it is available for
#'    the app to use
#' ----------------------------------------------------------------------------
#'
#' NOTE:
#' issues when running in R 4.6.1 but fine when running R 4.5.3.
#' Currently unclear whether this is related to newer R version or one of the
#' dependencies
#'
#' Errors reported in R 4.6.1 include:
#'
#' (1) ------------------------------------------------------------------------
#' Failed: "NNHIPDataCollectionTemplatev1.2_Nott_June2026_Final.xlsx" - In argument: `value_numeric = readr::parse_double(x = value, na = suppression_marker)`.
#'
#' (2) ------------------------------------------------------------------------
#' Error in `dplyr::count()` at nnhip_index_and_scorecard/R/data_ingest.R:1071:3:
#' ! Must group by variables found in `.data`.
#' ✖ Column `place` is not found.

source(here::here("R", "data_ingest.R"))

# authenticate with the MS Teams folders
folder <- get_ms_teams_folder()

# --- Validation checks -------------------------------------------------------
# run a validation check on submissions
validation <- validate_monthly_submissions(ms_teams_folder = folder)

# if issues detected then investigate:
validation$issues |> View()
validation$data |> View()

# see how the validated data look in the app
launch_app_with_test_data()

# --- Process data ------------------------------------------------------------

# when ready to update the data, run this:
update_pinned_data_for_month(ms_teams_folder = folder)

# run a report showing updates
quarto::quarto_render(
  input = here::here("outputs", "monthly_updates", "report_v1.qmd")
)

# open the report
browseURL(here::here("outputs", "monthly_updates", "report_v1.html"))
