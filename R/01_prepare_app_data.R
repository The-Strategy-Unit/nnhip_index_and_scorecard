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
