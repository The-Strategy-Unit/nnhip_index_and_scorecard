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

# --- Validation checks -------------------------------------------------------
# run a validation check on submissions
validation <- validate_monthly_submissions()

# if issues detected then investigate:
validation$issues |> View()
validation$data |> View()

# --- Process data ------------------------------------------------------------

# when ready to update the data, run this:
update_pinned_data_for_month()
