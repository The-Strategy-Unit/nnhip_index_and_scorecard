#' ----------------------------------------------------------------------------
#' PREPARE DATA FOR USE IN THE SCORECARD SHINY APP
#'
#' This code is designed to be run each month to:
#' 1. identify and load monthly Excel submissions from
#'    Neighbhourhood Places
#' 2. process this data to a tidy long format
#' 3. update the data on the SU's Posit Connect server so it is available for
#'    the app to use
#' ----------------------------------------------------------------------------

source(here::here("R", "data_ingest.R"))
update_pinned_data_for_month()
