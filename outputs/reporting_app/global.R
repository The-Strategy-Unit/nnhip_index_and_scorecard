# config ----------------------------------------------------------------------
shiny::enableBookmarking("server")

# data ------------------------------------------------------------------------
# read all data
df <- list.files(
  path = here::here(".secret", "data", "scorecard", "test"),
  pattern = ".Rds",
  full.names = TRUE
) |>
  purrr::map(.f = readRDS) |>
  dplyr::bind_rows() |>
  # prepare data
  dplyr::mutate(
    month_zoo = zoo::as.yearmon(month)
  )

# get a list of places
places <- df$place |> unique() |> sort()

# get a list of metrics
metrics <- df$metric_details |> unique()

# get a list of months
months <- df$month_zoo |> unique() |> sort(decreasing = TRUE)

# functions -------------------------------------------------------------------
# load in helper functions
source("R/dashboard_helpers.R")
