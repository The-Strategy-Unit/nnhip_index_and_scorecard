# config ----------------------------------------------------------------------
shiny::enableBookmarking("server")

# functions -------------------------------------------------------------------
# load in helper functions
source("R/dashboard_helpers.R")

# packages --------------------------------------------------------------------
# add packages which are required for the app to work when deployed
# but aren't called directly
library(brand.yml)
library(dataui)

# data ------------------------------------------------------------------------
# test data ----
# read all data
# df_old <- list.files(
#   path = here::here(".secret", "data", "scorecard", "test"),
#   pattern = ".Rds",
#   full.names = TRUE
# ) |>
#   purrr::map(.f = readRDS) |>
#   dplyr::bind_rows() |>
#   # prepare data
#   dplyr::mutate(
#     month_zoo = zoo::as.yearmon(month)
#   )

# real data ----
# connect to the Posit Connect board
server <- Sys.getenv("posit_server")
account <- Sys.getenv("posit_account")
prefix <- Sys.getenv("pin_prefix")
api_key <- Sys.getenv("posit_api_key")

board <- pins::board_connect(
  # auth = "rsconnect",
  auth = "manual",
  server = server,
  # account = account,
  key = api_key
)
pin_name <- glue::glue("{prefix}all")

# global cache
meta <- pins::pin_meta(board = board, name = pin_name)
df <- pins::pin_read(board = board, name = pin_name)

df <- df |>
  add_metric_column_to_df() |>
  dplyr::mutate(
    place = place |> factor(levels = sort(unique(place))),
    month_zoo = zoo::as.yearmon(month_zoo)
  )
df_version <- meta$version
