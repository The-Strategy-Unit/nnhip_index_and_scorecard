# config ----------------------------------------------------------------------
shiny::enableBookmarking("server")

# functions -------------------------------------------------------------------
# load in helper functions
source("R/dashboard_helpers.R")


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

board <- pins::board_connect(server = server, account = account)
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

# df <<- NULL
# df_version <<- NULL

# read the pin containing the accumulated monthly records
# df <- pins::pin_read(board = board, name = glue::glue("{prefix}all"))

# ensure we have a 'metric' column that covers each of the three components of the metric
# df <- add_metric_column_to_df(df = df)

# extract require details

# # get a list of places
# places <- df$place |> unique() |> sort()

# # get a list of metrics
# metrics <- df$metric |> unique()

# # get a list of months
# months <- df$month_zoo |> unique() |> sort(decreasing = TRUE)
