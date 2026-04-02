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
  auth = "manual",
  server = server,
  key = api_key
)
pin_name <- glue::glue("{prefix}all")

# define a list of places where we expect to receive information
expected_places <- c(
  "Fenland and Peterborough within the North Cambridgeshire and Peterborough Care Partnership",
  "Ipswich and East Suffolk",
  "North East Essex",
  "South and West Hertfordshire (Dacorum and Hertsmere)",
  "West Essex",
  "West Suffolk",
  "Barking and Dagenham",
  "Croydon",
  "Hillingdon",
  "Kensington, Chelsea and Westminster (Bi-Borough)",
  "Lambeth and Southwark",
  "Coventry",
  "East Birmingham",
  "Herefordshire",
  "Leicestershire (West)",
  "Nottingham City",
  "Shropshire",
  "Solihull",
  "Walsall",
  "Bradford and Craven (Bradford South, Keighley and Airedale)",
  "Doncaster",
  "Leeds (Hatch, South, East)",
  "North East Lincolnshire",
  "Rotherham",
  "Stockton",
  "Sunderland",
  "Wakefield",
  "Blackburn and Darwen",
  "Morecambe Bay",
  "Rochdale",
  "Sefton",
  "St Helens",
  "Stockport",
  "Buckinghamshire (North, High Wycombe, Marlow Beaconsfield)",
  "East Berkshire and Slough",
  "East Kent",
  "East Surrey (Surrey Downs)",
  "East Sussex (Hastings and Rother)",
  "Portsmouth",
  "Bristol (South Bristol)",
  "Cornwall and The Isles Of Scilly",
  "Dorset Place ",
  "Woodspring"
) |>
  stringr::str_trim() |>
  sort()
