# --- Data last updated (utils) -----------------------------------------------

# ui ----
mod_utils_last_updated_ui <- function(id) {
  # set up namespacing
  ns <- shiny::NS(id)

  # define the UI
  bslib::card(
    class = "p-2 text-center",
    uiOutput(ns("pin_last_updated"))
  ) |>
    bslib::tooltip(
      "Time since the app's underlying data was last refreshed, helping you see how current the figures are.",
      options = list(trigger = "hover")
    )
}

# server ----
mod_utils_last_updated_server <- function(id, time) {
  shiny::moduleServer(id, function(input, output, session) {
    # reactive that determines how often to update the UI
    update_interval <- shiny::reactive({
      shiny::req(time())
      t <- time()
      if (inherits(t, "Date")) {
        t <- as.POSIXct(t)
      }

      # how old is the time data?
      age <- difftime(time1 = Sys.time(), time2 = t, units = "secs")

      # decide how often to update the UI
      if (age < 3600) {
        1e3 * 10 # update every 10 seconds (< 1 hour old)
      } else if (age < 3600 * 6) {
        1e3 * 60 # update every minute (< 6 hours old)
      } else if (age < 3600 * 24) {
        1e3 * 60 * 5 # update every 5 minutes (< 24 hours old)
      } else {
        1e3 * 60 * 60 # update every hour (>= 1 day old)
      }
    })

    # update the age of the data
    output$pin_last_updated <- shiny::renderUI({
      # force re-execution based on adaptive interval
      shiny::invalidateLater(millis = update_interval(), session = session)

      req(time())
      t <- time()
      if (inherits(t, "Date")) {
        t <- as.POSIXct(t)
      }
      str_ago <- t |> prettyunits::time_ago()
      str_rtn <- glue::glue("Last refreshed {str_ago}")
      span(
        class = "text-muted small",
        icon("clock"),
        str_rtn
      )
    })
  })
}
