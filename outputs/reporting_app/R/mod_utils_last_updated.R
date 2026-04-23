# --- Data last updated (utils) -----------------------------------------------

# ui ----
mod_utils_last_updated_ui <- function(id) {
  # set up namespacing
  ns <- shiny::NS(id)

  # define the UI
  bslib::card(
    class = "p-2 text-center",
    uiOutput(ns("pin_last_updated"))
  )
}

# server ----
mod_utils_last_updated_server <- function(id, time) {
  shiny::moduleServer(id, function(input, output, session) {
    output$pin_last_updated <- shiny::renderUI({
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
