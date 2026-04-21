# --- Overview (Places) -------------------------------------------------------

# ui ----
mod_place_overview_ui <- function(id) {
  # set up namespacing
  ns <- shiny::NS(id)

  # define the UI
  bslib::nav_panel(
    title = "Overview",
    icon = bsicons::bs_icon("table"),
    bslib::layout_sidebar(
      fillable = TRUE,
      sidebar = bslib::sidebar(
        open = FALSE,
        shiny::includeMarkdown("descriptions/place_overview.md")
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("place_table")),
        fill = TRUE
      )
    )
  )
}

# server ----
mod_place_overview_server <- function(
  id,
  df,
  place,
  month_current,
  month_previous
) {
  shiny::moduleServer(id, function(input, output, session) {
    # update the table
    output$place_table <- reactable::renderReactable({
      req(
        df(),
        place(),
        month_current(),
        month_previous()
      )
      display_dashboard(
        df = df(),
        place_selected = place(),
        month_latest = month_current(),
        month_prev = month_previous()
      )
    })
  })
}
