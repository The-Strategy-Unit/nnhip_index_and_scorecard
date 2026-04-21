# --- Engagement (Places) -----------------------------------------------------

# ui ----
mod_place_engagement_ui <- function(id) {
  # set up namespacing
  ns <- shiny::NS(id)

  # define the UI
  bslib::nav_panel(
    title = "Engagement",
    icon = bsicons::bs_icon("table"),
    bslib::layout_sidebar(
      fillable = TRUE,
      open = FALSE,
      sidebar = bslib::sidebar(
        open = FALSE,
        shiny::includeMarkdown("descriptions/place_engagement.md")
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("engagement_table"))
      )
    )
  )
}

# server ----
mod_place_engagement_server <- function(id, df) {
  shiny::moduleServer(id, function(input, output, session) {
    # update the table
    output$engagement_table <- reactable::renderReactable({
      req(df())
      display_engagement_table(df = df())
    })
  })
}
