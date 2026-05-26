# --- Engagement (Places) -----------------------------------------------------

# ui ----
mod_place_engagement_ui <- function(id) {
  # set up namespacing
  ns <- shiny::NS(id)

  # define the UI
  bslib::nav_panel(
    value = "engagement_place",
    title = shiny::span(
      bsicons::bs_icon("table"),
      "Engagement"
    ) |>
      bslib::tooltip(
        "Summary of engagement activity for each Place, showing how participation changes over time relative to cohort size.",
        options = list(trigger = "hover")
      ),
    bslib::layout_sidebar(
      fillable = TRUE,
      open = FALSE,
      sidebar = bslib::sidebar(
        open = TRUE,
        width = "400px",
        shiny::includeMarkdown("descriptions/place_engagement.md")
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("engagement_table"))
      )
    )
  )
}

# server ----
mod_place_engagement_server <- function(id, df, place) {
  shiny::moduleServer(id, function(input, output, session) {
    # update the table
    output$engagement_table <- reactable::renderReactable({
      req(df(), place())
      display_engagement_table(df = df(), place_highlight = place())
    })
  })
}
