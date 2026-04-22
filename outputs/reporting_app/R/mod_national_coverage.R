# --- Data coverage -----------------------------------------------------------

# ui ----
mod_national_coverage_ui <- function(id) {
  # set up namespacing
  ns <- shiny::NS(id)

  # define the UI
  bslib::nav_panel(
    title = "Data coverage",
    icon = bsicons::bs_icon("ui-checks-grid"),
    bslib::layout_sidebar(
      fillable = TRUE,
      sidebar = bslib::sidebar(
        open = FALSE,
        shiny::includeMarkdown("descriptions/national_coverage.md")
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("national_data_coverage_table")),
        fill = TRUE
      )
    )
  )
}

# server ----
mod_national_coverage_server <- function(id, df) {
  shiny::moduleServer(id, function(input, output, session) {
    # national data coverage table
    output$national_data_coverage_table <- reactable::renderReactable({
      shiny::req(df())
      display_national_data_coverage_table(df = df())
    })
  })
}
