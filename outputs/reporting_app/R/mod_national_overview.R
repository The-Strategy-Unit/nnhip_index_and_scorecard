# --- Overview (national) -----------------------------------------------------

# ui ----
mod_national_overview_ui <- function(id) {
  # set up namespacing
  ns <- shiny::NS(id)

  # define the ui
  bslib::nav_panel(
    title = "Overview",
    icon = bsicons::bs_icon("table"),
    bslib::layout_sidebar(
      fillable = TRUE,
      sidebar = bslib::sidebar(
        open = FALSE,
        shiny::includeMarkdown("descriptions/national_overview.md")
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("national_table")),
        fill = TRUE
      )
    )
  )
}

# server ----
mod_national_overview_server <- function(
  id,
  df,
  month_current,
  month_previous
) {
  shiny::moduleServer(id, function(input, output, session) {
    # code goes here
    output$national_table <- reactable::renderReactable({
      req(df(), month_current(), month_previous())

      display_dashboard_national(
        df = df(),
        month_latest = month_current(),
        month_prev = month_previous()
      )
    })
  })
}
