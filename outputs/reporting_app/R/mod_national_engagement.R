# --- Engagement (National) ---------------------------------------------------

# ui ----
mod_national_engagement_ui <- function(id) {
  # set up namespacing
  ns <- shiny::NS(id)

  # define the ui
  bslib::nav_panel(
    value = "engagement_national",
    title = shiny::span(
      bsicons::bs_icon("graph-up"),
      "Engagement"
    ) |>
      bslib::tooltip(
        "Month-by-month view of engagement in the national target cohort, highlighting progress, momentum and key milestones.",
        options = list(trigger = "hover")
      ),
    bslib::layout_sidebar(
      fillable = TRUE,
      open = FALSE,
      sidebar = bslib::sidebar(
        open = FALSE,
        shiny::includeMarkdown("descriptions/national_engagement.md")
      ),
      bslib::card_body(
        plotly::plotlyOutput(ns("engagement_plot"))
      )
    )
  )
}

# server ----
mod_national_engagement_server <- function(id, df) {
  shiny::moduleServer(id, function(input, output, session) {
    # update the plot
    output$engagement_plot <- plotly::renderPlotly({
      req(df())
      plot_data <- get_data_for_national_engagement(df = df())
      display_national_engagement_plot(df = plot_data)
    })
  })
}
