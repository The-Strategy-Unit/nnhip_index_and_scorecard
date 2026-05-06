# --- Demographics (National) -------------------------------------------------

# ui ----
mod_national_demographics_ui <- function(id) {
  # set up namespacing
  ns <- shiny::NS(id)

  # define the ui
  bslib::nav_panel(
    title = "Demographics",
    icon = bsicons::bs_icon("bar-chart"),
    bslib::layout_sidebar(
      fillable = TRUE,
      open = FALSE,
      sidebar = bslib::sidebar(
        open = FALSE,
        shiny::includeMarkdown("descriptions/national_demographics.md")
      ),
      bslib::card_body(
        plotly::plotlyOutput(ns("demographics_plot"))
      )
    )
  )
}

# server ----
mod_national_demographics_server <- function(
  id,
  df,
  selected_demographic,
  df_version
) {
  shiny::moduleServer(id, function(input, output, session) {
    # shiny::req(df(), selected_demographic(), df_version())

    # get the data, cached for df_version and demographic
    plot_data <- shiny::reactive({
      shiny::req(df(), df_version())
      get_data_for_demographic_split(df = df())
    }) |>
      shiny::bindCache(df_version())

    # update the plot
    output$demographics_plot <- plotly::renderPlotly({
      shiny::req(df(), selected_demographic())

      # display the plot
      display_demographic_split_chart(
        df = plot_data(),
        selected_demographic = selected_demographic()
      )
    })
  })
}
