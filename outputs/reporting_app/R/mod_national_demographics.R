# --- Demographics (National) -------------------------------------------------

# ui ----
mod_national_demographics_ui <- function(id) {
  # set up namespacing
  ns <- shiny::NS(id)

  # define the ui
  bslib::nav_panel(
    value = "demographics",
    title = shiny::span(
      bsicons::bs_icon("bar-chart"),
      "Demographics"
    ) |>
      bslib::tooltip(
        "Month-by-month view of how the national cohort is distributed across demographic groups and how those proportions shift over time.",
        options = list(trigger = "hover")
      ),
    bslib::layout_sidebar(
      id = "national_overview_description",
      fillable = TRUE,
      open = FALSE,
      sidebar = bslib::sidebar(
        open = TRUE,
        width = "400px",
        shiny::includeMarkdown("descriptions/national_demographics.md")
      ),
      bslib::card_body(
        id = "national_overview",
        plotly::plotlyOutput(ns("demographics_plot")),
        fill = TRUE
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
