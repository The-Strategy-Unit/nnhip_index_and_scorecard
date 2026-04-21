# --- Funnel plot for Places --------------------------------------------------

# ui -----
mod_place_funnel_ui <- function(id) {
  # set up namespacing
  ns <- shiny::NS(id)

  # define the UI
  bslib::nav_panel(
    title = "Funnel plot",
    icon = bsicons::bs_icon("funnel"),
    bslib::layout_sidebar(
      fillable = TRUE,
      sidebar = bslib::sidebar(
        open = FALSE,
        shiny::includeMarkdown("descriptions/place_funnel.md")
      ),
      bslib::card_body(
        plotly::plotlyOutput(
          ns("place_funnel_plot"),
          height = "100%",
          fill = TRUE
        ),
        fill = TRUE,
        height = "100%"
      )
    )
  )
}

# server ----
mod_place_funnel_server <- function(id, df, place, metric, month, df_version) {
  shiny::moduleServer(id, function(input, output, session) {
    # cache funnel data for month:metric for improved UX ----
    df_funnel <- shiny::reactive({
      req(df(), df_version(), month(), metric(), place())

      get_data_for_funnel_plot(
        df = df(),
        month_selected = month(),
        metric_selected = metric()
      )
    }) |>
      shiny::bindCache(
        df_version(),
        month(),
        metric()
      )

    # cache limits data for month:metric for improved UX ----
    df_limits <- shiny::reactive({
      req(df_funnel())

      compute_funnel_limits(df_funnel = df_funnel())
    }) |>
      shiny::bindCache(
        df_version(),
        month(),
        metric()
      )

    # render the funnel ----
    output$place_funnel_plot <- plotly::renderPlotly({
      req(
        df(),
        df_funnel(),
        place(),
        metric()
      )

      get_funnel_plot(
        df_funnel = df_funnel(),
        df_limits = df_limits(),
        place_selected = place(),
        metric_selected = metric(),
        month_selected = month()
      )
    })
  })
}
