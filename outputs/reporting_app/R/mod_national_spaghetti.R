# --- National spaghetti plot -------------------------------------------------

# ui -----
mod_national_spaghetti_ui <- function(id) {
  # set up namespacing
  ns <- shiny::NS(id)

  # define the UI
  bslib::nav_panel(
    value = "national_spaghetti",
    title = shiny::span(
      bsicons::bs_icon("activity"),
      "Spaghetti plot"
    ) |>
      bslib::tooltip(
        "View Place-level trends over time and compare them with national summary measures.",
        options = list(trigger = "hover")
      ),
    bslib::layout_sidebar(
      fillable = TRUE,
      sidebar = bslib::sidebar(
        open = TRUE,
        width = "400px",
        shiny::includeMarkdown("descriptions/national_spaghetti.md")
      ),
      bslib::card_body(
        plotly::plotlyOutput(
          ns("national_spaghetti"),
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
mod_national_spaghetti_server <- function(
  id,
  df,
  flag_hq,
  metric,
  show_mean,
  show_median,
  df_version
) {
  shiny::moduleServer(id, function(input, output, session) {
    # cache spaghetti data for metric for improved UX ----
    list_data <- shiny::reactive({
      req(df(), df_version(), metric())
      # NB, don't req flag_hq as this is a logical value

      get_data_for_national_spaghetti_plot(
        df = df(),
        metric_selected = metric()
      )
    }) |>
      shiny::bindCache(
        df_version(),
        flag_hq(),
        metric()
      )

    # render the spaghetti plot ----
    output$national_spaghetti <- plotly::renderPlotly({
      req(list_data())
      # NB, don't req show_mean or show_median as they're logical values

      display_national_spaghetti_plot(
        data_list = list_data(),
        show_mean = show_mean(),
        show_median = show_median()
      )
    })
  })
}
