# --- Place spaghetti plot -------------------------------------------------

# ui -----
mod_place_spaghetti_ui <- function(id) {
  # set up namespacing
  ns <- shiny::NS(id)

  # define the UI
  bslib::nav_panel(
    value = "spaghetti_plot",
    title = shiny::span(
      bsicons::bs_icon("activity"),
      "Spaghetti plot"
    ) |>
      bslib::tooltip(
        "Explore trends for a selected Place over time, with optional national benchmarks, similar trajectories and cohort engagement markers.",
        options = list(trigger = "hover")
      ),
    bslib::layout_sidebar(
      fillable = TRUE,
      sidebar = bslib::sidebar(
        open = TRUE,
        width = "400px",
        shiny::includeMarkdown("descriptions/place_spaghetti.md")
      ),
      bslib::card_body(
        plotly::plotlyOutput(
          ns("place_spaghetti"),
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
mod_place_spaghetti_server <- function(
  id,
  df,
  df_comparators,
  metric,
  place,
  show_comparator,
  show_places,
  show_neighbours,
  show_engagement_month,
  show_mean,
  show_median,
  df_version
) {
  shiny::moduleServer(id, function(input, output, session) {
    # cache spaghetti data for metric for improved UX ----
    list_data <- shiny::reactive({
      req(df(), df_version(), metric(), place())

      get_data_for_place_spaghetti_plot(
        df = df(),
        df_comparators = df_comparators(),
        metric_selected = metric(),
        place_selected = place()
      )
    }) |>
      shiny::bindCache(
        df_version(),
        metric(),
        place()
      )

    # render the spaghetti plot ----
    output$place_spaghetti <- plotly::renderPlotly({
      req(list_data())
      # NB, don't req show_neighbours, show_engagement_month, show_mean or show_median as they're logical values

      display_place_spaghetti_plot(
        data_list = list_data(),
        show_comparator = show_comparator(),
        show_places = show_places(),
        show_neighbours_distance = show_neighbours(),
        show_engagement_month = show_engagement_month(),
        show_mean = show_mean(),
        show_median_iqr = show_median()
      )
    })
  })
}
