server <- function(input, output, session) {
  # reactives -----------------------------------------------------------------

  # update the df for the selected place
  filtered_df <- shiny::reactive({
    # ensure a place is selected
    req(input$selected_place)

    # filter df for records
    df |> dplyr::filter(place == input$selected_place)
  })

  # get a list of months
  filtered_months <- shiny::reactive({
    filtered_df() |>
      dplyr::pull(month_zoo) |>
      unique() |>
      sort()
  })

  # get the latest / current month
  filtered_month_current <- shiny::reactive({
    filtered_months() |>
      tail(n = 1L)
  })

  # get the month before the current one
  filtered_month_previous <- shiny::reactive({
    filtered_months() |>
      tail(n = 2L) |>
      min()
  })

  # get a list of metrics
  filtered_metrics <- shiny::reactive({
    filtered_df() |>
      # keep only rows where we have a rate reported
      dplyr::filter(value_type == "rate_per_1000", !is.na(value)) |>
      dplyr::pull(metric) |>
      unique() |>
      sort()
  })

  # update the df for the latest two months
  latest_df <- shiny::reactive({
    latest_months <- filtered_months() |> sort() |> tail(n = 2L)
    df |>
      dplyr::filter(month_zoo %in% latest_months)
  })

  # prepare funnel plot data
  funnel_data <- shiny::reactiveVal(NULL)
  shiny::observeEvent(
    list(input$selected_month, input$selected_metric),
    {
      req(input$selected_month, input$selected_metric)
      df_processed <- get_data_for_funnel_plot(
        df = df,
        month_selected = input$selected_month,
        metric_selected = input$selected_metric
      )
      funnel_data(df_processed)
    },
    ignoreNULL = TRUE
  )

  # observers -----------------------------------------------------------------

  # update the available metrics based on the filtered data
  shiny::observe({
    shiny::updateSelectizeInput(
      session = session,
      inputId = "selected_metric",
      choices = filtered_metrics(),
      server = TRUE
    )
  })

  # outputs -------------------------------------------------------------------
  ## national outputs ----
  output$national_table <- reactable::renderReactable({
    display_dashboard_national(
      df = df,
      month_latest = filtered_month_current(),
      month_prev = filtered_month_previous()
    )
  })

  ## place outputs ---
  output$place_header <- shiny::renderText({
    req(input$selected_place)
    input$selected_place
  })

  output$place_table <- reactable::renderReactable({
    # update the dashboard
    display_dashboard(
      df = df,
      place_selected = input$selected_place,
      month_latest = filtered_month_current(),
      month_prev = filtered_month_previous()
    )
  })

  output$place_funnel <- plotly::renderPlotly({
    df <- funnel_data()
    req(df)

    get_funnel_plot(
      df_funnel = funnel_data(),
      place_selected = input$selected_place,
      metric_selected = input$selected_metric,
      month_selected = input$selected_month
    )
  })
}
