# server ----------------------------------------------------------------------
server <- function(input, output, session) {
  # read pin reactively every hour --------------------------------------------
  df_raw <- pins::pin_reactive_read(
    board = board,
    name = pin_name,
    interval = 60 * 60 * 1000 # check hourly
  )

  # pre-process the data ------------------------------------------------------
  df <- shiny::reactive({
    req(df_raw())

    df_raw() |>
      add_metric_column_to_df() |>
      dplyr::mutate(
        place = place |> factor(levels = sort(unique(place))),
        month_zoo = month_zoo |> zoo::as.yearmon()
      )
  })

  # derived lists for UI inputs -----------------------------------------------
  df_places <- shiny::reactive({
    df()$place |> unique() |> sort() |> factor()
  })

  df_metrics <- shiny::reactive({
    df()$metric |> unique() |> factor()
  })

  df_months <- shiny::reactive({
    df()$month_zoo |> unique() |> sort(decreasing = TRUE) |> zoo::yearmon()
  })

  # filtering reactives -------------------------------------------------------
  # update the df for the selected place
  df_selected_place <- shiny::reactive({
    req(df(), input$selected_place)

    out <- df() |> dplyr::filter(place == input$selected_place)
    req(nrow(out) > 0)

    return(out)
  })

  # update the df for the selected month (useful for funnel plots)
  df_selected_month <- shiny::reactive({
    req(df(), input$selected_month)

    sel_month <- input$selected_month |> zoo::as.yearmon()

    out <- df() |> dplyr::filter(month_zoo == sel_month)
    req(nrow(out) > 0)

    return(out)
  })

  # list of months for the selected place
  filtered_months <- shiny::reactive({
    req(df_selected_place())

    df_selected_place() |>
      dplyr::pull(month_zoo) |>
      unique() |>
      sort() |>
      zoo::yearmon()
  })

  # get the latest / current month
  filtered_month_current <- shiny::reactive({
    req(filtered_months())

    filtered_months() |> tail(n = 1L)
  })

  # get the month before the current one
  filtered_month_previous <- shiny::reactive({
    req(filtered_months())

    filtered_months() |> tail(n = 2L) |> min()
  })

  # get a list of metrics
  filtered_metrics <- shiny::reactive({
    req(df_selected_place())

    df_selected_place() |>
      dplyr::pull(metric) |>
      unique() |>
      factor()
  })

  # update ui inputs ----------------------------------------------------------
  # update the available places
  shiny::observe({
    shiny::updateSelectizeInput(
      session = session,
      inputId = "selected_place",
      choices = df_places(),
      server = TRUE
    )
  })

  # update the available metrics
  shiny::observe({
    shiny::updateSelectizeInput(
      session = session,
      inputId = "selected_metric",
      choices = filtered_metrics()
    )
  })

  # update the available months
  shiny::observe({
    shiny::updateSelectizeInput(
      session = session,
      inputId = "selected_month",
      choices = df_months() |> as.character(), # need to send the labels
      server = FALSE # important!
    )
  })

  # outputs -------------------------------------------------------------------
  ## national dashboard -------------------------------------------------------
  output$national_table <- reactable::renderReactable({
    req(df(), filtered_month_current(), filtered_month_previous())
    display_dashboard_national(
      df = df(),
      month_latest = filtered_month_current(),
      month_prev = filtered_month_previous()
    )
  })

  ## place dashboard ----------------------------------------------------------
  output$place_header <- shiny::renderText({
    req(input$selected_place)
    input$selected_place
  })

  output$place_table <- reactable::renderReactable({
    req(
      df(),
      input$selected_place,
      filtered_month_current(),
      filtered_month_previous()
    )
    display_dashboard(
      df = df(),
      place_selected = input$selected_place,
      month_latest = filtered_month_current(),
      month_prev = filtered_month_previous()
    )
  })

  ## place funnel -------------------------------------------------------------
  # cache funnel data for month:metric for improved UX ---
  df_funnel <- shiny::reactive({
    req(df_selected_month(), input$selected_month, input$selected_metric)

    get_data_for_funnel_plot(
      df = df_selected_month(),
      month_selected = input$selected_month |> zoo::as.yearmon(),
      metric_selected = input$selected_metric
    )
  }) |>
    shiny::bindCache(input$selected_month, input$selected_metric)

  # cache limits data for month:metric for improved UX ---
  df_limits <- shiny::reactive({
    req(df_funnel())

    compute_funnel_limits(df_funnel = df_funnel())
  }) |>
    shiny::bindCache(input$selected_month, input$selected_metric)

  # render the funnel ---
  output$place_funnel <- plotly::renderPlotly({
    req(
      df_selected_month(),
      df_funnel(),
      input$selected_place,
      input$selected_metric
    )

    get_funnel_plot(
      df_funnel = df_funnel(),
      df_limits = df_limits(),
      place_selected = input$selected_place,
      metric_selected = input$selected_metric,
      month_selected = input$selected_month |> zoo::as.yearmon()
    )
  })

  # # prepare funnel plot data
  # # funnel_data <- shiny::reactiveVal(NULL)
  # # shiny::observeEvent(
  # #   list(filtered_df(), input$selected_month, input$selected_metric),
  # #   {
  # #     req(filtered_df(), input$selected_month, input$selected_metric)
  # #     df_processed <- get_data_for_funnel_plot(
  # #       df = filtered_df(),
  # #       month_selected = input$selected_month,
  # #       metric_selected = input$selected_metric
  # #     )
  # #     funnel_data(df_processed)
  # #   },
  # #   ignoreNULL = TRUE
  # # )
}
