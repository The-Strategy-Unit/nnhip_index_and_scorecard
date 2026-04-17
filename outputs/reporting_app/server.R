# server ----------------------------------------------------------------------
server <- function(input, output, session) {
  # read pin reactively every hour --------------------------------------------
  df_raw <- pins::pin_reactive_read(
    board = board,
    name = pin_name,
    interval = 60 * 60 * 1000 # check hourly
  )

  # read the pin version for use as a cache key
  df_version <- shiny::reactive(
    pins::pin_meta(board = board, name = pin_name)$version
  )

  # read the pin for issues / changelog
  df_issues <- pins::pin_reactive_read(
    board = board,
    name = pin_name_issues,
    interval = 60 * 60 * 1000 # check hourly
  )

  # pre-process the data ------------------------------------------------------
  df <- shiny::reactive({
    req(df_raw())

    # process the data to make it ready for use in the app
    df_raw() |>
      # ensure there is a consistent 'metric' for each block of values
      add_metric_column_to_df() |>
      # ensure counts below threshold are suppressed
      suppress_counts() |>
      # prepare columns for use in select inputs
      factorise_columns() |>
      # indicate whether place-month has record of engagement with target cohort
      add_active_engagement_columns()
  })

  # derived lists for UI inputs -----------------------------------------------
  df_places <- shiny::reactive({
    df()$place |> unique() |> sort() |> factor()
  })

  metrics <- shiny::reactive({
    req(df())

    # df()$metric |> unique() |> factor()
    df() |>
      dplyr::filter_out(metric_block == 15) |>
      dplyr::distinct(metric) |>
      dplyr::pull(metric)
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
      dplyr::filter_out(metric_block == 15) |>
      dplyr::distinct(metric) |>
      dplyr::pull(metric)
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
      # choices = filtered_metrics()
      choices = metrics()
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

  ## national data coverage ---------------------------------------------------
  output$national_data_coverage_table <- reactable::renderReactable({
    req(df())

    display_national_data_coverage_table(df = df())
  })

  ## national issues log ------------------------------------------------------
  output$changelog_table <- reactable::renderReactable({
    req(df_issues())

    display_issueslog(df_issues = df_issues())
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
    shiny::bindCache(df_version(), input$selected_month, input$selected_metric)

  # cache limits data for month:metric for improved UX ---
  df_limits <- shiny::reactive({
    req(df_funnel())

    compute_funnel_limits(df_funnel = df_funnel())
  }) |>
    shiny::bindCache(df_version(), input$selected_month, input$selected_metric)

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
}
