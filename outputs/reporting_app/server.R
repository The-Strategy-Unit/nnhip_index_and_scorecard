server <- function(input, output, session) {
  # manage data connection ----------------------------------------------------
  # set up reactive values for the main data input
  # df <- shiny::reactiveVal()
  # pin_version <- shiny::reactiveVal()
  df_places <- shiny::reactiveVal()
  df_metrics <- shiny::reactiveVal()
  df_months <- shiny::reactiveVal()

  # monitor for changes to the pin version
  shiny::observe({
    shiny::invalidateLater(60 * 60 * 1000) # check every hour
    meta <- pins::pin_meta(board = board, name = pin_name) # board and pin loaded in global.R

    # If first load OR version changed, then refresh df
    if (is.null(df_version) || meta$version != df_version) {
      message("Refreshing NNHIP aggregate data ...")

      # update the main data source for this dashboard
      df_temp <- pins::pin_read(board = board, name = pin_name)

      # add the metric column to df (to aid metric selection)
      df_temp <- df_temp |>
        add_metric_column_to_df() |>
        # speed up place filters
        dplyr::mutate(
          place = place |> factor(levels = df_temp$place |> unique() |> sort()),
          month_zoo = zoo::as.yearmon(month_zoo)
        )

      # troubleshooting
      # dplyr::glimpse(df_temp)

      # assign this globally (so all users can access up-dated information)
      df <<- df_temp
      df_version <<- meta$version

      # update the lists of places, metrics and months
      df_places(df$place |> unique() |> sort() |> factor())
      df_metrics(df$metric |> unique() |> factor())
      df_months(
        df$month_zoo |> unique() |> sort(decreasing = TRUE) |> zoo::yearmon()
      )
    }
  })

  # reactives -----------------------------------------------------------------

  # update the df for the selected place
  filtered_df <- shiny::reactive({
    # ensure a place is selected
    req(df, input$selected_place)

    # filter df for records
    out <- df |> dplyr::filter(place == input$selected_place)

    # guard against empty results
    req(nrow(out) > 0)

    return(out)
  })

  # update the df for the selected month (useful for funnel plots)
  df_selected_month <- shiny::reactive({
    # ensure a month is selected
    req(df, input$selected_month)

    # filter df for the selected month
    sel_month <- input$selected_month |> zoo::as.yearmon()
    out <- df |>
      dplyr::filter(month_zoo == sel_month)

    # guard against empty results
    req(nrow(out) > 0)

    return(out)
  })

  # get a list of months
  filtered_months <- shiny::reactive({
    filtered_df() |>
      dplyr::pull(month_zoo) |>
      unique() |>
      sort() |>
      zoo::yearmon()
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
      # dplyr::filter(value_type == "rate_per_1000", !is.na(value)) |>
      dplyr::pull(metric) |>
      unique() |>
      factor()
  })

  # update the df for the latest two months
  # latest_df <- shiny::reactive({
  #   latest_months <- filtered_months() |> sort() |> tail(n = 2L)
  #   df() |> dplyr::filter(month_zoo %in% latest_months)
  # })

  # prepare funnel plot data
  # funnel_data <- shiny::reactiveVal(NULL)
  # shiny::observeEvent(
  #   list(filtered_df(), input$selected_month, input$selected_metric),
  #   {
  #     req(filtered_df(), input$selected_month, input$selected_metric)
  #     df_processed <- get_data_for_funnel_plot(
  #       df = filtered_df(),
  #       month_selected = input$selected_month,
  #       metric_selected = input$selected_metric
  #     )
  #     funnel_data(df_processed)
  #   },
  #   ignoreNULL = TRUE
  # )

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

  # update the available places based on df()
  shiny::observe({
    shiny::updateSelectizeInput(
      session = session,
      inputId = "selected_place",
      choices = df_places(),
      server = TRUE
    )
  })

  # update the available months based on df()
  shiny::observe({
    shiny::updateSelectizeInput(
      session = session,
      inputId = "selected_month",
      choices = df_months() |> as.character(), # need to send the labels
      server = FALSE # important!
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

  # ## place outputs ---
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
    req(
      df_selected_month(),
      input$selected_place,
      input$selected_metric,
      input$selected_month
    )

    month <- input$selected_month |> zoo::as.yearmon()

    # get the funnel data
    df_funnel <- get_data_for_funnel_plot(
      df = df_selected_month(),
      month_selected = month,
      metric_selected = input$selected_metric
    )

    get_funnel_plot(
      # df_funnel = funnel_data(),
      df_funnel = df_funnel,
      place_selected = input$selected_place,
      metric_selected = input$selected_metric,
      month_selected = month
    )
  })
}
