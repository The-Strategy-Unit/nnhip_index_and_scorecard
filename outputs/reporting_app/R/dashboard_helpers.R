# -----------------------------------------------------------------------------
# DASHBOARD HELPER FUNCTIONS
# -----------------------------------------------------------------------------

#' Extract dashboard-ready metric summary for a given place and month
#'
#' @description
#' This function filters a large metric dataset to return the subset of rows
#' required for a dashboard view. It selects the data for a specified month
#' and place, restricts results to the "Total" breakdown and applies
#' metric-specific rules to retain only the appropriate measure (e.g., patients
#' for P1, rate_per_1000 for all other metrics).
#'
#' @param df A tibble or data frame containing metric data.
#' @param selected_month A zoo::yearmon identifying the month to filter data for
#' @param selected_place A character string specifying the place to filter data for
#'
#' @returns A tibble containing the filtered subset of rows suitable for use in dashboard summaries
#'
#' @examples
#' \dontrun{
#' get_place_month_summary(
#'   df = df,
#'   selected_month = zoo::as.yearmon("2026-01"),
#'   selected_place = "West Essex"
#' )
#' }
#' @noRd
get_place_month_summary <- function(df, selected_month, selected_place) {
  df_return <-
    df |>
    dplyr::filter(
      month_zoo == selected_month,
      place == selected_place,
      breakdown_dimension == "Total",
      dplyr::when_any(
        (metric_id == "P1" & measure == "patients"),
        (metric_id != "P1" & measure == "rate_per_1000")
      )
    )

  # return the result
  return(df_return)
}

#' Summarise average metric values for a given place
#'
#' @description
#' This function computes dashboard-ready average values for a specified place.
#' For outcome metrics, the function calculates an average rate per 1000 based
#' on the summed numerator and denominator. For the process metric P1, it
#' returns the mean number of patients.
#'
#' @param df A tibble of metric data.
#' @param selected_place A character string specifying the place.
#'
#' @returns A tibble containing average (year-to-date) values for outcome metrics.
#'
#' @examples
#' \dontrun{
#' summarise_place_average(
#'   df = df,
#'   selected_place = "West Essex"
#' )
#' }
summarise_place_averages <- function(df, selected_place) {
  # get a common df with most filters in place
  df_temp <-
    df |>
    dplyr::filter(
      place == selected_place,
      breakdown_dimension == "Total",
      measure != "rate_per_1000" # remove pre-calculated rates
    )

  # outcome metrics: compute rate_per_1000 from summed numerator + denominator
  df_outcomes <-
    df_temp |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .by = c(metric_id, metric_details, measure)
    ) |>
    # work out the rate
    tidyr::pivot_wider(
      names_from = measure,
      values_from = value
    ) |>
    dplyr::mutate(rate_per_1000 = (count / patients) * 1000) |>
    # collapse back to measure | value columns
    tidyr::pivot_longer(
      cols = c(patients, count, rate_per_1000),
      names_to = "measure",
      values_to = "value"
    ) |>
    # remove numerator and denominator from all outcome measures
    dplyr::filter(
      metric_id != "P1",
      measure == "rate_per_1000"
    )

  # process metric P1: average number of patients
  df_process <-
    df_temp |>
    dplyr::filter(
      metric_id == "P1",
      measure == "patients"
    ) |>
    # summarise for the average
    dplyr::summarise(
      value = mean(value, na.rm = TRUE),
      .by = c(metric_id, metric_details, measure)
    )

  # combine rows together
  df_return <- dplyr::bind_rows(df_outcomes, df_process)

  return(df_return)
}


#' Extract sparkline-ready trend data for a selected place
#'
#' @description
#' This function filters a metric dataset for a given place and returns sparkline-ready trend data. It keeps onlyl rows representing the *Total* breakdown and applies metric-specific measure rules:
#' - for metric `"P1"`, only rows where `measure == "patients"` are included
#' - for all other metrics, only rows where `measure == "rate_per_1000"` are included
#'
#' The filtered data is ordered by `month_zoo` and summarised into a list-column containing the time-ordered values for each metric.
#'
#' @param df A tibble of metric data.
#' @param selected_place A character string specifying the place.
#'
#' @returns A tibble for each metric containing metric details and the trendline list-column.
#'
#' @examples
#' \dontrun{
#' get_sparkline_data(
#'   df = df,
#'   selected_place = "West Essex"
#' )
#' }
get_sparkline_data <- function(df, selected_place) {
  df |>
    dplyr::filter(
      place == selected_place,
      breakdown_dimension == "Total",
      dplyr::when_any(
        (metric_id == "P1" & measure == "patients"),
        (metric_id != "P1" & measure == "rate_per_1000")
      )
    ) |>
    dplyr::select(metric_id, metric_details, measure, month_zoo, value) |>
    dplyr::summarise(
      trendline = list(value[order(month_zoo)]),
      .by = c(metric_id, metric_details, measure)
    )
}

#' Prepare combined dashboard data for display
#'
#' @description
#' This function assembles all data required for the dashboard view by
#' retrieving and combining:
#' - the selected place's values for the latest month
#' - the selected place's values for the previous month
#' - average values for the selected place across all months
#' - sparkline trendline data for each metric
#'
#' It merges these components into a single data frame containing current
#' month values, previous month values, month-on-month differencees, averages,
#' differences from average and a list-column of trendline values. The
#' resulting data frame is designed for use in dashboard tables such as
#' those produced by {reactable}.
#'
#' @param df A tibble of metric data.
#' @param selected_place A character string specifying the place.
#' @param month_latest A zoo::yearmon identifying the 'current' month
#' @param month_prev A zoo::yearmon identifying the 'previous' month
#'
#' @returns A data frame containing one row per metric with the following
#' columns:
#' - `metric_details` - descriptive metric label
#' - `month_current` - value for the latest month
#' - `month_previous` - value for the previous month
#' - `month_diff` - difference between current and previous month
#' - `average` - long-term average value for the metric
#' - `average_diff` - difference between current value and average
#' - `trendline` - list-column of ordered values for sparkline charts
#'
#' @details
#' This function acts as a data orchestrator for the dashboard. It relies
#' on:
#' - `get_place_month_summary()` to extract month-specific values,
#' - `summarise_place_averages()` to compute long-term averages,
#' - `get_sparkline_data()` to generate sparkline-ready trend data
#'
#' Joins are performed on `metric_details`, and differences are computed
#' directly after each join step.
#'
#' @examples
get_dashboard_data <- function(df, month_latest, month_prev, place_selected) {
  # get dashboard data for current month
  df_dash_month_curr <- get_place_month_summary(
    df = df,
    selected_month = month_latest,
    selected_place = place_selected
  )

  # get dashboard data for the previous month
  df_dash_month_prev <- get_place_month_summary(
    df = df,
    selected_month = month_prev,
    selected_place = place_selected
  )

  # get average dashboard data
  df_dash_average <- summarise_place_averages(
    df = df,
    selected_place = place_selected
  )

  # get trendline data for the sparkline
  df_trendline <- get_sparkline_data(
    df = df,
    selected_place = place_selected
  )

  # combine the data
  df_dashboard <-
    df_dash_month_curr |>
    dplyr::select(metric_details, month_current = value) |>
    # add in previous month's values and work out the difference
    dplyr::left_join(
      y = df_dash_month_prev |>
        dplyr::select(metric_details, month_previous = value),
      by = dplyr::join_by(x$metric_details == y$metric_details)
    ) |>
    dplyr::mutate(
      month_diff = month_current - month_previous
    ) |>
    # add in average values
    dplyr::left_join(
      y = df_dash_average |>
        dplyr::select(metric_details, average = value),
      by = dplyr::join_by(x$metric_details == y$metric_details)
    ) |>
    dplyr::mutate(
      average_diff = month_current - average
    ) |>
    # add in trendline
    dplyr::left_join(
      y = df_trendline |>
        dplyr::select(metric_details, trendline),
      by = dplyr::join_by(x$metric_details == y$metric_details)
    )

  return(df_dashboard)
}

#' Render a value with an up, down or neutral arrow for reactable tables
#'
#' @description
#' This helper function formats numeric values for use in a {reactable} column
#' by prepending an arrow icon that indicates the direction of change. Positive
#' values display an upward arrow, negative values display a downward arrow,
#' and zero values display a neutral dash. All icons are styled using the
#' colour code {#686f73}.
#'
#' The function returns HTML suitable for use inside a `colDef()` with
#' `html = TRUE`.
#'
#' @param value A numeric value to be formatted. May be positive, negative or `NA`. `NA` values return an empty string.
#'
#' @returns A HTML string containing an arrow icon and the formatted numeric value, intended for use in side a {reactable} cell.
#'
#' @examples
#' \dontrun{
#' reactable::reactable(
#'   data.frame(month_diff = c(-2.3, 0, 1.7)),
#'   columns = list(
#'     month_diff = reactable::colDef(
#'       html = TRUE,
#'       cell = arrow_cell
#'     )
#'   )
#' )
#' }
arrow_cell <- function(value) {
  if (is.na(value)) {
    return("")
  }

  arrow <- if (value > 0) {
    "<span style='color:#f9bf07'>&#9650;</span>" # up arrow
  } else if (value < 0) {
    "<span style='color:#5881c1'>&#9660;</span>" # down arrow
  } else {
    "<span style='color:#686f73'>&#8213;</span>" # neutral dash
  }

  htmltools::HTML(
    paste0(sprintf("%.1f", value), " ", arrow)
  )
}

#' Display a dashboard-style reactable view of metric data
#'
#' @description
#' This function generates an interactive dashboard table using {reactable}
#' and {reactablefmtr}. It retrieves pre-processed dashboard data via
#' `get_dashboard_data()` and renders a compact, styled table that includes
#' current and previous month values, differences with directional arrows,
#' averages and sparkline trendlines.
#'
#' The resulting table is intended for use in a reporting dashboard where users
#' can explore metric performance for a selected place and time period.
#'
#' @param df A tibble of metric data.
#' @param selected_place A character string specifying the place.
#' @param month_latest A zoo::yearmon identifying the 'current' month
#' @param month_prev A zoo::yearmon identifying the 'previous' month
#'
#' @returns A {reactable} table object displaying a dashboard-style view of metrics including:
#' \itemize{
#'   \item \strong{Metric name}
#'   \item \strong{Current and previous month values}
#'   \item \strong{Month-on-month difference} with directional arrows
#'   \item \strong{Average value} and difference from average
#'   \item \strong{Sparkline trendline} showing recent performance
#' }
#'
#' @details
#' The table includes several custom formatting features:
#' \itemize{
#'   \item Directional arrows for difference columns via `arrow_cell()`
#'   \item Semi-transparent right-hand border styling for visual grouping
#'   \item Sparkline trendlines generated with `reactablefmtr::react_sparkline()`
#'   \item A custom theme
#' }
#'
#' Pagination is disabled to present all metrics at once, and the table is searchable to support quick filtering.
#'
#' @examples
#' \dontrun{
#' display_dashboard(
#'   df = df,
#'   place_selected = "West Essex",
#'   month_latest = zoo::as.yearmon("2026-05")
#'   month_prev = zoo::as.yearmon("2026-04")
#' )
#' }
display_dashboard <- function(df, place_selected, month_latest, month_prev) {
  # get the data
  df_dashboard <- get_dashboard_data(
    df = df,
    month_latest = month_latest,
    month_prev = month_prev,
    place_selected = place_selected
  )

  # produce as a reactable table dashboard
  suppressWarnings(
    df_dashboard |>
      # dplyr::select(metric_details, value) |>
      reactable::reactable(
        pagination = FALSE,
        searchable = TRUE,
        defaultColDef = reactable::colDef(
          maxWidth = 100
        ),
        columns = list(
          metric_details = reactable::colDef(
            name = "Metric",
            minWidth = 250,
            maxWidth = 1000
          ),
          month_current = reactable::colDef(
            name = "This month",
            format = reactable::colFormat(digits = 1)
          ),
          month_previous = reactable::colDef(
            name = "Last month",
            format = reactable::colFormat(digits = 1)
          ),
          month_diff = reactable::colDef(
            name = "Difference",
            format = reactable::colFormat(digits = 1),
            cell = arrow_cell,
            html = TRUE,
            style = function(value) {
              list(
                borderRight = "0.5px solid #f5f6fa",
                paddingRight = "8px"
              )
            },
            headerStyle = list(
              borderRight = "0.5px solid #f5f6fa"
            )
          ),
          average = reactable::colDef(
            name = "Average",
            format = reactable::colFormat(digits = 1)
          ),
          average_diff = reactable::colDef(
            name = "Difference",
            format = reactable::colFormat(digits = 1),
            cell = arrow_cell,
            html = TRUE
          ),
          trendline = reactable::colDef(
            name = "Trend",
            minWidth = 150,
            maxWidth = 1000,
            cell = reactablefmtr::react_sparkline(
              data = df_dashboard,
              show_area = TRUE,
              line_color = "#5881c1",
              highlight_points = reactablefmtr::highlight_points(
                min = "#686f73",
                max = "#686f73"
              )
            )
          )
        ),
        theme = reactable::reactableTheme(
          style = list(
            # fontFamily = "Aptos Display, Segoe UI, Helvetica, Arial, sans-serif"
            fontFamily = "Roboto, Arial, sans-serif"
          )
        )
      )
  )
}

#' Prepare data for funnel plot visualisation
#'
#' @description
#' `get_data_for_funnel_plot()` filters, reshapes and enriches a dataset so it can be used to construct a funnel plot for a selected month and metric.
#'
#' The function:
#' - filters the input dataset to the chosen month and metric
#' - pivots numerator, denominator and rate onto a single row per place
#' - calculates Wilson confidence limits (95% and 99%)
#' - classifies each point according to its position relative to the limits
#' - generates formatted hover text for interactive visualisation (e.g. {plotly})
#'
#' @param df A tibble of metric data.
#' @param month_selected A zoo::yearmon identifying the 'current' month
#' @param metric_selected A character string specifying a metric
#'
#' @returns A tibble containing one row per place with the following additional fields:
#' - **overall_p**: overall proportion across all places
#' - **Wilson 95% and 99% limits**: (`lower_95`, `upper_95`, `lower_99`, `upper_99`)
#' - **marker_category**: factor indicating whether the point lies *within 95%*, *outside 95%* or *outside 99%* limits
#' - **hover_text**: HTML-formatted string suitable for {plotly} hover labels
#'
#' @details
#' The function uses the Wilson score interval to compute funnel limits. This method is preferred over the normal approximation because it:
#' - behaves well for small denominators
#' - avoids convidence limits outside the [0, 1] range
#' - produces smoother, more stable funnel boundaries
#'
#' Rates are expressed per 1,000 population (or per 1,000 denominator units) so confidence limits are multiplied by 1,000 for consistency with `rate_per_1000`
#'
#' @examples
#' \dontrun{
#' df_funnel <- get_data_for_funnel_plot(
#'   df = my_data,
#'   month_selected = zoo::as_yearmon("2026-01"),
#'   metric_selected = "Number of new consultant-led outpatient appointments for patients in the cohort"
#' )
#' }
get_data_for_funnel_plot <- function(df, month_selected, metric_selected) {
  df_return <-
    df |>
    # filter the data for the specified month and metric
    dplyr::filter(
      month_zoo == month_selected,
      metric_details == metric_selected,
      breakdown_dimension == "Total"
    ) |>
    # pivot wider to put the numerator / denominator / rate on the same row
    tidyr::pivot_wider(
      names_from = measure,
      values_from = value
    ) |>
    # work out some measures
    dplyr::mutate(
      # overall proportion
      overall_p = sum(count, na.rm = TRUE) / sum(patients, na.rm = TRUE),

      # z-values
      z95 = 1.96,
      z99 = 2.576,

      # Wilson centre adjustments
      centre_95 = (overall_p + (z95^2) / (2 * patients)) /
        (1 + (z95^2) / patients),
      centre_99 = (overall_p + (z99^2) / (2 * patients)) /
        (1 + (z99^2) / patients),

      # Wilson half-widths
      hw_95 = (z95 / (1 + (z95^2) / patients)) *
        sqrt(
          (overall_p * (1 - overall_p) / patients) + (z95^2) / (4 * patients^2)
        ),
      hw_99 = (z99 / (1 + (z99^2) / patients)) *
        sqrt(
          (overall_p * (1 - overall_p) / patients) + (z99^2) / (4 * patients^2)
        ),

      # final limits (multiplied by 1000 to get rate per 1000)
      lower_95 = (centre_95 - hw_95) * 1000,
      upper_95 = (centre_95 + hw_95) * 1000,
      lower_99 = (centre_99 - hw_99) * 1000,
      upper_99 = (centre_99 + hw_99) * 1000,

      # categorise points based on their position relative to confidence limits
      marker_category = dplyr::case_when(
        !(dplyr::between(
          x = rate_per_1000,
          left = lower_99,
          right = upper_99
        )) ~ "Outside 99%",
        !(dplyr::between(
          x = rate_per_1000,
          left = lower_95,
          right = upper_95
        )) ~ "Outside 95%",
        .default = "Within 95%"
      ) |>
        factor(levels = c("Outside 99%", "Outside 95%", "Within 95%")),

      # draft hover text for the points
      hover_text = glue::glue(
        "<b>{place}</b><br>",
        "{prettyunits::pretty_round(count, digits = 0)} / {prettyunits::pretty_round(patients, digits = 0)}<br>",
        "Rate per 1,000 = {prettyunits::pretty_round(rate_per_1000, digits = 1)}"
      )
    )

  return(df_return)
}

#' Generate a funnel plot for a selected place and metric
#'
#' @description
#' Creates an interactive {plotly} funnel plot showing observed rates, statistical control limits (95% and 99%) and highlighted marker for the selected place. The plot is designed for use in dashboards and supports hover text, custom colours and clear labelling of reference lines.
#'
#' @param df_funnel A tibble containing the processed funnel plot data. Must include the following columns:
#'   - `place`: place name
#'   - `patients`: denominator values
#'   - `rate_per_1000`: observed rate per 1000
#'   - `marker_category`: factor indicating whether the point is within or outside control limits
#'   - `upper_95`, `lower_95`, `upper_99`, `lower_99`: control limits
#'   - `overall_p`: overall proportion (used for the central line)
#'   - `hover_text`: text displayed on hover
#' @param place_selected A single place name (string) indicating which place should be highlighted in the plot
#' @param metric_selected A string describing the metric being plotted. Used to construct the plot title
#' @param month_selected A zoo::yearmon object representing the selected month. Also included n the plot title
#'
#' @returns A {plotly} object representing the funnel plot
#'
#' @details
#' The function:
#' - computes the rightmost data point to anchor text labels for the control limits
#' - Highlights the selected place with a distinct marker
#' - Draws 95% and 99% control limits and a central reference line
#' - Colours points according to whether they fall inside or outside the control limits
#' - Adds right-aligned text labels for each reference line
#' - Uses `scattergl` for improved performance with large datasets
#'
#' The plot is automatically generated using the selected metric and month in the format: `"metric_selected | month_selected"`
#'
#' @examples
#' \dontrun{
#' p <- get_funnel_plot(
#'   df_funnel = df_processed,
#'   place_selected = "Bristol (South Bristol)",
#'   metric_selected = "Number of category 1 ambulance conveyances",
#'   month_selected = zoo::as.yearmon("Mar 2027")
#' )
#' }
get_funnel_plot <- function(
  df_funnel,
  place_selected,
  metric_selected,
  month_selected
) {
  # prepare some data ---
  # get the last data point (for labelling the limits)
  df_last <-
    df_funnel |>
    dplyr::slice_max(order_by = patients)

  # get a title for the chart
  str_title <- glue::glue("{metric_selected} | {month_selected}")

  # get data for the selected place
  df_place <-
    df_funnel |>
    dplyr::filter(place == place_selected)

  # prepare formatting options ---
  colour_95_limit <- list(
    color = adjustcolor(col = "#959595", alpha.f = 0.5),
    width = 2
  )
  colour_99_limit <- list(
    color = adjustcolor(col = "#959595", alpha.f = 0.9),
    width = 2
  )
  colour_central <- list(
    color = adjustcolor(col = "#959595", alpha.f = 1),
    width = 2
  )
  colour_outside_99 <- adjustcolor(col = "#5881c1", red.f = 2.5)
  colour_outside_95 <- adjustcolor(col = "#5881c1", red.f = 2.0)
  colour_within_95 <- "#5881c1"
  marker_colours <- c(
    "Outside 99%" = colour_outside_99,
    "Outside 95%" = colour_outside_95,
    "Within 95%" = colour_within_95
  )

  # define a list of limits
  limit_lines <- list(
    upper_99 = list(
      var = ~upper_99,
      name = "99% limit",
      line = colour_99_limit
    ),
    lower_99 = list(
      var = ~lower_99,
      name = "99% limit",
      line = colour_99_limit
    ),
    upper_95 = list(
      var = ~upper_95,
      name = "95% limit",
      line = colour_95_limit
    ),
    lower_95 = list(
      var = ~lower_95,
      name = "95% limit",
      line = colour_95_limit
    ),
    central = list(
      var = ~ overall_p * 1000,
      name = "Reference rate",
      line = colour_central
    )
  )

  # define text labels
  labels <- list(
    upper_99 = list(var = ~upper_99, txt = "99% limit"),
    lower_99 = list(var = ~lower_99, txt = "99% limit"),
    upper_95 = list(var = ~upper_95, txt = "95% limit"),
    lower_95 = list(var = ~lower_95, txt = "95% limit"),
    central = list(var = ~ overall_p * 1000, txt = "Central")
  )

  # plot
  p <- plotly::plot_ly(
    data = df_funnel |> dplyr::filter(place != place_selected),
    x = ~patients,
    y = ~rate_per_1000,
    mode = "markers",
    color = ~marker_category,
    colors = marker_colours,
    type = "scattergl",
    frame = ~month_zoo
  )

  # add 99%, 95% and central lines
  for (ln in limit_lines) {
    p <-
      p |>
      plotly::add_lines(
        y = ln$var,
        name = ln$name,
        line = ln$line,
        hoverinfo = "skip",
        frame = ~month_zoo
      )
  }

  # add in text labels
  for (lab in labels) {
    p <-
      p |>
      plotly::add_text(
        data = df_last,
        y = lab$var,
        text = lab$txt,
        textposition = "right",
        hoverinfo = "skip",
        cliponaxis = FALSE,
        textfont = list(color = "#2c2825"),
        mode = "lines",
        frame = ~month_zoo
      )
  }

  # add the markers
  p <-
    p |>
    plotly::add_markers(
      data = df_funnel |> dplyr::filter(place != place_selected),
      name = "Observed rate",
      marker = list(size = 14, line = list(color = "#fff", width = 2)),
      hoverinfo = "text",
      text = ~hover_text,
      frame = ~month_zoo
    )

  # add the selected place marker
  p <-
    p |>
    plotly::add_markers(
      data = df_place,
      name = "Selected place",
      marker = list(
        size = 18,
        color = "#f9bd07",
        line = list(color = "#5881c1", width = 4)
      ),
      hoverinfo = "text",
      text = ~hover_text,
      frame = ~month_zoo
    )

  # add the layout
  p <-
    p |>
    plotly::layout(
      xaxis = list(
        title = "Patients",
        separatethousands = TRUE,
        zeroline = FALSE
      ),
      yaxis = list(
        title = "Rate per 1,000",
        zeroline = FALSE
      ),
      title = str_title,
      font = list(family = "Roboto, Arial, sans-serif", size = 16),
      showlegend = FALSE,
      margin = list(l = 40, r = 40, t = 60, b = 60)
    ) |>
    plotly::config(displaylogo = FALSE) |>
    plotly::animation_opts(
      frame = 1000,
      transition = 500,
      easing = "cubic-in-out"
    )

  # return the plot
  return(p)
}
