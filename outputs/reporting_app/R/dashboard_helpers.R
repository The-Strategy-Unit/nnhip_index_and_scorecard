# -----------------------------------------------------------------------------
# DASHBOARD HELPER FUNCTIONS
# -----------------------------------------------------------------------------

#' Add a metric column to a dataframe
#'
#' @description
#' This function creates a new `metric` column based on `value_type` and fills
#' the metric description upward within each `metric_block`.
#'
#' @param df_raw A data frame loaded from a Connect Pin, which contains the
#' combined monthly aggregate data submissions for the NNHIP project
#'
#' @returns A data frame with an added and filled `metric` column
add_metric_column_to_df <- function(df_raw) {
  df_raw |>
    dplyr::mutate(
      # create a new column called 'metric' which uses the metric details rate description
      metric = dplyr::case_when(
        value_type == "rate_per_1000" ~ metric_details,
        .default = NA_character_
      )
    ) |>
    # fill in the details for the three parts of the metric (count, patients, rate_per_1000)
    tidyr::fill(
      metric,
      .by = metric_block,
      .direction = "up"
    ) |>
    # convert to a factor to speed up selections
    dplyr::mutate(metric = metric |> factor())
}

#' Suppress small counts and flag affected metric blocks
#'
#' @description
#' Applies disclosure control rules to a long-format dataset by:
#' - Marking any count below a specified threshold as suppressed
#' - Replacing suppressed counts int he `value` column with `NA_integer_`
#' - Propagating suppression flags within each `metric_block`
#'
#' This function is designed for datasets where each metric is represented by
#' multiple rows (e.g., `count`, `patients`, `rate_per_1000`), and where small
#' counts must be suppressed before further processing or reshaping.
#'
#' @details
#' The function performs three key operations:
#'
#' **1. Update the `value_suppressed` flag**
#' - If a row already has `value_suppressed == TRUE`, it remains suppressed
#' - If `value_type == "count"` and the count is below `suppression_threshold`,
#'   the row is marked as suppressed
#'
#' **2. Replace suppressed counts in `value`**
#' - Only rows where `value_type == "count"` and the count is below the
#'   threshold are overwritten with `NA_integer_`
#' - Other value types (e.g., `patients`, `rate_per_1000`) are left unchanged
#'
#' **3. Flag entire metric blocks**
#' - A new logical column `count_suppressed` is added
#' - It is `TRUE` for all rows in a `metric_block` if *any* row in that block
#'   has `value_suppressed == TRUE`
#'
#' This block-level flag is especially useful before pivoting wider, since
#' `value_suppressed` typically needs to be dropped during reshaping.
#'
#' @param df_raw A tibble of metrics
#' @param suppression_threshold Integer threshold below which counts should be
#' suppressed. Defaults to `6L`, consistent with common disclosure control rules
#'
#' @returns
#' A tibble with the same rows as `df_raw`, but with:
#' - updated `value_suppressed` flags
#' - suppressed counts replaced with `NA_integer_` in `value`
#' - a new column `count_suppressed` indicating whether any count in the
#'   `metric_block` was suppressed
#'
#' @examples
#' \dontrun{
#' df_clean <- suppress_counts(df_raw)
#' }
suppress_counts <- function(df_raw, suppression_threshold = 6L) {
  df_raw |>
    # ensure counts are suppressed
    dplyr::mutate(
      .by = c(metric_block, demographic_type, demographic_value),

      # precompute whether this row should be suppressed
      is_small_count = value_type == "count" & value < suppression_threshold,

      # update suppression flag
      value_suppressed = value_suppressed | is_small_count,

      # suppress the value itself
      value = dplyr::if_else(
        condition = is_small_count,
        true = NA_integer_,
        false = value
      ),

      # flag the entire block if any row was suppressed
      count_suppressed = any(value_suppressed)
    ) |>
    dplyr::select(-is_small_count)
}

#' Prepare key columns for filtering and time-based operations
#'
#' @description
#' Ensures that two commonly-used columns in the dashboard are stored in the
#' most appropriate formats for efficient filtering and display:
#' - `place` is converted to a factor with levels sorted alphabetically
#' - `month_zoo` is coerced to a `zoo::yearmon` object for reliable
#'   month-based comparisons and ordering
#'
#' This preprocessing step is useful when the dataset is read from a pin or
#' external source where column types may not be preserved consistently.
#'
#' @details
#' Converting `place` to a factor:
#' - stabilises the ordering of choices in `selectizeInput()`
#' - avoids repeated sorting inside reactives
#' - ensures consistent behaviour when filtering by place
#'
#' Converting `month_zoo` to `yearmon`:
#' - guarantees correct chronological ordering
#' - avoids issues where the column is read as character or numeric
#' - ensures comparability with downstream time-based operations
#'
#' @param df_raw A tibble containing metric data
#'
#' @returns
#' A tibble with the same rows as `df_raw`, but with:
#' - `place` stored as a factor with sorted levels
#' - `month_zoo` stored as `zoo::yearmon` object
#'
#' @examples
#' \dontrun{
#' df_clean <- factorise_columns(df_raw)
#' }
factorise_columns <- function(df_raw) {
  df_raw |>
    dplyr::mutate(
      place = place |> factor(levels = sort(unique(place))),
      month_zoo = month_zoo |> zoo::as.yearmon()
    )
}

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
get_place_month_summary <- function(df, selected_month, selected_place = NULL) {
  df_return <-
    df |>
    dplyr::filter(
      month_zoo == selected_month,
      place == selected_place,
      demographic_type == "Total",
      dplyr::when_any(
        (metric_id == "P1" & value_type == "count"),
        (metric_id != "P1" & value_type == "rate_per_1000")
      )
    ) |>
    # fill in any NA values for P1 (e.g. Portsmouth in Jan 2026)
    dplyr::mutate(
      value = dplyr::case_when(
        (metric_id == "P1" & value_type == "count" & is.na(value)) ~ 0L,
        .default = value
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
#' summarise_averages(
#'   df = df,
#'   selected_place = "West Essex"
#' )
#' }
summarise_averages <- function(df, selected_place = NULL) {
  # get the names of the metrics
  df_metric_names <- get_metric_names(df = df, include_p1 = TRUE)

  # filter to total breakdown and remove pre-calculated rates
  df_temp <-
    df |>
    dplyr::filter(demographic_type == "Total") |>
    # handle suppressed `counts` - we need at least an estimate of the number
    # of people in order to correctly work out the average rate
    dplyr::mutate(
      rate_block = value[value_type == "rate_per_1000"],
      patients_block = value[value_type == "patients"],
      value = dplyr::case_when(
        value_suppressed & value_type == "count" ~
          (rate_block / 1000) * patients_block,
        .default = value
      ),
      .by = c(place, month_zoo, metric_block)
    ) |>
    # remove the newly created fields
    dplyr::select(-c(rate_block, patients_block, value_suppressed)) |>
    # filter out pre-calculated rates
    dplyr::filter(value_type != "rate_per_1000")

  # if a place is selected then filter to that place
  if (!is.null(selected_place)) {
    df_temp <-
      df_temp |>
      dplyr::filter(place == selected_place)
  }

  # outcome metrics: compute rate_per_1000 from summed numerator + denominator
  df_outcomes <-
    df_temp |>
    dplyr::filter(metric_id != "P1") |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .by = c(metric_id, metric_block, value_type)
    ) |>
    # work out the rate
    tidyr::pivot_wider(
      names_from = value_type,
      values_from = value
    ) |>
    dplyr::mutate(rate_per_1000 = (count / patients) * 1000) |>
    # collapse back to value_type | value columns
    tidyr::pivot_longer(
      cols = c(patients, count, rate_per_1000),
      names_to = "value_type",
      values_to = "value"
    ) |>
    # remove numerator and denominator from all outcome measures
    dplyr::filter(value_type == "rate_per_1000")

  # process metric P1: average number of patients
  df_process <-
    df_temp |>
    dplyr::filter(
      metric_id == "P1",
      value_type == "count"
    ) |>
    # summarise for the average
    dplyr::summarise(
      value = mean(value, na.rm = TRUE),
      .by = c(metric_id, metric_block, value_type)
    )

  # combine rows together
  df_combined <- dplyr::bind_rows(df_outcomes, df_process)

  # reconstruct the return df
  df_return <-
    df_metric_names |>
    dplyr::left_join(
      y = df_combined,
      by = dplyr::join_by(x$metric_block == y$metric_block)
    )

  return(df_return)
}


#' Extract sparkline-ready trend data for a selected place
#'
#' @description
#' This function filters a metric dataset for a given place and returns sparkline-ready trend data. It keeps onlyl rows representing the *Total* breakdown and applies metric-specific measure rules:
#' - for metric `"P1"`, only rows where `value_type == "patients"` are included
#' - for all other metrics, only rows where `value_type == "rate_per_1000"` are included
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
      demographic_type == "Total",
      dplyr::when_any(
        (metric_id == "P1" & value_type == "count"),
        (metric_id != "P1" & value_type == "rate_per_1000")
      )
    ) |>
    dplyr::select(metric_id, metric_details, value_type, month_zoo, value) |>
    # fill in missing trendline data with zeroes to avoid issues with
    # {reactable} and {reactablefmtr} not displaying the whole table
    dplyr::mutate(
      value = dplyr::case_when(
        (metric_id == "P1" & value_type == "count" & is.na(value)) ~ 0L,
        .default = value
      )
    ) |>
    dplyr::summarise(
      trendline = list(value[order(month_zoo)]),
      .by = c(metric_id, metric_details, value_type)
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
#' - `summarise_averages()` to compute long-term averages,
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
  df_dash_average <- summarise_averages(
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

  # troubleshooting
  # test <<- df_dashboard

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
              # data = _,
              data = ~trendline,
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
            fontFamily = "Roboto, Arial, sans-serif"
          )
        )
      )
  )
}

#' Prepare data for funnel plot visualisation
#'
#' @description
#' This function filters a dataset to a selected month and computers the
#' **Poisson-based 95% and 99% funnel plot limits** for a selected month and
#' metric.
#' It returns one row per metric with numerator, denominator, rate per 1,000,
#' Poisson confidence limits and a categorical marker indicating whether the
#' point lies inside or outside the funnel boundaries.
#'
#' @details
#' The function performs the following steps:
#'
#' **1. Filter and reshape**
#' - Keeps only rows for the selected month and the `"Total"` demographic
#' - Pivots the data wider so that `patients`, `count` and `rate_per_1000`
#'   appear on the same row for each metric.
#'
#' **2. Computes Poisson confidence limits**
#' - Calculates the *overall national rate* for each metric
#' - Computes the *expected count* under this overall rate
#' - Uses the chi-square method to derive exact Poisson 95% and 99% limits
#'   for the count
#' - Converts these limits into rates per 1,000 population
#'
#' **3. Categorise each metric**
#' - Labels each point as `"Outside 99%"`, `"Outside 95%"`, or `"Within 95%"`
#'   depending on where its rate falls relative to the funnel limits.
#'
#' @param df A tibble of metric data.
#' @param month_selected A zoo::yearmon identifying the 'current' month
#' @param metric_selected A character string specifying a metric
#'
#' @returns A data frame with one row per place with the following additional fields:
#' - numerator (`count`)
#' - denominator (`patients`)
#' - observed rate per 1,000
#' - Poisson 95% and 99% limits
#' - marker category for funnel plotting
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
    # exclude the metric_details column (this will adversely affect the below pivot)
    dplyr::select(-c(metric_details, value_suppressed)) |>
    # filter the data for the specified month and metric
    dplyr::filter(
      month_zoo == month_selected,
      metric == metric_selected,
      demographic_type == "Total"
    ) |>
    # pivot wider to put the numerator / denominator / rate on the same row
    tidyr::pivot_wider(
      names_from = value_type,
      values_from = value
    ) |>
    # remove records where there is no numerator data
    dplyr::filter_out(is.na(count)) |>
    # work out some measures
    dplyr::mutate(
      .by = c(metric_block),

      # overall rate
      overall_rate = sum(count, na.rm = TRUE) / sum(patients, na.rm = TRUE),

      # expected count under the overall rate
      expected_count = overall_rate * patients,

      # Poisson limits for counts
      lower_count_95 = 0.5 * stats::qchisq(p = 0.025, df = 2 * expected_count),
      upper_count_95 = 0.5 *
        stats::qchisq(p = 0.975, df = 2 * (expected_count + 1)),

      lower_count_99 = 0.5 * stats::qchisq(p = 0.005, df = 2 * expected_count),
      upper_count_99 = 0.5 *
        stats::qchisq(p = 0.995, df = 2 * (expected_count + 1)),

      # convert to rates per 1000
      lower_95 = (lower_count_95 / patients) * 1000,
      upper_95 = (upper_count_95 / patients) * 1000,
      lower_99 = (lower_count_99 / patients) * 1000,
      upper_99 = (upper_count_99 / patients) * 1000,

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
      hover_count = dplyr::if_else(
        condition = count_suppressed,
        true = "Below 6",
        false = prettyunits::pretty_round(count, digits = 0) |> as.character()
      ),
      # hover_count = "test",
      hover_patients = prettyunits::pretty_round(patients, digits = 0) |>
        as.character(),
      hover_rate = prettyunits::pretty_round(rate_per_1000, digits = 1) |>
        as.character(),
      hover_text = glue::glue(
        "<b>{place}</b><br>",
        "{hover_count} / {hover_patients}<br>",
        "Rate per 1,000 = {hover_rate}"
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
#'   - `overall_rate`: overall rate (used for the central line)
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
  df_limits,
  place_selected,
  metric_selected,
  month_selected
) {
  # ensure the funnel data excludes NA values
  df_funnel <- df_funnel |> dplyr::filter(!is.na(rate_per_1000))

  # prepare some data ---
  # get the last data point (for labelling the limits)
  df_last <-
    df_funnel |>
    dplyr::slice_max(order_by = patients)

  # get a title for the chart (ensure it fits in the plot area)
  str_title <- glue::glue("{metric_selected} | {month_selected}") |>
    as.character() |>
    stringr::str_wrap(width = 100) |>
    stringr::str_replace_all(pattern = "\n", replacement = "<br>")

  # get data for the selected place
  df_place <-
    df_funnel |>
    dplyr::filter(place == place_selected)

  # # compute the expected counts using the overall rate
  overall_rate <- df_funnel$overall_rate |> unique()

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
      var = ~ overall_rate * 1000,
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
    central = list(var = ~ overall_rate * 1000, txt = "Central")
  )

  # plot the markers
  p <- plotly::plot_ly(
    data = df_funnel |> dplyr::filter(place != place_selected),
    x = ~patients,
    y = ~rate_per_1000,
    type = "scattergl",
    mode = "markers",
    color = ~marker_category,
    colors = marker_colours,
    marker = list(size = 14, line = list(color = "#fff", width = 2)),
    hoverinfo = "text",
    text = ~hover_text
  )

  # add 99%, 95% and central lines
  for (ln in limit_lines) {
    p <-
      p |>
      plotly::add_lines(
        # data = df_funnel,
        data = df_limits,
        x = ~patients,
        y = ln$var,
        name = ln$name,
        line = ln$line,
        type = "scatter",
        hoverinfo = "skip",
        inherit = FALSE
      )
  }

  # add in text labels
  for (lab in labels) {
    p <-
      p |>
      plotly::add_text(
        data = df_last,
        x = ~patients,
        y = lab$var,
        text = lab$txt,
        textposition = "right",
        hoverinfo = "skip",
        cliponaxis = FALSE,
        textfont = list(color = "#2c2825"),
        mode = "text",
        inherit = FALSE
      )
  }

  # add the selected place marker
  p <-
    p |>
    plotly::add_markers(
      data = df_place,
      name = "Selected place",
      mode = "markers",
      marker = list(
        size = 18,
        color = "#f9bd07",
        line = list(color = "#5881c1", width = 4)
      ),
      hoverinfo = "text",
      text = ~hover_text
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
      title = list(text = str_title),
      font = list(family = "Roboto, Arial, sans-serif", size = 16),
      showlegend = FALSE,
      margin = list(l = 40, r = 40, t = 100, b = 60)
    ) |>
    plotly::config(displaylogo = FALSE)

  # return the plot
  return(p)
}

compute_funnel_limits <- function(df_funnel) {
  # get the overall rate
  overall_rate <- df_funnel$overall_rate |> unique()

  # create smooth lmits for the 99% and 95% limit lines
  df_limits <- tibble::tibble(
    patients = seq(
      from = min(df_funnel$patients, na.rm = TRUE),
      to = max(df_funnel$patients, na.rm = TRUE),
      length.out = 400
    )
  ) |>
    dplyr::mutate(
      expected = patients * overall_rate,
      # Poisson limits for expected counts
      lower_95 = 0.5 *
        stats::qchisq(p = 0.025, df = 2 * expected) /
        patients *
        1000,
      upper_95 = 0.5 *
        stats::qchisq(p = 0.975, df = 2 * (expected + 1)) /
        patients *
        1000,

      lower_99 = 0.5 *
        stats::qchisq(p = 0.005, df = 2 * expected) /
        patients *
        1000,
      upper_99 = 0.5 *
        stats::qchisq(p = 0.995, df = 2 * (expected + 1)) /
        patients *
        1000,

      central = overall_rate * 1000
    )
}

#' Compute national monthly averages for outcome and process metrics
#'
#' @description
#' This function takes a dataset of metric values and produces a national-level
#' monthly summary. It handles outcome metrics by recomputing rates from raw
#' numerators and denominators, and it handles process metrics (specifically
#' metric `P1`) by calculating the average number of patients.
#'
#' @details
#' The function performs three main operations:
#' **1. Pre-processing**
#' - Filters the dataset to include only rows where `demographic_type == "Total"`.
#' - Removes any rows where `value_type == "rate_per_1000"` to avoid using
#'   pre-calculated rates.
#'
#' **2. Outcome metrics (all except `P1`)**
#' - Sums numerator (`count`) and denominator (`patients`) values by `metric_id`,
#'   `metric_details`, `value_type`, `month_zoo`, and `month`
#' - Reshapes the data wide to compute a fresh `rate_per_1000` using
#' `rate_per_1000 = count / patients * 1000`.
#' - Reshapes back to long format and keeps only the calculated rate.
#'
#' **3. Process metric (`P1`)**
#' - Filters to metric `P1` and value_type `patients`
#' - Computes the *mean* number of patients per month at national level
#'
#' The final output is a combined dataset containing:
#' - One row per month per outcome metric with a calculated `rate_per_1000`.
#' - One row per month for metric `P1` with the average number of patients.
#'
#' @param df A tibble or data frame containing metric data.
#'
#' @returns A tibble with national-level monthly averages and calculated
#' outcome rates. Columns include:
#' - `metric_id`,
#' - `metric_details`,
#' - `value_type`,
#' - `value`,
#' - `month_zoo`,
#' - `month`
#'
#' @examples
#' \dontrun{
#' national_monthly_averages(df)
#' }
national_monthly_averages <- function(df) {
  # filter to total breakdown and remove pre-calculated rates
  df_temp <-
    df |>
    dplyr::filter(
      demographic_type == "Total",
      value_type != "rate_per_1000" # remove pre-calculated rates
    )

  # get the metric names
  df_metric_names <- get_metric_names(df = df)

  # outcome metrics: compute rate_per_1000 from summed numerator + denominator
  df_outcomes <-
    df_temp |>
    dplyr::filter(metric_id != "P1") |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      # .by = c(metric_id, metric_details, value_type, month_zoo, month)
      .by = c(metric_block, metric_id, value_type, month_zoo, month)
    ) |>
    # work out the rate
    tidyr::pivot_wider(
      names_from = value_type,
      values_from = value
    ) |>
    dplyr::mutate(rate_per_1000 = (count / patients) * 1000) |>
    # collapse back to value_type | value columns
    tidyr::pivot_longer(
      cols = c(patients, count, rate_per_1000),
      names_to = "value_type",
      values_to = "value"
    ) |>
    # remove numerator and denominator from all outcome measures
    dplyr::filter(value_type == "rate_per_1000")

  # process metric P1: average number of patients
  df_process <-
    df_temp |>
    dplyr::filter(
      metric_id == "P1",
      value_type == "count"
    ) |>
    # summarise for the average
    dplyr::summarise(
      value = mean(value, na.rm = TRUE),
      .by = c(metric_block, metric_id, value_type, month_zoo, month)
    )

  # combine outcome and process rows together
  df_outcome_process <- dplyr::bind_rows(df_outcomes, df_process)

  # join the combined details to the metric name
  df_return <-
    df_metric_names |>
    dplyr::left_join(
      y = df_outcome_process,
      by = dplyr::join_by(x$metric_block == y$metric_block)
    )

  return(df_return)
}

#' Prepare national-level data for funnel plotting
#'
#' @description
#' This function filters a dataset to a selected month and computes the
#' **Poisson-based 95% and 99% funnel plot limits** for national-level *rate*
#' metrics.
#' It returns one row per metric with numerator, denominator, rate per 1,000,
#' Poisson confidence limits and a categorical marker indicating whether the
#' point lies inside or outside the funnel boundaries.
#'
#' @details
#' The function performs the following steps:
#'
#' **1. Filter and reshape**
#' - Keeps only rows for the selected month and the `"Total"` demographic
#' - Aggregates numerator and denominator values.
#' - Pivots the data wider so that `patients`, `count` and `rate_per_1000`
#'   appear on the same row for each metric.
#'
#' **2. Compute Poisson confidence limits**
#' - Calculates the *overall national rate* for each metric
#' - Computes the *expected count* under this overall rate
#' - Uses the chi-square method to derive exact Poisson 95% and 99% limits
#'   for the count
#' - Converts these limits into rates per 1,000 population
#'
#' **3. Categorise each metric**
#' - Labels each point as `"Outside 99%"`, `"Outside 95%"`, or `"Within 95%"`
#'   depending on where its rate falls relative to the funnel limits.
#'
#' @param df A tibble or data frame containing metric data.
#' @param month_selected A zoo::yearmon object indicating which month to extract for the funnel plot
#'
#' @returns A data frame with one row per metric containing:
#' - numerator (`count`)
#' - denominator (`patients`)
#' - observed rate per 1,000
#' - Poisson 95% and 99% limits
#' - marker category for funnel plotting
#'
#' @examples
#' \dontrun{
#' get_data_for_funnel_plot_national(
#'   df = df,
#'   month_selected = zoo::as.yearmon("2027-03"))
#' }
get_data_for_funnel_plot_national <- function(df, month_selected) {
  # get the metric names as a separate tibble
  df_metric_names <- get_metric_names(df = df)

  # work out the limits and categorise
  df_limits <-
    df |>
    # filter the data for the specified month and metric
    dplyr::filter(
      month_zoo == month_selected,
      demographic_type == "Total"
    ) |>
    # summarise for each metric and value_type combination
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .by = c(place, metric_block, value_type)
    ) |>
    # pivot wider to put the numerator / denominator / rate on the same row
    tidyr::pivot_wider(
      names_from = value_type,
      values_from = value
    ) |>
    # work out some value_types
    dplyr::mutate(
      .by = c(metric_block),

      # overall rate
      overall_rate = sum(count, na.rm = TRUE) / sum(patients, na.rm = TRUE),

      # expected count under the overall rate
      expected_count = overall_rate * patients,

      # Poisson limits for counts
      lower_count_95 = 0.5 * stats::qchisq(p = 0.025, df = 2 * count),
      upper_count_95 = 0.5 * stats::qchisq(p = 0.975, df = 2 * (count + 1)),

      lower_count_99 = 0.5 * stats::qchisq(p = 0.005, df = 2 * count),
      upper_count_99 = 0.5 * stats::qchisq(p = 0.995, df = 2 * (count + 1)),

      # convert to rates per 1000
      lower_95 = (lower_count_95 / patients) * 1000,
      upper_95 = (upper_count_95 / patients) * 1000,
      lower_99 = (lower_count_99 / patients) * 1000,
      upper_99 = (upper_count_99 / patients) * 1000,

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
    )

  # combine the metric names and categories
  df_return <-
    df_metric_names |>
    dplyr::left_join(
      y = df_limits,
      by = dplyr::join_by(x$metric_block == y$metric_block)
    ) |>
    # remove place:metrics with zero patients (avoid infinite results)
    dplyr::filter_out(patients == 0)

  return(df_return)
}


#' Create dashboard-ready national summary data for the latest and previous months
#'
#' @description
#' This function prepares a national-level dataset suitable for use in the "National View" dashboard. It combines several components:
#' - **Monthly averages** for each metric (latest and previous month)
#' - **Month-to-month change** in national values
#' - **Funnel-plot variation indicators**, proportion of places outside the 95% and 99% limits
#' - **Trendline data**, stored as a list-column of ordered values for each metric
#'
#' @param df T
#' @param month_latest
#' @param month_prev
#'
#' @returns Tibble with one row per metric, containing all dashboard-ready summary fields.
#'
#' @export
#' @examples
get_national_dashboard_data <- function(df, month_latest, month_prev) {
  # get a summary of averages each month
  df_averages <- national_monthly_averages(df = df)

  # get dashboard data for current month
  df_dash_month_curr <-
    df_averages |>
    dplyr::filter(month_zoo == month_latest)

  # get dashboard data for previous month
  df_dash_month_prev <-
    df_averages |>
    dplyr::filter(month_zoo == month_prev)

  # get funnel plot data (to work out number of places outside of limits)
  df_funnel <-
    df |>
    dplyr::filter(month_zoo == month_latest) |>
    dplyr::distinct(metric) |>
    dplyr::pull(metric) |>
    purrr::map_dfr(
      .f = \(.x) {
        get_data_for_funnel_plot(
          df = df,
          month_selected = month_latest,
          metric_selected = .x
        )
      }
    ) |>
    dplyr::summarise(
      n_places = dplyr::n(),
      n_outside = sum(marker_category != "Within 95%"),
      place_outside_limit_rate = n_outside / n_places,
      .by = metric_block
    )

  # get the trendline data
  df_trendline <-
    df_averages |>
    dplyr::select(metric_id, metric_block, value_type, month_zoo, value) |>
    dplyr::summarise(
      trendline = list(value[order(month_zoo)]),
      .by = c(metric_id, metric_block, value_type)
    )

  # combine the data
  df_dashboard <-
    df_dash_month_curr |>
    dplyr::select(metric_block, metric_details, month_current = value) |>
    # add in previous month's values and work out the difference
    dplyr::left_join(
      y = df_dash_month_prev |>
        dplyr::select(metric_block, month_previous = value),
      by = dplyr::join_by(x$metric_block == y$metric_block)
    ) |>
    dplyr::mutate(
      month_diff = month_current - month_previous
    ) |>
    # add in funnel data
    dplyr::left_join(
      y = df_funnel |>
        dplyr::select(metric_block, place_outside_limit_rate),
      by = dplyr::join_by(x$metric_block == y$metric_block)
    ) |>
    # add in trendline
    dplyr::left_join(
      y = df_trendline |>
        dplyr::select(metric_block, trendline),
      by = dplyr::join_by(x$metric_block == y$metric_block)
    ) |>
    # remove `metric_block` as no longer needed
    dplyr::select(-c(metric_block))

  return(df_dashboard)
}

#' Display the national dashboard
#'
#' @description
#' Generates an interactive national-level dashboard table using `{reactable}`
#' and `{reactablefmtr}`.
#' The function first prepares the underlying dataset via
#' `get_national_dashboard_data()`, then renders a styled dashboard containing
#' key monthly metrics, differences, data bars and trendlines.
#'
#' @param df A data frame containing the raw input data from which national-level dashboard metrics will be derived
#' @param month_latest A zoo::yearmon() object indicating the most recent reporting month to display
#' @param month_prev A zoo::yearmon() object indicating the comparison month (typically the month preceding `month_latest`)
#'
#' @returns A `reactable` widget representing the national dashboard.
#'
#' @examples
#' \dontrun{
#' display_dashboard_national(
#'   df = my_data,
#'   month_latest = zoo::as.yearmon("Mar 2026"),
#'   month_prev = zoo::as.yearmon("Feb 2026")
#' )
#' }
display_dashboard_national <- function(df, month_latest, month_prev) {
  # get the data
  df_dashboard <- get_national_dashboard_data(
    df = df,
    month_latest = month_latest,
    month_prev = month_prev
  )

  # produce as a reactable table dashboard
  suppressWarnings(
    df_dashboard |>
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
          place_outside_limit_rate = reactable::colDef(
            name = "Outside limit rate",
            cell = reactablefmtr::data_bars(
              data = df_dashboard,
              fill_color = "#eec0e0",
              background = "#f5f6fa",
              text_position = "inside-base",
              number_fmt = scales::percent_format(accuracy = 1),
              max_value = 1,
              min_value = 0
            ),
            align = "left"
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
            fontFamily = "Roboto, Arial, sans-serif"
          )
        )
      )
  )
}


#' Extract the list of available metric names
#'
#' @description
#' Returns a tibble containing the distinct metric names available in the
#' dataset. By default, this excludes metric `"P1"` (a count-only metric),
#' but it can be included if required.
#'
#' @param df A tibble containing metric data.
#' @param include_p1 Logical; if `FALSE` (default), metric `"P1"` is excluded from the returned list
#'
#' @returns A tibble with one row per metric, containing:
#' - `metric_block`: the metric identifier
#' - `metric_details`: the human-readable metric name
#'
#' @examples
#' \dontrun{
#' get_metric_names(df_metrics)
#' get_metric_names(df_metrics, include_p1 = TRUE)
#' }
get_metric_names <- function(df, include_p1 = FALSE) {
  # get the metric names as a separate tibble
  df_metric_names <-
    df |>
    dplyr::filter(
      demographic_type == "Total",
      value_type == "rate_per_1000"
    )

  # exclude metric P1 (count only) unless specified
  if (!include_p1) {
    df_metric_names <-
      df_metric_names |>
      dplyr::filter(metric_id != "P1")
  }

  df_metric_names <-
    df_metric_names |>
    dplyr::select(metric_block, metric_details) |>
    dplyr::distinct()

  return(df_metric_names)
}

#' Display a national data coverage table
#'
#' @description
#' Creates a dashboard-friendly table showing which Places have processed data
#' available for each month. The table is structured with Places as rows and
#' months as columns, with each cell indicating whether data for that
#' Place-month combination is present in the processed dataset.
#'
#' This view is intended to provide a clear overview of **data coverage**
#' across all expected Places, highlighting gaps where processed data is missing.
#'
#' @details
#' The function performs the following steps:
#'
#' **1. Define expected Places**
#' Uses a fixed vector of Places (created in `global.R`) to ensure the table
#' includes all Places from which dadta is expected, even if no processed data
#' is present for a given month.
#'
#' **2. Construct a complete Place x Month grid**
#' All combinations of expected Places and observed months are generated using
#' `tidyr::expand_grid()`.
#'
#' **3. Identify processed submissions**
#' The input dataset is reduced to distict Place-month pairs, which represent
#' processed submissions. These are marked with a check symbol (`"✔️"`).
#'
#' **4. Join and reshape**
#' The processed submissions are left-joined onto the full grid, then pivoted
#' wider so that each month becomes a column. Missing submissions are represented
#' by empty strings.
#'
#' **5. Render a reactable table**
#' The resulting wide-format table is displayed using `{reactable}`, with:
#' - Places as sticky left-hand column
#' - Months as columns
#' - Checkmarks indicating processed data
#' - Search, sorting, highlighting and responsive resizing enabled
#'
#' @param df A tibble of metric information
#'
#' @returns A `reactable` widget displaying a Place x Month data coverage matrix.
#'
#' @examples
#' \dontrun{
#' display_national_data_coverage_table(df)
#' }
display_national_data_coverage_table <- function(df) {
  # get a matrix of submissions from each place
  all_months <- df$month_zoo |> unique() |> sort()

  # get all combinations of place and month
  submission_grid <- tidyr::expand_grid(
    place = expected_places,
    month_zoo = all_months
  )

  # get a summary of received & processed submissions
  submissions_received <- df |>
    dplyr::distinct(place, month_zoo) |>
    dplyr::mutate(received = "✔️")

  # join in received data
  submission_status <- submission_grid |>
    dplyr::left_join(
      y = submissions_received,
      by = c("place", "month_zoo")
    ) |>
    # dplyr::mutate(received = !is.na(received)) |>
    tidyr::pivot_wider(
      names_from = month_zoo,
      values_from = received,
      values_fill = ""
    )

  # render with reactable
  reactable::reactable(
    data = submission_status,
    pagination = FALSE,
    searchable = TRUE,
    sortable = TRUE,
    highlight = TRUE,
    resizable = TRUE,
    defaultColDef = reactable::colDef(
      minWidth = 45
    ),
    columns = list(
      place = reactable::colDef(sticky = "left", name = "Place", minWidth = 200)
    ),
    theme = reactable::reactableTheme(
      style = list(
        fontFamily = "Roboto, Arial, sans-serif"
      )
    )
  )
}
