#' List submission files stored in a Microsoft Teams channel
#'
#' @description
#' This function connects to a Microsoft Teams channel (using environment
#' variables `su_team` and `su_channel`), navigates to a base folder (default
#' `"Neighbourhood DC"`), optionally drills down to a specific month subfolder,
#' downloads the folder contents to a temporary directory and returns the full
#' file paths of the downloaded files.
#'
#' @param month Optional. A character string naming a subfolder (typically in `"YYYY-MM"` format) inside the base folder. If `NULL``, the entire base folder is downloaded.
#' @param base_folder The name of the top-level folder inside the channel containing submissions. Defaults to `"Neighbourhood DC"`
#'
#' @returns A character vector of full file paths pointing to the downloaded fles in a temporary directory
#'
#' @examples
#' \dontrun{
#' list_submission_files()
#' list_submission_files(month = "2026-01")
#' }
list_submission_files <- function(
  month = NULL,
  base_folder = "Neighbourhood DC"
) {
  # get environment variables
  team_name <- Sys.getenv("su_team")
  channel_name <- Sys.getenv("su_channel")
  if (team_name == "" || channel_name == "") {
    cli::cli_abort(
      "Environment variables {.var su_team} and {.var su_channel} must be set."
    )
  }

  # access the team and channel
  team <- Microsoft365R::get_team(team_name = team_name)
  channel <- team$get_channel(channel_name)
  root <- channel$get_folder()

  # access the base folder
  tryCatch(
    {
      folder <- root$get_item(base_folder)
    },
    error = function(e) {
      cli::cli_abort(
        "The folder {.val {base_folder}} does not exist in this channel."
      )
    }
  )

  # # get a specific submission month if requested
  if (!is.null(month)) {
    tryCatch(
      {
        folder <- folder$get_item(month)
      },
      error = function(e) {
        cli::cli_abort(
          "The folder {.val {month}} does not exist inside {.val {base_folder}}."
        )
      }
    )
  }

  # download to a persistent temporary directory
  tmp <- tempfile()
  dir.create(tmp)
  folder$download(dest = tmp, recursive = TRUE, parallel = TRUE)
  files <- list.files(tmp, recursive = TRUE, full.names = TRUE)

  # return the list of files
  return(files)
}

#' Extract and clean header information from a raw submission template
#'
#' @description
#' Many Excel-based submission templates use a two-row header structure: a
#' top-level grouping row (e.g., "Age Group", "Ethnic Group") followed by a
#' second row containing the detailed column names. This function extracts
#' those two header rows (assumed to be rows 4 and 5 and the raw import), fills
#' down missing group names, removes `NA`s and combines the two rows into a
#' single cleaned character vector of column headings.
#'
#' @param df_raw A data frame read directly from the "SubmissionTemplate" sheet of an Excel submission template where rows 4 and 5 contain the two-level header structure.
#'
#' @returns A character vector of cleaned column headings, one per column in `df_raw`
#'
#' @examples
#' \dontrun{
#' df <- readxl::read_excel("submission.xlsx", col_names = FALSE)
#' headers <- get_submissiontemplate_header(df)
#' headers
#' }
get_submissiontemplate_header <- function(df_raw) {
  # check that all columns have auto-generated names
  all_auto <- all(grepl(pattern = "^\\.\\.\\.[0-9]+$", x = names(df_raw)))
  if (!all_auto) {
    cli::cli_abort(
      "Column names do not appear to be auto-generated. The Excel file must be read with {.code col_names = FALSE}."
    )
  }

  # read the header component
  header <- df_raw |> dplyr::slice(4:5)

  # check that rows 4 and 5 contain some non-NA values
  if (all(is.na(unlist(header)))) {
    cli::cli_abort(
      "Header rows (4 and 5) appear to be empty or invalid. The submission template may be corrupted or misaligned."
    )
  }

  # check the number of columns is consistent between the two rows
  if (length(unlist(header[1, ])) != length(unlist(header[2, ]))) {
    cli::cli_abort("Header rows 3 and 4 have different numbers of columns.")
  }

  # format the headings to a single vector
  headings <-
    tibble::tibble(
      head1 = header[1, ] |> unlist(),
      head2 = header[2, ] |> unlist()
    ) |>
    # fill in missing head1 values with the previous value
    tidyr::fill(head1) |>
    # remove any NAs
    dplyr::mutate(
      head1 = head1 |> tidyr::replace_na(replace = ""),
      head2 = head2 |> tidyr::replace_na(replace = "")
    ) |>
    # combine the two headings to a single column
    dplyr::rowwise() |>
    dplyr::mutate(
      head = list(head1, head2) |>
        unique() |> # where the two headings are the same
        paste(collapse = "") |> # convert to a single character string
        stringr::str_trim() # trim any white space
    ) |>
    # extract
    dplyr::ungroup() |>
    dplyr::pull(head)

  # return the result
  return(headings)
}

#' Process a raw submission template into a tidy analytical form
#'
#' @description
#' This function takes a raw data frame read from the Excel submission template
#' (with `col_names = FALSE`), extracts the cleaned column headings using
#' `get_submissiontemplate_header()`, applies them to the data rows, reshapes the
#' demographic columns into long format, derives demographic metadata, flag
#' suppressed values and returns a tidy dataset ready for analysis.
#'
#' The function assumes:
#' - Rows 4-5 contain the two-row header structure
#' - Data begins on row 6
#' - Each metric block consists of three rows in order:
#'   1. count,
#'   2. patients
#'   3. rate per 1,000
#'
#' @param df_raw A raw data frame read from the submission template using `readxl::read_excel(..., col_names = FALSE)`
#' @param suppression_marker A character vector of values indicating suppressed entries (default: `"*"`)
#'
#' @returns
#' A tidy data frame with one row per metrix x demographic x value type.
#' Columns include:
#' - `metric_id` Character.
#' - `metric_block` Integer, representing a related block of three `value_type`
#' - `metric_type` ("Outcome", "Process")
#' - `metric_details` Character name of the metric
#' - `demographic_type` ("Total", "Age Group", "Ethnic Group", "Deprivation Quintile")
#' - `demographic_value` Character description of each value of the `demographic_type`
#' - `value_type` ("count", "patients", "rate_per_1000")
#' - `value` Numeric, with suppressed values converted to `NA`
#' - `value_suppressed` (logical)
#'
#' @examples
#' \dontrun{
#' df_raw <- readxl::read_excel("submission.xlsx", col_names = FALSE)
#' tidy <- process_submissiontemplate_data(df_raw)
#' }
process_submissiontemplate_data <- function(
  df_raw,
  suppression_marker = c("*")
) {
  # get the cleaned column headings
  headings <- get_submissiontemplate_header(df_raw = df_raw)

  # validate the expected columns
  if (
    !any(stringr::str_starts(
      headings,
      "Total|Age Group|Ethnic Group|Deprivation Quintile"
    ))
  ) {
    cli::cli_abort("No demographic columns found - template may be invalid.")
  }

  # get the contents of the raw data frame, excluding headings
  df <- df_raw |> dplyr::slice(6:dplyr::n())

  # apply the column names
  colnames(df) <- headings

  # process the data
  df_return <-
    df |>
    # add in row types
    dplyr::mutate(
      value_type = rep(
        c("count", "patients", "rate_per_1000"),
        length.out = dplyr::n()
      ) |>
        factor(),
      metric_block = ceiling(dplyr::row_number() / 3) |> as.integer()
    ) |>
    tidyr::pivot_longer(
      cols = c(
        dplyr::starts_with("Total"),
        dplyr::starts_with("Age Group"),
        dplyr::starts_with("Ethnic Group"),
        dplyr::starts_with("Deprivation Quintile")
      ),
      names_to = "demographic",
      values_to = "value"
    ) |>
    # work out the demographic columns
    dplyr::mutate(
      demographic_type = dplyr::case_when(
        stringr::str_starts(demographic, "Total") ~ "Total",
        stringr::str_starts(demographic, "Age Group") ~ "Age Group",
        stringr::str_starts(demographic, "Ethnic Group") ~ "Ethnic Group",
        stringr::str_starts(
          demographic,
          "Deprivation Quintile"
        ) ~ "Deprivation Quintile"
      ) |>
        factor(),
      demographic_value = stringr::str_remove(
        demographic,
        "^(Age Group|Ethnic Group|Deprivation Quintile)"
      ) |>
        factor()
    ) |>
    # clean up variable names
    janitor::clean_names() |>
    dplyr::mutate(metric_details = stringr::str_squish(metric_details)) |>
    dplyr::select(
      metric_id,
      metric_block,
      metric_type,
      metric_details,
      demographic_type,
      demographic_value,
      value_type,
      value
    ) |>
    # fill in missing metric blocks
    tidyr::fill(c(metric_id, metric_type, metric_details)) |>
    # flag suppressed values and convert (where possible) to a number
    dplyr::mutate(
      value_suppressed = value %in% suppression_marker,
      value = dplyr::na_if(x = value, y = suppression_marker) |> as.numeric()
    ) |>
    dplyr::arrange(metric_id, metric_block, value_type)

  # return the cleaned up data
  return(df_return)
}

#' Extract project name and reporting period from the Instructions sheet
#'
#' @description
#' This function extracts the submitting organisation ("Project Name") and the
#' reporting period ("Submission Period") from a raw data frame representing
#' the *Instructions* sheet of the submission template. The function expects
#' the sheet to contain two key rows:
#' - `Project Name` in column `col_1`, with its value in `col_2`
#' - `Submission Period` in column `col_1`, with its Excel-serial date in `col_2`
#'
#' The submission period is converted to both a `"YYYY-MM"` string and a `zoo::yearmon` object.
#'
#' @param df_raw A data frame containing the Instructions sheet, read using `readxl::read_excel(..., col_types = "text", col_names = c("col_1", "col_2"))`
#'
#' @returns
#' A named list with:
#' - `project_name`: character string
#' - `period_txt`: `"YYYY-MM"` formatted string
#' - `period_zoo`: `zoo::yearmon` object
#'
#' @examples
#' \dontrun{
#' df_raw <- readxl::read_excel("submission.xlsx", sheet = "Instructions", col_types = "text", col_names = c("col_1", "col_2"))
#' info <- process_instructions_data(df_raw)
#' }
process_instructions_data <- function(df_raw) {
  # extract the details from the raw file
  name_period <-
    df_raw |>
    dplyr::filter(col_1 %in% c("Project Name", "Submission Period")) |>
    dplyr::pull(col_2)

  # basic validation
  if (length(name_period) < 2) {
    cli::cli_abort(
      "Could not find both 'Project Name' and 'Submission Period' in the Instructions sheet."
    )
  }

  # convert Excel serial data to Date
  period_date <-
    name_period[2] |>
    as.integer() |>
    as.Date(origin = "1899-12-30")

  # prepare a formatted list of values to return
  ls_return <-
    list(
      project_name = name_period[1],
      period_txt = period_date |> format("%Y-%m"),
      period_zoo = period_date |> zoo::as.yearmon()
    )

  return(ls_return)
}

#' Load and process a full submission file
#'
#' @description
#' This function coordinates the full ingestion of a submission workbook. It:
#' 1. Reads the *Instructions* sheet to extract the submitting place and reporting period
#' 2. Reads the *Submission Template* sheet to extract and tidy the metric data
#' 3. Combines the metadata (place, month) with the tidy metric dataset
#'
#' The function expects:
#' - The Instructions sheet to contain two columns (`col_1` and `col_2`) with entries for `"Project Name"` and `"Submission Period"`.
#' - The Submission Template sheet to be read with `col_names = FALSE` so that header rows can be processed correctly by `process_submissiontemplate_data()`.
#'
#' @param str_submission_filepath A file path to the submission Excel workbook
#'
#' @returns
#' A tibble containing:
#' - `place` (character)
#' - `month` (YYYY-MM string)
#' - `month_zoo` (`zoo::yearmon`)
#' - All columns returned by `process_submissiontemplate_data()`
#'
#' @examples
#' \dontrun{
#' df <- process_submission("submission_file.xlsx")
#' }
process_submission <- function(str_submission_filepath) {
  # read instructions sheet
  raw_in <-
    readxl::read_xlsx(
      path = str_submission_filepath,
      sheet = "Instructions",
      col_types = "text",
      col_names = c("col_1", "col_2")
    )
  # read submission template (avoid console messages about name repair)
  raw_st <-
    suppressMessages(
      readxl::read_xlsx(
        path = str_submission_filepath,
        sheet = "SubmissionTemplate",
        col_types = "text",
        col_names = FALSE,
        # .name_repair = "minimal" # prevent console messages about column name creation
      )
    )

  # process the data
  ls_details <- process_instructions_data(df_raw = raw_in)
  df_template <- process_submissiontemplate_data(df_raw = raw_st)

  # combine the details and template
  df_return <-
    tibble::tibble(
      place = ls_details["project_name"] |> unlist(),
      month = ls_details["period_txt"] |> unlist(),
      month_zoo = ls_details["period_zoo"] |> unlist()
    ) |>
    dplyr::cross_join(y = df_template) |>
    # re-convert month_zoo to a zoo object
    dplyr::mutate(month_zoo = month_zoo |> zoo::as.yearmon())
}

#' Download, process and combine all submission files for a given month
#'
#' @description
#' This function coordinates the full ingestion workflow for a reporting month.
#' It:
#' 1. Retrieves the list of submission filepaths for the specified month
#'    (by downloading them from SharePoint)
#' 2. Iterates over each file and processes it using `process_submission()`
#' 3. Combines all processed submissions into a single tidy table
#'
#' A progress bar is displayed during processing via `{purrr}`'s
#' `.progress = TRUE` argument.
#'
#' @param month Optional. A character string in the format "YYYY-MM" indicating the name of the folder containing the data to ingest. Passed directly `list_submission_files()`.
#'
#' @returns
#' A tibble containing the combined tidy data from all submission for the specified month.
#'
#' @examples
#' \dontrun{
#' df <- ingest_data (month = "2026-01")
#' }
ingest_data <- function(month = NULL) {
  # call a process to download a local copy of the data and return their filepaths
  files <- list_submission_files(month = month)

  # handle cases where no files are returned
  if (length(files) == 0) {
    cli::cli_abort("No submission files found")
  }

  # NB, need to handle cases where files are rejected because they don't meet validation (see André's work)

  # create a safe process (so a single file doesn't disrupt the whole process)
  safe_process <- purrr::safely(process_submission)

  # create a progress bar
  cli::cli_progress_bar(
    name = "Processing submissions",
    total = length(files)
  )

  # iterate over the files and compile a tibble
  results <-
    purrr::map(
      .x = files,
      .f = \(.x) {
        # ephemeral message inside progress bar
        cli::cli_progress_message("Processing {.val {basename(.x)}}")

        # gather the results
        out <- safe_process(str_submission_filepath = .x)

        if (!is.null(out$error)) {
          # persistent message printed AFTER progress bar
          cli::cli_alert_danger(
            "Failed: {.val {basename(.x)}} - {out$error$message}"
          )
        }

        cli::cli_progress_update()

        # output the object
        out
      }
    )

  cli::cli_progress_done()

  # combine all the returned responses to a tibble
  df <- purrr::map_dfr(results, "result")

  # return the collated data
  return(df)
}
