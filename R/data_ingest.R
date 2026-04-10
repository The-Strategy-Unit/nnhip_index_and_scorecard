# MS Teams --------------------------------------------------------------------
#' Retrieve a refrence to a folder within a Microsoft Teams channel
#'
#' @description
#' This helper function connects to a Microsoft Teams channel (backed by
#' SharePoint) and returns a reference to a specific folder within that
#' channel's file storage. It centralises the logic for locating the correct
#' team, channel and folder, thereby reducing duplication across the codebase.
#'
#' @details
#' The function expects two environment variables to be set:
#' - `su_team`: the display name of the Microsoft Team
#' - `su_channel`: the name of the channel within that team
#'
#' These are used to locate the correct SharePoint document library.
#'
#' If the team, channel or folder cannot be found, the function aborts with a clear error message.
#'
#' @param base_folder Character string giving the name of the folder to retrive from the channel's root directory. Defaults to "Neighbourhood DC".
#'
#' @returns An `ms_drive_item` object representing the requested folder
get_ms_teams_folder <- function(base_folder = "Neighbourhood DC") {
  # get environment variables
  team_name <- Sys.getenv("su_team")
  channel_name <- Sys.getenv("su_channel")
  if (team_name == "" || channel_name == "") {
    cli::cli_abort(
      "Environment variables {.var su_team} and {.var su_channel} must be set."
    )
  }

  # access the team
  team <- tryCatch(
    Microsoft365R::get_team(team_name = team_name),
    error = function(e) {
      cli::cli_abort(
        "Could not access the Microsoft Team {.val {team_name}}.
        Check that the name is correct and that you have permission."
      )
    }
  )

  # access the channel
  channel <- tryCatch(
    team$get_channel(channel_name),
    error = function(e) {
      cli::cli_abort(
        "The channel {.val {channel_name}} does not exist in team {.val {team_name}}"
      )
    }
  )

  # access the root folder
  root <- channel$get_folder()

  # access the base folder
  folder <- tryCatch(
    root$get_item(base_folder),
    error = function(e) {
      cli::cli_abort(
        "The folder {.val {base_folder}} does not exist in this channel."
      )
    }
  )

  # return the folder object
  return(folder)
}

#' List subfolders within a Microsoft Teams / SharePoint folder
#'
#' @description
#' This function returns the names of all subfolders inside a specified
#' Microsoft Teams channel folder. If no folder object is supplied, it
#' retrieves the default folder using `get_ms_teams_folder()`.
#'
#' @details
#' The function lists all items within the folder and filters for those where
#' `isdir == TRUE`, returning their names. If no subfolders are found, the
#' function aborts with a descriptive error message.
#'
#' @param ms_teams_folder Optional `ms_drive_item` object representing a folder in a Teams / SharePoint document library. If `NULL` (the default), the function will call `get_ms_teams_folder()` to obtain the base folder.
#'
#' @returns A character vector of folder names
list_submission_folders <- function(ms_teams_folder = NULL) {
  # ensure we have a teams folder to work with
  if (is.null(ms_teams_folder)) {
    folder <- get_ms_teams_folder()
  } else {
    folder <- ms_teams_folder
  }

  # validate the folder object
  if (!folder$is_folder()) {
    cli::cli_abort(
      "The supplied object is not a valid {.cls ms_drive_item} folder."
    )
  }

  # update the user
  cli::cli_progress_message("Accessing folders ...")

  # list the items in this folder
  items <- folder$list_items()

  # keep only the folders
  submission_folders <-
    items |>
    dplyr::filter(isdir == TRUE) |>
    dplyr::pull(name)

  # check we have at least one folder to return
  if (length(submission_folders) == 0) {
    cli::cli_abort("No submission folders found.")
  }

  # return the list of files
  return(submission_folders)
}

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
  base_folder = "Neighbourhood DC",
  ms_teams_folder = NULL
) {
  # get a reference to the ms_teams_folder if it was not supplied
  if (is.null(ms_teams_folder)) {
    folder <- get_ms_teams_folder()
  } else {
    # otherwise use the supplied object
    folder <- ms_teams_folder
  }

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

  # update the user
  cli::cli_progress_message("Accessing submitted files ...")

  # download to a persistent temporary directory
  tmp <- tempfile()
  dir.create(tmp)
  folder$download(dest = tmp, recursive = TRUE, parallel = TRUE)
  files <- list.files(
    tmp,
    recursive = TRUE,
    full.names = TRUE,
    pattern = "*\\.xlsx$"
  )

  # return the list of files
  return(files)
}

# File processing -------------------------------------------------------------

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
  # header <- df_raw |> dplyr::slice(4:5)
  header <- df_raw |> dplyr::slice(1:2)

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

  # standardise the headings
  headings <-
    headings |>
    # remove double-spacings as well as all other whitespace (tabs, newlines, etc)
    stringr::str_squish() |>
    # standardise 'Not known' values
    stringr::str_replace_all(
      pattern = c("Not Known$|not known$|NOT KNOWN$|not Known$"),
      replacement = "Not known"
    )

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
  suppression_marker = c("*", "", "<5", "-"),
  # suppression_marker = c("*", ""),
  filepath = NULL
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
  df <- df_raw |> dplyr::slice(3:dplyr::n())

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
      value_suppressed = (value %in% suppression_marker) | is.na(value),
      value = replace(
        x = value,
        list = value %in% suppression_marker,
        values = NA_character_
      )
    ) |>
    dplyr::arrange(metric_id, metric_block, value_type)

  # identify suspicious values which don't parse as numeric
  df_suspicious <-
    df_return |>
    dplyr::filter(!is.na(value)) |>
    dplyr::distinct(value) |>
    dplyr::mutate(
      value_numeric = readr::parse_double(x = value, na = suppression_marker),
      value_numeric_flag = !is.na(value_numeric)
    ) |>
    dplyr::filter(!value_numeric_flag) |>
    # prevent warnings about coerced values
    suppressWarnings()

  # run a final check for non-numeric values - alert the user if any found
  if (nrow(df_suspicious) > 0) {
    # gather some information
    n_sus_vals <- nrow(df_suspicious)
    sus_vals <- df_suspicious$value |> unique()

    # give the user some information
    cli::cli_alert_danger(
      "Found {n_sus_vals} non-numeric entr{?y/ies} in {.val value} ({.val {sus_vals}}) in the file {.file {basename(filepath)}}. Consider adding {?it/them} to {.var suppression_marker} parameter of {.fn process_submissiontemplate_data}.",
      wrap = TRUE
    )
  }

  # finally, convert `value` to numeric and return (silently)
  df_return <-
    df_return |>
    dplyr::mutate(
      value = readr::parse_double(x = value, na = suppression_marker)
    ) |>
    suppressWarnings()

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
      col_names = c("col_1", "col_2"),
    )
  # read submission template (avoid console messages about name repair)
  raw_st <-
    suppressMessages(
      readxl::read_xlsx(
        path = str_submission_filepath,
        sheet = "SubmissionTemplate",
        col_types = "text",
        col_names = FALSE,
        range = "A1:Z100", # need this to cut off any trailing comments at the right-end of the data table, e.g. "No current data available"
        trim_ws = TRUE # cut blank rows of data above and below the table
      ) |>
        # remove blank rows (determined by blank metric_details)
        dplyr::filter_out(is.na(...3))
    )

  # process the data
  ls_details <- process_instructions_data(df_raw = raw_in)
  df_template <- process_submissiontemplate_data(
    df_raw = raw_st,
    filepath = str_submission_filepath
  )

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
ingest_data <- function(month = NULL, ms_teams_folder = NULL) {
  # call a process to download a local copy of the data and return their filepaths
  files <- list_submission_files(
    month = month,
    ms_teams_folder = ms_teams_folder
  )

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

  # let the user know how many files were read
  n_files <- length(files)
  cli::cli_alert_success("{n_files} file{?s} were processed.")

  cli::cli_progress_done()

  # combine all the returned responses to a tibble
  df <- purrr::map_dfr(results, "result")

  # return the collated data
  return(df)
}

# Posit Connect ---------------------------------------------------------------

#' Update pinned monthly and combined datasets on Posit Connect
#'
#' @description
#' This function ingests all submitted Excel files for a given month, stores the
#' cleaned monthly dataset as a pin, updates the list of processed months and
#' refereshes a combined dataset containing all months' submissions.
#'
#' @returns Invisibly returns TRUE on success
update_pinned_data_for_month <- function() {
  # collate the submissions for a month
  df_month <- collate_submissions_for_month()

  # get the month_id (textual representation of the month in YYYY-MM format)
  month_id <-
    df_month |>
    dplyr::filter(!is.na(month)) |>
    dplyr::slice_head() |>
    dplyr::pull(month)

  # connect to the Posit Connect board
  server <- Sys.getenv("posit_server")
  account <- Sys.getenv("posit_account")
  prefix <- Sys.getenv("pin_prefix")

  board <- pins::board_connect(server = server, account = account)

  # store the ingested month's data in a pin
  monthly_pin <- glue::glue("{prefix}{month_id}")
  pins::pin_write(
    board = board,
    name = monthly_pin,
    x = df_month,
    type = "rds"
  ) |>
    suppressMessages()

  # get a previous list of month_id's stored as pins
  months_pin <- glue::glue("{prefix}months")
  reporting_months <- pins::pin_read(
    board = board,
    name = months_pin
  )

  # add the latest month_id to this vector
  reporting_months <-
    c(reporting_months, month_id) |>
    unique() |>
    sort()

  # write this vector back to the pin listing months processed
  pins::pin_write(
    board = board,
    name = months_pin,
    x = reporting_months,
    type = "rds"
  ) |>
    suppressMessages()

  # combine all monthly submissions to a single overall dataset
  df_all <-
    purrr::map_dfr(
      .x = reporting_months,
      .f = \(.x) {
        pin_name = glue::glue("{prefix}{.x}")
        pins::pin_read(board = board, name = pin_name)
      }
    )

  # write / update this combined dataset to the pin board too
  pin_name <- glue::glue("{prefix}all")
  pins::pin_write(
    board = board,
    x = df_all,
    name = pin_name,
    type = "rds"
  ) |>
    suppressMessages()

  # clean up the connections to the temporary files downloaded from SharePoint
  closeAllConnections()

  # return
  invisible(TRUE)
}

#' Collate all data submissions for a selected reporting month
#'
#' @description
#' This function guides the user through selecting a monthly submission folder
#' from a Microsoft Teams / SharePoint site and then ingests all submissions
#' for that month. It provides a numbered menu of available folders, validates
#' the user's selections and safely processes the chosen month's data.
#'
#' @details
#' Workflow:
#'   1. Connect to the Teams/SharePoint site using `get_ms_teams_folder()`
#'   2. Retrieve the list of available submission folders via
#'      `list_submission_folders()`
#'   3. Display the folders as a numbered list and prompt the user to select one
#'   4. Validate the user's input to ensure it corresponds to a valid folder
#'   5. Ingests all submissions for the selected month using `ingest_data()`,
#'      with errors captured and reported using `purrr::safely()`
#'   6. Return a data frame containing the combined monthly submissions.
#'
#' @returns
#' A data frame (`df_month`) containing all successfully ingested submissions
#' for the selected reporting month.
#'
#' @raises
#' - An error if the user enters an invalid folder number
#' - An error if `ingest_data()` fails to read one or more submissions
#'
#' @seealso
#' - `get_ms_teams_folder()` for establishing the SharePoint connection
#' - `list_submission_folders()` for discovering available submission periods
#' - `ingest_data()` for the underlying ingestion logic
#'
#' @examples
#' \dontrun{
#' df <- collate_submissions_for_month()
#' }
collate_submissions_for_month <- function() {
  # connect to the Teams / SharePoint site
  folder <- get_ms_teams_folder()

  # list folders
  folders <- list_submission_folders(ms_teams_folder = folder)

  # display a numbered menu
  cli::cli_h2("Available submission folders:")
  cli::cli_ol(items = folders)

  # ask the user to choose
  choice <- readline("Select a folder by number: ")

  # validate the choice
  if (!choice %in% (length(folders) |> seq_len() |> as.character())) {
    cli::cli_abort("Invalid selection.")
  }

  # get the name of the selected folder
  month_id <- folders[as.integer(choice)]

  # process a month's data safely to catch any errors
  safe_ingest_data <- purrr::safely(ingest_data)
  out <- safe_ingest_data(month = month_id, ms_teams_folder = folder)
  if (!is.null(out$error)) {
    cli::cli_abort(
      "Failed to read all submissions for {.val {month_id}}: {out$error$message}"
    )
  }
  df_month <- out$result

  return(df_month)
}

#' Create placeholder monthly pins on Posit Connect
#'
#' @description
#' This function pre-creates a set of monthly pins on a Posit Connect board.
#' Each pin is populated with a simple placeholder object and named using a
#' prefix combined with the year-month (e.g.,
#' "craig.parylo/nnhip_scorecard_reporting").
#'
#' The purpose is to establish all required pins *in advance*, so that access
#' control can be configured once (e.g., granting a team group collaborator
#' access). After this setup, any team member can update the pin without
#' running into ownership issues.
#'
#' @details
#' This function:
#' 1. Connects to a Posit Connect board using environment variables:
#'   - `posit_server`
#'   - `posit_account`
#'   - `pin_prefix`
#' 2. Generates a monthly sequence from `from` to `to`
#' 3. Writes a placeholder RDS object to each pin name
#'
#' The placeholder is intentionally minimal; its only purpose is to create the
#' pin so that access control can be configured manually via the web GUI.
#'
#' @param from A character string coercible to Date. The first month to create.
#' @param to A character string coercible to Date. The last month to create.
#'
#' @returns Invisibly returns TRUE on success.
#'
#' @examples
#' \dontrun{
#' create_placeholder_pins(from = "2026-02-01", to = "2027-03-01")
#' }
create_placeholder_pins <- function(from = "2026-02-01", to = "2027-3-01") {
  # connect to the Posit Connect board
  server <- Sys.getenv("posit_server")
  account <- Sys.getenv("posit_account")
  prefix <- Sys.getenv("pin_prefix")

  board <- pins::board_connect(server = server, account = account)

  # create a placeholder object
  placeholder <- c("Placeholder")

  # create a list of months to generate pins for
  months <- seq(
    from = as.Date(from),
    to = as.Date(to),
    by = "month"
  )

  # iterate over each month and store a placeholder rds object as a pin
  purrr::walk(
    .x = months,
    .f = \(.x) {
      # create the pin name
      pin_name <- glue::glue("{prefix}{.x |> format('%Y-%m')}")
      pins::pin_write(
        board = board,
        name = pin_name,
        x = placeholder,
        type = "rds"
      )
      cli::cli_inform("Created: {.val {pin_name}}")
    }
  )

  invisible(TRUE)
}

# Validation ------------------------------------------------------------------

#' Validate monthly submissions and identify data issues
#'
#' @description
#' This function wraps `collate_submissions_for_month()` to ingest a month's submissions and then run a series of validation checks before the data is used downstream. It currently checks for unexpected record counts by `place`, but can be extended with additional validation rules.
#'
#' @returns
#' A list with two elements:
#' - `data`: the ingested data frame for the selected month
#' - `issues`: a data frame of detected issues suitable for logging
#'
#' @examples
#' \dontrun{
#' result <- validate_monthly_submissions()
#' result$issues
#' }
validate_monthly_submissions <- function() {
  # ingest the data
  df <- collate_submissions_for_month()

  # run validation checks
  issues <- list()

  # check 1: record counts by place
  issues[[length(issues) + 1]] <- check_record_counts_per_place(df = df)
  # check 2: demographic breakdowns
  issues[[length(issues) + 1]] <- check_demographic_breakdown(df = df)
  # check 3: no NA values in `month`
  issues[[length(issues) + 1]] <- check_month_completeness(df = df)
  # check 4: all months are the same - identify places that are 'abnormal'
  issues[[length(issues) + 1]] <- check_month_consistency(df = df)

  # combine issues into a single data frame
  issues_df <- if (length(issues) == 0) {
    dplyr::tibble()
  } else {
    dplyr::bind_rows(issues)
  }

  # return both data and issues
  list(
    data = df,
    issues = issues_df
  )
}

empty_issue_schema <- function() {
  tibble::tibble(
    issue_type = character(),
    description = character(),
    place = character(),
    field = character(),
    value = character(),
    impact = character(),
    status = character()
  )
}

#' Check for anomalies in record counts by place
#'
#' @param df A data frame of ingested metrics data
#'
#' @returns A data frame of issues (possibly empty) with fields suitable for
#' logging in the data-issues log
check_record_counts_per_place <- function(df) {
  # count records by place
  counts <- df |> dplyr::count(place, name = "n_records")

  # define suspicious counts
  suspicious <- counts |>
    dplyr::filter(n_records != 1035)

  # if nothing suspicious, return an empty tibble
  if (nrow(suspicious == 0)) {
    return(empty_issue_schema())
  }

  # otherwise return structured issues
  if (nrow(suspicious) > 0) {
    cli::cli_alert_danger("Issues detected with the number of rows per Place")

    suspicious |>
      dplyr::mutate(
        issue_type = "Record count anomaly",
        description = "Unexpected number of records for place (expecting 1035)",
        place = place,
        field = "n_records",
        value = as.character(n_records),
        impact = "Potentially incorrect or missing submission",
        status = "Open"
      ) |>
      dplyr::select(
        issue_type,
        description,
        place,
        field,
        value,
        impact,
        status
      )
  }
}

#' Check for unexpected demographic breakdown values
#'
#' @description
#' This validation check ensures that each record contains only approved demographic types and demographic values. Any unexpected combination is flagged as a potential data-quality issue, often indicating an outdated submission template or manual data entry error.
#'
#' @param df data frame of ingested metrics data
#'
#' @returns A data frame of issues (possibly empty) with fields suitable for
#' logging in the data-issues log
check_demographic_breakdown <- function(df) {
  # expected demographic types
  valid_types <- c("Age Group", "Deprivation Quintile", "Ethnic Group", "Total")

  # expected demographic values
  valid_values <- c(
    "Total",
    # Age group
    "18-19",
    "20-29",
    "30-39",
    "40-49",
    "50-59",
    "60-69",
    "70-79",
    "80-89",
    "90+",
    "Not known",
    # Ethnicity
    "Asian",
    "Black",
    "Mixed",
    "Not known",
    "Other",
    "White",
    # Deprivation quintile
    "1",
    "2",
    "3",
    "4",
    "5",
    "Not known"
  )

  # define suspicious counts
  suspicious <- df |>
    dplyr::filter(
      !demographic_type %in% valid_types | !demographic_value %in% valid_values
    ) |>
    dplyr::distinct(place, demographic_type, demographic_value)

  # if no issues then retun empty schema
  if (nrow(suspicious) == 0) {
    return(empty_issue_schema())
  }

  # if issues detected, then respond and gather data
  if (nrow(suspicious) > 0) {
    cli::cli_alert_danger(
      "Issues detected with demographic breakdowns. Check the issues log.",
      wrap = TRUE
    )

    suspicious |>
      dplyr::mutate(
        issue_type = "Demographic anomaly",
        description = "Unexpected demographic type or value",
        place = place,
        field = "demographic_type / demographic_value",
        value = paste(demographic_type, demographic_value, sep = " - "),
        impact = "Potentially incorrect or outdated submission template",
        status = "Open"
      ) |>
      dplyr::select(
        issue_type,
        description,
        place,
        field,
        value,
        impact,
        status
      )
  }
}

#' Check for missing month values in a submission
#'
#' @description
#' Identifies records where the `month` column is missing (`NA`). If no issues
#' are found, an empty issue scheme is returned
#'
#' @param df data frame of ingested metrics data
#'
#' @returns A data frame of issues (possibly empty) with fields suitable for
#' logging in the data-issues log
check_month_completeness <- function(df) {
  # define suspicious counts
  suspicious <- df |>
    dplyr::filter(is.na(month)) |>
    dplyr::distinct(place)

  # if no issues then retun empty schema
  if (nrow(suspicious) == 0) {
    return(empty_issue_schema())
  }

  # if issues detected, then respond and gather data
  if (nrow(suspicious) > 0) {
    suspicous_places <- suspicious$place |> unique() |> sort()
    cli::cli_alert_danger(
      "Missing month values detected for {.val {suspicous_places}}. Check the issues log.",
      wrap = TRUE
    )

    suspicious |>
      dplyr::mutate(
        issue_type = "Month anomaly",
        description = "NAs detected in the month column",
        place = place,
        field = "month",
        value = month,
        impact = "Month not complete or Instructions sheet missing",
        status = "Open"
      ) |>
      dplyr::select(
        issue_type,
        description,
        place,
        field,
        value,
        impact,
        status
      )
  }
}

#' Check consistency of record counts across months
#'
#' @description
#' Identifies months that have an unexpected number of records compared with
#' the modal (most common) month count. This helps detect typos or incorrectly
#' filed submission files.
#'
#' @details
#' The function groups the input data frame by `month` and counts the number of
#' records associated with each month. Months whose record count differs from
#' the maximum (i.e. the modal count) are flagged as suspicious.
#'
#' If no inconsistencies are found, the function returns an empty issue schema.
#'
#' @param df data frame of ingested metrics data
#'
#' @returns A data frame of issues (possibly empty) with fields suitable for
#' logging in the data-issues log
check_month_consistency <- function(df) {
  # define suspicious counts - months with fewer records than the modal month count
  suspicious <- df |>
    dplyr::mutate(
      counter = 1L,
      n_records = sum(counter, na.rm = TRUE),
      .by = month
    ) |>
    dplyr::distinct(place, month, n_records) |>
    dplyr::filter_out(n_records == max(n_records, na.rm = TRUE))

  # if nothing suspicious, return an empty tibble
  if (nrow(suspicious) == 0) {
    return(empty_issue_schema())
  }

  # otherwise return structured issues
  if (nrow(suspicious) > 0) {
    months_submitted <- df$month |> unique() |> sort()

    cli::cli_alert_danger(
      "Multiple months found in the submission files: {.val {months_submitted}}. Check the issues log.",
      wrap = TRUE
    )

    suspicious |>
      dplyr::mutate(
        issue_type = "Month anomaly",
        description = "More than one month in the submissions",
        place = place,
        field = "month",
        value = paste(place, month, sep = ": "),
        impact = "Month is likely to be incorrect or misfiled",
        status = "Open"
      ) |>
      dplyr::select(
        issue_type,
        description,
        place,
        field,
        value,
        impact,
        status
      )
  }
}
