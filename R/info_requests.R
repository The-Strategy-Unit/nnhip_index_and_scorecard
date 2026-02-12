# Info request 1: activity and costs by HES data ------------------------------
# Linked to GH issue #11
# This script MUST be run within the UDAL platform

## load in the utilies ----
source(here::here("R", "utils.R"))

## UDF ----
#' Calculate the year-to-date (YTD) change in activity by each site
#'
#' @param df Tibble. The UDAL dataset
#' @param df_lookup Tibble. A lookup between GP practice code and NNHIP site name. This file is called `lu_prac_nh`.
#' @param vec_censored_months Numeric vector. The month numbers to censor from the YTD calculation. Default = c(2, 3)
#' @param measure_description String. A string description of the measure being compiled.
#'
#' @returns Tibble. A summarised dataset by site with columns for 2024-25 (YTD) and 2025-26 (YTD) and the change in activity shown as absolute number, z-change and z-change outlier
#'
#' @examples
#' ytd_change_by_site(
#'   df = ecds_ae_t1_t2,
#'   df_lookup = lu_prac_nh,
#'   vec_censored_months = c(2, 3),
#'   measure_description = "A+E type 1 and 2"
#')
ytd_change_by_site <- function(
  df,
  df_lookup,
  vec_censored_months = c(2, 3),
  measure_description = "No description"
) {
  df_return <-
    df |>
    # convert month to numeric
    dplyr::mutate(der_activity_month = as.integer(der_activity_month)) |>
    # apply the lookup from practice to neighbhourhood
    dplyr::left_join(
      y = df_lookup |> dplyr::select(prac_code, nnhip_site),
      by = dplyr::join_by(x$gp_practice_code == y$prac_code)
    ) |>
    # limit to just nnhip sites
    dplyr::filter_out(is.na(nnhip_site)) |>
    # summarise by nnhip site and month
    dplyr::summarise(
      n = sum(n, na.rm = TRUE),
      .by = c(der_activity_month, nnhip_site, unit)
    ) |>
    # complete the sequence of month, site and unit
    tidyr::complete(der_activity_month, nnhip_site, unit) |>
    # work out some time dimension units
    dplyr::mutate(
      year = as.integer(der_activity_month %/% 100),
      month = as.integer(der_activity_month %% 100),
      financial_year = dplyr::if_else(
        condition = month > 3,
        true = glue::glue("{year}-{(year + 1) - 2000}"),
        false = glue::glue("{year - 1}-{year - 2000}")
      ),
      # feb & mar are censored for both years
      flag_censored = month %in% vec_censored_months
    ) |>
    # remove censored months
    dplyr::filter_out(flag_censored) |>
    # summarise the activity
    dplyr::summarise(
      n = sum(n, na.rm = TRUE),
      .by = c(nnhip_site, unit, financial_year)
    ) |>
    # pivot wider
    tidyr::pivot_wider(
      names_from = financial_year,
      names_expand = TRUE,
      values_from = n
    ) |>
    # work out the change
    dplyr::mutate(
      change = `2025-26` - `2024-25`,
      change_pct = (change / `2024-25`) * 100,
      z_change = scale(change) |> as.numeric(),
      z_change_outlier = abs(z_change) > 2, # i.e. flags sites whose change is unusually large relative to the group
      measure = measure_description
    )

  return(df_return)
}

## load files from LakeMart ----
# list the parquet files
df_list <- c(
  # practice to pcn lookup
  "gp_pcn",
  # accident and emergency
  "ecds_ae_all",
  "ecds_ae_t1_t2",
  "ecds_ae_tother",
  # outpatient
  "opa_first_fu",
  "opa_procedures",
  # admitted patient care
  "apc_elective_total",
  "apc_elective_daycase",
  "apc_elective_inpatient",
  "apc_nonelective",
  # length of stay / bed days
  "apc_average_los",
  "apc_nonelective_lossplit",
  "apc_ref_average_los"
)
## load the file from the LakeMart to objects in the environment
purrr::walk(
  .x = df_list,
  .f = \(obj_name) {
    # update message
    cli::cli_inform("Loading {.val {obj_name}}...")
    # load the file and assign to an object
    assign(
      x = obj_name,
      value = get_su_lakemart_parquet_file(
        str_file_pattern = glue::glue("{obj_name}.parquet/part")
      ) |>
        janitor::clean_names(),
      envir = .GlobalEnv
    )
  }
)

## store aggregate files for local access
purrr::walk(
  .x = df_list,
  .f = \(obj_name) {
    # check object exists
    if (!exists(obj_name, inherits = TRUE)) {
      cli::cli_abort(
        "Object {.val {obj_name}} does not exist in the environment"
      )
    }
    # ensure the dir exists
    fs::dir_create(here::here(".secret", "data", "ir1"))
    # update message
    cli::cli_inform("Saving {.val {obj_name}}")
    # save the object
    saveRDS(
      object = get(obj_name),
      file = here::here(
        ".secret",
        "data",
        "ir1",
        glue::glue("{obj_name}.Rds")
      )
    )
  }
)

# geography lookup ----

## PCN -> Neighbhourhood ----
# get the lookup between pcn and nnhip neighbourhoods
df_nnhip_geo <-
  readxl::read_xlsx(
    path = here::here(
      ".secret",
      "reference",
      "NNHIP Geographies_PCNs_LA_V5.3.xlsx"
    ),
    sheet = "PCNDetails"
  ) |>
  janitor::clean_names()

# limit to pcn and neighbourhood
lu_pcn_nh <-
  df_nnhip_geo |>
  # data quality - handle a duplicate pcn, remove the row likely to cause issues
  dplyr::filter_out(pcn_code == "U33065" & current_sub_icb_loc_code == "07P") |>
  dplyr::select(pcn_code, nnhip_site) |>
  dplyr::distinct()

# check for duplicates
# lu_pcn_nh |>
#   dplyr::summarise(any_duplicated = any(duplicated(pcn_code)))

# lu_pcn_nh |> dplyr::count(pcn_code) |> dplyr::filter(n > 1)
# pcn_code U33065 is duplicated twice

## Practice -> PCN ----
# the data from UDAL appears to have lots of duplicate practices per PCN
# # get a clean lookup from practice to pcn
# lu_prac_pcn <-
#   gp_pcn |>
#   dplyr::mutate(
#     # replace 'U' in pcn_code with 'NA' to avoid issues with joins
#     pcn_code = dplyr::replace_values(
#       x = pcn_code,
#       "U" ~ NA_character_
#     )
#   ) |>
#   dplyr::distinct()

# # check for duplicate gp practices
# lu_prac_pcn |> dplyr::summarise(any_duplicated = any(duplicated(gp_code)))

# # list the duplicates
# dupes <- lu_prac_pcn |> dplyr::count(gp_code) |> dplyr::filter(n > 1)

# loading a file downloaded from ODS
lu_prac_pcn <-
  readxl::read_xlsx(
    path = here::here(".secret", "reference", "epcn.xlsx"),
    sheet = "PCN Core Partner Details"
  ) |>
  janitor::clean_names() |>
  # keep only active relationships
  dplyr::filter(is.na(practice_to_pcn_relationship_end_date)) |>
  dplyr::select(pcn_code, partner_organisation_code) |>
  dplyr::distinct() |>
  dplyr::rename(prac_code = partner_organisation_code)

# # check for duplicates
# lu_prac_pcn |> dplyr::summarise(any_duplicated = any(duplicated(prac_code)))

# # list the duplicates
# dupes <- lu_prac_pcn |> dplyr::count(prac_code) |> dplyr::filter(n > 1)

# get a lookup from practice to neighbhourhood
lu_prac_nh <-
  lu_prac_pcn |>
  dplyr::left_join(
    y = lu_pcn_nh,
    by = dplyr::join_by(x$pcn_code == y$pcn_code)
  )

# A&E ----

## Type 1 and 2 A&E ----
df_ae_t1_t2 <-
  ytd_change_by_site(
    df = ecds_ae_t1_t2,
    df_lookup = lu_prac_nh,
    vec_censored_months = c(2, 3),
    measure_description = "A+E type 1 and 2"
  )

## Other types ----
df_ae_other <-
  ytd_change_by_site(
    df = ecds_ae_tother,
    df_lookup = lu_prac_nh,
    vec_censored_months = c(2, 3),
    measure_description = "A+E type other"
  )

## Total
df_ae_total <-
  ytd_change_by_site(
    df = ecds_ae_all,
    df_lookup = lu_prac_nh,
    vec_censored_months = c(2, 3),
    measure_description = "A+E total"
  )
