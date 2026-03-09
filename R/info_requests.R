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
      by = dplyr::join_by(x$gp_practice_code == y$prac_code),
      # NB, expecting m-m relationship because some PCNs are associated with
      # more than one NNHIP place
      relationship = "many-to-many"
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
      change_pct = (change / `2024-25`),
      # z_change = scale(change_pct) |> as.numeric(), # standardise the percentage change
      # z_change_outlier = abs(z_change) > 1.5, # i.e. flags sites whose change is unusually large relative to the group
      measure = measure_description
    ) |>
    dplyr::relocate(measure, .before = unit)

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

## load files from local store
purrr::walk(
  .x = df_list,
  .f = \(obj_name) {
    # construct the expected file path
    obj_path <- here::here(".secret", "data", "ir1", paste0(obj_name, ".Rds"))

    # check object exists
    if (!fs::file_exists(path = obj_path)) {
      cli::cli_abort(
        "Object {.val {obj_name}} does not exist in the folder"
      )
    }

    # load the file and assign to an object
    assign(
      x = obj_name,
      value = readRDS(file = obj_path),
      envir = .GlobalEnv
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
      "NNHIP Geographies_PCNs_LA_V5.31.xlsx"
    ),
    sheet = "PCNDetails"
  ) |>
  janitor::clean_names()

# some manual adjustments based on the nnhip file
df_nnhip_geo_v2 <-
  # need to duplicate some pcn -> nnhip sites as they're part of both
  dplyr::bind_rows(
    df_nnhip_geo,
    df_nnhip_geo |>
      # these PCNs belong to Hillingdon AND Kensignton & Chelse Westiminster
      dplyr::filter(pcn_code %in% c("U35513", "U91930", "U07392")) |>
      dplyr::mutate(nnhip_site = additional_nnhip_site_if_two_or_more)
  )

# limit to pcn and neighbourhood
lu_pcn_nh <-
  df_nnhip_geo_v2 |>
  dplyr::select(pcn_code, nnhip_site) |>
  dplyr::distinct()

# NB, duplicates are now present because three PCNs are part of multiple places

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
  dplyr::rename(prac_code = partner_organisation_code) |>
  # remove practices from PCN relationships where the NNHIP_Geographies excel file indicates
  # they are not part of places
  dplyr::filter_out(
    prac_code %in%
      c(
        "P88005", # Family Surgery
        "P88600", # The Surgery 1
        "P88610", # South Reddish Medical Ctr 2
        "P88034", # Cale Green Surgery
        "Y00912" # Dr H Lloyd's Practice
      )
  )

# # check for duplicates
# lu_prac_pcn |> dplyr::summarise(any_duplicated = any(duplicated(prac_code)))

# # list the duplicates
# dupes <- lu_prac_pcn |> dplyr::count(prac_code) |> dplyr::filter(n > 1)

# get a lookup from practice to neighbhourhood
lu_prac_nh <-
  lu_prac_pcn |>
  dplyr::left_join(
    y = lu_pcn_nh,
    by = dplyr::join_by(x$pcn_code == y$pcn_code),
    relationship = "many-to-many"
  )

# check for duplicates (these are expected because some PCNs have multiple NNHIP places)
dupes <- lu_prac_nh |> dplyr::count(prac_code) |> dplyr::filter(n > 1)
# manual review and checking with ODS service:
# all duplicate practices are associated with the three PCNs in question
# all practices are duplicated twice (as expected)

# A&E ----

# Type 1 and 2 A&E
df_ae_t1_t2 <-
  ytd_change_by_site(
    df = ecds_ae_t1_t2,
    df_lookup = lu_prac_nh,
    vec_censored_months = c(2, 3),
    measure_description = "A+E type 1 and 2"
  )

# Other types
df_ae_other <-
  ytd_change_by_site(
    df = ecds_ae_tother,
    df_lookup = lu_prac_nh,
    vec_censored_months = c(2, 3),
    measure_description = "A+E type other"
  )

# Total
df_ae_total <-
  ytd_change_by_site(
    df = ecds_ae_all,
    df_lookup = lu_prac_nh,
    vec_censored_months = c(2, 3),
    measure_description = "A+E total"
  )

# Outpatient -----

# First appointments
df_op_first <-
  ytd_change_by_site(
    df = opa_first_fu |> dplyr::filter(der_appointment_type == "New"),
    df_lookup = lu_prac_nh,
    vec_censored_months = c(2, 3),
    measure_description = "OP - First appointments"
  )

# Follow-up appointments
df_op_followup <-
  ytd_change_by_site(
    df = opa_first_fu |> dplyr::filter(der_appointment_type == "FUp"),
    df_lookup = lu_prac_nh,
    vec_censored_months = c(2, 3),
    measure_description = "OP - Follow-up appointments"
  )

# Procedures
df_op_procedure <-
  ytd_change_by_site(
    df = opa_procedures,
    df_lookup = lu_prac_nh,
    vec_censored_months = c(2, 3),
    measure_description = "OP - Procedures"
  )

# Admitted patient care -----

# elective daycase
df_apc_elective_daycase <-
  ytd_change_by_site(
    df = apc_elective_daycase,
    df_lookup = lu_prac_nh,
    vec_censored_months = c(2, 3),
    measure_description = "Elective day case"
  )

# elective inpatients
df_apc_elective_inpatient <-
  ytd_change_by_site(
    df = apc_elective_inpatient,
    df_lookup = lu_prac_nh,
    vec_censored_months = c(2, 3),
    measure_description = "Elective inpatient"
  )

# elective total
df_apc_elective_total <-
  ytd_change_by_site(
    df = apc_elective_total,
    df_lookup = lu_prac_nh,
    vec_censored_months = c(2, 3),
    measure_description = "Elective total"
  )

# non-elective
df_apc_non_elective <-
  ytd_change_by_site(
    df = apc_nonelective,
    df_lookup = lu_prac_nh,
    vec_censored_months = c(2, 3),
    measure_description = "Non-elective"
  )


# non-elective - split by LoS category
df_apc_non_elective_lossplit <-
  apc_nonelective_lossplit |>
  # tidy up the los name
  dplyr::rename(los_category = lo_s_category) |>
  # convert month to numeric
  dplyr::mutate(der_activity_month = as.integer(der_activity_month)) |>
  # apply the lookup from practice to neighbourhood
  dplyr::left_join(
    y = lu_prac_nh |> dplyr::select(prac_code, nnhip_site),
    by = dplyr::join_by(x$gp_practice_code == y$prac_code),
    # NB, expecting a m-m relationship because some PCNs are associated with
    # more than one NNHIP place
    relationship = "many-to-many"
  ) |>
  # limit to just nnhip sites
  dplyr::filter_out(is.na(nnhip_site)) |>
  # summarise by nnhip site and month and los_category
  dplyr::summarise(
    n = sum(n, na.rm = TRUE),
    .by = c(der_activity_month, nnhip_site, unit, los_category)
  ) |>
  # complete the sequence of month, site, unit and category
  tidyr::complete(der_activity_month, nnhip_site, unit, los_category) |>
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
    flag_censored = month %in% c(2, 3)
  ) |>
  # remove censored months
  dplyr::filter_out(flag_censored) |>
  # summarise the activity
  dplyr::summarise(
    n = sum(n, na.rm = TRUE),
    .by = c(nnhip_site, unit, financial_year, los_category)
  ) |>
  # add in the total spell count per year for each nnhip site (the denominator)
  dplyr::mutate(
    N = sum(n, na.rm = TRUE),
    .by = c(nnhip_site, financial_year)
  ) |>
  # work out the percentage split
  dplyr::mutate(pct = n / N) |>
  # drop the numerator & denominator
  dplyr::select(-c(n, N)) |>
  # pivot wider by year
  tidyr::pivot_wider(
    names_from = financial_year,
    names_expand = TRUE,
    values_from = pct
  ) |>
  # work out the change
  dplyr::mutate(
    change = `2025-26` - `2024-25`,
    change_pct = (change / `2024-25`),
    measure = "LoS category prop change",
    unit = "Spells (percent)"
  ) |>
  dplyr::relocate(measure, .before = unit)

# ALoS ----
df_apc_alos <-
  apc_average_los |>
  # tidy up the los name
  dplyr::rename(los = lo_s) |>
  # convert month to numeric
  dplyr::mutate(der_activity_month = as.integer(der_activity_month)) |>
  # apply the lookup from practice to neighbourhood
  dplyr::left_join(
    y = lu_prac_nh |> dplyr::select(prac_code, nnhip_site),
    by = dplyr::join_by(x$gp_practice_code == y$prac_code),
    # NB, expecting a m-m relationship because some PCNs are associated
    # with more than one NNHIP place
    relationship = "many-to-many"
  ) |>
  # limit to just nnhip sites
  dplyr::filter_out(is.na(nnhip_site)) |>
  # summarise by nnhip site and month and admission type
  dplyr::summarise(
    n = sum(n, na.rm = TRUE),
    los = sum(los, na.rm = TRUE),
    .by = c(
      der_activity_month,
      nnhip_site,
      n_unit,
      los_unit,
      calc_admission_type
    )
  ) |>
  # complete the sequence of month, site, units and admission type
  tidyr::complete(
    der_activity_month,
    nnhip_site,
    n_unit,
    los_unit,
    calc_admission_type
  ) |>
  # work out some time dimentions units
  dplyr::mutate(
    year = as.integer(der_activity_month %/% 100),
    month = as.integer(der_activity_month %% 100),
    financial_year = dplyr::if_else(
      condition = month > 3,
      true = glue::glue("{year}-{(year + 1) - 2000}"),
      false = glue::glue("{year - 1}-{year - 2000}")
    ),
    # feb & mar are censored for both years
    flag_censored = month %in% c(2, 3)
  ) |>
  # remove censored months
  dplyr::filter_out(flag_censored) |>
  # summarise the activity
  dplyr::summarise(
    n = sum(n, na.rm = TRUE),
    los = sum(los, na.rm = TRUE),
    .by = c(nnhip_site, n_unit, los_unit, calc_admission_type, financial_year)
  ) |>
  # work out the average length of stay
  dplyr::mutate(
    alos = los / n,
    # alos_unit = "Days per Spell"
  ) |>
  # drop the numerator & denominator
  dplyr::select(-c(n, n_unit, los, los_unit)) |>
  # pivot wider by year
  tidyr::pivot_wider(
    names_from = financial_year,
    names_expand = TRUE,
    values_from = alos
  ) |>
  # work out the change
  dplyr::mutate(
    change = `2025-26` - `2024-25`,
    change_pct = (change / `2024-25`),
    measure = "Average length of stay",
    unit = "Days per spell"
  ) |>
  dplyr::relocate(measure, .after = nnhip_site) |>
  dplyr::relocate(unit, .after = measure)

# get just the electives
df_apc_alos_elective <-
  df_apc_alos |>
  dplyr::filter(calc_admission_type == "Elective") |>
  dplyr::select(-calc_admission_type)

# get just the non-electives
df_apc_alos_non_elective <-
  df_apc_alos |>
  dplyr::filter(calc_admission_type == "Non-Elective") |>
  dplyr::select(-calc_admission_type)

# Bed days based on SuS average LoS ----

# # work out the 'average LoS' across England (i.e. not just NNHIP) ----
# df_average_los <-
#   apc_ref_average_los |>
#   # tidy up the los name
#   dplyr::rename(los = lo_s) |>
#   # convert month to numeric
#   dplyr::mutate(der_activity_month = as.integer(der_activity_month)) |>
#   # work out some time dimension units
#   dplyr::mutate(
#     year = as.integer(der_activity_month %/% 100),
#     month = as.integer(der_activity_month %% 100),
#     financial_year = dplyr::if_else(
#       condition = month > 3,
#       true = glue::glue("{year}-{(year + 1) - 2000}"),
#       false = glue::glue("{year - 1}-{year - 2000}")
#     ),
#     # feb & mar are censored for both years
#     flag_censored = month %in% c(2, 3)
#   ) |>
#   # remove censored months
#   dplyr::filter_out(flag_censored) |>
#   # summarise the activity
#   dplyr::summarise(
#     n = sum(n, na.rm = TRUE),
#     los = sum(los, na.rm = TRUE),
#     .by = c(financial_year, calc_admission_type)
#   ) |>
#   # calculate the average los
#   dplyr::mutate(alos = los / n)

# version 2:
# work out the 'average los' for each place and financial year and type
df_average_los <-
  apc_average_los |>
  # tidy up the los name
  dplyr::rename(los = lo_s) |>
  # convert month to numeric
  dplyr::mutate(der_activity_month = as.integer(der_activity_month)) |>
  # apply the lookup from practice to neighbourhood
  dplyr::left_join(
    y = lu_prac_nh |> dplyr::select(prac_code, nnhip_site),
    by = dplyr::join_by(x$gp_practice_code == y$prac_code),
    # NB, expecting a m-m relationship because some PCNs are associated with
    # more than one NNHIP place
    relationship = "many-to-many"
  ) |>
  # limit to just nnhip sites
  dplyr::filter_out(is.na(nnhip_site)) |>
  # summarise by nnhip site and month and calc_admission_type
  dplyr::summarise(
    n = sum(n, na.rm = TRUE),
    los = sum(los, na.rm = TRUE),
    .by = c(der_activity_month, nnhip_site, calc_admission_type)
  ) |>
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
    flag_censored = month %in% c(2, 3)
  ) |>
  # remove censored months
  dplyr::filter_out(flag_censored) |>
  # summarise the activity
  dplyr::summarise(
    n = sum(n, na.rm = TRUE),
    los = sum(los, na.rm = TRUE),
    .by = c(nnhip_site, financial_year, calc_admission_type)
  ) |>
  # calculate alos
  dplyr::mutate(
    alos = los / n,
    unit = "Days per spell"
  )

# work out the bed days (Spells x ALoS) ----
# elective (start with the pre-calculated df_apc_elective_total)
df_apc_beddays_elective <-
  df_apc_elective_total |>
  # drop the change, change_pct
  dplyr::select(-c(change, change_pct)) |>
  # pivot longer to get financial years as rows
  tidyr::pivot_longer(
    cols = c(`2024-25`, `2025-26`),
    names_to = "financial_year",
    values_to = "n"
  ) |>
  # join alos based on year
  dplyr::left_join(
    y = df_average_los |>
      dplyr::filter(calc_admission_type == "Elective") |>
      dplyr::select(nnhip_site, financial_year, alos),
    by = dplyr::join_by(
      x$nnhip_site == y$nnhip_site,
      x$financial_year == y$financial_year
    )
  ) |>
  # work out the bed days
  dplyr::mutate(
    bed_days = n * alos,
    unit = "Days"
  ) |>
  # drop the numerator and denominator
  dplyr::select(-c(n, alos)) |>
  # pivot wider by year
  tidyr::pivot_wider(
    names_from = financial_year,
    names_expand = TRUE,
    values_from = bed_days
  ) |>
  # work out the change
  dplyr::mutate(
    change = `2025-26` - `2024-25`,
    change_pct = (change / `2024-25`),
    measure = "Bed days (Spells x ALoS)",
    unit = "Days"
  )

# non-elective (start with the pre-calculated df_apc_non_elective_total)
df_apc_beddays_non_elective <-
  df_apc_non_elective |>
  # drop the change, change_pct
  dplyr::select(-c(change, change_pct)) |>
  # pivot longer to get financial years as rows
  tidyr::pivot_longer(
    cols = c(`2024-25`, `2025-26`),
    names_to = "financial_year",
    values_to = "n"
  ) |>
  # join alos based on year
  dplyr::left_join(
    y = df_average_los |>
      dplyr::filter(calc_admission_type == "Non-Elective") |>
      dplyr::select(nnhip_site, financial_year, alos),
    by = dplyr::join_by(
      x$nnhip_site == y$nnhip_site,
      x$financial_year == y$financial_year
    )
  ) |>
  # work out the bed days
  dplyr::mutate(
    bed_days = n * alos,
    unit = "Days"
  ) |>
  # drop the numerator and denominator
  dplyr::select(-c(n, alos)) |>
  # pivot wider by year
  tidyr::pivot_wider(
    names_from = financial_year,
    names_expand = TRUE,
    values_from = bed_days
  ) |>
  # work out the change
  dplyr::mutate(
    change = `2025-26` - `2024-25`,
    change_pct = (change / `2024-25`),
    measure = "Bed days (Spells x ALoS)",
    unit = "Days"
  )


# write the data -------
#' Write tibble to an Excel workbook
#'
#' @param wb Object. Reference to an {openxl} workbook object
#' @param df Tibble. The data to be written to the workbook
#' @param sheet Character. The name of the sheet to use for this data
#'
#' @returns NA. Modifies the `wb` object in the global environment.
write_data_to_xl <- function(wb, df, sheet) {
  openxlsx::addWorksheet(wb = wb, sheetName = sheet)
  openxlsx::writeDataTable(wb = wb, sheet = sheet, x = df)
}

# write to an Excel file
wb <- openxlsx::createWorkbook()
# A+E
write_data_to_xl(wb = wb, df = df_ae_t1_t2, sheet = "ae_t1_t2")
write_data_to_xl(wb = wb, df = df_ae_other, sheet = "ae_other")
write_data_to_xl(wb = wb, df = df_ae_total, sheet = "ae_total")
# OP
write_data_to_xl(wb = wb, df = df_op_first, sheet = "op_first")
write_data_to_xl(wb = wb, df = df_op_followup, sheet = "op_followup")
write_data_to_xl(wb = wb, df = df_op_procedure, sheet = "op_procedure")
# APC
write_data_to_xl(
  wb = wb,
  df = df_apc_elective_daycase,
  sheet = "apc_elective_daycase"
)
write_data_to_xl(
  wb = wb,
  df = df_apc_elective_inpatient,
  sheet = "apc_elective_inpatient"
)
write_data_to_xl(
  wb = wb,
  df = df_apc_elective_total,
  sheet = "apc_elective_total"
)
write_data_to_xl(wb = wb, df = df_apc_non_elective, sheet = "apc_non_elective")
write_data_to_xl(
  wb = wb,
  df = df_apc_non_elective_lossplit,
  sheet = "apc_non_elective_lossplit"
)
write_data_to_xl(
  wb = wb,
  df = df_apc_alos_elective,
  sheet = "apc_alos_elective"
)
write_data_to_xl(
  wb = wb,
  df = df_apc_alos_non_elective,
  sheet = "apc_alos_non_elective"
)
write_data_to_xl(
  wb = wb,
  df = df_apc_beddays_elective,
  sheet = "apc_beddays_elective"
)
write_data_to_xl(
  wb = wb,
  df = df_apc_beddays_non_elective,
  sheet = "apc_beddays_non_elective"
)
# save the workbook
openxlsx::saveWorkbook(
  wb = wb,
  file = here::here(".secret", "data", "ir1", "nnhip_v1.xlsx"),
  overwrite = TRUE
)
