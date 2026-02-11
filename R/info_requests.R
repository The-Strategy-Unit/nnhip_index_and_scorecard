# Info request 1: activity and costs by HES data ------------------------------
# Linked to GH issue #11
# This script MUST be run within the UDAL platform

## load in the utilies ----
source(here::here("R", "utils.R"))

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
  .f = \(.object_name) {
    # update message
    cli::cli_inform("Loading {.val {.object_name}}...")
    # load the file and assign to an object
    assign(
      x = .object_name,
      value = get_su_lakemart_parquet_file(
        str_file_pattern = glue::glue("{.object_name}.parquet/part")
      ),
      envir = .GlobalEnv
    )
  }
)

## store aggregate files for local access
purrr::walk(
  .x = df_list,
  .f = \(.object_name) {
    # check object exists
    if (!exists(.object_name, inherits = TRUE)) {
      cli::cli_abort(
        "Object {.val {.object_name}} does not exist in the environment"
      )
    }
    # ensure the dir exists
    fs::dir_create(here::here(".secret", "data", "ir1"))
    # update message
    cli::cli_inform("Saving {.val {.object_name}}")
    # save the object
    saveRDS(
      object = get(.object_name),
      file = here::here(
        ".secret",
        "data",
        "ir1",
        glue::glue("{.object_name}.Rds")
      )
    )
  }
)
