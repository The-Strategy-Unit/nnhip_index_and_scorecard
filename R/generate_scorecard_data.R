# -----------------------------------------------------------------------------
# Synthetic aggregate data generator for NNHIP metrics
# Generates tidy long-format data for any month and all places
# -----------------------------------------------------------------------------

# Reference lists -------------------------------------------------------------
places <- c(
  "Fenland and Peterborough within the North Cambridgeshire and Peterborough Care Partnership",
  "Ipswich and East Suffolk",
  "North East Essex",
  "South and West Hertfordshire (Dacorum and Hertsmere)",
  "West Essex",
  "West Suffolk",
  "Barking and Dagenham",
  "Croydon",
  "Hillingdon",
  "Kensington, Chelsea and Westminster (Bi-Borough)",
  "Lambeth and Southwark",
  "Coventry",
  "East Birmingham",
  "Herefordshire",
  "Leicestershire (West)",
  "Nottingham City",
  "Shropshire",
  "Solihull",
  "Walsall",
  "Bradford and Craven (Bradford South, Keighley and Airedale)",
  "Doncaster",
  "Leeds (Hatch, South, East)",
  "North East Lincolnshire",
  "Rotherham",
  "Stockton",
  "Sunderland",
  "Wakefield",
  "Blackburn and Darwen",
  "Morecambe Bay",
  "Rochdale",
  "Sefton",
  "St Helens",
  "Stockport",
  "Buckinghamshire (North, High Wycombe, Marlow Beaconsfield)",
  "East Berkshire and Slough",
  "East Kent",
  "East Surrey (Surrey Downs)",
  "East Sussex (Hastings and Rother)",
  "Portsmouth",
  "Bristol (South Bristol)",
  "Cornwall and The Isles Of Scilly",
  "Dorset Place",
  "Woodspring"
)

age_groups <- c(
  "18-19",
  "20-29",
  "30-39",
  "40-49",
  "50-59",
  "60-69",
  "70-79",
  "80+",
  "Not known"
)

ethnic_groups <- c("White", "Black", "Asian", "Mixed", "Other", "Not known")

deprivation_groups <- c("1", "2", "3", "4", "5", "Not known")

metrics <- tibble::tribble(
  ~metric_id , ~metric_type , ~metric_details                                                                         ,
  "O1"       , "Outcome"    , "Number of new consultant-led outpatient appointments for patients in the cohort"       ,
  "O1"       , "Outcome"    , "Number of follow up consultant-led outpatient appointments for patients in the cohort" ,
  "O1"       , "Outcome"    , "Number of consultant-led outpatient procedures for patients in the cohort"             ,
  "O2"       , "Outcome"    , "Number of specific acute non-elective spells with length of stay 0 days"               ,
  "O2"       , "Outcome"    , "Number of specific acute non-elective spells with length of stay 1+ days"              ,
  "O3"       , "Outcome"    , "Number of inpatient bed days per 1,000 patients in the cohort"                         ,
  "O4"       , "Outcome"    , "Number of category 1 ambulance conveyances"                                            ,
  "O4"       , "Outcome"    , "Number of category 2 ambulance conveyances"                                            ,
  "O4"       , "Outcome"    , "Number of category 3 ambulance conveyances"                                            ,
  "O4"       , "Outcome"    , "Number of category 4 ambulance conveyances"                                            ,
  "O5"       , "Outcome"    , "Number of A&E attendances at a type 1 department"                                      ,
  "O5"       , "Outcome"    , "Number of A&E attendances at an other type department"                                 ,
  "O6"       , "Outcome"    , "Number of appointments in general practice and Primary Care Networks"                  ,
  "O6"       , "Outcome"    , "Number of Community Care Contacts attended"                                            ,
  "P1"       , "Process"    , "Number of patients on the NNHIP caseload"
)

# 2. Helper: generate cohort size per place -----------------------------------

#' Generate a synthetic cohort size for a place
#'
#' @description
#' This helper function assigns a synthetic cohort to a Place based on simple
#' population heuristics. Larger urban areas receive higher expected cohort
#' sizes, while rural or coastal areas receive smaller ones. Random noise is
#' added to introduce naturation variation.
#'
#' @details
#' The function uses pattern matching on the Place name to assign a baseline
#' mean cohort size, then draws from a normal distribution with a Place-
#' specific standard deviation. The result is rounded to the nearest whole
#' number.
#'
#' @param place A character string giving the name of a Place
#'
#' @returns An integer representing the synthetic cohort size for the Place.
#'
#' @examples
#' generate_cohort("Leeds (Hatch, South, East)")
#' generate_cohort("Cornwall and The Isles Of Scilly")
#'
#' @keywords internal
#' @noRd
generate_cohort <- function(place) {
  # Assign typical cohort sizes by rough population profile
  if (stringr::str_detect(place, "London")) {
    rnorm(1, mean = 2500, sd = 200)
  } else if (stringr::str_detect(place, "Bristol|Portsmouth|Leeds|Bradford")) {
    rnorm(1, mean = 2200, sd = 180)
  } else if (
    stringr::str_detect(place, "Bay|Cornwall|Dorset|Herefordshire|Shropshire")
  ) {
    rnorm(1, mean = 1400, sd = 150)
  } else {
    {
      rnorm(1, mean = 1800, sd = 160)
    } |>
      round()
  }
}

# 3. Helper: distribute cohort across breakdowns ------------------------------

#' Randomly split a total count across a set of groups
#'
#' @description
#' This helper function distributes a total number of patients across a set of categories (e.g., age, ethnicity, deprivation) using a simple random proportional allocation. The resulting vector always sums to the original total, with natural variation introduced by random weights.
#'
#' @details
#' The functions generates a random weight for each group using `runif()`, normalises the weights to sum to 1 and multiplies them by the total. The results are rounded to whole numbers. Because rounding can introduce small discrepancies, the final element is adjusted to ensure the vector sums exactly to `total`.
#'
#' This approach is intentionally simple and is designed for synthetic data generateion rather than statistical modelling.
#'
#' @param total Integer. The total number of patients to be distributed across groups
#' @param groups Character vector of group names. The length of this vector determines how many values are returned.
#'
#' @returns An integer vector of the same length as `groups`, containing the allocated patient counts. The values sum exactly to `total`
#'
#' @examples
#' random_split(1000, c("18-39", "40-64", "65+"))
#' random_split(500, c("White", "Black", "Asian", "Mixed", "Other"))
#'
#' @keywords internal
#' @noRd
random_split <- function(total, groups) {
  w <- runif(length(groups))
  p <- w / sum(w)
  round(total * p)
}

# 4. Helper: generate counts + rates ------------------------------------------

#' Generate synthetic count and rate values for a metric
#'
#' @description
#' This helper function produces a synthetic count and rate per 1,000 patients
#' for a given metric and patient denominator. Each metric type has a baseline
#' rate, and random variation is applied to create realistic differences across
#' places and breakdown categories.
#'
#' @details
#' Baseline rates are determined by pattern matching on the metric description.
#' A randome multiplier introduces natural variation. Counts are derived from
#' the rate and patient denominator.
#'
#' @param patients Numeric. The number of patients in the breakdown group
#' @param metric_details Character string describing the metric
#'
#' @returns A list with two elements:
#' \describe{
#'   \item{count}{The synthetic count for the metric}
#'   \item{rate}{The synthetic rate per 1,000 patients}
#' }
#'
#' @examples
#' generate_metric_values(1200, "Number of new consultant-led outpatient appointments")
#'
#' @keywords internal
#' @noRd
generate_metric_values <- function(patients, metric_details) {
  # Baseline rates vary by metric
  base_rate <- dplyr::case_when(
    stringr::str_detect(metric_details, "new consultant") ~ 450,
    stringr::str_detect(metric_details, "follow up") ~ 850,
    stringr::str_detect(metric_details, "procedures") ~ 110,
    stringr::str_detect(metric_details, "0 days") ~ 70,
    stringr::str_detect(metric_details, "1\\+ days") ~ 80,
    stringr::str_detect(metric_details, "bed days") ~ 75,
    stringr::str_detect(metric_details, "category 1") ~ 150,
    stringr::str_detect(metric_details, "category 2") ~ 170,
    stringr::str_detect(metric_details, "category 3") ~ 125,
    stringr::str_detect(metric_details, "category 4") ~ 115,
    stringr::str_detect(metric_details, "type 1 department") ~ 125,
    stringr::str_detect(metric_details, "other type department") ~ 155,
    stringr::str_detect(metric_details, "general practice") ~ 145,
    stringr::str_detect(metric_details, "Community Care") ~ 660,
    stringr::str_detect(metric_details, "caseload") ~ 1,
    TRUE ~ 100
  )

  # Add noise
  rate <- base_rate * runif(1, 0.9, 1.1)
  count <- round(patients * rate / 1000)

  # return a list of results
  list(count = count, rate = rate)
}

# 5. Main generator for one month ---------------------------------------------

#' Generate a synthetic NNHIP aggregate data for a single month
#'
#' @description
#' This function generate a complete tidy dataset for all palces, all metrics,
#' all breakdown dimensions and all measure types for a specified month. The
#' output is suitable for dashboards, scorecards, modelling and testing data
#' pipelines.
#'
#' @param month A character string in `"YYYY-MM"` format specifying the month to generate.
#'
#' @returns A tibble containing the synthetic aggregate data with the following columns:
#' \describe{
#'   \item{place}{Place name}
#'   \item{month}{The month of data}
#'   \item{metric_id}{Short metric identifier (e.g., `"O1"`, `"P1"`)}
#'   \item{metric_type}{Outcome or Process}
#'   \item{metric_details}{Full metric description}
#'   \item{measure}{One of `"patients"`, `"count"`, `"rate_per_1000"`}
#'   \item{breakdown_dimension}{Breakdown dimension (Total, Age, Ethnicity, Deprivation)}
#'   \item{breadown_category}{Category within the dimension}
#'   \item{value}{Synthetic numeric value}
#' }
#'
#' @details
#' The function:
#' \itemize{
#'   \item assigns a synthetic cohort size to each place
#'   \item splits the cohort across age, ethnicity and deprivation groups
#'   \item generates synthetic counts and rates for each metric
#'   \item constructs a tidy long-format dataset with one row per metric x place x breakdown x measure
#' }
#'
#' The resulting dataset typically contains ~37,0000 rows for a single month.
#'
#' @examples
#' \dontrun{
#' jan26 <- generate_month("2026-01")
#' }
#'
#' @export
generate_month <- function(month) {
  purrr::map_dfr(places, function(place) {
    cohort_total <- generate_cohort(place)

    # Breakdown distributions
    age_split <- random_split(cohort_total, age_groups)
    eth_split <- random_split(cohort_total, ethnic_groups)
    dep_split <- random_split(cohort_total, deprivation_groups)

    # Build tidy rows
    purrr::map_dfr(1:nrow(metrics), function(i) {
      m <- metrics[i, ]

      # For each breakdown dimension
      dplyr::bind_rows(
        # Total
        tibble::tibble(
          place = place,
          month = month,
          metric_id = m$metric_id,
          metric_type = m$metric_type,
          metric_details = m$metric_details,
          measure = c("patients", "count", "rate_per_1000"),
          breakdown_dimension = "Total",
          breakdown_category = "All",
          value = {
            vals <- generate_metric_values(cohort_total, m$metric_details)
            c(cohort_total, vals$count, vals$rate)
          }
        ),

        # Age
        {
          patients_vec <- age_split
          count_vec <- purrr::map_dbl(
            age_split,
            ~ generate_metric_values(.x, m$metric_details)$count
          )
          rate_vec <- purrr::map_dbl(
            age_split,
            ~ generate_metric_values(.x, m$metric_details)$rate
          )

          tibble::tibble(
            place = place,
            month = month,
            metric_id = m$metric_id,
            metric_type = m$metric_type,
            metric_details = m$metric_details,
            measure = rep(
              c("patients", "count", "rate_per_1000"),
              each = length(age_groups)
            ),
            breakdown_dimension = "Age",
            breakdown_category = rep(age_groups, times = 3),
            value = c(patients_vec, count_vec, rate_vec)
          )
        },

        # Ethnicity
        {
          patients_vec <- dep_split
          count_vec <- purrr::map_dbl(
            eth_split,
            ~ generate_metric_values(.x, m$metric_details)$count
          )
          rate_vec <- purrr::map_dbl(
            eth_split,
            ~ generate_metric_values(.x, m$metric_details)$rate
          )

          tibble::tibble(
            place = place,
            month = month,
            metric_id = m$metric_id,
            metric_type = m$metric_type,
            metric_details = m$metric_details,
            measure = rep(
              c("patients", "count", "rate_per_1000"),
              each = length(ethnic_groups)
            ),
            breakdown_dimension = "Ethnicity",
            breakdown_category = rep(ethnic_groups, times = 3),
            value = c(patients_vec, count_vec, rate_vec)
          )
        },

        # Deprivation
        {
          patients_vec <- dep_split
          count_vec <- purrr::map_dbl(
            dep_split,
            ~ generate_metric_values(.x, m$metric_details)$count
          )
          rate_vec <- purrr::map_dbl(
            dep_split,
            ~ generate_metric_values(.x, m$metric_details)$rate
          )

          tibble::tibble(
            place = place,
            month = month,
            metric_id = m$metric_id,
            metric_type = m$metric_type,
            metric_details = m$metric_details,
            measure = rep(
              c("patients", "count", "rate_per_1000"),
              each = length(deprivation_groups)
            ),
            breakdown_dimension = "Deprivation",
            breakdown_category = rep(deprivation_groups, times = 3),
            value = c(patients_vec, count_vec, rate_vec)
          )
        }
      )
    })
  })
}
