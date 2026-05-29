# --- Submission view (Places) ------------------------------------------------

# ui ----
mod_place_submission_ui <- function(id) {
  # set up namespacing
  ns <- shiny::NS(id)

  # define the UI
  bslib::nav_panel(
    value = "submission_view",
    title = shiny::span(
      bsicons::bs_icon("table"),
      "Submission"
    ) |>
      bslib::tooltip(
        "View of the underlying submitted data, formatted like the original template to support debugging and validation.",
        options = list(trigger = "hover")
      ),
    bslib::layout_sidebar(
      fillable = TRUE,
      open = FALSE,
      sidebar = bslib::sidebar(
        open = TRUE,
        width = "400px",
        shiny::includeMarkdown("descriptions/place_submission_view.md")
      ),
      bslib::card_body(
        gt::gt_output(ns("submission_table"))
      )
    )
  )
}

# server ----
mod_place_submission_server <- function(id, df, place, month, pin_version) {
  shiny::moduleServer(id, function(input, output, session) {
    # cache data for place:month for improved UX ----
    df_submission <- shiny::reactive({
      req(df(), pin_version(), place(), month())

      get_data_for_submission_view(
        df = df(),
        selected_place = place(),
        selected_month = month()
      )
    }) |>
      shiny::bindCache(
        pin_version(),
        place(),
        month()
      )

    # render the table (cached)----
    output$submission_table <- gt::render_gt({
      req(df_submission(), place(), month())
      display_submission_view_table(
        df = df_submission(),
        selected_place = place(),
        selected_month = month()
      )
    }) |>
      shiny::bindCache(
        pin_version(),
        place(),
        month()
      )
  })
}
