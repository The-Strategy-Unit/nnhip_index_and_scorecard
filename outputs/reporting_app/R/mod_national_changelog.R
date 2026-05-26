# --- Changelog (National) ----------------------------------------------------

# ui ----
mod_national_changelog_ui <- function(id) {
  # set up namespacing
  ns <- shiny::NS(id)

  # define the UI
  bslib::nav_panel(
    value = "change_log",
    title = shiny::span(
      bsicons::bs_icon("journal-text"),
      "Change log"
    ) |>
      bslib::tooltip(
        "Record of issues, observations and data-ingestion changes to support learning and transparency.",
        options = list(trigger = "hover")
      ),
    bslib::layout_sidebar(
      fillable = TRUE,
      sidebar = bslib::sidebar(
        open = TRUE,
        width = "400px",
        shiny::includeMarkdown("descriptions/national_changelog.md")
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("changelog_table")),
        fill = TRUE
      )
    )
  )
}

# server ----
mod_national_changelog_server <- function(id, df_issues) {
  shiny::moduleServer(id, function(input, output, session) {
    output$changelog_table <- reactable::renderReactable({
      req(df_issues())

      display_issueslog(
        df_issues = df_issues() |> dplyr::arrange(dplyr::desc(date))
      )
    })
  })
}
