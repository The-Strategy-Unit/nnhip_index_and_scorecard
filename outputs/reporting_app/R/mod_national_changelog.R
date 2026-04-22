# --- Changelog (National) ----------------------------------------------------

# ui ----
mod_national_changelog_ui <- function(id) {
  # set up namespacing
  ns <- shiny::NS(id)

  # define the UI
  bslib::nav_panel(
    title = "Change log",
    icon = bsicons::bs_icon("journal-text"),
    bslib::layout_sidebar(
      fillable = TRUE,
      sidebar = bslib::sidebar(
        open = FALSE,
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
    # code goes here
    output$changelog_table <- reactable::renderReactable({
      req(df_issues())

      display_issueslog(df_issues = df_issues())
    })
  })
}
