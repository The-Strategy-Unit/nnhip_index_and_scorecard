ui <- function(request) {
  bslib::page_navbar(
    # title = "NNHIP scorecard dashboard",
    title = shiny::tags$img(
      src = "logos/nnhip_logo.png",
      style = "height:60px;",
    ),
    window_title = "NNHIP dashboard",

    # theming
    theme = bslib::bs_theme(brand = TRUE, card_bg = "white"),
    header = shiny::tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "_SUBrand.SyleSheet.css"
    ),

    # national section --------------------------------------------------------
    bslib::nav_panel(
      title = "National view",
      icon = bsicons::bs_icon("map"),
      bslib::page_sidebar(
        # sidebar ----
        sidebar = bslib::sidebar(
          shiny::div(
            # add some branding
            shiny::hr(),
            shiny::tags$div(
              style = "text-align:center; padding: 10px 0;",
              shiny::tags$img(
                src = "logos/logo_black.svg",
                style = "max-width: 120px; height: auto"
              )
            )
          )
        ),
        # main ----
        bslib::navset_card_tab(
          id = "national_tabs",
          bslib::nav_panel(
            title = "Overview",
            icon = bsicons::bs_icon("table"),
            bslib::layout_sidebar(
              fillable = TRUE,
              sidebar = bslib::sidebar(
                open = FALSE,
                shiny::includeMarkdown("descriptions/national_overview.md")
              ),
              bslib::card_body(
                reactable::reactableOutput("national_table"),
                fill = TRUE
              )
            )
          ),
          bslib::nav_panel(
            title = "Data coverage",
            icon = bsicons::bs_icon("ui-checks-grid"),
            # bslib::card_body(reactable::reactableOutput(
            #   "national_data_coverage_table"
            # ))
            bslib::layout_sidebar(
              fillable = TRUE,
              sidebar = bslib::sidebar(
                open = FALSE,
                shiny::includeMarkdown("descriptions/national_coverage.md")
              ),
              bslib::card_body(
                reactable::reactableOutput("national_data_coverage_table"),
                fill = TRUE
              )
            )
          ),
          bslib::nav_panel(
            title = "Change log",
            icon = bsicons::bs_icon("journal-text"),
            # bslib::card_body(reactable::reactableOutput("changelog_table"))
            bslib::layout_sidebar(
              fillable = TRUE,
              sidebar = bslib::sidebar(
                open = FALSE,
                shiny::includeMarkdown("descriptions/national_changelog.md")
              ),
              bslib::card_body(
                reactable::reactableOutput("changelog_table"),
                fill = TRUE
              )
            )
          )
        )
      )
    ),

    # place section -----------------------------------------------------------
    bslib::nav_panel(
      title = "Place view",
      icon = bsicons::bs_icon("pin-map"),
      bslib::page_sidebar(
        # sidebar ----
        sidebar = bslib::sidebar(
          shiny::div(
            # select a place (always visible)
            shiny::selectizeInput(
              inputId = "selected_place",
              label = "Place:",
              # choices = places,
              choices = NULL, # will update this reactively in server.R
              multiple = FALSE
            ),

            # select a metric (conditional)
            shiny::conditionalPanel(
              condition = "input.place_tabs == 'Funnel plot'",
              shiny::selectizeInput(
                inputId = "selected_metric",
                label = "Metric:",
                choices = NULL, # will update this reactively in server.R
                multiple = FALSE
              )
            ),

            # select a month (conditional)
            shiny::conditionalPanel(
              condition = "input.place_tabs == 'Funnel plot'",
              shiny::selectizeInput(
                inputId = "selected_month",
                label = "Month:",
                choices = NULL, # will update this reactively in server.R
                multiple = FALSE
              )
            ),

            # divider
            shiny::hr(),

            # bookmark button
            shiny::bookmarkButton(label = "Bookmark"),

            # add some branding
            shiny::tags$div(
              style = "text-align:center; padding: 10px 0;",
              shiny::tags$img(
                src = "logos/logo_black.svg",
                style = "max-width: 120px; height: auto"
              )
            )
          )
        ),

        # main ----
        bslib::card(
          bslib::card_title(shiny::textOutput(outputId = "place_header")),
          bslib::navset_card_tab(
            id = "place_tabs",
            full_screen = TRUE,

            bslib::nav_panel(
              title = "Overview",
              icon = bsicons::bs_icon("table"),
              bslib::layout_sidebar(
                fillable = TRUE,
                sidebar = bslib::sidebar(
                  open = FALSE,
                  shiny::includeMarkdown("descriptions/place_overview.md")
                ),
                bslib::card_body(
                  reactable::reactableOutput("place_table"),
                  fill = TRUE
                )
              )
            ),

            # funnel plot ----
            mod_place_funnel_ui("place_funnel"),

            bslib::nav_panel(
              title = "Engagement",
              icon = bsicons::bs_icon("table"),
              bslib::layout_sidebar(
                fillable = TRUE,
                open = FALSE,
                sidebar = bslib::sidebar(
                  open = FALSE,
                  shiny::includeMarkdown("descriptions/place_engagement.md")
                ),
                bslib::card_body(
                  reactable::reactableOutput("engagement_table")
                )
              )
            )
          )
        )
      )
    )
  )
}
