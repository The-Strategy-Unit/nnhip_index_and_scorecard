ui <- function(request) {
  bslib::page_navbar(
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

    # documentation section ---------------------------------------------------
    bslib::nav_menu(
      title = span(
        bsicons::bs_icon("book"),
        "Documentation",
        class = "documentation-menu"
      ) |>
        bslib::tooltip(
          "Helpful documents for understanding the measures and completing monthly submisisons",
          options = list(trigger = "hover")
        ),
      align = "right",

      # measurement guide
      bslib::nav_item(
        shiny::tags$a(
          href = "NNHIP measurement guide_v1.2.pdf",
          target = "_blank",
          style = "text-decoration: none; color: inherit;",
          bsicons::bs_icon("file-earmark-pdf"),
          htmltools::span("Measurement Guide") |>
            bslib::tooltip(
              "Guidance on how NNHIP measures are defined and how Places should implement them",
              options = list(trigger = "hover")
            )
        )
      ),
      # submission template
      bslib::nav_item(
        shiny::tags$a(
          href = "NNHIPDataCollectionTemplatev1.2.xlsx",
          target = "_blank",
          style = "text-decoration: none; color: inherit;",
          bsicons::bs_icon("file-earmark-spreadsheet"),
          htmltools::span("Submission template") |>
            bslib::tooltip(
              "Excel template for submitting monthly NNHIP data",
              options = list(trigger = "hover")
            )
        )
      )
    ),

    # national section --------------------------------------------------------
    bslib::nav_panel(
      value = "national_view",
      title = shiny::span(
        bsicons::bs_icon("map"),
        "National view"
      ) |>
        bslib::tooltip(
          "National-level views of NNHIP data, showing key trends, patterns and progress across all Places.",
          options = list(trigger = "hover")
        ),
      bslib::page_sidebar(
        # sidebar ----
        sidebar = bslib::sidebar(
          shiny::div(
            # select a demographic (conditional)
            shiny::conditionalPanel(
              condition = "input.national_tabs == 'demographics'",
              shiny::selectizeInput(
                inputId = "selected_demographic",
                label = "Demographic:",
                choices = NULL, # will update this reactively in server.R
                multiple = FALSE
              )
            ),

            # bookmark button (conditional)
            shiny::conditionalPanel(
              condition = "input.national_tabs == 'demographics'",
              shiny::bookmarkButton(label = "Bookmark", width = "100%"),
            ),

            # separator line
            shiny::hr(),

            # show when the data was last updated
            mod_utils_last_updated_ui("national"),

            # SU logo
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

          # overview metrics
          mod_national_overview_ui("national_overview"),

          # engagement plot (disabled for now)
          # mod_national_engagement_ui("national_engagement"),

          # demographics plot
          mod_national_demographics_ui("national_demographics"),

          # data coverage table
          mod_national_coverage_ui("national_coverage"),

          # change / issues table
          mod_national_changelog_ui("national_changelog")
        )
      )
    ),

    # place section -----------------------------------------------------------
    bslib::nav_panel(
      value = "place_view",
      title = shiny::span(
        bsicons::bs_icon("pin-map"),
        "Place view"
      ) |>
        bslib::tooltip(
          "Place-level views of NNHIP data, showing local patterns, progress and variation across individual Places.",
          options = list(trigger = "hover")
        ),
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
              condition = "input.place_tabs == 'funnel_plot'",
              shiny::selectizeInput(
                inputId = "selected_metric",
                label = "Metric:",
                choices = NULL, # will update this reactively in server.R
                multiple = FALSE
              )
            ),

            # select a month (conditional)
            shiny::conditionalPanel(
              condition = "input.place_tabs == 'funnel_plot' || input.place_tabs == 'submission_view'",
              shiny::selectizeInput(
                inputId = "selected_month",
                label = "Month:",
                choices = NULL, # will update this reactively in server.R
                multiple = FALSE
              )
            ),

            # bookmark button
            shiny::bookmarkButton(label = "Bookmark", width = "100%"),

            # divider
            shiny::hr(),

            # show when the data was last updated
            mod_utils_last_updated_ui("place"),

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

            # overview table ----
            mod_place_overview_ui("place_overview"),

            # funnel plot ----
            mod_place_funnel_ui("place_funnel"),

            # engagement table ----
            mod_place_engagement_ui("place_engagement"),

            # submission view table ----
            mod_place_submission_ui("place_submission")
          )
        )
      )
    )
  )
}
