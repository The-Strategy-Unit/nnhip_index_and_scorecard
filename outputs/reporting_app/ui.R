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

          # engagemet plot
          mod_national_engagement_ui("national_engagement"),

          # data coverage table
          mod_national_coverage_ui("national_coverage"),

          # change / issues table
          mod_national_changelog_ui("national_changelog")
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
            mod_place_engagement_ui("place_engagement")
          )
        )
      )
    )
  )
}
