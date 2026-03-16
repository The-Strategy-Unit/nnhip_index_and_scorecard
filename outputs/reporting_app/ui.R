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
      bslib::page_sidebar(
        # sidebar ----
        sidebar = bslib::sidebar(),
        # main ----
        "National main"
      )
    ),

    # place section -----------------------------------------------------------
    bslib::nav_panel(
      title = "Place view",
      bslib::page_sidebar(
        # sidebar ----
        sidebar = bslib::sidebar(
          # select a place (always visible)
          shiny::selectizeInput(
            inputId = "selected_place",
            label = "Place:",
            choices = places,
            multiple = FALSE
          ),

          # select a metric (conditional)
          shiny::conditionalPanel(
            condition = "input.place_tabs == 'Funnel plot'",
            shiny::selectizeInput(
              inputId = "selected_metric",
              label = "Metric:",
              choices = metrics,
              multiple = FALSE
            )
          ),

          # select a month (conditional)
          shiny::conditionalPanel(
            condition = "input.place_tabs == 'Funnel plot'",
            shiny::selectizeInput(
              inputId = "selected_month",
              label = "Month:",
              choices = months,
              multiple = FALSE
            )
          ),

          # bookmark button
          shiny::bookmarkButton(label = "Bookmark"),

          # add some branding
          shiny::hr(),
          shiny::tags$div(
            style = "text-align:center; padding: 10px 0;",
            shiny::tags$img(
              src = "logos/logo_black.svg",
              style = "max-width: 120px; height: auto"
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
              title = "Dashboard table",
              bslib::card_body(reactable::reactableOutput("place_table")),
            ),
            bslib::nav_panel(
              title = "Funnel plot",
              plotly::plotlyOutput("place_funnel", height = "100%")
            )
          )
        )
      )
    )
  )
}
