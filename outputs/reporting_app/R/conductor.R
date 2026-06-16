# This is the {conductor} script to provide a brief tour of the app
# Designed to help orient new users to the app
conductor <- conductor::Conductor$new()$step(
  title = "🧭 Welcome to the NNHIP Dashboard",
  text = shiny::HTML(
    "<p>This tour will introduce the main navigation, sidebars and view descriptions to help you get started exploring NNHIP data.</p>"
  )
)$step(
  el = "a .documentation-menu",
  title = "Documentation",
  text = shiny::HTML(
    "<p>Find the <strong>Measurement Guide</strong> and <strong>Submission Template</strong> here.</p>
    <p>These documents explain how measures are defined and how to submit monthly data.</p>"
  ),
  tab = "national_view",
  tabId = "nav_main"
)$step(
  el = "a .nav-national",
  title = "National View",
  text = shiny::HTML(
    "<p>Use the National View to explore trends, patterns and variation across all Places.</p>"
  ),
  tab = "national_overview",
  tabId = "national_tabs"
)$step(
  el = "#national_tabs",
  title = "National Views",
  text = shiny::HTML(
    "<p>Each tab in this menu provides a different national-level view, including:</p>
    <ul>
      <li><strong>Overview</strong> - key monthly metrics</li>
      <li><strong>Demographics</strong> - variation across groups</li>
      <li><strong>Coverage</strong> - data completeness</li>
      <li><strong>Changelog</strong> - recent updates and issues</li>
    </ul>"
  ),
  # ensure on the national view of the main navbar
  tab = "national_view",
  tabId = "nav_main"
)$step(
  el = "#national_overview_description",
  title = "View Description",
  text = shiny::HTML(
    "<p>Each view includes a description explaining what the chart or table shows and how to interpret it.</p>
    <p>You can collapse or expand this sidebar using the chevron button (<code><</code>) at the top.</p>"
  ),
  # ensure the 'overview' tab is selected in the 'national_tabs' navbar
  tab = "overview_national",
  tabId = "national_tabs"
)$step(
  el = "#national_overview",
  title = "Interactive Views",
  text = shiny::HTML(
    "<p>Most views in the dashboard are interactive:</p>
    <ul>
      <li><strong>Tables</strong> - click column headings to sort and scroll to see more rows.</li>
      <li><strong>Charts</strong> - hover over points for additional information and use the toolbar to zoom or reset.</li>
    </ul>
    <p>Try interacting with tables and charts to explore the data in more depth.</p>
    "
  )
)$step(
  el = "a .nav-place",
  title = "Place View",
  text = shiny::HTML(
    "<p>Switch to the Place View to explore data for individual Places, including funnel plots, submissions and engagement.</p>",
  ),
  tab = "place_view",
  tabId = "nav_main"
)$step(
  el = "#place_sidebar",
  title = "Sidebar Controls",
  text = shiny::HTML(
    "<p>This sidebar contains the key inputs for exploring Place-level data:</p>
    <ul>
      <li><strong>Place selector</strong> - choose which Place you want to view.</li>
      <li><strong>Metric and Month selectors</strong> - appear when relevant, such as in the Funnel Plot or Submission View.</li>
      <li><strong>Bookmark button</strong> - save your current view so you can return to it later.</li>
      <li><strong>Last updated panel</strong> - shows when the data was most recently refreshed.</li>
    </ul>
    <p>You can collapse or expand this sidebar at any time using the chevron button (<code><</code>) at the top.</p>
    "
  ),
  tab = "place_view",
  tabId = "nav_main"
)$step(
  el = "#place_header_title",
  position = "bottom-start",
  title = "Place Header",
  text = shiny::HTML(
    "<p>This title shows the name of the selected Place.</p>
    <p>All views in this section update automatically when you choose a different Place from the sidebar.</p>"
  ),
  tab = "place_view",
  tabId = "nav_main"
)$step(
  title = "✅ You're ready to explore",
  text = shiny::HTML(
    "<p>You now know how to navigate the dashboard.</p>
    <p>You can restart this tour anytime using the help button in the navbar.</p>"
  ),
  buttons = list(list(text = "End tour", action = "next"))
)
