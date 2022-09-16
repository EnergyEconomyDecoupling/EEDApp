################################################################################
# UI
################################################################################

ui = dashboardPage(

  # useShinyjs(),

  ################################################################################
  # Import font awesome icons
  ################################################################################

  # tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),

  ################################################################################
  # Header UI
  ################################################################################


  header = dashboardHeader(

    title = tags$h4(HTML("Energy-Economy <br/> Decoupling"),
                    style =
                      "
                    font-weight: bold;
                    font-family: Georgia, Times, Times New Roman, serif;
                    font-size: 20px;
                    padding-bottom:0px;
                    padding-top:0px;
                    margin-top:3px;
                    margin-bottom:0px;
                    background-color: #000000;
                    color: #FFFFFF;
                    "
    ),

    tags$li(a(href = 'http://www.leeds.ac.uk',
              img(src = 'Leeds Logo White Text 1.png',
                  title = "Leeds", height = "40px"),
              style = "padding-top:5px;
                            padding-bottom:5px;
                            padding-right:0px;
                            padding-left:0px;
                            margin-right:20px;"),
            class = "dropdown"),
    tags$li(a(href = 'https://calvin.edu/',
              img(src = 'Calvin Logo White Text 1.png',
                  title = "Calvin", height = "40px"),
              style = "padding-top:5px;
                            padding-bottom:5px;
                            padding-right:0px;
                            padding-left:0px;
                            margin-right:10px;"),
            class = "dropdown")
  ),

  ################################################################################
  # Left Sidebar UI
  ################################################################################

  sidebar = dashboardSidebar(

    width = 272.25, # Header is 230px button is 42.5px

    minified = FALSE,

    sidebarMenu(id = "sidebarmenu",

                menuItem("Outline",
                         tabName = "outline",
                         icon = icon("pen", verify_fa = FALSE)),

                menuItem("PFU Database",
                         expandedName = "pfu_menu",
                         icon = icon("battery-quarter", verify_fa = FALSE),

                         menuItem("Introduction", tabName = "intro_pfu", icon = icon("book", verify_fa = FALSE)),

                         menuItem("PFU Database Dashboard", tabName = "dashboard_pfu", icon = icon("chalkboard", verify_fa = FALSE)),

                         menuItem("PFU Consumption", tabName = "pfuex_cons", icon = icon("lightbulb")),

                         menuItem("PFU Efficiency", tabName = "pfuex_eta", icon = icon("lightbulb")),

                         menuItem("Final-to-useful Allocations", tabName = "allocations", icon = icon("chart-pie", verify_fa = FALSE)),

                         menuItem("Final-to-useful Efficiencies", tabName = "fu_efficiencies", icon = icon("line-chart", verify_fa = FALSE))#,

                         # menuItem("Citation", tabName = "citation_pfu", icon = icon("fa-user-graduate"))

                ),

                menuItem("PFU Analysis",
                         tabName = "energyecon",
                         icon = icon("money-bill-wave", verify_fa = FALSE),

                         menuItem("PFU Analysis Dashboard", tabName = "dashboard_pfuanalysis", icon = icon("chalkboard", verify_fa = FALSE)),

                         menuItem("Rebound Space", tabName = "rebound_space", icon = icon("compress", verify_fa = FALSE)),

                         menuItem("Decoupling Space", tabName = "decoupling_space", icon = icon("book", verify_fa = FALSE))
                ),

                menuItem("Rebound",
                         expandedName = "rebound",
                         icon = icon("compress", verify_fa = FALSE),

                         menuItem("Introduction", tabName = "intro_rebound", icon = icon("book", verify_fa = FALSE)),

                         menuItem("Rebound Dashboard", tabName = "dashboard_rebound", icon = icon("chalkboard", verify_fa = FALSE)),

                         menuItem("Citation", tabName = "citation_rebound", icon = icon("user-graduate", verify_fa = FALSE))

                ),

                menuItem("Net Energy Analysis",
                         tabName = "netenergy",
                         icon = icon("balance-scale-left", verify_fa = FALSE),
                         badgeLabel = "Coming Soon",
                         badgeColor = "orange"
                ),

                menuItem("Contact",
                         tabName = "contact",
                         icon = icon("phone", verify_fa = FALSE))

    )
  ),

  ################################################################################
  # Body UI
  ################################################################################

  body = dashboardBody(

    # Load katex to allow the rendering mathematical symbols
    tags$head(
      tags$link(rel="stylesheet", href="https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.css", integrity="sha384-9tPv11A+glH/on/wEu99NVwDPwkMQESOocs/ZGXPoIiLE8MU/qkqUcZ3zzL+6DuH", crossorigin="anonymous"),
      tags$script(src="https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.js", integrity="sha384-U8Vrjwb8fuHMt6ewaCy8uqeUXv4oitYACKdB0VziCerzt011iQ/0TqlSlv8MReCm", crossorigin="anonymous"),
      tags$script(HTML(render_katex))
    ),

    # Load custom theme from customTheme.R script
    customTheme,

    tabItems(

      # General
      tabItem(tabName = "outline",
              outlineUI(id = "outline_id_1")),

      # PFU-Database items
      tabItem(tabName = "intro_pfu",
              intro_pfuUI(id = "intropfu_id_1")),

      tabItem(tabName = "dashboard_pfu",
              sumdashplotsUI(id = "dashpfu_id_1")),

      tabItem(tabName = "pfuex_cons",
              pfuex_consUI(id = "pfuex_cons_id_1")),

      tabItem(tabName = "pfuex_eta",
              pfuex_etaUI(id = "pfuex_eta_id_1")),

      tabItem(tabName = "allocations",
              allocplotsUI(id = "alloc_id_1")),

      tabItem(tabName = "fu_efficiencies",
              etaplotsUI(id = "fu_id_1")),

      ## PFU Analysis - UI
      tabItem(tabName = "dashboard_pfuanalysis",
              pfuanalysis_dashUI(id = "dashpfuanalysis_id_1")),

      tabItem(tabName = "rebound_space",
              rebound_spaceUI(id = "rebound_id_1")),

      tabItem(tabName = "decoupling_space",
              decoupling_spaceUI(id = "decsp_id_1")),



      ## Rebound modules - UI
      tabItem(tabName = "intro_rebound",
              intro_reboundUI(id = "introreb_id_1")),

      tabItem(tabName = "dashboard_rebound",
              rebound_dashUI(id = "dashreb_id_1")),

      tabItem(tabName = "citation_rebound",
              citation_reboundUI(id = "citreb_id_1")),


      # Related resources UI
      tabItem(tabName = "relatedresources",
              relresourcesUI(id = "rr_id_1"))

      # Contact UI

    ) # Close tabitems

  ) # Close dashboard body

) # Close UI

# Secure app
ui <- shinymanager::secure_app(ui, enable_admin = TRUE)
