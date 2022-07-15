################################################################################
# Set-up
################################################################################

# Load shiny related packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(dashboardthemes)
library(htmltools)
library(htmlwidgets)
library(cicerone)
library(fontawesome)

# Load credential management packages
library(shinymanager)
library(keyring)

# Load general R packages
library(rmarkdown)
library(tidyverse)

# Load specific visualization packages
library(plotly)
library(scales)
library(xtable)
library(colorspace)

# Load Energy-Economy Decoupling organisation related packages
library(ReboundTools)
library(MKHthemes)

# Loads shiny modules to get and wrangle data
source("App-Modules/load_data.R", local = TRUE)
source("App-Modules/prepare_data.R", local = TRUE)

# Load main shiny modules
source("App-Modules/outline.R", local = TRUE)
source("App-Modules/relresources.R", local = TRUE)

# Load PFU modules
source("App-Modules/intro_pfu.R", local = TRUE)
source("App-Modules/sum_dash_pfu.R", local = TRUE)
source("App-Modules/pfuex.R", local = TRUE)
source("App-Modules/allocations.R", local = TRUE)
source("App-Modules/fu_efficiencies.R", local = TRUE)
source("App-Modules/rebound_space.R", local = TRUE)
source("App-Modules/ecc.R", local = TRUE)

# Load Energy-Economy modules


# Load Rebound modules
source("App-Modules/intro_rebound.R", local = TRUE)
source("App-Modules/sum_dash_rebound.R", local = TRUE)
source("App-Modules/reboundtools_rebound.R", local = TRUE)
source("App-Modules/citation_rebound.R", local = TRUE)

# Loads bespoke functions for use in the app
source("App-Modules/utility_functions.R", local = TRUE)

# Loads custom theme
source("App-Modules/customTheme.R", local = TRUE)

# Loads cicerone guides
source("App-Modules/guides_pfu.R", local = TRUE)

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

                  # title = span("Energy-Economy Decoupling",
                  #              style = "color: white; font-size: 18px"),
                  # titleWidth = 300,

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

                         menuItem("PFU Dashboard", tabName = "dashboard_pfu", icon = icon("chalkboard", verify_fa = FALSE)),

                         menuItem("PFU EX Consumption", tabName = "pfuex", icon = icon("lightbulb")),

                         menuItem("Final-to-useful Allocations", tabName = "allocations", icon = icon("chart-pie", verify_fa = FALSE)),

                         menuItem("Final-to-useful Efficiencies", tabName = "fu_efficiencies", icon = icon("line-chart", verify_fa = FALSE)),

                         menuItem("Rebound Space", tabName = "rebound_space", icon = icon("compress", verify_fa = FALSE)),

                         menuItem("Energy Conversion Chain", tabName = "ecc", icon = icon("link", verify_fa = FALSE))#,
                         #
                         # menuItem("Database documentation", tabName = "documentation", icon = icon("book")),
                         #
                         # menuItem("Citation", tabName = "citation_pfu", icon = icon("fa-user-graduate"))

                ),

                menuItem("Rebound",
                         expandedName = "rebound",
                         icon = icon("compress", verify_fa = FALSE),

                         menuItem("Introduction", tabName = "intro_rebound", icon = icon("book", verify_fa = FALSE)),

                         menuItem("Rebound Dashboard", tabName = "dashboard_rebound", icon = icon("chalkboard", verify_fa = FALSE)),

                         menuItem("ReboundTools", tabName = "rebound_tools", icon = icon("r-project", verify_fa = FALSE)),

                         menuItem("Citation", tabName = "citation_rebound", icon = icon("user-graduate", verify_fa = FALSE))

                         ),

                menuItem("Net Energy Analysis",
                         tabName = "netenergy",
                         icon = icon("balance-scale-left", verify_fa = FALSE),
                         badgeLabel = "Coming Soon",
                         badgeColor = "orange"
                         ),

                menuItem("Energy-Economy",
                         tabName = "energyecon",
                         icon = icon("money-bill-wave", verify_fa = FALSE),
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

      tabItem(tabName = "pfuex",
              pfuexUI(id = "pfuex_id_1")),

      tabItem(tabName = "allocations",
              allocplotsUI(id = "alloc_id_1")),

      tabItem(tabName = "fu_efficiencies",
              etaplotsUI(id = "fu_id_1")),

      tabItem(tabName = "rebound_space",
              rebound_spaceUI(id = "rebound_id_1")),

      tabItem(tabName = "ecc",
              eccUI(id = "ecc_id_1")),

      # tabItem(tabName = "documentation",
      #         documentationUI(id = "doc1")),


      ## Rebound modules - UI
      tabItem(tabName = "intro_rebound",
              intro_reboundUI(id = "introreb_id_1")),

      tabItem(tabName = "dashboard_rebound",
              rebound_dashUI(id = "dashreb_id_1")),

      tabItem(tabName = "rebound_tools",
              rebound_docUI(id = "docreb_id_1")),

      tabItem(tabName = "citation_rebound",
              citation_reboundUI(id = "citreb_id_1")),

      ## Other modules - UI

      # Related resources UI
      tabItem(tabName = "relatedresources",
              relresourcesUI(id = "rr_id_1"))

      # Contact UI

      ) # Close tabitems

  ) # Close dashboard body

) # Close UI

# Secure app
# ui <- shinymanager::secure_app(ui, enable_admin = TRUE)

################################################################################
# Server
################################################################################

server <- function(input, output, session) {

################################################################################
# Authenticate user
################################################################################

  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- shinymanager::secure_server(
    # check_credentials = shinymanager::check_credentials(credentials)
    check_credentials = shinymanager::check_credentials(credentials_path,
                                                        passphrase = "passphrase_wihtout_keyring")
  )

  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })


################################################################################
# Call guides
################################################################################

  # Triggers PFU Database guide when childless menuItem is opened
  observeEvent(input$sidebarItemExpanded, {
    req(input$sidebarItemExpanded)
    if (input$sidebarItemExpanded == "pfu_menu") {
      guide_pfu$start()
    }
  })

  # Initialise guide_pfu_dash, but do not start automatically
  guide_pfu_dash$
    init()


################################################################################
# Call server-side module components
################################################################################

  ## PFU-Database modules

  # Calls sum_dash_pfu.R module
  callModule(module = sumdashplots,
             id = "dashpfu_id_1")

  # Calls the pfuex.R module
  callModule(module = pfuex,
             id = "pfuex_id_1")

  callModule(module = allocplots,
             id = "alloc_id_1",
             comp_alloc_tables_prepped)

  callModule(module = etaplots,
             id = "fu_id_1",
             comp_effic_tables_prepped)

  callModule(module = rebound_space,
             id = "rebound_id_1",
             rebound_space_data_prepped)

  callModule(module = ecc,
             id = "ecc_id_1",
             psut_useful)


  ## Rebound modules

  # Calls rebound introduction module
  callModule(module = intro_rebound,
             id = "introreb_id_1")

  # Calls rebound dashboard module
  callModule(module = rebound_dash,
             id = "dashreb_id_1")

  # Calls rebound citation module


}

shinyApp(ui, server)
