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

# Load general R packages
library(rmarkdown)
library(tidyverse)

# Load specific visualisation packages
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

# Load bespoke functions
source("App-Modules/functions.R", local = TRUE)

################################################################################
# UI
################################################################################

ui = dashboardPage(

  # useShinyjs(),


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

                menuItem("Outline", tabName = "outline", icon = icon("chalkboard-teacher fa-fw")),

                menuItem("PFU Database", expandedName = "pfu_menu", icon = icon("battery-quarter"),

                         menuItem("Introduction", tabName = "intro_pfu", icon = icon("book-reader")),

                         menuItem("PFU Dashboard", tabName = "dashboard_pfu", icon = icon("dashboard"))#,

                         # menuItem("Final-to-useful Allocations", tabName = "allocations", icon = icon("chart-pie")),
                         #
                         # menuItem("Final-to-useful Efficiencies", tabName = "eta", icon = icon("chart-line")),
                         #
                         # menuItem("PFU Consumption", tabName = "consumption", icon = icon("lightbulb")),
                         #
                         # menuItem("Energy Conversion Chain", tabName = "sankey", icon = icon("project-diagram")),
                         #
                         # menuItem("Database documentation", tabName = "documentation", icon = icon("book")),
                         #
                         # menuItem("Citation", tabName = "citation_pfu", icon = icon("user-graduate"))

                ),

                menuItem("Rebound", expandedName = "rebound", icon = icon("compress-alt fa-fw"),

                         menuItem("Introduction", tabName = "intro_rebound", icon = icon("book-reader fa-fw")),

                         menuItem("Rebound Dashboard", tabName = "dashboard_rebound", icon = icon("dashboard fa-fw")),

                         menuItem("ReboundTools", tabName = "rebound_tools", icon = icon("r-project")),

                         menuItem("Citation", tabName = "citation_rebound", icon = icon("user-graduate fa-fw"))

                         ),

                menuItem("Contact", tabName = "contact", icon = icon("phone-alt fa-fw"))

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
              outlineUI(id = "outline1")),

      # PFU-Database items
      tabItem(tabName = "intro_pfu",
              intro_pfuUI(id = "intro2")),

      tabItem(tabName = "dashboard_pfu",
              sumdashplotsUI(id = "dash1")),

      # tabItem(tabName = "sankey",
      #         eccUI(id = "ecc1")),
      #
      # tabItem(tabName = "allocations",
      #         allocplotsUI(id = "allocations1")),
      #
      # tabItem(tabName = "eta",
      #         etaplotsUI(id = "eta1")),
      #
      # tabItem(tabName = "consumption",
      #         consumptionUI(id = "consumption1")),
      #
      # tabItem(tabName = "documentation",
      #         documentationUI(id = "doc1")),


      ## Rebound modules - UI
      tabItem(tabName = "intro_rebound",
              intro_reboundUI(id = "intro3")),

      tabItem(tabName = "dashboard_rebound",
              rebound_dashUI(id = "dash3")),

      tabItem(tabName = "rebound_tools",
              rebound_docUI(id = "doc2")),

      tabItem(tabName = "citation_rebound",
              citation_reboundUI(id = "cit2")),

      ## Other modules - UI

      # Related resources UI
      tabItem(tabName = "relatedresources",
              relresourcesUI(id = "rr1"))

      # Contact UI

      ) # Close tabitems


  ) # Close dashboard body

) # Close UI


################################################################################
# Server
################################################################################

server <- function(input, output, session) {




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
             id = "dash1",
             PSUT_etas,
             PSUT_metrics_total,
             GDP_metrics)


  ## Rebound modules

  # Calls rebound introduction module

  callModule(module = intro_rebound,
             id = "intro3")

  # Calls rebound dashboard module

  callModule(module = rebound_dash,
             id = "dash3")

  # Calls rebound citation module


}

shinyApp(ui, server)
