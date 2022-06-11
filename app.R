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

# Load main shiny modules

source("App-Modules/outline.R", local = TRUE)
source("App-Modules/relresources.R", local = TRUE)

# Load PFU modules


# Load Energy-Economy modules


# Load Rebound modules
source("App-Modules/intro_rebound.R", local = TRUE)
source("App-Modules/sum_dash_rebound.R", local = TRUE)
source("App-Modules/reboundtools_rebound.R", local = TRUE)
source("App-Modules/citation_rebound.R", local = TRUE)

# Loads bespoke functions for use in the app


# Loads custom theme
source("App-Modules/customTheme.R", local = TRUE)

# Loads cicerone guides

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




################################################################################
# Call server-side module components
################################################################################

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
