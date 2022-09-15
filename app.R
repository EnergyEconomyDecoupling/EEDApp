################################################################################
# Set-up
################################################################################

# Use renv::dependencies() to check the packages the app depends on.
# All functions other than those in {shiny}, {shinydashboard}, and {base} should
# be prefixed by the package name, and the correct package space should be
# loaded by renv::restore in the dockerfile of EEDAppBaseImage,
# but each dependency should still be loaded below.

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
library(shinymanager)

# Load general R packages
library(targets)
library(rmarkdown)
library(tidyverse)
library(rlang)
library(lazyeval)
library(knitr)

# Load specific visualization packages
library(plotly)
library(scales)
library(DT)
library(networkD3)

# Load Energy-Economy Decoupling organisation related packages
library(ReboundTools)
library(MKHthemes)
library(IEATools)
library(Recca)
library(PFUDatabase)

# Load constants
source("Utility/constants.R", local = TRUE)

# Loads bespoke functions for use in the app
source("Utility/utility_functions.R", local = TRUE)

# Loads custom theme
source("Utility/customTheme.R", local = TRUE)

# Loads shiny modules to get and wrangle data
source("Utility/load_data.R", local = TRUE)
source("Utility/prepare_data.R", local = TRUE)

# Load PFU Database modules
source("App-Modules/PFUDatabase/intro_pfu.R", local = TRUE)
source("App-Modules/PFUDatabase/pfudatabase_dashboard.R", local = TRUE)
source("App-Modules/PFUDatabase/pfuex_cons.R", local = TRUE)
source("App-Modules/PFUDatabase/pfuex_eta.R", local = TRUE)
source("App-Modules/PFUDatabase/allocations.R", local = TRUE)
source("App-Modules/PFUDatabase/fu_efficiencies.R", local = TRUE)
source("App-Modules/PFUDatabase/guides_pfu.R", local = TRUE)

# Load PFU Analysis modules
source("App-Modules/PFUAnalysis/pfuanalysis_dashboard.R", local = TRUE)
source("App-Modules/PFUAnalysis/rebound_space.R", local = TRUE)
source("App-Modules/PFUAnalysis/decoupling_space.R", local = TRUE)

# Load Rebound modules
source("App-Modules/Rebound/intro_rebound.R", local = TRUE)
source("App-Modules/Rebound/rebound_dashboard.R", local = TRUE)
source("App-Modules/Rebound/citation_rebound.R", local = TRUE)

# Load main shiny modules
source("App-Modules/outline.R", local = TRUE)
source("App-Modules/relresources.R", local = TRUE)
###
source("App-Modules/ui.R", local = TRUE)
source("App-Modules/server.R", local = TRUE)

# Run App
shinyApp(ui, server)
