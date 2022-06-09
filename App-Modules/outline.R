outlineUI <- function(id) {
  ns <- NS(id)
        fluidRow(
          box(width = 12,
              tags$h1("Outline"),
              tags$hr(),
              tags$p("This Shiny App is the home of the Energy-Economy Research 
                     partnership between the University of Leeds and Calvin University.....")
              )
        )
        }