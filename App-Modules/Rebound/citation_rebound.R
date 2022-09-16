citation_reboundUI <- function(id) {
  ns <- NS(id)
        fluidRow(
          box(title = "Citation Information",
              width = 12,

              tags$h3(a(fontawesome::fa(name = "users-cog", fill = "black"), "|", "Contributors",
                        href = "http://www.google.com")),
              tags$hr(),
              tags$h4("Paper Authors"),
              tags$p("Matthew Kuperus Heun"),
              tags$p("Gregor Semieniuk"),
              tags$p("Paul E. Brockway"),
              tags$h4("App Maintainer"),
              tags$p("Zeke Marshall"),
              tags$br(),

              tags$h3(a(fontawesome::fa(name = "book", fill = "brown"), "|", "Paper",
                        href = "http://www.google.com")),
              tags$hr(),
              tags$h4("Heun et al (2021) - Toward a comprehensive, consumer-sided energy rebound analysis framework"),
              tags$p("DOI:"),
              tags$p("Recommended citation:"),
              tags$p(""),
              tags$br(),

              tags$h3(a(fontawesome::fa(name = "database", fill = "blue"), "|", "Data Repository",
                        href = "http://www.google.com")),
              tags$hr(),
              tags$h4("Datasets associated with the paper......"),
              tags$p("DOI:"),
              tags$p("Recommended citation:"),
              tags$p(""),
              tags$br(),

              tags$h3(a(fontawesome::fa(name = "github", fill = "black"), "|", "R Package",
                        href = "https://github.com/MatthewHeun/ReboundTools")),
              tags$hr(),
              tags$h4("ReboundTools"),
              tags$p("DOI:"),
              tags$p("Recommended citation:"),
              tags$p(""),
              tags$br()

              )
        )
        }
