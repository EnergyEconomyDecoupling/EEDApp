intro_pfuUI <- function(id) {
  ns <- NS(id)
        fluidRow(
          box(width = 12,
              tags$h1("Introduction"),
              tags$hr(),
              tags$blockquote(HTML("A modern industrial society can be
                                   viewed as a complex machine for
                                   degrading high-quality energy
                                   into waste heat while extracting the energy
                                   needed for creating an enormous
                                   catalogue of goods and services (Summers, 1971)"),
                              style ="font-style: italic;
                                      font-family: Georgia, Times, Times New Roman, serif;
                                      font-size: 14px;
                                      padding-bottom:5px;
                                      padding-top:5px;
                                      margin-top:5px;
                                      margin-bottom:5px;
                                      background-color: #FFFFFF;
                                      color: #000000;"),
              tags$hr(),
              tags$p("This database serves to extend the the International
                      Energy Agencies (IEA) World extended energy balance data
                      from the Final energy stage, to the Useful energy stage")
              )
        )
        }
