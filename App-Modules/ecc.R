# Load required packages
library(Recca)
library(magrittr)
library(networkD3)

# Establishes UI module function
eccUI <- function(id) {
  ns <- NS(id)
  fluidRow(

    box(
      title = "Variables",
      id = "Variables_pfu",
      # status = "primary",
      solidHeader = FALSE,
      closable = FALSE,
      collapsible = TRUE,
      width = 12,
      # height = 100,
      splitLayout(

        sliderInput(
          inputId = ns("year"),
          label = "Year",
          min = 1960,
          max = 2019,
          value = 2019,
          step = 1,
          sep = "",
          width = "100%",
          animate = FALSE),

        selectizeInput(
          inputId = ns("country"),
          label = "Country:",
          choices = country_options,
          width = "100%",
          options = list(dropdownParent = 'body')),

        selectizeInput(
          inputId = ns("agg_by"),
          label = "Aggregate by:",
          choices = c("Product Group"),
          width = "100%",
          options = list(dropdownParent = 'body')),

        # style = "z-index:1002;",

        cellWidths = c("80%", "10%", "10%"),

        cellArgs = list(style = "vertical-align: center;
                                  padding-right: 6px;
                                  padding-left: 6px;
                                  padding-bottom: 0px;
                                  padding-top: 0px;"
        )

      )),

    box(
      title = "Energy Conversion Chain",
      id = "ccc",
      width = 12,
      sankeyNetworkOutput(outputId = ns("sankey"),
                          height = 860)),

    tags$head(tags$style('#ccc .box-header{ display: none}'))

  ) # Closes fluidRow

}

# Establishes server module function
ecc <- function(input, output, session,
                year,
                country) {


  # Creates reactive data frame for energy conversion chain from PSUT_useful target
  selected_data <- reactive({
    dplyr::filter(psut_useful,
                  Country == input$country)

  })


  # These observe events update the year slider
  observeEvent(input$country,  {

    req(input$country)

    year_min <- min(selected_data()$Year)

    updateSliderInput(session,
                      inputId = "year",
                      min = year_min,
                      value = year_min)
  })

  # Creates reactive data frame for energy conversion chain from PSUT_useful target
  selected_data_2 <- reactive({
    dplyr::filter(selected_data(),
                  Year == input$year)

  })


  # Energy conversion chain sankey diagrams
  output$sankey <- renderSankeyNetwork({

    ecc_sankey <- selected_data_2() %>%
      Recca::make_sankey(nodeWidth = 20,
                         units = "ktoe",
                         sinksRight = TRUE,
                         margin = list(left = 200)
      ) %>%
      magrittr::extract2("Sankey") %>%
      magrittr::extract2(2)

    # Add onRender JavaScript code
    ecc_sankey <- htmlwidgets::onRender(ecc_sankey,
                                        'function(el, x) {

                                        // Select all node text
                                        d3.selectAll(".node text")
                                          .style("fill", "black") // Set text colour to black
                                          .style("font-size", "12px") // Change font size to 12
                                          .attr("text-anchor", "begin") // Set text node side to right
                                          .attr("x", 20);

                                        }'
    )

    ecc_sankey


  })

}
