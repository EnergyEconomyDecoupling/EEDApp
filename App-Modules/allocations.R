# Loads required packages
library(tidyverse)

# Establishes UI module function
allocplotsUI <- function(id) {
  ns <- NS(id)

  fluidRow(

    tabBox(title = "Final-to-useful Allocations",
           id = "alloc_id",
           width = 10,
           height = 920,
           tabPanel(title = "Plots",
                    # height = 910,
                    plotly::plotlyOutput(outputId = ns("allocations_plot"))),
           tabPanel(title = "Data",
                    # height = 910,
                    DT::dataTableOutput(outputId = ns("allocations_table")),
                    style = "font-size:78%")
           ),

    tabBox(width = 2,
           tabPanel(title = "Options",

           selectizeInput(inputId = ns("country"),
                          label = "Country:",
                          choices = country_options,
                          multiple = TRUE %>%
                            sort()),

           selectInput(inputId = ns("efproduct"),
                       label = "Final energy carrier:",
                       choices = unique(comp_alloc_tables_prepped$Ef.product) %>%
                         sort()),

           selectInput(inputId = ns("destination"),
                       label = "Destination:",
                       choices = unique(comp_alloc_tables_prepped$Destination) %>%
                         sort()),

           selectInput(inputId = ns("dataformat"),
                       label = "Data Format:",
                       choices = c("Wide", "Long"))

          ),

          tabPanel(title = "Download",

                   tags$h5(tags$b("Selected Allocation Data")),

                   downloadButton(outputId = ns("download_data"),
                                  label = "Download",
                                  class = NULL,
                                  icon = shiny::icon("download")),

                   tags$h5(tags$b("All Allocation Data")),

                   downloadButton(outputId = ns("download_alldata"),
                                  label = "Download",
                                  class = NULL,
                                  icon = shiny::icon("download"))
        )
        )
    )
}

# Establishes the server module function
allocplots <- function(input, output, session,
                       comp_alloc_tables_prepped,
                       country,
                       efproduct,
                       destination,
                       dataformat) {

  # Creates a dataframe with the selected country, efproduct, and destination
  selected_data <- reactive({

    validate(
      need(input$country != "", "Please select atleast one Country"),
      need(input$efproduct != "", "Please select one Final energy carrier"),
      need(input$destination != "", "Please select one Destination")
    )

    dplyr::filter(comp_alloc_tables_prepped,
                  Country %in% input$country,
                  Ef.product == input$efproduct,
                  Destination == input$destination)

  })


  # These observe events update the allocations tab
  observeEvent(input$country,  {
    req(input$country)
    post_country_data <- comp_alloc_tables_prepped %>%
      dplyr::filter(Country %in% input$country)

    updateSelectInput(session,
                      inputId = "efproduct",
                      choices = sort(unique(post_country_data$Ef.product)))
    })

  observeEvent(input$efproduct,  {
    req(input$country)
    req(input$efproduct)
    post_efproduct_data <- comp_alloc_tables_prepped %>%
      dplyr::filter(Country %in% input$country) %>%
      dplyr::filter(Ef.product %in% input$efproduct)

    updateSelectInput(session,
                      inputId = "destination",
                      choices = sort(unique(post_efproduct_data$Destination)))
    })

  # Final Energy carrier-Destination to Machine-useful work allocation plots
  output$allocations_plot <- renderPlotly({
      p <- ggplot2::ggplot(data = selected_data()) +
        ggplot2::geom_area(mapping = aes(x = Year,
                                         y = .values,
                                         fill = Machine_Eu.product),
                           position = "fill") +
        ggplot2::scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
        ggplot2::scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
        # ggplot2::theme(axis.title.y = element_text(margin = margin(r=10))) +
        ggplot2::facet_wrap(facets = "Country", ncol = 2) +
        ggplot2::labs(fill = "Machine - Useful Work") +
        ggplot2::xlab("") +
        ggplot2::ylab("Proportion of energy consumption [-]") +
        MKHthemes::xy_theme()


      p_plotly <- plotly::ggplotly(p,
                                   height = 850,
                                   tooltip = c("Year", ".values", "Machine_Eu.product")) %>%
        plotly::layout(showlegend = TRUE,
                       legend = list(font = list(size = 12))) %>%
        move_annotations(x = -0.05, y = 0.97, mar = 80)

  })

  output$allocations_table <- DT::renderDataTable({

    req(input$dataformat)

    if(input$dataformat == "Long"){

      data_long <- selected_data() %>%
        as.data.frame()

      allocations_table <- DT::datatable(data = data_long,
                                         rownames = TRUE,
                                         fillContainer = TRUE,
                                         # height = 880,
                                         options = list(paging = FALSE,    ## paginate the output
                                                        # pageLength = 20,  ## number of rows to output for each page
                                                        scrollX = TRUE,   ## enable scrolling on X axis
                                                        scrollY = "800px",   ## enable scrolling on Y axis
                                                        autoWidth = FALSE, ## use smart column width handling
                                                        server = FALSE,   ## use client-side processing
                                                        dom = 'Bfrtip',
                                                        columnDefs = list(

                                                          # Centers columns
                                                          list(targets = '_all',
                                                              className = 'dt-center'),

                                                          # Removes columns
                                                          list(targets = c(0, 15),
                                                              visible = FALSE)

                                                        ))) %>%
        DT::formatRound(columns=c(".values"), digits=3)

      return(allocations_table)


    } else if (input$dataformat == "Wide") {

      data_wide <- selected_data() %>%
        as.data.frame() %>%
        tidyr::pivot_wider(names_from = "Year",
                           values_from = ".values")

      allocations_table <- DT::datatable(data = data_wide,
                                         rownames = TRUE,
                                         fillContainer = TRUE,
                                         # height = 880,
                                         options = list(paging = FALSE,    ## paginate the output
                                                        # pageLength = 20,  ## number of rows to output for each page
                                                        scrollX = TRUE,   ## enable scrolling on X axis
                                                        scrollY = "800px",   ## enable scrolling on Y axis
                                                        autoWidth = FALSE, ## use smart column width handling
                                                        server = FALSE,   ## use client-side processing
                                                        dom = 'Bfrtip',
                                                        columnDefs = list(

                                                          # Centers columns
                                                          list(targets = '_all',
                                                               className = 'dt-center'),

                                                          # Removes columns
                                                          list(targets = c(0, 13),
                                                               visible = FALSE)

                                                        ))) %>%
        DT::formatRound(columns = IEATools::year_cols(data_wide), digits=3)

      return(allocations_table)

    } else {

      print("Error")

    }

    return(allocations_table)

  })


  output$download_data <- downloadHandler(

    filename = function() {

      paste("PFU_",
            as.character(unique(selected_data()$Ef.product)),
            "_",
            as.character(unique(selected_data()$Destination)),
            "_",
            Sys.Date(),
            ".csv",
            sep="")
    },

    content = function(file) {

      req(input$dataformat)

      if(input$dataformat == "Long"){

        data <- selected_data() %>%
          as.data.frame()


      } else if (input$dataformat == "Wide") {

        data <- selected_data() %>%
          as.data.frame() %>%
          tidyr::pivot_wider(names_from = "Year",
                             values_from = ".values")

      } else {

        print("Error")

      }

      write.csv(data, file)
    }

  )


  output$download_alldata <- downloadHandler(

    filename = function() {

      paste("PFU_",
            Sys.Date(),
            ".csv",
            sep="")
    },

    content = function(file) {

      req(input$dataformat)

      if(input$dataformat == "Long"){

        data <- comp_alloc_tables_prepped %>%
          as.data.frame()


      } else if (input$dataformat == "Wide") {

        data <- comp_alloc_tables_prepped %>%
          as.data.frame() %>%
          tidyr::pivot_wider(names_from = "Year",
                             values_from = ".values")

      } else {

        print("Error")

      }

      write.csv(data, file)
    }

  )

}


