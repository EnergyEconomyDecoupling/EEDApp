# Loads required packages
library(tidyverse)

# Establishes UI module function
etaplotsUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    tabBox(title = "Final-to-Useful Efficiencies",
           id = "eta",
           width = 10,
           height = 920,
           tabPanel(title = "Plots",
                    plotlyOutput(outputId = ns("eta_plot"))
                    ),
           tabPanel(title = "Data",
                    DT::dataTableOutput(outputId = ns("eta_data")),
                    style = "font-size:78%"
                    )
           ),

    box(title = "Variables",
        solidHeader = TRUE,
        width = 2,

        selectizeInput(inputId = ns("country"),
                       label = "Country:",
                       choices = countries,
                       multiple = TRUE
                       %>% sort()),

        # selectInput(inputId = ns("metric"),
        #             label = "Metric",
        #             choices = c(`Energy efficiency` = "eta.fu",
        #                         `Exergy efficiency` = "etaX.fu"#,
        #                         # `Exergy-to-energy ratio` = "phi.u"
        #                         )),

        selectInput(inputId = ns("machine"),
                    label = "Final-to-useful machine:",
                    choices = unique(comp_effic_tables_prepped$Machine)
                    %>% sort()),

        selectInput(inputId = ns("euproduct"),
                    label = "Useful product:",
                    choices = unique(comp_effic_tables_prepped$Eu.product)
                    %>% sort()),

        selectInput(inputId = ns("dataformat"),
                    label = "Data Format:",
                    choices = c("Wide", "Long")),

        tags$h5(tags$b("Download Selected Data")),

        downloadButton(outputId = ns("download_data"),
                       label = "Download",
                       class = NULL,
                       icon = shiny::icon("download")),

        tags$h5(tags$b("Download All Data")),

        downloadButton(outputId = ns("download_alldata"),
                       label = "Download",
                       class = NULL,
                       icon = shiny::icon("download"))

        ) # Closes variables box

  ) # Closes fluidrow

} # Closes UI

# Establishes the server module function
etaplots <- function(input, output, session,
                     country,
                     metric,
                     machine,
                     euproduct) {

  # Creates a dataframe with the selected country, machine, and Eu.product
  selected_data <- reactive({
    validate(
      # need(input$metric != "", "Please select atleast one Energy quantification"),
      need(input$country != "", "Please select atleast one Country"),
      need(input$machine != "", "Please select one Machine"),
      need(input$euproduct != "", "Please select one Useful work product")
    )
    dplyr::filter(comp_effic_tables_prepped,
                  # Quantity == input$metric,
                  Country %in% input$country,
                  Machine == input$machine,
                  Eu.product == input$euproduct)
    })

  # These observe events update the variables for selection
  observeEvent(input$country,  {
    req(input$country)
    post_country_data <- comp_effic_tables_prepped %>%
      dplyr::filter(Country %in% input$country)

    updateSelectInput(session,
                      inputId = "machine",
                      choices = sort(unique(post_country_data$machine)))
    })

  observeEvent(input$machine,  {
    req(input$country)
    req(input$machine)
    post_machine_data <- comp_effic_tables_prepped %>%
      dplyr::filter(Country %in% input$country) %>%
      dplyr::filter(Machine %in% input$machine)

    updateSelectInput(session,
                      inputId = "euproduct",
                      choices = sort(unique(post_machine_data$Eu.product)))
    })

  # Machine - Useful work combination eta.fu plots
  output$eta_plot <- renderPlotly({

      p <- ggplot2::ggplot(data = selected_data()) +
        ggplot2::geom_line(mapping = ggplot2::aes(x = Year, y = .values, colour = Country)) +
        ggplot2::scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
        ggplot2::scale_x_continuous(limits = c(1960, 2020), breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
        ggplot2::xlab("") +
        ggplot2::ylab("Final-to-useful Efficiency [-]") +
        MKHthemes::xy_theme()

      plotly::ggplotly(p,
                       height = 850,
                       tooltip = c("Year", ".values", "Country")) #%>%
        # move_legend_annotation_no_facet(y = 0.97, mar = 80)

    })

  output$eta_data <- DT::renderDataTable({

    req(input$dataformat)

    if(input$dataformat == "Long"){

      data_long <- selected_data() %>%
        as.data.frame()

      eta_table <- DT::datatable(data = data_long,
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
                                                  list(targets = c(0),
                                                       visible = FALSE)

                                                ))) %>%
        DT::formatRound(columns=c(".values"), digits=3)

    } else if (input$dataformat == "Wide") {

      data_wide <- selected_data() %>%
        as.data.frame() %>%
        tidyr::pivot_wider(names_from = "Year",
                           values_from = ".values")

      eta_table <- DT::datatable(data = data_wide,
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
                                                  list(targets = c(0),
                                                       visible = FALSE)

                                                ))) %>%
        DT::formatRound(columns = IEATools::year_cols(data_wide), digits=3)

    } else {

      print("Error")

    }

    return(eta_table)

  })

  output$download_data <- downloadHandler(

    filename = function() {

      paste("PFU_",
            as.character(unique(selected_data()$Machine)),
            "_",
            as.character(unique(selected_data()$Eu.product)),
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

        data <- eta_data %>%
          as.data.frame()


      } else if (input$dataformat == "Wide") {

        data <- eta_data %>%
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


