# Loads required packages
library(tidyverse)

# Establishes UI module function
pfuex_etaUI <- function(id) {
  ns <- NS(id)

  fluidRow(

    tabBox(title = "PFU EX Efficiency",
           id = "pfuex.eta",
           width = 10,
           height = 920,

           tabPanel(title = "Plots",
                    plotly::plotlyOutput(outputId = ns("eta_plot"))),

           tabPanel(title = "Data",
                    DT::dataTableOutput(outputId = ns("eta_datatable")),
                    style = "font-size:78%")
           ),

    tabBox(#title = "Variables",
           width = 2,
           tabPanel(title = "Options",
                    selectizeInput(inputId = ns("country"),
                                   label = "Country:",
                                   choices = country_options_agg,
                                   multiple = TRUE,
                                   width = "100%",
                                   options = list(dropdownParent = 'body')),

                    selectizeInput(inputId = ns("iea_andor_mw"),
                                   label = "IEA and/or MW:",
                                   choices = iea.mw_options,
                                   width = "100%",
                                   options = list(dropdownParent = 'body')),

                    selectizeInput(inputId = ns("indexed"),
                                   label = "Index Data:",
                                   choices = c(Yes = "Eta.i",
                                               No = "Eta"),
                                   width = "100%",
                                   selected = "Eta",
                                   options = list(dropdownParent = 'body')),

                    selectizeInput(inputId = ns("EorX"),
                                   label = "Energy Type:",
                                   choices = ex_options,
                                   width = "100%",
                                   options = list(dropdownParent = 'body')),

                    selectizeInput(inputId = ns("stages"),
                                   label = "Eta Stages:",
                                   choices = stages_options,
                                   multiple = TRUE %>%
                                     sort(),
                                   selected = c("Primary-Final", "Primary-Useful", "Final-Useful"),
                                   width = "100%",
                                   options = list(dropdownParent = 'body')),

                    selectizeInput(inputId = ns("gross_net"),
                                   label = "Gross or Net:",
                                   choices = gronet_options,
                                   width = "100%",
                                   options = list(dropdownParent = 'body')),

                    selectizeInput(inputId = ns("aggby"),
                                   label = "Aggregation:",
                                   choices = c(Total = "Total",
                                               Product = "Product",
                                               Sector = "Sector"),
                                   width = "100%",
                                   options = list(dropdownParent = 'body')),

                    selectizeInput(inputId = ns("dataformat"),
                                   label = "Data Format:",
                                   choices = c("Long", "Wide"),
                                   width = "100%",
                                   options = list(dropdownParent = 'body')),

                    selectizeInput(inputId = ns("facet_variable"),
                                   label = "Facet Variable:",
                                   choices = c(Country = "Country",
                                               Stages = "Stages"),
                                   selected = "Country",
                                   multiple = TRUE,
                                   width = "100%",
                                   options = list(dropdownParent = 'body')),

                    selectizeInput(inputId = ns("facet_scales"),
                                   label = "Facet Scales:",
                                   choices = c(Fixed = "fixed",
                                               `Variable Y Axis` = "free_y"),
                                   width = "100%",
                                   options = list(dropdownParent = 'body')),


           ),

           tabPanel(title = "Download",

                    tags$h5(tags$b("Download Selected Data")),
                    tags$div(class="noindent",
                             tags$ul(style="font-size:75%; list-style: none; margin:0; padding:0",
                                     tags$li("Includes efficiency data as selected in the Options tab.")
                             )
                    ),
                    downloadButton(outputId = ns("download_data"),
                                   label = "Download",
                                   class = NULL,
                                   icon = shiny::icon("download")),

                    tags$br(),

                    tags$h5(tags$b("Selected Efficiency Consumption Data")),
                    tags$div(class="noindent",
                             tags$ul(style="font-size:75%; list-style: none; margin:0; padding:0",
                                     tags$li("Includes all efficiency data for the selected countries in the Options tab.")
                             )
                    ),
                    downloadButton(outputId = ns("download_country_data"),
                                   label = "Download",
                                   class = NULL,
                                   icon = shiny::icon("download")),

                    tags$br(),

                    tags$h5(tags$b("Download All Data")),
                    tags$div(
                      tags$ul(style="font-size:75%; list-style: none; margin:0; padding:0",
                              tags$li("Includes all efficiency data for all countries.")
                      )
                    ),
                    downloadButton(outputId = ns("download_alldata"),
                                   label = "Download",
                                   class = NULL,
                                   icon = shiny::icon("download"))
                )
           )
    )
}

################################################################################
# Server
################################################################################

# Establishes the server module function
pfuex_eta <- function(input, output, session,
                      country,
                      EorX,
                      stages,
                      gross_net,
                      aggby,
                      stackfill,
                      facet_scales,
                      facet_variable,
                      dataformat) {

################################################################################
# Update events
################################################################################


################################################################################
# Select data
################################################################################

  # Creates reactive data frame for total aggregate PFU EX
  PSUT_Eta_Re_all_St_pfu_plotdata <- reactive({

    validate(
      need(input$country != "", "Please select atleast one country"),
      need(input$stages != "", "Please select atleast one ECC stage")
    )

    data <- EtaData_i

    data$Stages <- factor(data$Stages,
                          levels = c("Primary-Useful", "Primary-Final", "Final-Useful"))

    data_filtered <- data %>%
      dplyr::filter(Country %in% input$country) %>%
      dplyr::filter(Energy.type == input$EorX) %>%
      dplyr::filter(Gross.Net == input$gross_net) %>%
      dplyr::filter(Stages %in% input$stages) %>%
      dplyr::filter(IEA.MW == input$iea_andor_mw)


  })


################################################################################
# Outputs - plots
################################################################################

  # EX plot - aggreated by selected value
  output$eta_plot <- renderPlotly({

    req(input$facet_variable)

    facets <- input$facet_variable %>%
      stringr::str_replace_all(",", "+") %>%
      rlang::parse_exprs()

    plot_data <- PSUT_Eta_Re_all_St_pfu_plotdata()

    facet_variable_collapsed <- paste(input$facet_variable, collapse = " ")


    if(facet_variable_collapsed == "Country"){

      facet_col <- NULL
      facet_row <- NULL
      groupcolor_val <- "Stages"
      legend_val <- "right"
      # cols <- stage_cols

    } else if(facet_variable_collapsed == "Stages"){

      facet_col <- 3
      facet_row <- NULL
      groupcolor_val <- "Country"
      legend_val <- "right"
      # cols <- country_cols

    }
    else if(facet_variable_collapsed == "Country Stages"){
    # else if(isTRUE(all.equal(input$facet_variable, c("Country", "Stage"))) || isTRUE(all.equal(input$facet_variable, c("Stage", "Country")))){
    # else if(identical(~get(input$facet_variable), c("Country", "Stage")) || identical(~get(input$facet_variable), c("Stage", "Country"))){

      facet_col <- 3
      facet_row <- NULL
      groupcolor_val <- "Stages"
      legend_val <- "none"

    }
    else if(facet_variable_collapsed == "Stages Country"){
      # else if(isTRUE(all.equal(input$facet_variable, c("Country", "Stage"))) || isTRUE(all.equal(input$facet_variable, c("Stage", "Country")))){
      # else if(identical(~get(input$facet_variable), c("Country", "Stage")) || identical(~get(input$facet_variable), c("Stage", "Country"))){

      facet_col <- NULL
      facet_row <- 3
      groupcolor_val <- "E.dot"
      legend_val <- "none"

    }

    # Builds ggplot object
    p <- ggplot2::ggplot(data = plot_data) +

      ggplot2::geom_line(mapping = ggplot2::aes(x = Year,
                                                y = .data[[!!input$indexed]],
                                                group = .data[[!!groupcolor_val]],
                                                color = .data[[!!groupcolor_val]]
                                                )) +

      # ggplot2::scale_colour_manual(values = cols) +
      ggplot2::facet_wrap(dplyr::vars(!!!facets),
                          nrow = facet_row,
                          ncol = facet_col,
                          scales = input$facet_scales) +

      ggplot2::scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10)) +

      ggplot2::xlab("") +
      ggplot2::ylab(input$indexed) +
      MKHthemes::xy_theme() +
      ggplot2::theme(panel.spacing.y = unit(1, "lines"),
                     legend.position = legend_val)

    p_plotly <- plotly::ggplotly(p,
                                 height = 850,
                                 tooltip = c("Year",
                                             input$indexed,
                                             # "Stage",
                                             "Country",
                                             "Energy.type",
                                             "Gross.Net"
    )) %>%

      plotly::layout(showlegend = as.logical(input$legend),
                     legend = list(itemclick = TRUE,
                                   itemdoubleclick = TRUE)) #%>%

    # move_legend_annotation_no_facet(y = 0.925, mar = 80)

  })


################################################################################
# Outputs - tables
################################################################################

  # Consumption data
  output$eta_datatable <- DT::renderDataTable({

    req(input$dataformat)

    if(input$dataformat == "Long"){

      data_long <- PSUT_Eta_Re_all_St_pfu_plotdata() %>%
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
        DT::formatRound(columns = c("Eta"), digits=3)

    } else if (input$dataformat == "Wide") {

      data_wide <- PSUT_Eta_Re_all_St_pfu_plotdata() %>%
        as.data.frame() %>%
        tidyr::pivot_wider(id_cols = c(Country, Method, Energy.type, Gross.Net, Stages),
                           names_from = "Year",
                           values_from = "Eta")

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


################################################################################
# Outputs - downloads
################################################################################

  output$download_data <- downloadHandler(

    filename = function() {

      paste("PFU.Efficiency.Data.",
            as.character(gsub(x = gsub(x = Sys.time(),
                                       pattern = "\\s",
                                       replacement = "."),
                              pattern = ":",
                              replacement = "-")),
            ".csv",
            sep="")
    },

    content = function(file) {

      req(input$dataformat)

      if(input$dataformat == "Long"){

        data <- PSUT_Eta_Re_all_St_pfu_plotdata() %>%
          as.data.frame()


      } else if (input$dataformat == "Wide") {

        data <- PSUT_Eta_Re_all_St_pfu_plotdata() %>%
          as.data.frame() %>%
          tidyr::pivot_wider(names_from = "Year",
                             values_from = "Eta")

      } else {

        print("Error")

      }

      write.csv(data, file, row.names = FALSE)
    }

  )

  # Download all allocations data for the countries selected in the options tab
  output$download_country_data <- downloadHandler(

    filename = function() {

      paste0("PFU.",
             "SelectedCountries",
             # gsub(x = toString(unique(PSUT_Eta_Re_all_St_pfu_plotdata()$Country)),
             #      pattern = ",\\s",
             #      replacement = "."),
             ".Efficiency.Data.",
             gsub(x = gsub(x = Sys.time(),
                           pattern = "\\s",
                           replacement = "."),
                  pattern = ":",
                  replacement = "-"),
             ".csv",
             sep="")
    },

    content = function(file) {

      req(input$dataformat)

      if(input$dataformat == "Long"){

        data <- EtaData %>%
          dplyr::filter(Country %in% input$country) %>%
          as.data.frame()


      } else if (input$dataformat == "Wide") {

        data <- EtaData %>%
          as.data.frame() %>%
          dplyr::filter(Country %in% input$country) %>%
          tidyr::pivot_wider(names_from = "Year",
                             values_from = ".values")

      } else {

        print("Error")

      }

      write.csv(data, file, row.names = FALSE)
    }

  )


  output$download_alldata <- downloadHandler(

    filename = function() {

      paste("PFU.All.Efficiency.Data.",
            as.character(gsub(x = gsub(x = Sys.time(),
                                       pattern = "\\s",
                                       replacement = "."),
                              pattern = ":",
                              replacement = "-")),
            ".csv",
            sep="")
    },

    content = function(file) {

      req(input$dataformat)

      if(input$dataformat == "Long"){

        data <- EtaData %>%
          as.data.frame()


      } else if (input$dataformat == "Wide") {

        data <- EtaData %>%
          as.data.frame() %>%
          tidyr::pivot_wider(names_from = "Year",
                             values_from = "Eta")

      } else {

        print("Error")

      }

      write.csv(data, file, row.names = FALSE)
    }

  )

}


