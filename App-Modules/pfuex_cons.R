# Establishes UI module function
pfuex_consUI <- function(id) {
  ns <- NS(id)

  fluidRow(

    tabBox(title = "Consumption of PFU Energy and Exergy",
           id = "pfuex",
           width = 10,
           height = 920,

           tabPanel(title = "Plots",
                    plotly::plotlyOutput(outputId = ns("consumption_plot"))),

           tabPanel(title = "Data",
                    DT::dataTableOutput(outputId = ns("consumption_datatable")),
                    style = "font-size:78%")
           ),

    tabBox(#title = "Variables",
           width = 2,
           tabPanel(title = "Options",
                    selectizeInput(inputId = ns("iea_andor_mw"),
                                   label = "IEA and/or MW:",
                                   choices = c(IEA = "IEA",
                                               MW = "MW",
                                               Both = "both"),
                                   width = "100%",
                                   options = list(dropdownParent = 'body')),

                    selectizeInput(inputId = ns("indexed"),
                                   label = "Index Data:",
                                   choices = c(Yes = "Yes",
                                               No = "No"),
                                   width = "100%",
                                   options = list(dropdownParent = 'body')),

                    selectizeInput(inputId = ns("country"),
                                   label = "Country:",
                                   choices = country_options,
                                   multiple = TRUE,
                                   width = "100%",
                                   options = list(dropdownParent = 'body')),

                    selectizeInput(inputId = ns("EorX"),
                                   label = "Energy Type:",
                                   choices = c(Energy = "E", `Exergy` = "X"),
                                   width = "100%",
                                   options = list(dropdownParent = 'body')),

                    selectizeInput(inputId = ns("stage"),
                                   label = "ECC Stage:",
                                   choices = c(Primary = "Primary",
                                               Final = "Final",
                                               Useful = "Useful"),
                                   multiple = TRUE %>%
                                     sort(),
                                   selected = c("Primary", "Final", "Useful"),
                                   width = "100%",
                                   options = list(dropdownParent = 'body')),

                    selectizeInput(inputId = ns("gross_net"),
                                   label = "Gross or Net:",
                                   choices = c(Gross = "Gross",
                                               Net = "Net") %>%
                                     sort(),
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
                                               Stage = "Stage"),
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

                    downloadButton(outputId = ns("download_data"),
                                   label = "Download",
                                   class = NULL,
                                   icon = shiny::icon("download")),

                    tags$h5(tags$b("Download All Data")),

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
pfuex_cons <- function(input, output, session,
                       country,
                       EorX,
                       stage,
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
  PSUT_Agg_Re_all_St_pfu_plotdata <- reactive({

    validate(
      need(input$country != "", "Please select atleast one country"),
      need(input$stage != "", "Please select atleast one ECC stage")
    )

    data <- PSUT_Agg_Re_all_St_pfu_prepped

    data$Stage <- factor(data$Stage,
                         levels = c("Primary", "Final", "Useful"))

    data_filtered <- data %>%
      dplyr::filter(Country %in% input$country) %>%
      dplyr::filter(Energy.type == input$EorX) %>%
      dplyr::filter(Gross.Net == input$gross_net) %>%
      dplyr::filter(Stage %in% input$stage)

    # if(input$indexed == "Yes"){
    #
    #   data_indexed <- data_filtered
    #
    # }
    #
    # data_final

  })


################################################################################
# Outputs - plots
################################################################################


  # Creates colour scheme
  stage_cols <- c("Primary" = "red4",
                  "Final" = "red",
                  "Useful" = "orange")

  country_cols <- NULL


  # EX plot - aggreated by selected value
  output$consumption_plot <- renderPlotly({

    req(input$facet_variable)

    facets <- input$facet_variable %>%
      stringr::str_replace_all(",", "+") %>%
      rlang::parse_exprs()

    plot_data <- PSUT_Agg_Re_all_St_pfu_plotdata()

    facet_variable_collapsed <- paste(input$facet_variable, collapse = " ")


    if(facet_variable_collapsed == "Country"){

      facet_col <- NULL
      facet_row <- NULL
      groupcolor_val <- "Stage"
      legend_val <- "right"
      # cols <- stage_cols

    } else if(facet_variable_collapsed == "Stage"){

      facet_col <- 3
      facet_row <- NULL
      groupcolor_val <- "Country"
      legend_val <- "right"
      # cols <- country_cols

    }
    else if(facet_variable_collapsed == "Country Stage"){
    # else if(isTRUE(all.equal(input$facet_variable, c("Country", "Stage"))) || isTRUE(all.equal(input$facet_variable, c("Stage", "Country")))){
    # else if(identical(~get(input$facet_variable), c("Country", "Stage")) || identical(~get(input$facet_variable), c("Stage", "Country"))){

      facet_col <- 3
      facet_row <- NULL
      groupcolor_val <- "Stage"
      legend_val <- "none"

    }
    else if(facet_variable_collapsed == "Stage Country"){
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
                                                y = E.dot,
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
      ggplot2::ylab("E.dot [ktoe]") +
      MKHthemes::xy_theme() +
      ggplot2::theme(panel.spacing.y = unit(1, "lines"),
                     legend.position = legend_val)

    p_plotly <- plotly::ggplotly(p,
                                 height = 850,
                                 tooltip = c("Year",
                                             "E.dot",
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
  output$consumption_datatable <- DT::renderDataTable({

    req(input$dataformat)

    if(input$dataformat == "Long"){

      data_long <- PSUT_Agg_Re_all_St_pfu_plotdata() %>%
        as.data.frame()

      consumption_table <- DT::datatable(data = data_long,
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
        DT::formatRound(columns = c("E.dot"), digits=3)

    } else if (input$dataformat == "Wide") {

      data_wide <- PSUT_Agg_Re_all_St_pfu_plotdata() %>%
        as.data.frame() %>%
        tidyr::pivot_wider(id_cols = c(Country, Method, Energy.type, Gross.Net, Stage),
                           names_from = "Year",
                           values_from = "E.dot")

      consumption_table <- DT::datatable(data = data_wide,
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


    return(consumption_table)

  })


################################################################################
# Outputs - downloads
################################################################################

  output$download_data <- downloadHandler(

    filename = function() {

      paste("PFU_",
            as.character(unique(selected_data_consumption()$Aggregation.by)),
            ".Consumption.Data_",
            Sys.Date(),
            ".csv",
            sep="")
    },

    content = function(file) {

      req(input$dataformat)

      if(input$dataformat == "Long"){

        data <- selected_data_consumption() %>%
          as.data.frame()


      } else if (input$dataformat == "Wide") {

        data <- selected_data_consumption() %>%
          as.data.frame() %>%
          tidyr::pivot_wider(names_from = "Year",
                             values_from = "E.dot")

      } else {

        print("Error")

      }

      write.csv(data, file)
    }

  )


  output$download_alldata <- downloadHandler(

    filename = function() {

      paste("PFU_All.Consumption.Data_",
            Sys.Date(),
            ".csv",
            sep="")
    },

    content = function(file) {

      req(input$dataformat)

      if(input$dataformat == "Long"){

        data <- Agg_all_data %>%
          as.data.frame()


      } else if (input$dataformat == "Wide") {

        data <- Agg_all_data %>%
          as.data.frame() %>%
          tidyr::pivot_wider(names_from = "Year",
                             values_from = "E.dot")

      } else {

        print("Error")

      }

      write.csv(data, file)
    }

  )

}


