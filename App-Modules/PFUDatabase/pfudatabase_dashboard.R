# Loads required packages
library(tidyverse)

# Establishes UI module function
sumdashplotsUI <- function(id) {
  ns <- NS(id)

  fluidRow(

    use_cicerone(),

    column(width = 10,

           # Consumption by total
           box(
             width = 6,
             height = 390,
             title = "Total Consumption",
             id = "con_by_total",
             closable = FALSE,
             # status = "warning",
             solidHeader = FALSE,
             collapsible = TRUE,
             enable_sidebar = TRUE,
             sidebar_width = 25,
             sidebar_start_open = FALSE,
             sidebar_background = "#FFFFFF",
             sidebar_title = "Variables",
             sidebar_content = tagList(),
             plotlyOutput(outputId = ns("pfudash_e.dot"))
           ),

           # Indexed Data box and sidebar
           box(
             width = 6,
             height = 390,
             title = "Indexed Consumption",
             id = "index_data",
             closable = FALSE,
             # status = "warning",
             solidHeader = FALSE,
             collapsible = TRUE,
             enable_sidebar = TRUE,
             sidebar_width = 25,
             sidebar_start_open = FALSE,
             sidebar_background = "#FFFFFF",
             sidebar_title = "Variables",
             sidebar_content = tagList(),
             plotlyOutput(outputId = ns("pfudash_e.dot.i"))
           ),

           # Consumption by total
           box(
             width = 6,
             height = 390,
             title = "Total Efficiency",
             id = "eta",
             closable = FALSE,
             # status = "warning",
             solidHeader = FALSE,
             collapsible = TRUE,
             enable_sidebar = TRUE,
             sidebar_width = 25,
             sidebar_start_open = FALSE,
             sidebar_background = "#FFFFFF",
             sidebar_title = "Variables",
             sidebar_content = tagList(),
             plotlyOutput(outputId = ns("pfudash_eta"))
           ),

           # Indexed Data box and sidebar
           box(
             width = 6,
             height = 390,
             title = "Indexed Efficiency",
             id = "indexed_eta",
             closable = FALSE,
             # status = "warning",
             solidHeader = FALSE,
             collapsible = TRUE,
             enable_sidebar = TRUE,
             sidebar_width = 25,
             sidebar_start_open = FALSE,
             sidebar_background = "#FFFFFF",
             sidebar_title = "Variables",
             sidebar_content = tagList(),
             plotlyOutput(outputId = ns("pfudash_eta.i"))
           ),

           # Consumption by product
           box(
             width = 6,
             height = 390,
             title = "Consumption by Product",
             id = "con_by_prod",
             closable = FALSE,
             # status = "warning",
             solidHeader = FALSE,
             collapsible = TRUE,
             enable_sidebar = TRUE,
             sidebar_width = 25,
             sidebar_start_open = FALSE,
             sidebar_background = "#FFFFFF",
             sidebar_title = "Variables",
             sidebar_content = tagList()#,
             # plotlyOutput(outputId = ns("sumdash_prod"))
           ),

           # Consumption by Flow or Sector
           box(
             width = 6,
             height = 390,
             title = "Consumption by Sector",
             id = "con_by_flowsec",
             closable = FALSE,
             # status = "warning",
             solidHeader = FALSE,
             collapsible = TRUE,
             enable_sidebar = TRUE,
             sidebar_width = 25,
             sidebar_start_open = FALSE,
             sidebar_background = "#FFFFFF",
             sidebar_title = "Variables",
             sidebar_content = tagList()#,
             # plotlyOutput(outputId = ns("sumdash_flowsec"))

             )

           ),

    column(width = 2,

           box(
             title = "Variables",
             id = "Variables_pfu",
             # status = "primary",
             solidHeader = FALSE,
             closable = FALSE,
             collapsible = TRUE,
             width = 12,
             # height = 100,

             selectizeInput(inputId = ns("country"),
                            label = "Country:",
                            choices = country_options_agg,
                            # width = "125px",
                            options = list(dropdownParent = 'body')),

             selectizeInput(inputId = ns("iea_andor_mw"),
                            label = "IEA and/or MW:",
                            choices = iea.mw_options,
                            # width = "125px",
                            options = list(dropdownParent = 'body')),

             selectizeInput(inputId = ns("EorX"),
                            label = "Energy Quantification:",
                            choices = c(Energy = "E", `Exergy` = "X"),
                            # width = "125px",
                            options = list(dropdownParent = 'body')),

             selectizeInput(inputId = ns("stage"),
                            label = "ECC Stage:",
                            choices = c(Primary = "Primary",
                                        Final = "Final",
                                        Useful = "Useful"),
                            # width = "125px",
                            options = list(dropdownParent = 'body')),

             selectizeInput(inputId = ns("stages"),
                            label = "Eta Stages:",
                            choices = c(`Primary-Final` = "Primary-Final",
                                        `Primary-Useful` = "Primary-Useful",
                                        `Final-Useful` = "Final-Useful"),
                            multiple = FALSE,
                            # width = "125px",
                            options = list(dropdownParent = 'body')),

             selectizeInput(inputId = ns("gross_net"),
                            label = "Gross or Net:",
                            choices = c(Gross = "Gross",
                                        Net = "Net")
                            %>% sort(),
                            # width = "125px",
                            options = list(dropdownParent = 'body')),

             selectizeInput(inputId = ns("legend"),
                            label = "Show Legend:",
                            choices = c(Yes = "TRUE", No = "FALSE"),
                            # width = "125px",
                            options =  list(dropdownParent = 'body')),

             selectizeInput(inputId = ns("stackfill"),
                            label = "Stack or Fill Area Plots:",
                            choices = c(Stack = "Stack", Fill = "Fill"),
                            # width = "125px",
                            options = list(dropdownParent = 'body')),

             actionButton(inputId = ns("pfu_dash_button"),
                          label = "Help",
                          style = 'margin-top:25px;
                            margin-left:30px;')
           )

           )

    )

}

# Establishes the server module function
sumdashplots <- function(input, output, session,
                         EorX,
                         country,
                         stage,
                         stages_rs,
                         gross_net,
                         iea_andor_mw,
                         rollavg,
                         legend,
                         stackfill
                         ) {

################################################################################
# Update Events #
################################################################################

  observeEvent(input$pfu_dash_button, {
    guide_pfu_dash$start()
  })


################################################################################
# Select Data #
################################################################################

# Creates reactive data frame for total aggregate PFU EX
  AggData_plotdata <- reactive({

    validate(
      # need(input$stage != "", "Please select atleast one Energy Conversion Chain (ECC) stage")
    )

    data <- AggData_i %>%
      dplyr::filter(Energy.type == input$EorX) %>%
      dplyr::filter(Gross.Net == input$gross_net) %>%
      dplyr::filter(Country == input$country) %>%
      dplyr::filter(IEA.MW == input$iea_andor_mw)

    data$Stage <- factor(data$Stage,
                         levels = c("Primary", "Final", "Useful"))

    data


  })

  # Creates reactive data frame for total aggregate PFU EX
  EtaData_plotdata <- reactive({

    validate(
      # need(input$stage != "", "Please select atleast one Energy Conversion Chain (ECC) stage")
    )

    data <- EtaData_i %>%
      dplyr::filter(Energy.type == input$EorX) %>%
      dplyr::filter(Gross.Net == input$gross_net) %>%
      dplyr::filter(Country == input$country) %>%
      dplyr::filter(IEA.MW == input$iea_andor_mw)

    data$Stage <- factor(data$Stages,
                         levels = c("Primary-Final", "Primary-Useful", "Final-Useful"))

    data

  })


################################################################################
# Outputs #
################################################################################

  # Plots total data
  output$pfudash_e.dot <- renderPlotly({

    p <- ggplot2::ggplot(data = AggData_plotdata()) +

      ggplot2::geom_line(mapping = aes(x = Year,
                                       y = E.dot,
                                       # group = Stage,
                                       color = Stage)) +

      ggplot2::scale_colour_manual(values = stage.cols) +

      ggplot2::scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10)) +

      ggplot2::xlab("") +
      ggplot2::ylab("E.dot [ktoe]") +
      MKHthemes::xy_theme()

    p_plotly <- plotly::ggplotly(p, height = 380, tooltip = c("Year",
                                                              "E.dot",
                                                              "Stage"#,
                                                              # "Energy.type",
                                                              # "Gross.Net"
                                                              )) %>%

      plotly::layout(showlegend = as.logical(input$legend),
                     legend = list(itemclick = TRUE,
                                   itemdoubleclick = TRUE)) #%>%

      # move_legend_annotation_no_facet(y = 0.925, mar = 80)

  })

  # Plots indexed data
  output$pfudash_e.dot.i <- renderPlotly({

    p <- ggplot2::ggplot(data = AggData_plotdata()) +

      ggplot2::geom_line(mapping = aes(x = Year,
                                       y = E.dot.i,
                                       # group = Stage,
                                       color = Stage)) +

      ggplot2::scale_colour_manual(values = stage.cols) +

      ggplot2::scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10)) +

      ggplot2::xlab("") +
      ggplot2::ylab("E.dot [i]") +
      MKHthemes::xy_theme()

    p_plotly <- plotly::ggplotly(p, height = 380, tooltip = c("Year",
                                                              "E.dot.i",
                                                              "Stage"#,
                                                              # "Energy.type",
                                                              # "Gross.Net"
    )) %>%

      plotly::layout(showlegend = as.logical(input$legend),
                     legend = list(itemclick = TRUE,
                                   itemdoubleclick = TRUE)) #%>%

    # move_legend_annotation_no_facet(y = 0.925, mar = 80)

  })



  # Plots total data
  output$pfudash_eta <- renderPlotly({

    p <- ggplot2::ggplot(data = EtaData_plotdata()) +

      ggplot2::geom_line(mapping = aes(x = Year,
                                       y = Eta,
                                       # group = Stage,
                                       color = Stages)) +

      ggplot2::scale_colour_manual(values = stages.cols) +

      ggplot2::scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10)) +

      ggplot2::xlab("") +
      ggplot2::ylab("Eta [-]") +
      MKHthemes::xy_theme()

    p_plotly <- plotly::ggplotly(p, height = 380, tooltip = c("Year",
                                                              "Eta",
                                                              "Stages"#,
                                                              # "Energy.type",
                                                              # "Gross.Net"
    )) %>%

      plotly::layout(showlegend = as.logical(input$legend),
                     legend = list(itemclick = TRUE,
                                   itemdoubleclick = TRUE)) #%>%

    # move_legend_annotation_no_facet(y = 0.925, mar = 80)

  })

  # Plots indexed data
  output$pfudash_eta.i <- renderPlotly({

    p <- ggplot2::ggplot(data = EtaData_plotdata()) +

      ggplot2::geom_line(mapping = aes(x = Year,
                                       y = Eta.i,
                                       # group = Stage,
                                       color = Stages)) +

      ggplot2::scale_colour_manual(values = stages.cols) +

      ggplot2::scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10)) +

      ggplot2::xlab("") +
      ggplot2::ylab("Eta.i [-]") +
      MKHthemes::xy_theme()

    p_plotly <- plotly::ggplotly(p, height = 380, tooltip = c("Year",
                                                              "Eta.i",
                                                              "Stages"#,
                                                              # "Energy.type",
                                                              # "Gross.Net"
    )) %>%

      plotly::layout(showlegend = as.logical(input$legend),
                     legend = list(itemclick = TRUE,
                                   itemdoubleclick = TRUE)) #%>%

    # move_legend_annotation_no_facet(y = 0.925, mar = 80)

  })

}


