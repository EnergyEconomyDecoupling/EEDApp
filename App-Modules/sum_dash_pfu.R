# Loads required packages
library(tidyverse)

# Establishes UI module function
sumdashplotsUI <- function(id) {
  ns <- NS(id)

  fluidRow(

    use_cicerone(),

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

        cellArgs = list(style = "padding: 0px"),

        selectizeInput(inputId = ns("country"),
                       label = "Country:",
                       choices = country_options,
                       width = "125px",
                       options = list(dropdownParent = 'body')),

        selectizeInput(inputId = ns("iea_andor_mw"),
                       label = "IEA and/or MW:",
                       choices = c(IEA = "IEA",
                                   MW = "MW",
                                   Both = "both"),
                       width = "125px",
                       options = list(dropdownParent = 'body')),

        selectizeInput(inputId = ns("EorX"),
                       label = "Energy Quantification:",
                       choices = c(Energy = "E", `Exergy` = "X"),
                       width = "125px",
                       options = list(dropdownParent = 'body')),

        selectizeInput(inputId = ns("stage"),
                       label = "ECC Stage:",
                       choices = c(Primary = "Primary",
                                   Final = "Final",
                                   Useful = "Useful"),
                       width = "125px",
                       options = list(dropdownParent = 'body')),

        selectizeInput(inputId = ns("gross_net"),
                       label = "Gross or Net:",
                       choices = c(Gross = "Gross",
                                   Net = "Net")
                       %>% sort(),
                       width = "125px",
                       options = list(dropdownParent = 'body')),

        # selectizeInput(inputId = ns("percap"),
        #                label = "Per Capita:",
        #                choices = c(No = "abs", Yes = "pc"),
        #                width = "150px",
        #                options = list(dropdownParent = 'body')),

        selectizeInput(inputId = ns("legend"),
                       label = "Show Legend:",
                       choices = c(Yes = "TRUE", No = "FALSE"),
                       width = "125px",
                       options =  list(dropdownParent = 'body')),

        selectizeInput(inputId = ns("stackfill"),
                       label = "Stack or Fill Area Plots:",
                       choices = c(Stack = "Stack", Fill = "Fill"),
                       width = "125px",
                       options = list(dropdownParent = 'body')),

        actionButton(inputId = ns("pfu_dash_button"),
                     label = "Help",
                     style = 'margin-top:25px;
                              margin-left:30px;')
        )),

    # Consumption by total
    box(
      width = 6,
      # height = 450,
      title = "Consumption by Total",
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
      plotlyOutput(outputId = ns("sumdash_total"))
    ),

    # Consumption by product
    box(
      width = 6,
      # height = 450,
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
      # height = 450,
      title = "Consumption by Flow or Sector",
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
      ),

    # Indexed Data box and sidebar
    box(
      width = 4,
      # height = 450,
      title = "Indexed Data",
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
      sidebar_content = tagList()#,
      # plotlyOutput(outputId = ns("sumdash_i"))
      ),

    # Decomposition Analysis box and sidebar
    box(
      width = 4,
      # height = 450,
      title = "Decomposition Analysis",
      closable = FALSE,
      # status = "warning",
      solidHeader = FALSE,
      collapsible = TRUE,
      enable_sidebar = TRUE,
      sidebar_width = 25,
      sidebar_start_open = FALSE,
      sidebar_background = "#FFFFFF",
      sidebar_title = "Variables",
      sidebar_content = tagList()
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
  PSUT_Agg_Re_all_St_pfu_plotdata <- reactive({

    validate(
      # need(input$stage != "", "Please select atleast one Energy Conversion Chain (ECC) stage")
    )

    data <- PSUT_Agg_Re_all_St_pfu_prepped %>%
      dplyr::filter(Energy.type == input$EorX) %>%
      dplyr::filter(Gross.Net == input$gross_net) %>%
      dplyr::filter(Country == input$country)

    data$Stage <- factor(data$Stage,
                         levels = c("Primary", "Final", "Useful"))

    data


  })


################################################################################
# Outputs #
################################################################################

  # Creates color scheme
  cols <- c("Primary" = "red4",
            "Final" = "red",
            "Useful" = "orange")

  # Plots indexed data
  output$sumdash_total <- renderPlotly({

    p <- ggplot2::ggplot(data = PSUT_Agg_Re_all_St_pfu_plotdata()) +

      ggplot2::geom_line(mapping = aes(x = Year,
                                       y = E.dot,
                                       # group = Stage,
                                       color = Stage)) +

      ggplot2::scale_colour_manual(values = cols) +

      ggplot2::scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10)) +

      ggplot2::xlab("") +
      ggplot2::ylab("E.dot [ktoe]") +
      MKHthemes::xy_theme()

    p_plotly <- plotly::ggplotly(p, height = 400, tooltip = c("Year",
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

}


